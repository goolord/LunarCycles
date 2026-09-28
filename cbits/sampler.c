/* The built-in sampler's mixer. It runs on SDL's audio thread, apart from
 * the Haskell runtime, so a garbage collection cannot starve the device.
 * Haskell hands it notes a little ahead of time; each note waits as a
 * pending voice until the block that contains its start frame. */

#include "sampler.h"

#include <SDL3/SDL.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if defined(__SSE__)
#include <xmmintrin.h>
#endif

#define MAX_VOICES 256
#define BLOCK 512
/* Frames a voice fades over when it starts partway into its sample, and
 * before the end of its part, so a cut does not click. */
#define FADE 64

struct lunar_sample {
  float *data;
  int frames;
  int channels;
};

enum { VOICE_FREE, VOICE_PENDING, VOICE_PLAYING };

typedef struct {
  int state;
  const lunar_sample *sample;
  int64_t start;
  double pos, rate;
  double lo, hi;
  int fade_in, played;
  float gain_l, gain_r;
  float shape;
  int filtered;
  float b0, b1, b2, a1, a2;
  float z1[2], z2[2];
} voice;

struct lunar_sampler {
  int rate;
  SDL_AudioStream *stream;
  SDL_Mutex *lock;
  int64_t rendered;
  /* SDL's clock, in seconds, at stream frame 0: how a delay from now
   * becomes a frame. The device's callbacks keep it current. */
  double origin;
  int callbacks;
  voice voices[MAX_VOICES];
};

static double clamp(double x, double lo, double hi) { return x < lo ? lo : x > hi ? hi : x; }

static double ticks(void) { return (double)SDL_GetTicksNS() / 1e9; }

const char *lunar_error(void) { return SDL_GetError(); }

lunar_sample *lunar_sample_load(const char *path, int rate) {
  SDL_AudioSpec src;
  Uint8 *raw;
  Uint32 len;
  if (!SDL_LoadWAV(path, &src, &raw, &len)) return NULL;
  int channels = src.channels >= 2 ? 2 : 1;
  SDL_AudioSpec dst = {SDL_AUDIO_F32, channels, rate};
  Uint8 *data;
  int bytes;
  bool ok = SDL_ConvertAudioSamples(&src, raw, (int)len, &dst, &data, &bytes);
  SDL_free(raw);
  if (!ok) return NULL;
  lunar_sample *sample = malloc(sizeof *sample);
  if (!sample) {
    SDL_free(data);
    SDL_OutOfMemory();
    return NULL;
  }
  sample->data = (float *)data;
  sample->channels = channels;
  sample->frames = bytes / (int)(channels * sizeof(float));
  return sample;
}

void lunar_sample_free(lunar_sample *sample) {
  if (!sample) return;
  SDL_free(sample->data);
  free(sample);
}

int lunar_sample_frames(const lunar_sample *sample) { return sample->frames; }

lunar_sampler *lunar_sampler_new(int rate) {
  lunar_sampler *s = calloc(1, sizeof *s);
  if (!s) return NULL;
  s->rate = rate;
  s->lock = SDL_CreateMutex();
  if (!s->lock) {
    free(s);
    return NULL;
  }
  return s;
}

/* The frame playing now, by the device's clock once it has one. */
static int64_t now_frame(lunar_sampler *s) {
  if (!s->stream || !s->callbacks) return s->rendered;
  return (int64_t)llround((ticks() - s->origin) * s->rate);
}

/* Each callback renders the frames the device needs next, so the time it
 * runs, less the frames already rendered, estimates the origin. The estimate
 * jitters with scheduling, so it is averaged: over every callback at first,
 * so it settles quickly, then over roughly the last hundred, to follow drift
 * between the device's clock and SDL's. A jump, such as after an underrun,
 * starts the average again. */
static void follow_device(lunar_sampler *s) {
  double estimate = ticks() - (double)s->rendered / s->rate;
  if (!s->callbacks || fabs(estimate - s->origin) > 0.05) s->callbacks = 0;
  s->callbacks++;
  double weight = s->callbacks < 100 ? 1.0 / s->callbacks : 0.01;
  s->origin += weight * (estimate - s->origin);
}

void lunar_sampler_play(lunar_sampler *s, const lunar_sample *sample, double delay,
                        double rate, double begin, double end, double gain, double pan,
                        double cutoff, double resonance, double shape) {
  if (!sample || sample->frames < 2 || fabs(rate) < 1e-4) return;
  double lo = clamp(begin, 0, 1) * sample->frames;
  double hi = clamp(end, 0, 1) * sample->frames;
  if (hi - lo < 1) return;

  voice v;
  memset(&v, 0, sizeof v);
  v.state = VOICE_PENDING;
  v.sample = sample;
  v.rate = rate;
  v.lo = lo;
  v.hi = hi;
  v.pos = rate > 0 ? lo : hi - 1;
  v.fade_in = (rate > 0 ? lo > 0 : hi < sample->frames) ? FADE : 0;

  /* SuperDirt's level: 0.4 at gain 1, rising with the fourth power. */
  double amp = 0.4 * pow(clamp(gain, 0, 2), 4);
  double p = clamp(pan, 0, 1);
  if (sample->channels == 1) {
    v.gain_l = (float)(amp * cos(p * SDL_PI_D / 2));
    v.gain_r = (float)(amp * sin(p * SDL_PI_D / 2));
  } else {
    v.gain_l = (float)(amp * fmin(1, 2 * (1 - p)));
    v.gain_r = (float)(amp * fmin(1, 2 * p));
  }

  /* SuperDirt's shape: (1 + k) x / (1 + k |x|), k = 2 shape / (1 - shape). */
  double sh = clamp(shape, 0, 0.99);
  v.shape = (float)(2 * sh / (1 - sh));

  /* SuperDirt's RLPF, whose reciprocal Q runs exponentially from 1 down as
   * resonance rises, here capped at a Q of 20 to spare ears and speakers. */
  if (cutoff > 0 && cutoff < 20000) {
    double fc = clamp(cutoff, 20, 0.45 * s->rate);
    double rq = fmax(pow(0.001, clamp(resonance, 0, 1)), 0.05);
    double w = 2 * SDL_PI_D * fc / s->rate;
    double cs = cos(w), alpha = sin(w) * rq / 2, a0 = 1 + alpha;
    v.filtered = 1;
    v.b0 = (float)((1 - cs) / 2 / a0);
    v.b1 = (float)((1 - cs) / a0);
    v.b2 = v.b0;
    v.a1 = (float)(-2 * cs / a0);
    v.a2 = (float)((1 - alpha) / a0);
  }

  SDL_LockMutex(s->lock);
  v.start = now_frame(s) + (int64_t)llround(delay * s->rate);
  voice *slot = NULL, *oldest = NULL;
  for (int i = 0; i < MAX_VOICES && !slot; i++) {
    voice *u = &s->voices[i];
    if (u->state == VOICE_FREE)
      slot = u;
    else if (u->state == VOICE_PLAYING && (!oldest || u->start < oldest->start))
      oldest = u;
  }
  if (!slot) slot = oldest;
  if (slot) *slot = v;
  SDL_UnlockMutex(s->lock);
}

void lunar_sampler_cancel(lunar_sampler *s) {
  SDL_LockMutex(s->lock);
  for (int i = 0; i < MAX_VOICES; i++)
    if (s->voices[i].state == VOICE_PENDING) s->voices[i].state = VOICE_FREE;
  SDL_UnlockMutex(s->lock);
}

static void render_voice(voice *v, float *out, int from, int to) {
  const lunar_sample *sample = v->sample;
  const float *d = sample->data;
  int stereo = sample->channels == 2, last = sample->frames - 1;
  double speed = fabs(v->rate);
  for (int i = from; i < to; i++) {
    if (v->pos < v->lo || v->pos >= v->hi) {
      v->state = VOICE_FREE;
      return;
    }
    int j = (int)v->pos, k = j < last ? j + 1 : last;
    float f = (float)(v->pos - j);
    float x[2];
    if (stereo) {
      x[0] = d[2 * j] + f * (d[2 * k] - d[2 * j]);
      x[1] = d[2 * j + 1] + f * (d[2 * k + 1] - d[2 * j + 1]);
    } else {
      x[0] = d[j] + f * (d[k] - d[j]);
    }
    float env = 1;
    double left = (v->rate > 0 ? v->hi - v->pos : v->pos - v->lo) / speed;
    if (left < FADE) env = (float)(left / FADE);
    if (v->played < v->fade_in) env *= (float)v->played / v->fade_in;
    v->played++;
    for (int c = 0; c < (stereo ? 2 : 1); c++) {
      float y = x[c];
      if (v->shape > 0) y = (1 + v->shape) * y / (1 + v->shape * fabsf(y));
      if (v->filtered) {
        float o = v->b0 * y + v->z1[c];
        v->z1[c] = v->b1 * y - v->a1 * o + v->z2[c];
        v->z2[c] = v->b2 * y - v->a2 * o;
        y = o;
      }
      x[c] = y * env;
    }
    if (!stereo) x[1] = x[0];
    out[2 * i] += x[0] * v->gain_l;
    out[2 * i + 1] += x[1] * v->gain_r;
    v->pos += v->rate;
  }
}

/* Unity up to 0.75, then bending smoothly towards 1. */
static float soft_clip(float x) {
  float a = fabsf(x);
  if (a <= 0.75f) return x;
  float y = 0.75f + 0.25f * tanhf((a - 0.75f) / 0.25f);
  return x < 0 ? -y : y;
}

static void mix(lunar_sampler *s, float *out, int frames) {
  memset(out, 0, sizeof(float) * 2 * (size_t)frames);
  int64_t end = s->rendered + frames;
  for (int i = 0; i < MAX_VOICES; i++) {
    voice *v = &s->voices[i];
    if (v->state == VOICE_PENDING && v->start < end) {
      /* A note already due starts at the top of the block. */
      int from = v->start > s->rendered ? (int)(v->start - s->rendered) : 0;
      v->state = VOICE_PLAYING;
      render_voice(v, out, from, frames);
    } else if (v->state == VOICE_PLAYING) {
      render_voice(v, out, 0, frames);
    }
  }
  s->rendered = end;
  for (int i = 0; i < 2 * frames; i++) out[i] = soft_clip(out[i]);
}

void lunar_sampler_render(lunar_sampler *s, float *out, int frames) {
  SDL_LockMutex(s->lock);
  while (frames > 0) {
    int n = frames < BLOCK ? frames : BLOCK;
    mix(s, out, n);
    out += 2 * n;
    frames -= n;
  }
  SDL_UnlockMutex(s->lock);
}

static void SDLCALL feed(void *userdata, SDL_AudioStream *stream, int additional, int total) {
  (void)total;
  lunar_sampler *s = userdata;
  float buf[2 * BLOCK];
#if defined(__SSE__)
  /* Flush denormals, which filter tails decay into, on this thread only. */
  _mm_setcsr(_mm_getcsr() | 0x8040);
#endif
  int frames = (additional + (int)(2 * sizeof(float)) - 1) / (int)(2 * sizeof(float));
  SDL_LockMutex(s->lock);
  follow_device(s);
  SDL_UnlockMutex(s->lock);
  while (frames > 0) {
    int n = frames < BLOCK ? frames : BLOCK;
    SDL_LockMutex(s->lock);
    mix(s, buf, n);
    SDL_UnlockMutex(s->lock);
    SDL_PutAudioStreamData(stream, buf, n * 2 * (int)sizeof(float));
    frames -= n;
  }
}

int lunar_sampler_start(lunar_sampler *s) {
  if (s->stream) return 1;
  if (!SDL_InitSubSystem(SDL_INIT_AUDIO)) return 0;
  SDL_AudioSpec spec = {SDL_AUDIO_F32, 2, s->rate};
  SDL_AudioStream *stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec, feed, s);
  if (!stream) {
    SDL_QuitSubSystem(SDL_INIT_AUDIO);
    return 0;
  }
  SDL_LockMutex(s->lock);
  s->stream = stream;
  SDL_UnlockMutex(s->lock);
  if (!SDL_ResumeAudioStreamDevice(stream)) {
    SDL_DestroyAudioStream(stream);
    SDL_QuitSubSystem(SDL_INIT_AUDIO);
    s->stream = NULL;
    return 0;
  }
  return 1;
}

void lunar_sampler_free(lunar_sampler *s) {
  if (!s) return;
  /* SDL_Quit, when the window closed first, has already closed the device. */
  if (s->stream && SDL_WasInit(SDL_INIT_AUDIO)) {
    SDL_DestroyAudioStream(s->stream);
    SDL_QuitSubSystem(SDL_INIT_AUDIO);
  }
  SDL_DestroyMutex(s->lock);
  free(s);
}
