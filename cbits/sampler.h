#ifndef LUNAR_SAMPLER_H
#define LUNAR_SAMPLER_H

typedef struct lunar_sample lunar_sample;
typedef struct lunar_sampler lunar_sampler;

/* A WAV file as 32-bit float frames at `rate`, mono or stereo. NULL when it
 * cannot be read; lunar_error says why. */
lunar_sample *lunar_sample_load(const char *path, int rate);
void lunar_sample_free(lunar_sample *sample);
int lunar_sample_frames(const lunar_sample *sample);

/* A mixer at `rate`. Until lunar_sampler_start it only renders offline. */
lunar_sampler *lunar_sampler_new(int rate);
/* Open the default playback device and render into it from SDL's audio
 * thread. Returns 0 on failure. */
int lunar_sampler_start(lunar_sampler *s);
void lunar_sampler_free(lunar_sampler *s);

/* Play `sample` `delay` seconds from now. `rate` is the playback speed
 * (negative plays backwards), `begin` and `end` the part of the sample as
 * fractions, `cutoff` a low-pass frequency in Hz (20000 or more for none),
 * and gain, pan, resonance and shape are as SuperDirt reads them. */
void lunar_sampler_play(lunar_sampler *s, const lunar_sample *sample, double delay,
                        double rate, double begin, double end, double gain, double pan,
                        double cutoff, double resonance, double shape);
/* Drop the notes that have not started yet; playing ones ring on. */
void lunar_sampler_cancel(lunar_sampler *s);
/* Mix `frames` stereo frames into `out`, advancing the offline clock. */
void lunar_sampler_render(lunar_sampler *s, float *out, int frames);

const char *lunar_error(void);

#endif
