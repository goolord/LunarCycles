# Synthesizer percussion

A drum-machine arrangement using the built-in sampler's own kit: FreePats'
synthesizer percussion, drum sounds in the manner of vintage analog machines,
made by Roberto with the Yoshimi and Geonkick synthesizers. Its `bass` and
`keys` tracks add SuperDirt's `bass3` and `superpiano` synths.

The kit's files are laid out as Dirt-Samples is, one folder per sound, so
`n` (or `bd:1` in mini-notation) picks between the files in a folder:

| Sound    | Files                         |
| -------- | ----------------------------- |
| `bd`     | two kicks                     |
| `sn`     | two snares                    |
| `hh`     | two closed hi-hats            |
| `oh`     | open hi-hat                   |
| `cp`     | clap                          |
| `lt`, `mt`, `ht` | low, mid and high toms, two each |
| `cr`     | a cymbal three ways, then a second cymbal |
| `shaker` | short and long shaker         |
| `claves` | claves                        |

The song plays an intro, then twice over a groove with a crash on its first
cycle and a tom fill.

## Playing it

The WAV files are kept in Git LFS. Without `git lfs` installed when you
clone, they are small pointer files the sampler cannot play: install it and
run `git lfs pull`.

1. Choose **Project ▾ → Open…** and pick `synth-percussion.lunar`. LunarCycles
   finds this project's sibling `samples/` folder automatically.
2. Switch the transport to **Song** to hear the whole arrangement, or stay
   on **Sequence** to loop the one you are editing.

Press Play to connect the bass and keys to SuperDirt. LunarCycles uses a
SuperDirt server already listening locally, or starts SuperCollider when the
`sclang` executable and SuperDirt Quark are installed. If that is unavailable,
it can fall back to FluidSynth from an installed SoundFont when no MIDI output
is selected. The synths are not part of the bundled WAV kit, so the Samples
panel may list them as missing. A selected MIDI output can play them as
General MIDI patches instead.

The bundled folder is used for this project only. Other projects use your
chosen sample folder or Dirt-Samples if it is installed. The snare's `room` is
for SuperDirt; the built-in sampler has no reverb yet.

## Credits

The samples come from the [FreePats project](http://freepats.zenvoid.org/Percussion/electric-percussion.html#FSynthPercussion)
(version 2022-07-18) and are dedicated to the public domain under
[CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/). The pack's
own readme and license are in `samples/`.
