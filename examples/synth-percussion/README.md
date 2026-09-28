# Synthesizer percussion

A song for the built-in sampler, with its own kit: FreePats' synthesizer
percussion, drum sounds in the manner of vintage analog machines, made by
Roberto with the Yoshimi and Geonkick synthesizers.

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

1. Choose **Output ▾ → Samples → Choose folder…** and pick
   `examples/synth-percussion/samples`.
2. Choose **Project ▾ → Open…** and pick `synth-percussion.lunar`.
3. Switch the transport to **Song** to hear the whole arrangement, or stay
   on **Sequence** to loop the one you are editing.

The sampler remembers the folder, so switch back to Dirt-Samples the same
way. The snare's `room` is for SuperDirt; the built-in sampler has no reverb
yet.

## Credits

The samples come from the [FreePats project](http://freepats.zenvoid.org/Percussion/electric-percussion.html#FSynthPercussion)
(version 2022-07-18) and are dedicated to the public domain under
[CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/). The pack's
own readme and license are in `samples/`.
