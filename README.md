# LunarCycles

A visual sequencer for [TidalCycles](https://tidalcycles.org) patterns, built
with [nano-ui](https://github.com/goolord/nano-ui) (pinned to `c6e766b2`).

Patterns are evaluated by `tidal-core`, the same engine Tidal runs, so what the
views draw is what Tidal plays. Every edit rewrites the Tidal code in the Code
pane, and that code builds exactly the patterns on screen.

## What's on screen

The petrol-colored workspace pairs a circular score and track list on the
left with a wider timeline, editor, and generated code on the right. Track
colors connect all four representations. The visual rationale and palette
are in [DESIGN.md](DESIGN.md).

Below the transport bar is a grid of panes. Drag a pane by its title to move
it: dropping it on another pane's centre swaps the two, and dropping it near
an edge splits that pane. Drag the gaps between panes to resize them, and use
the ⤢ button to fill the window with one pane. The arrangement is saved to
`~/.config/lunar-cycles/layout` (under `$XDG_CONFIG_HOME` when that is set)
each time you let go of a pane or divider, and the next run opens with it.
**Reset layout** deletes the saved arrangement and puts the panes back where
they started.

Below 1100 logical pixels wide, view tabs show one pane at a time. Selecting
a track opens its editor. Returning to a wider window restores the desktop
arrangement. Compact navigation does not overwrite that arrangement.

- **Cycle.** One ring per track. The current cycle runs clockwise from twelve
  o'clock. When `jux rev` (or anything else that pans) splits a track, its
  left channel draws on the inside of the ring and its right channel on the
  outside. The first track is the outermost band; select a track through the
  legend below the score.
- **Timeline.** A page of 1–8 cycles with one lane per track. `every 4 (fast 2)`
  shows up as a denser first cycle, and `pan` moves events up and down their
  lane. Pitched tracks draw as a piano roll. A modulated parameter's signal is
  traced across its lane. Hover an event to see its values; click a lane to
  edit its track.
- **Tracks.** Each track on two lines: its name and sound, then mute, solo and
  its pattern. Click a track to open it in the editor.
- **Editor.** The selected track, on three tabs:
  - **Rhythm** edits the source as a numbered step grid (click or drag to paint,
    scroll to pick a sample or note), a euclidean ring (scroll to add
    pulses, drag around to rotate), or Tidal mini-notation.
  - **Functions** shows the transform blocks. They read left to right as the
    code does (`every 4 (fast 2) $ jux rev $ s "…"`). Drag a block to reorder
    the functions, click it to change its settings or move it earlier/later,
    and use `+ fn` to wrap
    the pattern in another function.
  - **Sound** holds the sound and the parameter knobs for cutoff, resonance,
    gain, pan, speed, begin, end, room and shape. The `∿` button under a knob
    sweeps it with a sine, triangle, saw, square, rand or perlin signal; the
    knob shows the swept range and where the signal is right now.
    Right-click a knob to reset it. Arrow keys adjust a focused knob.
- **Code.** The Tidal code for the whole song, with the selected track's lines
  highlighted. **Copy** puts it on the clipboard.

Step grids are refactored as you edit them. LunarCycles tries shorter
spellings (`bd*4`, `sn(3,8,2)`, `[hh hh:2 hh [hh hh:1]]*2`, `bd!3`), keeps the
simplest one that tidal-core confirms plays the same as the grid, and says
which rule shortened it.

## Sound

- **MIDI, through Euterpea.** Pick an output from the **Output** menu. Drums
  go to the General MIDI kit on channel 10. Pitched sounds get their own channel and
  program; `gain` sets velocity, and `pan`, `cutoff`, `resonance` and `room`
  send controllers. For a software synth, start FluidSynth with a General MIDI
  soundfont and press **Rescan**:

  ```sh
  fluidsynth -a pipewire -m alsa_seq -i -s /path/to/FluidR3_GM.sf2
  ```

  When a `.sf2` is in `/usr/share/soundfonts` or `~/.local/share/soundfonts`,
  the Output menu offers **Start FluidSynth** to do this for you.
- **SuperDirt, through Tidal's stream.** Turn on the SuperDirt switch in the
  Output menu while SuperDirt is listening on `127.0.0.1:57120`.
- **Export .mid**, also in the Output menu, renders 16 cycles to `lunar-cycles.mid` as a Euterpea
  `Music` value.

Space plays and stops. Stopping rewinds to cycle 0.

When the step grid has keyboard focus, left/right selects a step, Enter or
Space toggles it, and up/down changes its sample or note. Space edits the
focused grid rather than starting playback. Tab moves between controls.

## Building

You need GHC 9.14, Cabal, SDL3, SDL3_ttf and PortMidi.

```sh
cabal run lunar-cycles
cabal test all
```

`cabal.project` lifts the upper bounds that Euterpea and tidal set below
GHC 9.14's libraries. Both build unchanged.

`lunar-cycles --play --screenshot shot.png --after 3` renders a hidden window,
saves it as a PNG after three seconds, and quits. Use `--width 440 --height 820`
to inspect the compact layout. To capture the default layout without changing
your saved arrangement, set `XDG_CONFIG_HOME` to a scratch directory.

The app uses installed Input Sans Regular and Input Mono Regular fonts when
available, with Noto Sans and DejaVu Sans Mono fallbacks.
