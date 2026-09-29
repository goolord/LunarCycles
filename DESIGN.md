# LunarCycles: a tidal score

A native instrument for people composing TidalCycles patterns. Its main job
is to make the connection between an edited rhythm, its transformed events,
and its generated Haskell expression visible.

## Direction

Energy 2 / rhythm 2 / motion 2. The circular score is the focal point;
supporting controls are quiet. Playback is the reason for motion. There are
no entrance animations, ambient pulses, or decorative transitions.

The centre of the circular score is a faint moon that goes through its
phases once per cycle: new at twelve o'clock, full at six. It is the only
decorative mark in the app, and it is drawn from the playhead's position.

The design uses the language of musical notation and instrument controls:
cycle divisions, onset marks, concentric track bands, and rotary parameters.
The identity comes from the actual pattern data, rather than an illustration.

## Tokens

The surface of the moon. Three depths of warm, near-neutral grey (dark mare
basalt, regolith, and the lighter highlands) separate the workspace, the
instrument surfaces and the controls, so a pane never blends into the window
behind it and a button never blends into its pane. The greys carry almost no
chroma, as in Bitwig Studio, so colour appears only on tracks and on the
harvest moon.

| Token | Value | Purpose |
| --- | --- | --- |
| Mare | `#141312` | Workspace, gutters, recessed wells (timeline, grid, code, inputs) |
| Regolith | `#242321` | Pane surfaces |
| Highland | `#363431` | Raised controls; hover `#42403C` |
| Rim | `#76726C` | Control borders, 3.3:1 against Regolith |
| Moonlight | `#E2DED6` | Primary text |
| Dust | `#A49F96` | Secondary text, 6.0:1 on Regolith |
| Harvest | `#ECA34C` | Transport, playheads, focus, a soloed track |

Tracks are lichen `#92B276`, tide `#6EA8A0`, blood moon `#CE725C` and straw
`#CCBA7A`, muted so that a full timeline stays calm and each at least 4.6:1
on Regolith. Lichen, tide and blood moon also colour strings, functions and
numbers in the code. Straw also lights a muted track; `#E86056` is kept for
errors. Track colours repeat after four tracks; names and channel
identifiers also identify them, so colour is never the only means of
identification.

Controls follow one rule: at rest a button is a Cap with a Rim; when it
turns something on (mute, solo, a running modulation) it fills with that
colour and its label turns dark, as a lit key on a hardware sequencer does.
Only the maximize and track-name buttons stay borderless.

Input Sans Condensed is the interface face: its open forms and
differentiated characters suit small, closely spaced musical controls, and
its narrower width keeps it clearly apart from Input Mono, which is reserved
for expressions, channel identifiers, and exact parameter values. Installed
Noto Sans and DejaVu Sans Mono provide fallbacks. Fonts are resolved
locally, with no runtime network dependency.

Type scale: 11–12 for secondary detail, 13–14 for controls and code, 15 for
pane titles, 20 for the product name, and 24–32 for the cycle readout.

## Composition

Four views share the transport and each give one kind of work the room
it needs. A tab row under the transport switches them, as do Ctrl+1, 2 and
3 and 4, and each view keeps its own pane arrangement.

```text
Project / undo / transport / Sequence·Song / tempo / output
Arrange   Pattern   Code   Mixer

Arrange                      Pattern                      Code
┌─────────────────────┐      ┌───────┬─────────────┐      ┌──────────┬──────────┐
│ Playlist            │      │ Cycle │ Timeline    │      │ Code     │ Editor   │
│                     │      │       ├─────────────┤      │          │          │
├──────┬──────────────┤      ├───────┤ Editor      │      │          ├──────────┤
│Tracks│ Timeline     │      │Tracks │             │      │          │ Tracks   │
└──────┴──────────────┘      └───────┴─────────────┘      └──────────┴──────────┘
Playback state / what loops / track count / transport shortcut
```

- Arrange gives the playlist the full width and 60% of the height; the
  edited sequence's tracks (for mute and solo) and timeline sit under it.
  A double-click on a clip opens its sequence in Pattern.
- Pattern is the original listening column beside the timeline and a
  full-width editor.
- Code puts the generated code beside the editor, so a change and the code
  it writes are read together; the track list stays for mute and solo.
- Mixer puts the shared channels on vertical gain faders, with pan, mute and
  solo on each strip. Selecting a channel exposes grouped effect controls.

- In Pattern, a listening column occupies 27% of the width. Its circular
  score shows one cycle; the wider timeline compares several.
- At widths below 1100 logical pixels, the view tabs give way to a tab for
  each of the seven panes, which focus one instrument at a time. Below 600
  pixels these tabs use two rows so Mixer stays visible. Choosing a
  track opens its editor. Compact navigation preserves the views' saved
  arrangements.
- Recessed 7-pixel gutters mark draggable boundaries. Pane padding is 16
  pixels horizontally and 12 vertically; larger gaps separate editing tasks.
- Labels and code align left. The circular readout and its legend center on
  the score. Small radii distinguish controls from flat instrument surfaces.
- The selected expression stays visible above the editor, including its
  transformation chain. This matters especially when code is in another view.
- Timeline rules mark actual cycle and beat boundaries. Event edges mark
  onsets; split events show pan, and curves show real parameter signals.

## Sequences and the playlist

A song is a set of channels, a set of sequences, and a playlist that
places the sequences in time, the way a DAW arranges patterns. Channels
are shared, as in a channel rack: a channel's name, sound, knobs, mute and
solo are the same in every sequence, so changing the bass's sound in one
sequence changes it in all of them. Each sequence gives a channel a part,
its rhythm and functions, or none, and a channel without a part is silent
there; the track list shows a dash for it, and the editor says so. Editing
its rhythm adds a part. A channel keeps its `dN` and MIDI channel in every
sequence. + Track adds a channel with a part in the edited sequence;
Duplicate copies the channel with only this sequence's part; Remove takes
the channel out of every sequence. Songs saved before channels were shared
open with same-named, same-sound tracks merged into one channel. The track list,
editor, cycle and timeline always show the sequence being edited. The
transport's Sequence/Song switch picks what plays. Sequence mode loops the
edited sequence as `d1`…`dN`, as before. Song mode plays the playlist, and
the code becomes the playlist itself: each placed sequence bound to its name
as a `stack`, and `d1 $ timeLoop <end> $ seqP [(start, end, name)]`. Tidal's
`seqP` starts each clip from its sequence's cycle 0, and the app compiles
through the same `seqP` and `timeLoop`, so the song on screen is the one that
plays. In Song mode the cycle and timeline show the edited sequence as the
song plays it, silent outside its clips.

Clips are Cap-coloured blocks with the Rim, drawn with their tracks' notes
in the track colours, so colour still belongs to tracks. Clips of the edited
sequence are lit a little and outlined in Moonlight; the selected clip gets
a heavier outline. A muted bar and flag mark where the song loops. The moon
playhead shows only in Song mode.

The sequence list beside the grid chooses what is edited and what a click
places; its "⋯" holds name, length, Duplicate and Delete. A click on an
empty lane places a clip, a drag moves it along and across lanes in whole
cycles, and its right edge stretches it. The lane count is held while a
clip is dragged, so lanes don't resize under the pointer. Right-click, or
Delete while pointing at it, removes a clip. The grid takes no keyboard
focus, so Space still plays. A press on the ruler sets where Play starts
and Stop returns, and switches to Song mode.

Edits undo with Ctrl+Z and redo with Ctrl+Shift+Z or Ctrl+Y, or with the
arrows beside Project. One drag is one step, as is a burst of typing or
scrolling within a second. The Project menu opens and saves `.lunar` files
through the native file dialogs, and asks before New or Open discards
unsaved changes. It also exports MIDI: 16 cycles of the sequence, or the
whole song in Song mode. The window title names the file and marks unsaved
changes.

## Interaction

Step cells are taller and numbered. Pointer dragging paints the grid; after
focusing it, left/right selects a step, Enter or Space toggles it, and up/down
changes its sample or note. The grid claims these keys so Space does not also
start the transport.

Parameter knobs are 56 pixels across and accept arrow keys as well as drag
and wheel input. Focus has a visible moon-yellow outline. Function menus expose
Move earlier and Move later as alternatives to dragging. Euclidean parameters
remain editable through numeric fields as alternatives to ring gestures.

## Review against the brief

A generic dark DAW reskin would change colors without changing hierarchy.
This version instead gives the circular score a dedicated listening area,
places editing beside the generated notation, and replaces cramped narrow
panes with a focused workspace. Colors connect representations of the same
track. There are no decorative meters or invented playback measurements.

The first screenshot review caught a clipped cycle legend and a font resolver
selecting the medium weight. The circle was resized to reserve legend space,
and font searches now request the regular faces explicitly. Large circles
use adaptive path tessellation rather than visibly polygonal primitives.

## Verification

- PASS, design intent: palette, type, layout, spacing, color assignments, and
  playback motion have product-specific purposes recorded above.
- PASS, build and runtime: the SDL executable builds and captures a playing
  desktop window without runtime errors.
- PASS, visual review: rendered at 1600 × 980, 440 × 820, and 400 × 600 logical
  pixels. The minimum-size editor scrolls vertically, with all view tabs visible.
- PASS, text contrast checks: primary text on the pane surface is 11.7:1;
  secondary text is 6.0:1; track colours are at least 4.6:1; control rims
  are 3.3:1 against the pane (WCAG 1.4.11 asks 3:1).
- PASS, both Cabal test suites: step painting changes the pattern; keyboard
  input toggles a step; dragging and arrow keys change gain; dragging and menu
  actions reorder functions; adding a function updates the chain; Mini converts
  the source; pane swapping persists and reopens; compact track creation opens
  the editor; resizing restores the desktop panes; Play and Stop change engine
  state. These are scripted headless interactions, with separate SDL screenshots
  for visual inspection.

## Control and layout fixes

- Popups use an opaque raised surface with a brighter border and a compact
  shadow, so output settings remain distinct from the timeline underneath.
  Their padding is applied after the toolkit's `tight` reset.
- Select labels are measured and ellipsized before reaching the renderer;
  explanatory text wraps. Control rows explicitly center their children.
- Track padding and pattern text select the track, while mute and solo keep
  their own click handling. The circular score uses its remaining layout space
  so it cannot push its legend into the neighbouring pane.
- Play and Stop request an immediate follow-up frame. Euclid rotation captures
  a drag until release, including pointer movement outside the ring.
- PASS, regression tests: track-row padding selects the snare; mute toggles
  independently; Play schedules a frame immediately and shows Stop on the next
  frame; Euclid rotates outside its bounds and ignores drags begun elsewhere;
  long select labels stay inside the control; Output opens and Escape closes it.
- PASS, visual review: desktop playback and Output at 1600 × 980, plus compact
  editor and Output at 400 × 700. Popups separate clearly from the score, and
  explanatory text wraps inside the menu.

The cycle graphic reserves only 18 logical pixels outside its track bands;
quarter-cycle labels share the tick band. Short, wide panes move the legend
beside the circle to recover vertical drawing space. The center readout is
measured against a square inside the clear inner disc. When all three lines
would become too small, only the cycle count is shown; tempo remains available
in the transport. This keeps the text separate from the musical events at
every pane size.

## Pane geometry

The pane grid holds each pane to the rect its split gives it; content that
wants more room scrolls or clips inside the pane instead of pushing its
neighbours out of place. Each pane frame is capped at that rect, because a
pane's content otherwise set its minimum and moved the dividers as tracks
were added, or when the song's longer code appeared. Each view's
arrangement is saved to its own file (`layout-arrange`, `layout-pattern`,
`layout-code`); one that does not hold exactly the view's panes is set
aside for the view's starting arrangement. Reset layout resets only the
view on screen. A pane is dragged by its whole title bar. A pane
too narrow for its header controls shows only its title and maximize
button. The arrangement is saved when a drag or resize lets go, and Reset
layout hands the grid the starting arrangement and forgets the saved one.

## Mixer

Energy 2 / rhythm 2 / motion 2, inherited from the instrument. The reference
is FL Studio's channel-strip bank: one vertical gain fader per shared channel,
with pan, mute and solo. Gain uses Tidal's actual multiplier (0 to 1.5, unity
at 1), with a separate marker when gain is modulated. These are parameter
positions, not invented peak meters. Playback-driven modulation is the only
motion.

- The existing track palette connects each strip to its score and timeline.
  A selected name button and outline identify which channel the controls edit;
  text also identifies mute, solo and solo exclusion.
- The existing condensed interface face keeps channel names readable in
  112-pixel strips; exact gain and pan values retain Input Mono. Long channel
  and sound names are ellipsized, with their full names in tooltips.
- Faders come first in each strip so volume stays reachable in short windows.
  A 44-pixel minimum is used for channel selection, mute/solo, paging and
  modulation buttons. Faders and knobs have keyboard focus outlines.
- Controls are grouped by their actual purpose: level/stereo, low-pass filter,
  effects and sample playback. There is no fixed effect-slot stack. Wide
  windows place these groups beside the bank to keep them visible while
  mixing; smaller windows place them below in a vertically scrolling pane.
- An 8-pixel gap separates strips; 28 pixels separate the bank and controls.
  Channel banks page when there is not enough width for usable faders. Below
  600 pixels the pane tabs use two rows, keeping Mixer visible on entry.
- Gain/pan edits keep modulation intact. All edits use the shared channel
  model, so undo, saved songs, generated code and every sequence agree.
  Adjustments affect new events, as the existing playback engine does.
- Output support is stated in the view: room is SuperDirt reverb or a MIDI
  reverb send; shape and sample playback controls use Samples or SuperDirt.
  An empty mixer explains how to add a channel. Pattern errors appear with
  the selected channel; output loading/errors remain in Output and the status
  bar. Mixer itself reads in-memory state and has no asynchronous loading step.

### Mixer delivery checks

- PASS, hard gates: Mixer and Ctrl+4 open a working view. All channel data
  comes from the song. There are no new assets, placeholder meters or
  fabricated measurements. Native renders at 1600 × 980, 1100 × 800 and
  400 × 600 keep the content inside its scrolling pane; compact tabs are
  visible. The app has one established dark theme.
- PASS, contrast and keyboard: calculated primary/pane contrast is 11.70:1,
  secondary/pane 5.97:1, primary/hover 7.71:1, and the weakest selected
  channel-button label 5.47:1. Faders accept arrow keys and show a focus
  outline; shared knobs also draw a focus outline. Escape closes modulation.
- PASS, purpose and liveliness: palette, typography, spacing and responsive
  placement follow the reasons above. Fader banks are the focus, track colour
  is the identity cue, and live markers show actual parameter signals.
- PASS, build/runtime: Cabal builds the executable; both test suites pass.
  SDL screenshot runs, including playback, exit without runtime errors.
- PASS, scripted headless click-through: Mixer tab and Ctrl+4 open the view;
  strip selection changes the effects target; fader dragging, wheel and arrows
  change gain; the fader clamps at 0 and 1.5; right-click restores unity;
  pan changes stereo position; M/S update shared mute/solo state.
- PASS, effects click-through: cutoff, resonance, room, shape, speed, begin
  and end each edit the selected channel; the signal menu selects sine;
  Done and Escape close it; dragging a modulated fader preserves its signal.
- PASS, integration and compact state: one drag undoes in one step, redo
  restores it, edits reach Tidal code and survive save/reopen, and channel
  edits leave sequence parts intact. Previous/Next reach every channel;
  scrolling reaches an editable sample-end knob in a 400-pixel window;
  resizing preserves channel selection. Removing the last channel shows
  the empty-state instructions.
