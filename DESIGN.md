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

A harbour at night. Three depths of grey-blue slate separate the workspace,
the instrument surfaces and the controls, so a pane never blends into the
window behind it and a button never blends into its pane. The slate is kept
low in chroma and leans toward sea rather than violet; colour belongs to the
tracks and the moon.

| Token | Value | Purpose |
| --- | --- | --- |
| Water | `#080F15` | Workspace, gutters, recessed wells (timeline, grid, code, inputs) |
| Hull | `#172430` | Pane surfaces |
| Cap | `#2B3D50` | Raised controls; hover `#364C62` |
| Rim | `#7892A9` | Control borders, 4.9:1 against Hull |
| Moonlight | `#E8EDF1` | Primary text |
| Mist | `#9FB0BF` | Secondary text, 7.1:1 on Hull |
| Moon | `#F2D27A` | Transport, playheads, focus, a soloed track |

Tracks are sea glass `#7FD1B2`, harbour blue `#82BCEB`, buoy coral `#F0916F`
and kelp `#BCD072`, each above 6.5:1 on Hull. Sea glass, harbour blue and
coral also colour strings, functions and numbers in the code. Amber `#FFBE66` lights a muted
track; `#FF7B7B` is kept for errors. Track colours repeat after four tracks;
names and channel identifiers also identify them, so colour is never the
only means of identification.

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

Three views share the transport and each give one kind of work the room
it needs. A tab row under the transport switches them, as do Ctrl+1, 2 and
3, and each view keeps its own pane arrangement.

```text
Project / undo / transport / Sequence·Song / tempo / output
Arrange   Pattern   Code

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

- In Pattern, a listening column occupies 27% of the width. Its circular
  score shows one cycle; the wider timeline compares several.
- At widths below 1100 logical pixels, the view tabs give way to a tab for
  each of the six panes, which focus one instrument at a time. Choosing a
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

A song is a set of sequences, each a group of tracks, and a playlist that
places them in time, the way a DAW arranges patterns. The track list,
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
- PASS, text contrast checks: primary text on the pane surface is 13.4:1;
  secondary text is 7.1:1; track colours are at least 6.7:1; control rims
  are 4.9:1 against the pane (WCAG 1.4.11 asks 3:1).
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
