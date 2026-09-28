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

```text
Transport / tempo / output
┌─────────────┬───────────────────────────┐
│ Cycle       │ Timeline                  │
│             ├────────────────┬──────────┤
├─────────────┤ Track editor   │ Tidal    │
│ Tracks      │                │ code     │
└─────────────┴────────────────┴──────────┘
Playback state / track count / transport shortcut
```

- A listening column occupies 28% of the default desktop workspace. Its
  circular score shows one cycle; the wider timeline compares several.
- At widths below 1100 logical pixels, view tabs focus one instrument.
  Choosing a track opens its editor. Compact navigation preserves the saved
  desktop arrangement.
- Recessed 7-pixel gutters mark draggable boundaries. Pane padding is 16
  pixels horizontally and 12 vertically; larger gaps separate editing tasks.
- Labels and code align left. The circular readout and its legend center on
  the score. Small radii distinguish controls from flat instrument surfaces.
- The selected expression stays visible above the editor, including its
  transformation chain. This matters especially when code is in another view.
- Timeline rules mark actual cycle and beat boundaries. Event edges mark
  onsets; split events show pan, and curves show real parameter signals.

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

The pane grid decides where each pane is, and drags a pane by a handle over
its title at that position. Each pane is therefore held to exactly the rect
the grid gives it; content that wants more room scrolls or clips inside the
pane instead of pushing its neighbours out of place. The grid's margin sits
outside the grid, since the grid lays panes out over its whole rect. A pane
too narrow for its header controls shows only its title and maximize
button, so nothing spills over the next pane's title. Reset layout skips
saving on the frame it is pressed, when the old arrangement is still on
screen.
