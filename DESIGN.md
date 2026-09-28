# LunarCycles: a tidal score

A native instrument for people composing TidalCycles patterns. Its main job
is to make the connection between an edited rhythm, its transformed events,
and its generated Haskell expression visible.

## Direction

Energy 2 / rhythm 2 / motion 2. The circular score is the focal point;
supporting controls are quiet. Playback is the reason for motion. There are
no entrance animations, ambient pulses, or decorative transitions.

The design uses the language of musical notation and instrument controls:
cycle divisions, onset marks, concentric track bands, and rotary parameters.
The identity comes from the actual pattern data, rather than an illustration.

## Tokens

| Token | Value | Purpose |
| --- | --- | --- |
| Deep water | `#182C32` | Workspace, recessed editing surfaces |
| Instrument surface | `#213940` | Pane surfaces |
| Chalk | `#E2EBE6` | Primary text and fourth track |
| Sea glass | `#9CD3BC` | First track, strings, success |
| Lilac | `#B5B9E8` | Second track and Tidal functions |
| Apricot | `#F0B879` | Transport, playheads, numeric literals, third track |

Supporting neutrals: `#A5B8B8` for secondary text, `#435E64` for rules,
and `#2D4950` for controls. Errors use `#F5A2A6` only when needed.
Track colors repeat after four tracks; names and channel identifiers also
identify them, so color is never the only means of identification.

Input Sans Regular is the interface face: its open forms and differentiated
characters suit small, closely spaced musical controls. Input Mono Regular
is reserved for expressions, channel identifiers, and exact parameter values.
Installed Noto Sans and DejaVu Sans Mono provide fallbacks. Fonts are resolved
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
and wheel input. Focus has a visible apricot outline. Function menus expose
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
- PASS, text contrast checks: chalk on the instrument surface is 10.01:1;
  secondary text on that surface is 5.89:1; colored track labels on that
  surface are at least 6.42:1.
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
