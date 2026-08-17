# Builder Dataset Rail State Design

## Purpose

Give every dataset row a clear, consistent visual state without turning the
rail into a field of dark orange blocks. The CerebroNexus logo orange remains
the brand selection colour. Import lifecycle, pointer interaction, keyboard
focus, and selection are separate visual layers.

This specification changes presentation only. It does not change import
ordering, upload scheduling, dataset selection, or server state.

## Design principles

1. Logo orange means **current selection**, not loading, success, or warning.
2. Lifecycle colours answer “what is happening to this dataset?”
3. Interaction styling answers “what is the user pointing at or operating?”
4. Colour is never the only signal. Every state retains text plus a shape,
   progress indicator, or selection marker.
5. Large saturated backgrounds are avoided in the rail. Strong colours appear
   only in small semantic marks and controls.

## Visual layers

### Lifecycle layer

| State | Colour | Row treatment | Secondary signal |
|---|---|---|---|
| Queued | neutral grey `#9a9aa0` | white background | grey dot and “Waiting · N in queue” |
| Uploading | blue `#2f6fd6` | pale blue `#eef4fb` | blue animated dot and determinate progress when available |
| Reading / inspecting / preparing | blue `#2f6fd6` | pale blue `#eef4fb` | blue spinner and exact stage text |
| Ready | green `#16a34a` | white background | solid green dot and “Ready” |
| Error / rejected | red `#dc2626` | pale red `#fff1f2` | red dot, explicit error text, Retry or Remove |
| Cancelled | neutral grey `#9a9aa0` | muted grey `#f4f4f5` | stopped icon or dot and “Cancelled” |
| Paused / reconnecting | blue-grey | muted blue-grey background | pause icon and reconnecting text; no animation |
| Unknown | warning brown `#85510a` | pale warning `#fdf3e3` | question/status icon and recovery action |

Uploading and server processing intentionally share blue. Stage text and the
progress treatment distinguish their phase without adding another hue.

### Interaction layer

- **Resting, unselected:** lifecycle treatment only.
- **Hover, unselected:** warm off-white `#fffaf6` overlay with a subtle
  `#ffb27a` inset border. It must not replace the lifecycle dot or status text.
- **Selected:** pale logo-orange background `#fff4ec`, a 4 px left marker in
  logo orange `#f97316`, dark text, and `aria-current="true"`.
- **Selected + lifecycle state:** the selected background and orange marker
  remain; the dot, progress, and status text retain their lifecycle colour.
- **Selected + hover:** selection remains dominant. Hover may slightly
  strengthen the border but must not change the background to a new colour.
- **Keyboard focus:** a blue focus ring independent of both selection and
  lifecycle colour.

The left marker is reserved exclusively for selection. Lifecycle states do not
use the same marker, preventing upload and selection from becoming ambiguous.

## State priority

The final row is composed rather than selected from one mutually exclusive CSS
state:

1. Base row structure
2. Lifecycle background, dot, text, and progress
3. Hover overlay when the row is not selected
4. Selected logo-orange background and left marker
5. Keyboard focus ring
6. Error/recovery actions

Error remains visible on selected rows through its red dot, error text, and
actions. Selection does not turn error content orange or white.

## Component anatomy

Each row contains:

1. Index or lifecycle icon area
2. Dataset name
3. Metadata or current import-stage text
4. Semantic status dot or spinner
5. Contextual actions such as Retry, Cancel, Remove, or reorder controls
6. A selection marker rendered by the row container

Ready rows preserve the existing index and ordering affordance. Import rows may
use the status dot in the leading position, but spacing and name alignment must
match ready rows so that the rail does not jump when a dataset becomes ready.

## Motion

- Uploading may use a restrained blue pulse or determinate progress movement.
- Server processing may retain the current spinner.
- Selection transition is 160–220 ms and changes background and marker only.
- Hover transition is 120–160 ms.
- Under `prefers-reduced-motion: reduce`, spinner/pulse movement stops while
  status text and static semantic colour remain.
- Paused or disconnected imports never display an active sweep or spinner.

## Token strategy

Reuse existing palette tokens where possible:

- Logo selection: `--c-amber`, `--c-amber-50`, `--c-amber-300`
- In progress: `--c-blue`, `--c-blue-50`, `--c-blue-100`
- Ready: `--c-success`, `--c-success-50`, `--c-success-700`
- Error: `--c-error`, `--c-error-50`, `--c-error-700`
- Queued/cancelled: existing neutral surface and text tokens

The current dark Builder action colours may remain on primary action buttons,
but dataset selection must no longer use `--builder-action` as a full saturated
row background. Selection should use the logo palette directly or new semantic
aliases that resolve to it.

Recommended semantic aliases:

- `--builder-rail-selected-bg`
- `--builder-rail-selected-marker`
- `--builder-rail-hover-bg`
- `--builder-rail-progress-bg`
- `--builder-rail-progress-fg`
- `--builder-rail-ready-fg`
- `--builder-rail-error-bg`
- `--builder-rail-error-fg`

## Responsive behaviour

The same semantic colours apply on desktop and narrow-screen Dataset Manager.
Narrow layouts may reduce metadata or move actions to a second line, but must
not hide status text, the status dot, or the selected marker. Touch interaction
does not rely on hover; resting and selected states remain complete without it.

## Accessibility

- Status text is always visible; colour alone never identifies state.
- Selected rows use `aria-current="true"`.
- Loading status continues to be announced through the existing live region.
- Text contrast must meet WCAG AA: 4.5:1 for normal text and 3:1 for large or
  bold interface text and meaningful graphical marks.
- Focus uses a blue ring so focus and selection remain distinguishable.
- Error, pause, and unknown states use different text and icon shapes, not only
  different colours.

## Acceptance criteria

1. No ready or selected dataset row uses the current dark orange full-row fill.
2. A selected ready row has a pale logo-orange background, orange left marker,
   green ready dot, and dark readable text.
3. A selected uploading row has the same orange selection marker while its
   progress indicator and status remain blue.
4. Hover on an unselected ready row is visibly different from both resting and
   selected states.
5. Queued, uploading, processing, ready, error, paused, cancelled, and unknown
   states are distinguishable using text plus a non-text visual signal.
6. Hover never masks lifecycle state; selection never recolours lifecycle state.
7. Keyboard focus is independently visible on selected and unselected rows.
8. Reduced-motion mode leaves every state understandable without animation.
9. Desktop and narrow-screen rails expose the same state meaning.
10. Existing upload queue ordering and dataset actions are behaviourally
    unchanged.

## Verification design

- Static UI contract checks for semantic state classes and token usage.
- Rendering tests for every lifecycle state and selected combinations.
- Browser assertions for resting → hover → selected transitions.
- Contrast checks for text, dots, borders, and focus rings.
- Reduced-motion browser check confirming that loading remains legible with
  animation disabled.
- Narrow viewport check confirming that status and selection signals remain
  visible.

## Out of scope

- Changing upload scheduling or queue semantics
- Changing the Dataset Manager layout
- Redesigning primary buttons across the rest of Builder
- Dark mode
- Adding new dataset lifecycle states

