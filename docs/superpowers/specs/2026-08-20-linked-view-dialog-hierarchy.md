# Linked-view import/export dialog hierarchy

## Goal

Make the Linked views import/export dialog immediately scannable by separating
three distinct jobs: moving a JSON file, saving the current view locally, and
managing already-saved local views.

## Layout

The dialog retains its existing eyebrow, title, close control, and short JSON
privacy statement. Its content below the introduction is three stacked,
visually independent regions.

### Move a view

- Heading: `Move a view`.
- Supporting text: JSON can be downloaded, copied, or opened from disk.
- Contains the existing equal-width Download JSON, Copy JSON, and Open JSON
  actions.
- This section contains no local-snapshot controls.

### Save on this device

- Heading: `Save on this device`.
- Supporting text: snapshots are private to this browser and the current cell
  population.
- Contains the existing Save current view action and no list of existing views.

### Saved views

- Heading: `Saved views` plus a quiet count where useful.
- Supporting text: saved views are available when returning to this data set in
  the same browser.
- Each snapshot is one clearly separated row with bookmark marker, name, saved
  timestamp, and actions.
- `Open` is the visually primary action. Download, Rename, and Delete remain
  available as quieter secondary actions; Delete retains its destructive colour.

## Visual rules

- Every region has its own light border, rounded surface, 16–18px internal
  spacing, and heading/description pairing.
- The three file actions remain equal size and use the established amber
  selection state, but do not compete with local saving.
- Region descriptions use muted text and sit directly beneath their own
  heading, never between unrelated controls.
- On narrow screens, the three file actions continue to stack; each region
  stays independently legible.

## Behaviour and data

No configuration format, validation, localStorage key, save/restore flow, or
import/export behaviour changes. This is solely a semantic markup and styling
reorganisation of the existing controls.

## Acceptance criteria

1. A user can identify file movement, local saving, and saved-view management
   without reading across section boundaries.
2. All current import/export and local snapshot actions continue to work.
3. Saved snapshot rows keep name, timestamp, Open, Download, Rename, and
   Delete actions.
4. The dialog remains usable at the existing mobile breakpoint.
