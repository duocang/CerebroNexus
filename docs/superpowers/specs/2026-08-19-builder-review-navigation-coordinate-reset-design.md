# Builder review navigation and coordinate reset

## Goal

Make sequential dataset review start in a predictable place and make the
Coordinate settings Reset button restore every control that belongs to that
panel.

## Review navigation

When **Done checking this dataset** selects another unchecked dataset, the
Builder will switch datasets, reset the main document scroll position to the
top, and focus the new dataset workbench heading. The scroll and focus change
will happen only after the new workbench content is present. Marking the final
dataset checked will not move the page because no dataset switch occurs.

The behavior belongs to the automatic review-advance path. Ordinary manual
dataset selection keeps its existing scroll behavior.

## Coordinate settings reset

Reset applies to the active dataset and active spatial section. It restores:

- coordinate rotation to `0` degrees;
- point opacity to the Builder default;
- point size to the Builder default.

The reset updates the visible controls, the coordinate draft and point-style
state, and the current canvas immediately. The restored values remain in force
when the section is materialized or the user switches datasets.

Reset does not change tissue-image translation, scale, rotation, flips,
opacity, bounds, or uploaded image state. Those remain owned by the separate
image-alignment reset action.

If there is no current dataset or active section, Reset remains a safe no-op.

## Implementation boundaries

- Add a narrow client message for post-switch scroll and heading focus; send it
  only from the successful automatic-next-dataset callback.
- Extend the existing coordinate-reset observer rather than adding another
  reset authority.
- Use the canonical `builder_alignment_defaults()` values for point appearance.
- Keep the existing draft/materialization and canvas reset mechanisms as the
  persistence and redraw authorities.

## Verification

- A server/UI contract proves automatic review advancement requests the
  post-switch navigation, while the final checked dataset does not.
- A browser contract proves the request scrolls to the top and focuses the new
  workbench heading after it exists.
- Spatial alignment server tests prove Reset updates rotation, opacity, size,
  the stored current-section state, and the canvas reset token without changing
  image-alignment parameters.
