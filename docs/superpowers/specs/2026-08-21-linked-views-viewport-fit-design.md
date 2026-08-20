# Linked views viewport-fit design

## Goal

Keep every linked visualization complete and square while making the collection
as large as the current viewport permits. The same layout must be used for a
normal visit and for a restored share link.

## Layout

For `N` visible panels, evaluate every column count from `1` through `N`. For
each candidate, derive `rows = ceil(N / columns)`, then calculate the square
side permitted independently by the usable width and usable height. Select the
candidate with the largest minimum side. The usable rectangle is the visible
area between the panel grid and the viewport bottom, less an 18px breathing
margin on every side and the measured card chrome and grid gaps.

Canvas geometry remains square and is never stretched. When the best square is
below the existing 300px readability floor, keep the floor and allow scrolling.
Focus mode retains its established primary/context hierarchy.

## Page scope

The Linked views landing page contains the controls, cohort state, legend, and
linked visualization grid. The Composition, Clonotypes, and server-rendered
selected-cell analysis blocks are not shown below the grid. This gives the grid
an unambiguous viewport budget without placing cards directly against an edge.

## Restore and resize

The existing grid `ResizeObserver` and window resize handler run the same
two-dimensional calculation whenever the final container width, viewport
height, visible panel count, or focus state changes. Shared configuration
restoration therefore cannot preserve a temporary narrow pixel track.

## Verification

Static regressions assert that both dimensions participate in the layout and
that the removed analysis regions are absent. Browser coverage checks several
panel counts on a laptop viewport and confirms complete canvases, the minimum
readability floor, and no horizontal overflow.
