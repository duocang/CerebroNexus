# Native Brave console observations

## memory-final (before shared-hint context guard)

2026-10-10T12:21:46.452Z: TypeError: Cannot read properties of undefined (reading
'length'), unitOf -> project -> resizePanel -> resizeAll, cell_views.js.
Subsequent full bundle: 102,216,534 B. This path is fixed by validating hint context.

## memory-fixed (context guard active, original empty selection UI)

2026-10-10T12:28:04.628Z: Shiny server has sent a error for the output
'coordviews_selected_cells_UI', but the output is in an unexpected state of:
'invalidated'. Same occurrence emitted both Shiny reporting and Error stack.
No unitOf error, wire fallback or visible output-error node in the saved run.

## lifecycle-smoke (rejected explicit-empty-UI experiment)

2026-10-10T12:33:11.760Z: Shiny server has sent a value for the output
'coordviews_selected_cells_UI', but the output is in an unexpected state of:
'invalidated'. Stack: Shiny output progress updateStateFromMessage ->
dispatchMessage -> startActionQueueLoop (installed Shiny 1.14.0).

Changing req(empty_selection) to return(NULL) changed error -> value but did not
resolve the invalidated-state transition. The experiment was reverted. These
logs are not suppressed or reclassified as a passed clean-console check. The
shared-coordinate context fix is independent of this unresolved output state.

The final post-warning brush recovered normal functionality: 3,354 selected
cells in all four panels and 3,354 table records, with plot and selectors present.
This is a functional recovery observation, not a clean-console pass.
