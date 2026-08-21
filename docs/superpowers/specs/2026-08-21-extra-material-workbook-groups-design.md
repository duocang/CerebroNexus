# Extra-material workbook groups

## Goal

Make many spreadsheet sheets manageable in Builder with a vertical workbook
list. Tables remain a flat, uniquely named list at build time; an index records
their workbook → sheet hierarchy for a later Viewer implementation.

## UI

- Render one vertically stacked, collapsible workbook card per uploaded file.
  A newly uploaded workbook is expanded; existing workbooks are collapsed by
  default.
- A card shows an editable Viewer workbook name, immutable source filename,
  type, size, sheet count, readiness, and one
  `Remove workbook` action.
- Its compact sheet rows show the Viewer table name and immutable source-sheet
  name. Each row has `Edit` and `Remove` actions.
- Workbook and sheet `Edit` reveal local text inputs with `Save` and `Cancel`.
  Typing has no Shiny event. `Save` or Enter sends one rename action; Escape or
  Cancel leaves the saved name unchanged.
- Deleting a workbook removes all of its sheets. Deleting a sheet removes only
  that table.

## Data and server behaviour

- Preserve immutable `file_name`, `file_type`, and `file_size` on every parsed
  table. Add `workbook_name` (the editable Viewer label) and `sheet_name` (the
  immutable source sheet).
- Keep the table key as the Viewer-facing sheet display name. A build writes a
  backward-compatible flat `tables` list plus `table_index`, keyed by table
  name and containing `workbook_name`, `file_name`, and `sheet_name`.
- Add actions `remove_workbook`, `rename_workbook`, and `rename`. Existing
  `remove` remains the per-sheet removal action.
- The client listens for `click` and `keydown` only for table edits. It must
  not send a rename from the document `input` listener.

## Tests

- Unit-test grouping, workbook rename, and workbook deletion with two files
  and several sheets.
- Assert the rendered UI contains workbook metadata once per group and sheet
  rows beneath it.
- Assert the browser script has no workbook or sheet display-name input handler,
  and sends a rename only through the explicit commit path.
- Run the focused Builder enhancement and UI-contract tests, then a browser
  smoke test against an uploaded workbook when available.

## Out of scope

- Altering original XLSX files or worksheet names.
- A master-detail file manager, drag-and-drop reordering, bulk sheet rename,
  or the Viewer’s two-dropdown UI.
