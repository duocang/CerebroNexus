# Extra-material workbook groups

## Goal

Make many spreadsheet sheets manageable in Builder without changing the CRB
contract: tables remain a flat, uniquely named list at build time, while the
attachment UI preserves the uploaded workbook → sheet hierarchy.

## UI

- Render one collapsed workbook card per uploaded file. A newly uploaded
  workbook is expanded; existing workbooks are collapsed by default.
- A card shows the file name, type, size, sheet count, readiness, and one
  `Remove workbook` action.
- Its compact sheet rows show the Viewer table name and immutable source-sheet
  name. Each row has `Edit` and `Remove` actions.
- `Edit` reveals a local text input with `Save` and `Cancel`. Typing has no
  Shiny event. `Save` or Enter sends one rename action; Escape or Cancel leaves
  the saved name unchanged.
- Deleting a workbook removes all of its sheets. Deleting a sheet removes only
  that table.

## Data and server behaviour

- Preserve `file_name`, `file_type`, and `file_size` on every parsed table;
  derive groups from `file_name`, so no persisted schema migration is needed.
- Keep the table key as the Viewer-facing display name. The original worksheet
  name remains as `sheet_name` metadata and is never altered by a rename.
- Add actions `remove_workbook` and `rename`. Existing `remove` remains the
  per-sheet removal action.
- The client listens for `click` and `keydown` only for table edits. It must
  not send a rename from the document `input` listener.

## Tests

- Unit-test grouping and workbook deletion with two files and several sheets.
- Assert the rendered UI contains workbook metadata once per group and sheet
  rows beneath it.
- Assert the browser script has no `.enhance-table-display-name` input handler,
  and sends a rename only through the explicit commit path.
- Run the focused Builder enhancement and UI-contract tests, then a browser
  smoke test against an uploaded workbook when available.

## Out of scope

- Altering original XLSX files or worksheet names.
- A master-detail file manager, drag-and-drop reordering, or bulk sheet rename.
