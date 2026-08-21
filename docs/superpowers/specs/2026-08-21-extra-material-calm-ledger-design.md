# Extra material calm-ledger design

## Decision

Use the approved **Calm ledger** layout for Builder's Extra material attachments.
Each uploaded workbook is one full-width disclosure card; its sheets are compact
rows below the header. This is the only layout introduced in this change.

## Required behaviour

- The attachment list has no `max-height`, `overflow-y`, or internal scrollbar.
  It grows naturally as workbooks and sheets are added, so all content remains
  visible in the page scroll.
- An upload keeps existing workbooks in place and appends the new workbook in
  its collapsed state. The page may scroll it into the viewport only when
  necessary; it must not expand a large sheet list unexpectedly.
- The newly added workbook receives a short amber highlight; a compact live
  announcement states how many tables were added and names the workbook.
- A workbook header presents its editable Viewer label, source filename/type/
  size/sheet count, Ready status, Edit, and Remove workbook actions.
- An expanded workbook presents one full-width sheet row per table, with a
  Viewer table label, immutable source-sheet label, Edit, and Remove actions.
- Workbook and sheet Edit actions remain browser-local. Only Save or Enter
  sends one Shiny action; Cancel or Escape discards the local draft.

## Visual rules

- Use one quiet white card per workbook with a thin neutral border and modest
  rounding. Avoid nested coloured panels, large empty regions, and per-row
  file metadata.
- Keep workbook headers visually stronger than sheet rows; sheet rows are
  separated with hairline rules and use a modest left inset.
- Reserve green only for Ready, amber only for the temporary new-item cue, and
  red only for destructive actions.
- At narrow widths, actions wrap below the content without clipping labels.

## Data and integration

The existing `file_name` continues to define one Builder workbook group.
`workbook_name` is the user-facing Viewer workbook label, `sheet_name` retains
the original sheet provenance, and `display_name` is the editable Viewer table
label. The existing table-list key remains an internal unique identifier; it
must not be shown as the sheet title or changed during a label edit.
`table_index` persists this mapping into the CRB for the subsequent Viewer
two-selector work.

## Acceptance checks

1. Five files with seven non-empty sheets render as five workbook cards and
   thirty-five sheet rows, with no internal list scrollbar.
2. Uploading a sixth file makes its collapsed card visibly appear, highlights
   it once, and announces the added-table count.
3. A long list increases document height rather than clipping existing cards.
4. Typing in either edit field does not trigger a Shiny action or rerender;
   Save/Enter does, Cancel/Escape does not.
5. Existing upload, rename, removal, metadata, responsive, and accessibility
   contracts continue to pass.

## Self-review

- No placeholder decisions remain: the selected layout, scroll behaviour,
  feedback, hierarchy, and persistence contract are explicit.
- Scope is restricted to Builder attachments and does not alter the pending
  Viewer selector implementation.
- The visual and behavioural requirements agree: page growth is the mechanism
  that makes newly added cards visible, while the live cue communicates success.
