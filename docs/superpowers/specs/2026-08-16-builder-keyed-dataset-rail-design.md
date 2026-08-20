# Builder keyed dataset rail design

## Goal

Stop unrelated ready-dataset rows from visually or structurally changing when one dataset finishes import, becomes selected, is reordered, or saves spatial settings. Preserve the existing rail appearance, upload scheduler, dataset state, BuildPlan, and Save/Reset semantics.

## Selected approach

The server remains the single source of truth for row markup. It produces a target rail model keyed by dataset ID; the browser reconciles that model against existing `[data-ds]` nodes. This is preferred over retaining the monolithic `renderUI`, which cannot preserve unchanged DOM nodes, and over rebuilding row templates in JavaScript, which would duplicate accessibility and state-label logic.

The CSS rule that disables Shiny's ready-rail recalculation fade remains as a fallback. It is not the primary update mechanism.

## Server model

Add focused helpers alongside the existing rail renderer:

- `builder_dataset_rail_row_model(entry, index, total, current)` computes all visible row state once: ID, label, cell count, format, readiness, active selection, remove-confirm requirement, and up/down availability.
- `builder_dataset_rail_row_ui(model)` produces the current server-authored row markup.
- `builder_dataset_rail_model(state, current)` returns ordered row models plus the empty-state markup.
- Each rendered row carries `data-rail-fingerprint`. The fingerprint covers every value that can alter markup: index, ID, label, cell count, format, readiness label, selected state, remove-confirm requirement, and up/down availability.

`builder_dataset_rail_ui()` delegates to these helpers so initial rendering and later patches cannot drift.

## Reactive boundary

The existing `output$ds_ready_list` becomes initial/fallback rendering only and does not stay reactively dependent on the complete `store()` and `current()` objects.

A dedicated observer reads both `store()` and `current()`, builds one target rail model, and compares it with the last model held in a `reactiveVal`. It sends `builder_dataset_rail_patch` only when the visible model changes. Reading both inputs in one observer lets Shiny coalesce import completion's `store` and `current` writes into one target model.

After the first UI flush, the server sends a complete target snapshot. Subsequent messages also carry complete target snapshots rather than imperative deltas. This makes reconnect and missed-message recovery deterministic: the current snapshot always wins.

## Message contract

`builder_dataset_rail_patch` contains:

```json
{
  "rows": [
    {"id": "dataset-id", "fingerprint": "...", "html": "<div ...>...</div>"}
  ],
  "empty_html": "<div class=\"rail-empty\">...</div>"
}
```

Rows are in authoritative server order. HTML is generated only by the R UI helper and escaped through Shiny's tag rendering.

## Browser reconciliation

The handler locates `#ds_ready_list` and performs a keyed reconciliation:

1. Record the focused control as dataset ID plus a stable action signature when focus is inside the rail.
2. Parse each server row as exactly one element and reject malformed rows or mismatched IDs.
3. Reuse an existing row when its fingerprint matches.
4. Replace only rows whose fingerprint changed.
5. Insert missing rows, move existing nodes into server order, and remove IDs absent from the snapshot.
6. Render the empty state only when the target has no rows.
7. Restore focus to the equivalent control if its node was replaced; otherwise use the existing rail focus fallback.
8. Refresh the compact Dataset Manager summary after reconciliation.

All ready-row actions already use document-level event delegation, so inserted and replaced rows need no event rebinding. Unchanged nodes retain hover, focus, and browser-local state naturally.

## Confirmation and race handling

The remove confirmation dialog is outside the row and holds the dataset ID as its durable identity. A patch normally reuses the unchanged source row. If that row must be replaced while the dialog is open, focus restoration resolves the new control by dataset ID instead of focusing a detached element. If the dataset disappears, the existing rail fallback receives focus.

Every snapshot is authoritative and reconciliation is synchronous. Later snapshots replace earlier target state, so Save/import/reorder races converge without client-side guessing.

## Failure handling

Malformed messages, duplicate IDs, invalid row HTML, or a missing rail container do not partially mutate the DOM. The handler logs one diagnostic and leaves the last valid rail intact. The next complete snapshot can recover it. The scoped recalculation-opacity rule prevents a fallback server rerender from flashing the whole rail.

## Scope exclusions

- No change to `ds_import_list` or the strict serial client upload scheduler.
- No change to dataset import, spatial transform persistence, BuildPlan generation, build/export, or reconnect protocols.
- No general virtual DOM framework and no row template duplication in JavaScript.
- No visual redesign beyond the already approved Ready stamp, state colors, hover behavior, and Reset outline button.

## Acceptance criteria

- Importing a new dataset inserts that row without replacing existing row nodes.
- Spatial Save with unchanged rail-visible state sends no patch and preserves every row node.
- A readiness change replaces only the affected dataset row.
- Selection changes replace only the old and new selected rows.
- Reordering moves existing row nodes and updates only rows whose index/button availability changed.
- Removing a dataset removes only its row; an open confirmation cannot restore focus to a detached node.
- Empty-to-populated and populated-to-empty transitions render correctly.
- Upload queue behavior and all existing server-authored labels/actions remain unchanged.
