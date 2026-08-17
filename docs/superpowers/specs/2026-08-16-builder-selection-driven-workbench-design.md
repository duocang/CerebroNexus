# Builder selection-driven workbench design

## Goal

Make the Builder workbench follow the dataset row selected by the user while multiple imports continue in the background.

## Behavior

- Clicking a Ready dataset selects that dataset and renders its normal configuration content even while another import is pending.
- Clicking a server-backed importing row selects that import and renders its loading or error workbench.
- Starting a new import may select its loading row, preserving the existing immediate loading feedback.
- If the selected import becomes Ready, it becomes the current Ready dataset and its configuration content replaces the loading view.
- If the user switched to another Ready dataset before the background import completes, completion does not steal selection.
- Upload transport and import execution remain strictly FIFO with one active server load.

## State authority

`active_import_id` is the selected import row. `current()` is the selected Ready dataset and clears `active_import_id` when a Ready dataset is selected. The workbench and workflow-progress visibility must use `active_import_id`; they must not derive navigation from `builder_import_focus_id(imports())`, which represents global queue progress rather than user selection.

`builder_import_focus_id()` remains available for queue-oriented logic and compatibility tests, but it no longer controls the workbench.

## Scope

This change does not make pre-server client queue rows selectable. Once the active file has a server import row, that row is selectable through the existing delegated `pick_import` action. It does not modify upload dispatch, retry, cancellation, reconnect, BuildPlan, or dataset content rendering.

## Acceptance criteria

- With one Ready dataset and one pending import, selecting Ready removes the loading workbench and shows configuration.
- Selecting the pending import restores its loading workbench.
- Background completion preserves a different Ready selection.
- Completion of the selected import opens its Ready content.
- Existing multi-file FIFO behavior remains unchanged.
