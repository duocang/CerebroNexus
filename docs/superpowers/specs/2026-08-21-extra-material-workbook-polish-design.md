# Extra-material workbook-row polish

## Goal

Make the grouped workbook rows read as ordinary, deliberate disclosure controls
instead of animated cards. The result must make the current state, the action
area, and the upload acknowledgement immediately legible.

## Confirmed visual direction

Use the approved **A — outlined, roomy chevron** prototype.

- A closed workbook displays a clear right-pointing chevron; an open workbook
  displays a down-pointing chevron.
- The chevron is a 28 px outlined control with a conventional 90° state change.
  It replaces the current text glyph and does not animate during open/close.
- The filename and source metadata occupy the flexible center column.
- `Ready`, `Edit`, and `Remove workbook` are one right-aligned action group in
  the workbook header. They remain at the far right independently of filename
  length.
- Upload acknowledgement remains a concise green notice above the list. A newly
  added workbook receives a static, faint green surface/border treatment only;
  no pulsing, outline flash, motion, or automatic expansion.

## Behaviour and accessibility

`<details>/<summary>` remains the disclosure mechanism. CSS derives the visual
chevron state from the native `open` attribute, preserving keyboard operation
and the semantic expanded state. The existing client message continues to close
new workbooks and populate the live status notice; it only applies the quieter
new-workbook class.

## Scope and verification

This is presentation-only. Workbook/table identifiers, rename-on-save behaviour,
upload parsing, and Viewer metadata do not change.

Add or update the UI contract to assert the native-state chevron styling,
right-aligned action group, and absence of the former keyframe animation. Run
the targeted UI contract, the Builder upload integration test, R/JavaScript
syntax checks, and `git diff --check`.
