# Saved-view name dialog

## Goal

Replace browser-native save and rename prompts with a small application-owned
dialog that follows the existing linked-workspace visual system.

## Behaviour

- One reusable dialog supports both save and rename modes.
- Save mode uses title `Save current view`, explains that the name is for finding
  the view later, and confirms with `Save view`.
- Rename mode uses title `Rename saved view`, pre-fills the current name, and
  confirms with `Rename view`.
- Escape, Cancel, backdrop close, an empty name, and an unchanged rename make no
  data change.
- Submitting normalises the name through the existing `snapshotName()` function,
  then invokes the existing save or rename logic.

## Visual rules

- The name dialog sits above the workspace dialog with the same white surface,
  rounded corners, amber eyebrow, close affordance, muted helper copy and amber
  focus ring.
- Cancel is a quiet secondary button; the confirm action is a shallow amber
  primary button with dark readable text.
- The input receives focus when opened and returns focus to the triggering
  Save/Rename control when closed.

## Non-goals

- No changes to localStorage records, JSON configuration, import/export,
  snapshot limits, or restore validation.
