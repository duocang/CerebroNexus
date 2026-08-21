# Builder CRB Dialog State Race Design

## Problem

After `Prepare checked CRBs` is clicked, the Project result dialog currently
changes from the wide result layout to the narrower generic busy layout. The
whole card contracts from `43rem` to `32rem`, so the icon and copy visibly jump.

A second, more serious issue is caused by two independent progress channels
sharing the same dialog. A late `builder_project_source_progress` message can
arrive while CRBs are being prepared. Its `ready` or `failed` state currently
calls `showBuilderProjectSaveCompletion()` unconditionally, replacing the CRB
progress UI with the earlier `Project saved` result and exposing `Prepare
checked CRBs` again even though the CRB build is still active.

## Chosen design

Treat the CRB dialog as an explicit client-side ownership phase.

- Set a `builderProjectCrbDialogActive` flag before sending
  `prepare_builder_project_crbs`.
- Keep the existing `is-result` layout class while the action buttons are
  removed. This preserves the dialog width and padding through the transition.
- While that flag is active, ignore both late save-result and source-sync
  progress messages for this dialog. Source synchronization continues normally
  and remains represented by the top-bar project status; neither older channel
  may take ownership of the CRB dialog.
- Keep the flag through CRB `ready` or `failed`, because registering the CRB
  triggers a final project/source save whose late progress must not replace the
  terminal CRB result. Clear ownership only when the user closes the dialog.
  The page is inert while this dialog is open, so there is no legitimate
  concurrent user save that needs to replace it.
- The CRB progress handler remains the sole authority for the visible dialog
  during CRB preparation.

## Alternatives considered

1. CSS-only fixed width. This removes the jump but leaves the incorrect state
   overwrite and duplicate action, so it is insufficient.
2. A complete generic operation state machine. This would model every save,
   source-sync, and build phase, but is unnecessary for this isolated race and
   would enlarge the regression surface.
3. The chosen scoped flag plus stable result geometry. It fixes both observed
   failures with a small, explicit ownership rule.

## State flow

1. `Project saved` owns the dialog and offers `Prepare checked CRBs`.
2. Clicking the action marks CRB preparation active, removes actions and success
   styling, retains the wide card geometry, and shows planning/building copy.
3. Late save-result or source-sync messages may update their own server state,
   but cannot change this dialog.
4. CRB `building` and `registering` messages update the same stable card.
5. CRB `ready` or `failed` shows the terminal result with a single `Done`
   action, while retaining ownership until `Done` closes the dialog.

## Regression coverage

The focused UI contract must assert that:

- the prepare action activates the CRB phase without removing `is-result`;
- source-progress completion is ignored while the CRB phase is active;
- CRB terminal states clear the phase;
- closing the dialog clears the phase defensively.

No server protocol or CRB build behavior changes.
