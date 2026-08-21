# Builder Create Project: Non-empty Folder Warning

## Goal

Prevent the initial **Create project → Choose project folder** flow from silently
placing Builder-managed files into a directory that already contains unrelated
content.

## Folder classification

After the operating-system folder picker returns a valid directory, classify it
before creating the project:

1. **Empty directory** — create and save the Builder project immediately.
2. **Existing Builder project** — do not create anything; keep the existing
   warning that directs the user to **Open project**.
3. **Non-empty directory without a Builder manifest** — do not create anything
   yet; show a confirmation dialog.

Hidden files count as existing content. The check is read-only and happens before
the new manifest or any managed subdirectory is written.

## Confirmation dialog

The dialog identifies the selected folder and explains that Builder files will be
added without deleting unrelated content. It provides two explicit actions:

- **Choose another folder** — close the warning and reopen the system folder
  picker.
- **Create project here** — create the manifest and run the existing first-save
  flow in the selected directory.

Closing or cancelling the dialog performs no filesystem writes. A pending folder
selection is kept only in the current Shiny session and is cleared after cancel,
successful creation, session close, or a superseding selection.

## Safety and concurrency

Immediately before the user-confirmed create operation, re-check the directory.
If a Builder manifest appeared in the meantime, stop and direct the user to
**Open project**. Existing unrelated files are never overwritten or removed;
Builder's existing managed-path and atomic-save rules remain authoritative.

## Tests

Cover empty, existing-project, and unrelated-non-empty directories; confirmation
and cancellation; hidden-file detection; and the manifest race between initial
selection and confirmation. Add a server contract confirming that the initial
selection does not write until the user confirms.
