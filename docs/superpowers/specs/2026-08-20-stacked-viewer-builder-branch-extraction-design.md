# Stacked Coordinated Viewer and Builder Branch Extraction

Date: 2026-08-20

## Context

The delivery must be reviewable as two stacked pull requests:

1. `feat/coordinated-linked-views`: the complete Viewer capability, without a
   Builder UI dependency.
2. `feat/builder-project-workspace`: the Builder UI and project workflow, based
   on the complete Viewer branch.

At design approval, the branches form this ancestry:

```text
master@c4ee5267
  `- feat/coordinated-linked-views@c31d2558
       `- feat/builder-project-workspace@c7b90df1
```

The Builder branch contains 74 commits above the Viewer branch. Most later
Viewer work is mixed into the first Builder integration commits rather than
recorded as independent Viewer commits. Copying files or merging the branches
would leave the second pull request with misleading duplicate history.

## Goal

Rewrite the two feature branches into a clean stacked history in which:

- the first branch contains every capability that works without the Builder UI;
- the second branch contains the Builder UI, Builder project/CRB workflow, and
  Builder-specific documentation and tests;
- `feat/coordinated-linked-views` is an ancestor of
  `feat/builder-project-workspace`;
- the rebuilt Builder branch preserves the effective product tree of the
  pre-rewrite Builder branch, apart from this approved history/design material;
- both rewritten remote branches are published only with exact backups and
  `--force-with-lease` protection.

## Classification Boundary

A change belongs in `feat/coordinated-linked-views` when all of the following
are true:

1. It can run in the Viewer or through the public `createShinyApp()` API without
   loading the Builder UI.
2. Its production dependencies are available from the package or generated App
   bundle without sourcing `inst/builder/`.
3. Its tests can be expressed as Viewer, generated-App, public API, spatial
   contract, Trekker, or shared data-model tests rather than Builder workflow
   tests.

This includes shared code whose current symbol names contain `builder_` when the
code is genuinely a pure Viewer/generated-App contract. Names may be cleaned up
only when doing so does not widen this branch-extraction task.

A change remains in `feat/builder-project-workspace` when it requires or
describes any of the following:

- `inst/builder/` UI, server, worker, browser, or project code;
- Save/Open Project, reusable CRB preparation, Builder review/build flow, or
  Builder session state;
- Builder-only fixtures, browser contracts, screenshots, or guided workflow
  documentation;
- a test whose subject is the Builder even if it happens to exercise a shared
  Viewer helper.

## Viewer Branch Content

The extraction uses the final Builder tree as the source of truth for approved
Viewer behavior, not the intermediate state of the original mixed commits.
Expected Viewer-side groups include:

### Coordinated Viewer runtime

- Linked/coordinated projection, Spatial, Trekker, immune, trajectory, and
  shared-selection behavior.
- Multi-FOV coordinate contracts and Viewer-side image/alignment consumption.
- Viewer shell, navigation, accessibility, and responsive runtime assets.
- Viewer fixes made after `c31d2558`, including independent Trekker/Spatial
  rendering and stale-input handling.

### Public and generated-App runtime

- `createShinyApp()` options and bundle runtime needed to configure the Viewer
  without the Builder UI.
- Initial dataset, initial page, point-size, palette, and Viewer-content
  configuration with backward-compatible fallbacks.
- Pure Viewer page/content contracts used by the package and generated bundles.
- Shared Cerebro/spatial data-model support required by those Viewer features.

### Viewer verification and documentation

- Coordinated Viewer, Viewer shell, Trekker, Spatial contract, generated-App,
  and public API tests that do not require the Builder UI.
- Viewer/public-API NEWS and README material.
- Documentation for using the Viewer or `createShinyApp()` directly.

## Builder Branch Content

The rebuilt Builder branch retains:

- all of `inst/builder/`;
- Builder Project persistence and reusable CRB behavior;
- Builder source import, checking, Review, Build, publication, and authentication
  flows;
- Builder-only tests and test helpers;
- Builder screenshots, Builder flow diagrams, and guided Builder vignettes;
- integration code whose only producer or consumer is the Builder UI.

When one file contains both Viewer and Builder behavior, the change is split at
the hunk or function level rather than classified by pathname alone.

## History Rewrite

### 1. Protect both original heads

Before modifying either branch, create timestamped local and remote backup refs
for the exact approved heads of:

- `feat/coordinated-linked-views`;
- `feat/builder-project-workspace`.

Record the object IDs and verify that each backup resolves to the expected tree.
No existing backup ref is moved.

### 2. Build the updated Viewer branch

Use an isolated temporary worktree based on the current Viewer branch. Import
the final, independently usable Viewer/public-API state from the Builder branch
and commit it in reviewable groups:

1. Viewer runtime and shared contracts.
2. Generated-App/public API integration.
3. Viewer-focused tests and documentation.

Do not import whole mixed commits. Stage selected files and hunks, then inspect
every staged path and dependency before committing.

### 3. Rebuild the Builder branch on the new Viewer head

Create a temporary rebuilt Builder branch at the updated Viewer head. Replay the
old Builder range in order. Split the original mixed integration commits so
already-extracted Viewer hunks are omitted and Builder-specific hunks remain.
Preserve later logical commits whenever they apply without semantic change.

The final rewritten Builder branch must have the updated Viewer head as an
ancestor and must not reintroduce duplicate Viewer patches.

### 4. Replace feature refs safely

Only after verification:

- move the local feature refs to the verified rebuilt heads;
- push the Viewer branch with an exact `--force-with-lease` expectation;
- push the Builder branch with an exact `--force-with-lease` expectation;
- confirm local and remote refs match;
- retain the timestamped backups until both pull requests are accepted.

## Verification

### Structural contracts

- `git merge-base --is-ancestor feat/coordinated-linked-views
  feat/builder-project-workspace` succeeds.
- The Viewer branch adds no `inst/builder/` paths, Builder UI tests, Builder
  screenshots, or Builder Project/CRB documentation relative to `master`.
- Viewer production files do not source or reference `inst/builder/` runtime
  code.
- The rewritten Builder tree matches the protected pre-rewrite Builder tree,
  except for explicitly recorded design/history-only changes.
- The Builder pull-request diff against the Viewer branch contains no duplicated
  pure Viewer patch.

### Behavioral verification

Run the smallest focused suites while extracting each logical group, including:

- Coordinated Views and Viewer shell contracts/browser tests.
- Spatial coordinate and multi-FOV tests.
- Trekker and immune integration tests affected by the extraction.
- `createShinyApp()` and generated-App tests affected by moved runtime code.
- Viewer content-contract tests.

After the branch histories and commits are stable, run the repository's full
project verification once. Record exact commands, results, and any conditional
skips before pushing rewritten refs.

## Failure and Recovery

- Any ambiguous mixed hunk stays on the Builder branch until its independence is
  demonstrated.
- A failed extraction or rebase is abandoned on the temporary branch; the
  original feature refs are not moved.
- A failed remote update leaves the other branch untouched until the mismatch is
  understood.
- Both original heads remain recoverable through the timestamped backup refs.
- No worktree or backup ref is deleted as part of this extraction.

## Non-goals

- Redesigning Coordinated Views or Builder behavior.
- Renaming every historical `builder_` helper that is now shared.
- Folding unrelated deploy-hardening work into either branch.
- Squashing the complete feature histories into one commit.
- Opening or merging the upstream pull requests as part of the branch rewrite.
