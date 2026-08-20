# Builder Upload and Build Motion Implementation Plan

> **For AI agent workers:** Required sub-skill: use superpowers:executing-plans to implement this plan task by task. Track progress with the checkboxes below.

**Goal:** Eliminate upload rail flicker and bound Build status auto-scrolling to two deliberate movements per click.

**Architecture:** Extend the existing server-authored rail snapshot protocol to pending imports so browser reconciliation preserves unchanged keyed nodes. Replace document-bottom scrolling with a two-phase status-host reveal lifecycle that ends after the first server status arrives.

**Tech stack:** R, Shiny, vanilla JavaScript, testthat, shinytest2/Chrome.

---

### Task 1: Stable import rail snapshots

**Files:**
- Modify: `inst/builder/ui/dataset_rail.R`
- Modify: `inst/builder/server/datasets.R`
- Modify: `inst/builder/www/builder.js`
- Test: `tests/testthat/test-builder-loading-ui.R`
- Test: `tests/testthat/test-builder-rail.R`

- [ ] **Step 1: Write failing R contract tests**

Add tests that call `builder_import_rail_row_model()`,
`builder_import_rail_row_fingerprint()`, and `builder_import_rail_patch()` with
two queued entries. Assert ordered ids, deterministic fingerprints, a changed
fingerprint for a progress-label change, and an unchanged sibling fingerprint.
Assert `inst/builder/server/datasets.R` publishes
`builder_import_rail_patch()` through `builder_import_rail_patch` and no longer
defines `output$ds_import_list <- renderUI`.

- [ ] **Step 2: Run the focused tests and verify RED**

Run:
`Rscript -e 'devtools::test(filter="builder-(loading-ui|rail)", stop_on_failure=TRUE)'`

Expected: failures because the import snapshot functions and message handler do
not exist and the old `renderUI()` path remains.

- [ ] **Step 3: Implement the smallest server snapshot model**

In `dataset_rail.R`, derive one plain row model per `builder_import_entry`, hash
all visible fields into a JSON fingerprint, render a row from that model, and
return an ordered `{rows, empty_html}` patch. Keep the existing public
`builder_import_rail_ui()` as a thin renderer over the same models so initial
markup and patch markup cannot drift.

In `datasets.R`, replace `renderUI()` with a deduplicated observer plus a sync
event that sends `builder_import_rail_patch` snapshots.

- [ ] **Step 4: Implement keyed browser reconciliation**

In `builder.js`, add a generic keyed-row reconciliation boundary or an import
counterpart to `reconcileDatasetRail()`. Validate the complete snapshot before
mutation, reuse rows with matching fingerprints, replace only changed rows,
append in authoritative order, remove stale rows, and restore action focus by
import id. Register the new custom-message handler and request a sync after
Shiny connection.

- [ ] **Step 5: Run focused tests and verify GREEN**

Run the command from Step 2. Expected: zero failures.

- [ ] **Step 6: Commit the upload rail change**

Run:
`git add inst/builder/ui/dataset_rail.R inst/builder/server/datasets.R inst/builder/www/builder.js tests/testthat/test-builder-loading-ui.R tests/testthat/test-builder-rail.R && git commit -m "fix(builder): stabilize import queue updates"`

### Task 2: Preserve explicit selection during queue advancement

**Files:**
- Modify: `inst/builder/loading.R`
- Modify: `inst/builder/server/imports.R`
- Test: `tests/testthat/test-builder-loading-state.R`
- Test: `tests/testthat/test-builder-import-queue.R`
- Test: `tests/testthat/test-builder-loading-browser.R`

- [ ] **Step 1: Write failing selection tests**

Add a pure-state test where import B completes while ready dataset A is the
explicit current selection and assert A remains current. Add a server/browser
case where a user selects A, B advances from queued through running, and A keeps
`aria-current=true` while B's status changes in place.

- [ ] **Step 2: Run focused tests and verify RED**

Run:
`Rscript -e 'devtools::test(filter="builder-(loading-state|import-queue|loading-browser)", stop_on_failure=TRUE)'`

Expected: the new explicit-selection case fails against automatic import focus.

- [ ] **Step 3: Implement minimal selection ownership**

Represent whether the current dataset was explicitly selected at the existing
server selection boundary. Pass that fact into the ready-target decision so an
unwatched completion cannot replace an explicit ready selection. Clear automatic
ownership only when the selected dataset is removed or the workflow resets.

- [ ] **Step 4: Run focused tests and verify GREEN**

Run the command from Step 2. Expected: zero failures.

- [ ] **Step 5: Commit the selection change**

Run:
`git add inst/builder/loading.R inst/builder/server/imports.R tests/testthat/test-builder-loading-state.R tests/testthat/test-builder-import-queue.R tests/testthat/test-builder-loading-browser.R && git commit -m "fix(builder): preserve dataset selection"`

### Task 3: Bounded two-phase Build status reveal

**Files:**
- Modify: `inst/builder/www/builder.js`
- Test: `tests/testthat/test-builder-ui-contract.R`
- Test: `tests/testthat/test-builder-motion-browser.R`

- [ ] **Step 1: Write failing motion tests**

Replace the old `window.scrollTo` contract with assertions for a status-host
reveal helper, a click-scoped phase counter, smooth versus reduced-motion
behavior, and termination after the authoritative status appears. In the browser
test, stub scrolling, click Build, install the server status, and assert exactly
two calls target `#build-stage-status`; a later status mutation must not add a
third call.

- [ ] **Step 2: Run focused tests and verify RED**

Run:
`Rscript -e 'devtools::test(filter="builder-(ui-contract|motion-browser)", stop_on_failure=TRUE)'`

Expected: failures because the current implementation scrolls the document to
its absolute bottom with `behavior: auto`.

- [ ] **Step 3: Implement the two-phase lifecycle**

Replace the boolean pending flag with a click-scoped lifecycle holding the next
phase. Compute only the positive delta required to bring the host's bottom above
the viewport bottom, call `window.scrollBy` with smooth or auto behavior, and
advance once for the client placeholder and once for the first non-client
server status. Clear the lifecycle immediately after phase two.

- [ ] **Step 4: Run focused tests and verify GREEN**

Run the command from Step 2. Expected: zero failures.

- [ ] **Step 5: Commit the motion change**

Run:
`git add inst/builder/www/builder.js tests/testthat/test-builder-ui-contract.R tests/testthat/test-builder-motion-browser.R && git commit -m "fix(builder): bound build status scrolling"`

### Task 4: Final verification and project memory

**Files:**
- Modify after verification: `/Users/nuioi/projects/shiny/cerebroAppLite/.loci/memory.md`
- Modify after verification: `/Users/nuioi/loci/.loci/activity/2026-08.md`

- [ ] **Step 1: Run all affected tests together**

Run:
`Rscript -e 'devtools::test(filter="builder-(loading|import-queue|rail|ui-contract|motion-browser)", stop_on_failure=TRUE)'`

Expected: zero failures.

- [ ] **Step 2: Run repository precheck**

Run: `scripts/precheck.sh`

Expected: exit status 0. If unrelated pre-existing failures remain, record their
exact commands and evidence without claiming the repository check passed.

- [ ] **Step 3: Review the final diff against the design**

Run: `git diff HEAD~2 --check && git status --short --branch`

Confirm stable import nodes, selection ownership, exactly two bounded Build
scroll phases, reduced-motion support, and no unrelated source changes.

- [ ] **Step 4: Record the verified project milestone**

Update the project memory with the final commit ids, test counts, verification
result, and next step. Append one timestamped activity line describing the
Builder UI stability milestone.
