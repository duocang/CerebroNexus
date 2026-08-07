# Builder Private App Output Implementation Plan

> **For AI agent workers:** Required sub-skill: use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan. Track every step with
> these checkboxes and use `superpowers:test-driven-development` for every
> behavior change.

**Goal:** Enable Cerebro Builder to generate and publish a verified private
Viewer App alongside its CRBs when, and only when, the installed package
declares privacy contract v1.

**Architecture:** Keep Task 0's fail-closed gate. The disposable worker may
assemble an App only from already verified CRBs inside the exact Task 9 stage,
using the accepted `createShinyApp()` implementation. The parent reruns a typed
App verifier against its own frozen expectation, then writes the recursive
release ownership record that gates the existing parent-only transaction; it
remaps the staged App path only after publication. The exact namespace marker
is installed last so no intermediate commit exposes an unverified capability.

**Technical stack:** R, Shiny, callr, testthat, shinytest2, base-R filesystem
primitives, the existing `createShinyApp()` privacy/publication contract, and
the Task 9 coordinator/journal/CAS boundary.

**Starting point:** `feat/cerebro-builder` at `a236e0a9`, based on upstream
`c192a6a0`; upstream PR #102 is present at `18dcef0f`. Do not port code from an
old PR branch and do not push as part of this plan.

---

## File responsibilities

| File | Responsibility |
| --- | --- |
| `R/zzz.R` | Create the eager, locked installed privacy-contract marker during namespace load |
| `R/createShinyApp.R` | Validate and freeze an explicit initial dataset without changing selector order |
| `inst/shiny/v1.4/shiny_server.R` | Honor the frozen initial dataset before smallest/first fallback |
| `inst/builder/prerequisite.R` | Read only an exact locked installed marker; keep every other installation CRB-only |
| `inst/builder/app_bundle.R` | Compile frozen App arguments, assemble inside the assigned stage, and verify the generated App |
| `inst/builder/plan.R` | Freeze only supported App settings, Viewer-bundle assets, and exact expected release members |
| `inst/builder/build.R` | Invoke App assembly only after all CRBs pass read-back verification |
| `inst/builder/session.R` | Recheck the installed marker immediately before worker dispatch |
| `inst/builder/worker.R` | Load App assembly code but never load final publication code |
| `inst/builder/coordinator.R` | Reverify the App as the parent, write/read ownership records, publish, and remap `app_dir` |
| `inst/builder/publish.R` | Support strict ownership-record parsing without weakening lock, CAS, or recovery |
| `tests/testthat/helper-app-privacy.R` | Shared process/HTTP helpers for direct and Builder-generated Apps |
| `tests/testthat/test-builder-app-bundle.R` | Real staged App, verifier, package-free boot, and privacy integration |

## Explicit non-goals

- Do not create a second App packer or copy the internals of
  `createShinyApp()` into Builder code.
- Do not let the worker rename, replace, or otherwise publish the final release.
- Do not make `spatial-assets/` directly downloadable. Contract v1 renders
  configured files server-side through validated paths.
- Do not implement the Task 11 dataset rail, Task 12 workbench UI, or Task 14
  full content matrix here.
- Do not accept arbitrary inert `app_options` that Review does not understand
  and the App builder does not consume.

---

### Task 10.1: Make release membership safely shrinkable

**Files:**
- Modify: `inst/builder/coordinator.R`
- Modify: `inst/builder/publish.R`
- Modify: `tests/testthat/test-builder-coordinator.R`
- Modify: `tests/testthat/test-builder-publish.R`

- [ ] **Step 1: Write failing ownership-record tests**

```r
test_that("a parent-written record permits removal of old owned members", {
  release <- builder_owned_release_fixture(
    members = c("01-a.crb", "02-b.crb", "cerebro_app")
  )
  next_plan <- builder_release_plan_fixture(members = "01-a.crb")

  result <- builder_replace_release_fixture(release, next_plan)

  expect_true(file.exists(file.path(result, "01-a.crb")))
  expect_false(file.exists(file.path(result, "02-b.crb")))
  expect_false(dir.exists(file.path(result, "cerebro_app")))
})

test_that("an unrecorded release member remains foreign and untouched", {
  release <- builder_owned_release_fixture(members = "01-a.crb")
  writeLines("keep", file.path(release, "notes.txt"))

  expect_error(
    builder_replace_release_fixture(release, builder_release_plan_fixture()),
    "foreign release entry"
  )
  expect_identical(readLines(file.path(release, "notes.txt")), "keep")
})
```

Add malformed version/header/path/type, duplicate member, nested foreign file,
symlink, missing recorded member, unknown hidden file, interrupted record
write, and legacy no-record cases. A legacy release may be replaced only when
every existing entry remains expected; it cannot be silently shrunk.

- [ ] **Step 2: Run the focused tests and verify RED**

```bash
Rscript -e 'devtools::test(".", filter = "builder-(coordinator|publish)", reporter = "summary")'
```

Expected: the current coordinator rejects a previous App or dataset that is no
longer in the new expected roots as foreign.

- [ ] **Step 3: Implement the strict parent ownership record**

Use the fixed release member `.cerebro-builder-release-v1`. Its format is
non-executable UTF-8 text: the exact header
`CEREBRO_BUILDER_RELEASE_V1`, followed by sorted `F<TAB>path` and
`D<TAB>path` lines for every recursively owned payload entry. The record is an
implicit fixed owned member and must not list itself. Reject blank values,
unknown type tags, malformed delimiters, dot-segments, duplicates, absolute
paths, Windows devices, control characters in paths, symlinks, and any record
whose listed entries plus the record itself disagree with the actual recursive
release identity.

The worker never writes this record. Give the coordinator separate frozen
`expected_payload_targets` and `expected_final_targets`: the first contains
only worker-produced CRBs, sidecars, and optional App; the second adds every
parent-produced artifact and the ownership record. Validate the payload set,
then atomically write and reread the record with an injectable rename
operation, then validate the final set before `builder_publish_release()`. On a
later prepare, an exact valid record distinguishes removable old Builder
entries from foreign entries. Any foreign entry blocks replacement and remains
untouched. The record bytes, parsed member set, and complete recursive release
identity are captured in expected-prior state and checked again under the
publication lock. The record explains ownership; it never replaces Task 9
expected-prior CAS.

- [ ] **Step 4: Verify GREEN and existing recovery behavior**

```bash
Rscript -e 'devtools::test(".", filter = "builder-(coordinator|publish)", reporter = "summary")'
```

Expected: ownership tests pass together with every Task 9 race, crash, stale-
lock, restoration, and unknown-occupant test.

---

### Task 10.2: Add a dormant verified App assembly path

**Files:**
- Create: `inst/builder/app_bundle.R`
- Create: `tests/testthat/test-builder-app-bundle.R`
- Modify: `R/createShinyApp.R`
- Modify: `man/createShinyApp.Rd`
- Modify: `inst/shiny/v1.4/shiny_server.R`
- Modify: `inst/builder/plan.R`
- Modify: `inst/builder/build.R`
- Modify: `inst/builder/worker.R`
- Modify: `inst/builder/app.R`
- Modify: `tests/testthat/test-createShinyApp-run-options.R`
- Modify: `tests/testthat/test-builder-plan.R`
- Modify: `tests/testthat/test-builder-build.R`
- Modify: `tests/testthat/test-builder-worker-app.R`

- [ ] **Step 1: Write failing initial-dataset and App-request tests**

```r
test_that("explicit initial dataset does not reorder the App selector", {
  app <- create_test_bundle(
    labels = c("A", "B"),
    initial_dataset = "B",
    crb_pick_smallest_file = TRUE
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(names(config$crb_file_to_load), c("A", "B"))
  expect_identical(config$initial_dataset, "B")
})

test_that("App arguments come only from the frozen plan", {
  request <- builder_app_bundle_request(
    builder_app_plan_fixture(),
    built = builder_verified_crb_paths(),
    labels = c("Dataset A", "Dataset B")
  )
  expect_identical(names(request$cerebro_data), request$selector_order)
  expect_identical(request$initial_dataset, "Dataset B")
  expect_false(request$show_upload_ui)
  expect_identical(request$contract_version, 1L)
})

test_that("Viewer-bundle assets are never described as HTTP-public", {
  entry <- builder_dataset_entry_fixture(spatial = TRUE)
  entry$settings$viewer_bundle_assets <- entry$settings$spatial_assets
  plan <- builder_app_plan_fixture(entries = list(entry))
  expect_true("viewer_bundle_assets" %in% names(plan))
  expect_true("viewer_bundle_asset_claims" %in% names(plan))
  recursive_names <- builder_recursive_field_names(plan)
  expect_false(any(c("public_assets", "public_asset_claims") %in% recursive_names))

  legacy <- entry
  legacy$settings$public_assets <- legacy$settings$viewer_bundle_assets
  legacy$settings$viewer_bundle_assets <- NULL
  rejected <- builder_app_plan_fixture(entries = list(legacy))
  expect_s3_class(rejected, "builder_plan_failure")
  expect_identical(rejected$error_code, "invalid_entries")
})
```

Also reject unknown App options, duplicate or mismatched labels, missing or
unverified CRBs, paths outside the assigned stage, an initial dataset absent
from the frozen dataset IDs, and mutable/reference-valued option payloads. Add
tests proving an explicit selection equal to the first dataset remains marked
explicit, while an omitted selection is marked automatic. Replace the older
test that accepted inert `welcome` data: in Task 10 unknown options must return
`invalid_app_options`; Task 12 will add each visible, consumed option back one
at a time.

- [ ] **Step 2: Verify RED**

```bash
Rscript -e 'devtools::test(".", filter = "builder-(app-bundle|plan|build)|createShinyApp-run-options", reporter = "summary")'
```

Expected: `createShinyApp()` lacks `initial_dataset`, `app_bundle.R` is absent,
and `builder_execute_plan()` still explicitly rejects `make_app = TRUE`.

- [ ] **Step 3: Implement the frozen request and package-free initial selection**

Add `initial_dataset = NULL` to `createShinyApp()`. Treat `initial_dataset` as a
reserved config key: remove any caller-supplied value from `cerebro_options`,
validate one exact configured label through the dedicated argument, then write
only that validated value. Update the copied runtime with this priority:
uploaded file > URL selection > current session selection > configured initial
dataset > smallest/first fallback. Match the configured label exactly, take its
unnamed path value, and preserve switcher order. Keep `NULL` backward
compatible. Test URL precedence, later manual switching, and failed config-key
injection.

Implement:

```r
builder_app_bundle_request <- function(plan, built, labels) { ... }
builder_build_app <- function(request, stage,
                              create_app = CerebroNexus::createShinyApp) { ... }
builder_verify_app <- function(app_dir, request) { ... }
```

Rename the internal unpublished BuildPlan fields `public_assets` and
`public_asset_claims` to `viewer_bundle_assets` and
`viewer_bundle_asset_claims` across draft `entry$settings`, every frozen item,
and the top-level plan. They mean eligible Viewer-bundle inputs, never Shiny
HTTP resources; contract v1 has no user-configurable HTTP-public asset class.
Do not retain a compatibility key in frozen state, and let nothing in these
fields create an `addResourcePath()` mapping.

Task 10 accepts only input options `show_upload_ui` and `initial_dataset`.
Freeze the effective dataset ID plus a derived `initial_dataset_mode` of
`automatic` or `explicit`; callers cannot forge the mode. Reject unknown
`app_options` until a later UI task defines and tests them. Map dataset IDs to
labels explicitly. Keep selector order equal to `plan$dataset_order`, set
`crb_pick_smallest_file = FALSE`, and pass palettes keyed by dataset label.

- [ ] **Step 4: Assemble only after CRB verification and verify read-back**

After every CRB succeeds, call the accepted function with the exact named
staged CRBs, `result_dir = file.path(stage, "cerebro_app")`,
`overwrite = FALSE`, `launch_browser = FALSE`, `quiet = TRUE`, and
`verbose = FALSE`.

The verifier must parse `app.R`; read `cerebro_config.rds`; compare labels,
order, initial selection, palettes, upload policy, and backend plan; prove all
configured CRBs and sidecars lie under `private-data/`; reject symlinks and path
escapes; require no legacy `data/`; and return a typed inert verification.
`built` remains the CRB vector. Store the directory separately as `app_dir` and
the evidence as `app_verification`.

Builder-normalized histology is already embedded in the verified CRB as a data
URI plus bounds/transforms; preserve and render that representation rather than
inventing an external `spatial_images` path. External spatial-file bytes and
their server-side allowlist remain covered by direct `createShinyApp()` tests.
Load `app_bundle.R` before `coordinator.R` and `build.R` in the parent Builder,
and before `build.R` in the worker. Add static contracts for both source orders
and prove the worker loads App assembly but never `publish.R`.

Do not define the installed marker yet. Tests may inject contract `1L`, but the
real UI must remain disabled throughout this task.

- [ ] **Step 5: Run focused GREEN**

```bash
Rscript -e 'devtools::test(".", filter = "builder-(app-bundle|plan|build|worker-app)|createShinyApp-run-options", reporter = "summary")'
```

Expected: the dormant path passes, while the real installed capability remains
unavailable because no marker exists.

---

### Task 10.3: Gate parent publication on App evidence and remap final paths

**Files:**
- Modify: `inst/builder/coordinator.R`
- Modify: `inst/builder/session.R`
- Modify: `inst/builder/app.R`
- Modify: `tests/testthat/test-builder-coordinator.R`
- Modify: `tests/testthat/test-builder-prerequisite.R`

- [ ] **Step 1: Write failing publication-boundary tests**

```r
test_that("an App plan requires typed App verification", {
  handle <- builder_app_coordinator_fixture()
  result <- builder_success_fixture(app_dir = handle$app_dir)
  result$app_verification <- NULL
  expect_error(builder_coordinator_publish(handle, result), "App verification")
})

test_that("published App paths are remapped out of the stage", {
  result <- builder_publish_verified_app_fixture()
  expect_identical(
    result$app_dir,
    file.path(result$release$target, "cerebro_app")
  )
  expect_false(grepl("cerebro-control", result$app_dir, fixed = TRUE))
})
```

Reject a valid-looking App outside the assigned stage, a mismatched build ID,
a CRB-only plan carrying an unexpected App, an App plan missing the expected
directory, a forged worker verification, and an App directory changed between
worker verification and the parent's own read-back.

- [ ] **Step 2: Implement the exact parent boundary**

The coordinator handle deep-copies the App expectation from BuildPlan: whether
`cerebro_app` is expected, labels and order, initial dataset/mode, upload
policy, palettes, backend closure, and exact directory. Worker verification is
diagnostic evidence, not publication authority. For App plans, require
`state = "success"`, `publishable = TRUE`, exact assigned stage, and matching
build identity, then have the parent call `builder_verify_app()` again against
its frozen expectation. Recompute the App tree identity from the current stage
before writing the ownership record and final stage identity. For CRB-only
plans, reject an unexpected App. After publication, remap `app_dir`
independently from `built` and clear all stage paths from the user result.

Apply Task 10.1's two-set rule here: first validate
`expected_payload_targets`, then add parent-authored files, write/read the
ownership record, and validate `expected_final_targets`. The worker can never
declare either set complete.

The session retains its immediate real-namespace recheck. A plan frozen under
contract v1 fails before `createShinyApp()` if the current installed marker is
missing or changed.

- [ ] **Step 3: Verify GREEN with Task 9 regressions**

```bash
Rscript -e 'devtools::test(".", filter = "builder-(prerequisite|session|coordinator|publish|app-bundle|worker-app)", reporter = "summary")'
```

Expected: App evidence/remapping and all existing parent-only publication tests
pass; `inst/builder/worker.R` still contains no source or call to `publish.R`.

---

### Task 10.4: Activate contract v1 and prove the real privacy path

**Files:**
- Create: `R/zzz.R`
- Create: `tests/testthat/helper-app-privacy.R`
- Modify: `inst/builder/prerequisite.R`
- Modify: `tests/testthat/test-builder-prerequisite.R`
- Modify: `tests/testthat/test-builder-app-bundle.R`
- Modify: `tests/testthat/test-createShinyApp-http-privacy.R`
- Modify: `tests/testthat/test-smoke-production.R`

- [ ] **Step 1: Move reusable HTTP helpers without changing assertions**

Move the process start/stop/wait/log/GET and legacy-App helpers from
`test-createShinyApp-http-privacy.R` into `helper-app-privacy.R`. Run the direct
privacy test before adding new behavior and require the same result.

- [ ] **Step 2: Prove the complete dormant path before exposing it**

Add one small deterministic integration fixture that uses the real adapter,
snapshot, frozen plan, verified CRBs, staged App, parent publication, and
package-free boot. Inject contract `1L` only at the test seam; first assert that
the actual installed namespace still reports contract `0L`. Assert exact
configuration identity, initial dataset, selector order, and Viewer startup.
HTTP requests to `/data/*`, `/private-data/*`, and `/spatial-assets/*` must
return 404. For the Builder fixture, prove that normalized histology remains
embedded in the verified CRB, its bounds/transforms survive App loading, and it
renders without an HTTP file URL. The existing direct `createShinyApp()` test
continues to prove external spatial-file bytes remain identical inside
`spatial-assets/` while direct access returns 404. Existing direct tests also
cover H5/BPCells closure and the running-legacy-`/data` upgrade scenario; use
one representative real backend in the Builder fixture and assert that its
configured location remains private.

Use the same generated App to prove that the second initial dataset is active
on first boot, while URL selection and later user selector changes still win.
Task 14, not this task, owns the full backend/content Cartesian matrix.

- [ ] **Step 3: Run the dormant integration gate and require GREEN**

```bash
Rscript -e 'devtools::test(".", filter = "builder-app-bundle|createShinyApp-http-privacy|smoke-production", reporter = "summary")'
```

If any assertion fails, fix the dormant path and rerun it while the real
installed marker is still absent. Do not proceed by temporarily defining the
marker.

- [ ] **Step 4: Write the failing real installed-marker and user-path tests**

```r
test_that("installed privacy contract v1 is eager and locked", {
  namespace <- asNamespace("CerebroNexus")
  marker <- ".cerebro_bundle_privacy_contract_version"
  expect_true(exists(marker, namespace, inherits = FALSE))
  expect_false(bindingIsActive(marker, namespace))
  expect_false(rlang::env_binding_are_lazy(namespace, marker))
  expect_true(bindingIsLocked(marker, namespace))
  expect_identical(get(marker, namespace, inherits = FALSE), 1L)
})
```

Update the detector so unlocked test bindings are rejected as contract `0L`.
Retain no-execution assertions for functions, active bindings, and promises.
Update the existing fake positive fixture to call `lockBinding()` before it
expects `1L`; an unlocked ordinary value is now deliberately negative.
Repeat the integration fixture through `builder_session_build()` without a
capability stub; before activation it must fail at the real namespace recheck.

- [ ] **Step 5: Define the marker only after the dormant gate passes**

```r
.onLoad <- function(libname, pkgname) {
  namespace <- asNamespace(pkgname)
  marker <- ".cerebro_bundle_privacy_contract_version"
  assign(marker, 1L, envir = namespace)
  lockBinding(marker, namespace)
}
```

Do not export the marker and do not derive it from version text. The value
declares the implementation contract protected by the privacy suite.

- [ ] **Step 6: Reinstall and rerun through the real namespace**

Run `R CMD INSTALL .`, require the marker test to pass against that fresh
installation in a clean process:

```bash
Rscript --vanilla -e 'ns <- asNamespace("CerebroNexus"); m <- ".cerebro_bundle_privacy_contract_version"; stopifnot(exists(m, ns, inherits = FALSE), !bindingIsActive(m, ns), !isTRUE(unname(rlang::env_binding_are_lazy(ns, m))), bindingIsLocked(m, ns), identical(get(m, ns, inherits = FALSE), 1L))'
```

Then rerun the same integration fixture without injected capability. This
second pass proves the actual user path reaches exactly the dormant
implementation that already passed the privacy gate. `devtools::test()` is not
a substitute for the clean-process command because it loads the source tree.

- [ ] **Step 7: Run focused and full verification**

```bash
R CMD INSTALL .
Rscript --vanilla -e 'ns <- asNamespace("CerebroNexus"); m <- ".cerebro_bundle_privacy_contract_version"; stopifnot(exists(m, ns, inherits = FALSE), !bindingIsActive(m, ns), !isTRUE(unname(rlang::env_binding_are_lazy(ns, m))), bindingIsLocked(m, ns), identical(get(m, ns, inherits = FALSE), 1L))'
Rscript -e 'devtools::test(".", filter = "builder-(prerequisite|plan|app-bundle|build|worker-app|session|coordinator|publish)", reporter = "summary")'
Rscript -e 'devtools::test(".", filter = "createShinyApp-(http-privacy|sibling|publication|lock|run-options)|smoke-production|export-data-integrity|app-inst", reporter = "summary")'
scripts/precheck.sh
git diff --check
```

Expected: all tests pass; R CMD check has zero errors and warnings; pkgdown
passes; only explicitly pre-existing NOTEs may remain.

- [ ] **Step 8: Commit the complete activation**

```bash
git add R/zzz.R R/createShinyApp.R man/createShinyApp.Rd \
  inst/shiny/v1.4/shiny_server.R inst/builder \
  tests/testthat/helper-app-privacy.R \
  tests/testthat/test-builder-app-bundle.R \
  tests/testthat/test-builder-prerequisite.R \
  tests/testthat/test-builder-plan.R tests/testthat/test-builder-build.R \
  tests/testthat/test-builder-worker-app.R \
  tests/testthat/test-builder-coordinator.R \
  tests/testthat/test-builder-publish.R \
  tests/testthat/test-createShinyApp-http-privacy.R \
  tests/testthat/test-createShinyApp-run-options.R \
  tests/testthat/test-smoke-production.R
git commit -m "feat(builder): enable private app bundles"
```

Do not push. Task 11 begins only after this commit is independently reviewed
and the working tree is clean.
