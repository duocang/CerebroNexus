# Builder Real Export Recovery Implementation Plan

> **For the AI implementer:** Required sub-skill: use
> `superpowers:executing-plans` to execute this plan task by task. Every code
> task follows red-green TDD.

**Goal:** Restore Builder's real two-dataset export path so it can produce a
complete Viewer App with full metadata and 14 external named spatial images.

**Architecture:** Keep final frozen-plan validation strict, but infer the App
requirement while building the Configure/Review preview plan. Explicitly add
the Bootstrap dependency required by Shiny modals without wrapping or
restyling the Builder shell. Lock the Build output to App when the reviewed
plan contains external spatial assets.

**Technical stack:** R, Shiny, htmltools, testthat, shinytest2, existing Builder
state/plan/workflow contracts.

---

## Files and responsibilities

- Modify `inst/builder/app.R`: attach the Bootstrap dependency used by
  `showModal()`.
- Modify `inst/builder/plan/preflight.R`: expose a pure predicate that detects
  whether current entries require a Viewer App.
- Modify `inst/builder/server/review.R`: freeze Configure/Review preview plans
  as App plans when external images require one, and initialize Build mode from
  the reviewed plan.
- Modify `inst/builder/server/build.R`: prevent an external-image plan from
  being switched back to CRB-only output.
- Modify `inst/builder/ui/build_status.R`: disable CRB-only selection and show
  the requirement when an App is mandatory.
- Modify `tests/testthat/test-builder-ui-contract.R`: lock the modal dependency
  contract.
- Modify `tests/testthat/test-builder-plan-readiness.R`: lock App-requirement
  inference while preserving strict final validation.
- Modify `tests/testthat/test-builder-stage-server.R`: lock preview-plan and
  Build-mode behavior.
- Modify `tests/testthat/test-builder-browser.R`: exercise the real modal and
  external-image workflow in a browser.

### Task 1: Restore the Shiny modal dependency

- [ ] **Step 1: Add failing UI dependency and browser tests**

Append to `tests/testthat/test-builder-ui-contract.R`:

```r
test_that("Builder provides the Bootstrap JavaScript required by Shiny modals", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(dirname(builder_asset_path("app.R")))
  sys.source("app.R", envir = app_env)

  dependencies <- htmltools::renderTags(app_env$ui)$dependencies
  dependency_names <- vapply(dependencies, `[[`, character(1), "name")
expect_true("bootstrap" %in% dependency_names)
})
```

Also add a browser test to `tests/testthat/test-builder-browser.R`: create a
temporary PNG with `png::writePNG()`, load the `all_content` example, upload it
through `enhance-tissue_image_file`, save alignment, then upload the same
basename a second time. Assert the `Name this image` modal is visible, set
`enhance-new_image_label = "DAPI"`, click `enhance-add_image_confirm`, save,
and assert two image options exist. Finish with:

```r
builder_expect_clean_browser_logs(app)
```

- [ ] **Step 2: Verify the test fails for the missing dependency**

Run both tests:

```bash
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-builder-ui-contract.R", reporter="summary")'
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-builder-browser.R", reporter="summary")'
```

Expected: the UI test fails because `bootstrap` is absent, and the browser test
fails with `.modal is not a function`.

- [ ] **Step 3: Add the minimum dependency**

Change the start of `ui` in `inst/builder/app.R` to:

```r
ui <- tagList(
  shiny::bootstrapLib(),
  tags$head(
```

Do not replace `tagList()` with `fluidPage()` or `bootstrapPage()`; the existing
shell and CSS remain authoritative.

- [ ] **Step 4: Verify the focused UI and browser tests pass**

Run both commands from Step 2.

Expected: PASS with zero failures.

- [ ] **Step 5: Commit the modal dependency fix**

```bash
git add inst/builder/app.R tests/testthat/test-builder-ui-contract.R \
  tests/testthat/test-builder-browser.R
git commit -m "fix(builder): restore modal dependency"
```

### Task 2: Infer the App requirement without weakening final validation

- [ ] **Step 1: Add failing predicate tests**

In `tests/testthat/test-builder-plan-readiness.R`, extend the existing
`spatial image storage and nested image counts freeze exactly` test immediately
after the saved record is assigned:

```r
expect_false(builder_plan_requires_app(list(entry)))

entry$settings$spatial_image_storage <- "external"
expect_true(builder_plan_requires_app(list(entry)))

entry$settings$images <- list()
expect_false(builder_plan_requires_app(list(entry)))
entry$settings$images <- list(fov = list(`H&E` = record))
```

Keep the existing assertion that
`builder_freeze_plan(..., make_app = FALSE)` returns
`external_images_require_app`.

- [ ] **Step 2: Verify the test fails because the predicate is absent**

Run:

```bash
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-builder-plan-readiness.R", reporter="summary")'
```

Expected: ERROR/FAIL stating `builder_plan_requires_app` is not found.

- [ ] **Step 3: Implement the pure predicate**

Add above `.builder_plan_preflight_entries()` in
`inst/builder/plan/preflight.R`:

```r
builder_plan_requires_app <- function(entries) {
  if (!is.list(entries)) {
    stop("Builder entries must be supplied as a list.", call. = FALSE)
  }
  any(vapply(entries, function(entry) {
    if (!is.list(entry) || !is.list(entry$settings)) {
      return(FALSE)
    }
    if (!identical(
      entry$settings$spatial_image_storage %||% "embedded",
      "external"
    )) {
      return(FALSE)
    }
    alignments <- .builder_plan_partition_alignments(
      entry$settings$images %||% list()
    )
    length(.builder_plan_flatten_spatial_images(alignments$spatial)) > 0L ||
      !is.null(alignments$trekker)
  }, logical(1)))
}
```

- [ ] **Step 4: Verify focused plan tests pass**

Run the command from Step 2.

Expected: PASS, including the unchanged strict CRB-only rejection.

- [ ] **Step 5: Commit the inference predicate**

```bash
git add inst/builder/plan/preflight.R tests/testthat/test-builder-plan-readiness.R
git commit -m "fix(builder): infer external image app requirement"
```

### Task 3: Carry the required App mode through Review and Build

- [ ] **Step 1: Add failing server and UI tests**

In `tests/testthat/test-builder-stage-server.R`, add a server test whose
`sets()` fixture contains a saved external image and assert:

```r
preview <- frozen_review_plan()
expect_true(builder_review_can_build(preview))
expect_true(preview$make_app)
```

Then transition the workflow through `open_review` and `confirm_review` and
assert after flushing:

```r
expect_true(build_mode())
```

In the existing Build options UI assertions, call:

```r
required_ui <- builder_build_options_ui(
  builder_build_options(make_app = TRUE),
  app_required = TRUE
)
required_html <- htmltools::renderTags(required_ui)$html
expect_match(required_html, "External spatial images require", fixed = TRUE)
expect_match(required_html, 'value="crb" disabled', fixed = TRUE)
```

Extend Task 1's duplicate-image browser test: after two saved external images
exist, click `continue_to_review`, wait for `[data-workflow-stage=review]`, and
assert Review declares `CRB files + Viewer App`.

- [ ] **Step 2: Verify the tests fail for CRB-only preview behavior and the
missing UI argument**

Run:

```bash
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-builder-stage-server.R", reporter="summary")'
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-builder-browser.R", reporter="summary")'
```

Expected: FAIL because the preview plan is blocked and
`app_required` is not accepted; the browser test cannot enter Review.

- [ ] **Step 3: Infer App mode in preview planning**

In `freeze_plan_for_output()` in `inst/builder/server/review.R`, obtain `all <-
sets()` before deciding `make_app`, then use:

```r
explicit_output <- inherits(output_options, "builder_build_options")
make_app <- if (explicit_output) {
  isTRUE(output_options$make_app)
} else {
  builder_plan_requires_app(all)
}
```

Retain explicit CRB-only rejection by never overriding an explicitly supplied
`builder_build_options(make_app = FALSE)`.

In `observeEvent(input$confirm_review, ...)`, before updating `workflow`, add:

```r
if (isTRUE(live$make_app)) {
  build_mode(TRUE)
}
```

- [ ] **Step 4: Lock required output in Build state and UI**

Add `app_required = FALSE` to `builder_build_options_ui()` in
`inst/builder/ui/build_status.R`. When true, recursively add `disabled` and
`aria-disabled` to the radio input whose value is `crb`, and render:

```r
p(
  class = "hint builder-app-required-reason",
  "External spatial images require CRB files + Viewer App output."
)
```

In `output$build_output_options` in `inst/builder/server/build.R`, pass:

```r
app_required = isTRUE(plan$make_app)
```

At the start of the `input$build_output_mode` observer, derive:

```r
app_required <- isTRUE(isolate(workflow())$review_plan$make_app)
requested <- identical(input$build_output_mode, "app")
enabled <- (requested || app_required) && isTRUE(app_capability$available)
```

This preserves App mode even if a client submits `crb` for a required plan.

- [ ] **Step 5: Verify server, UI, and browser tests pass**

Run the command from Step 2.

Expected: PASS with zero failures.

- [ ] **Step 6: Commit the workflow fix**

```bash
git add inst/builder/server/review.R inst/builder/server/build.R \
  inst/builder/ui/build_status.R tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-browser.R
git commit -m "fix(builder): carry required app output through workflow"
```

### Task 4: Prove the complete staged workflow in a real browser

- [ ] **Step 1: Run the complete focused browser regression**

Run:

```bash
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-builder-browser.R", reporter="summary")'
```

Expected: PASS, two named images, Review stage reached, and no browser console
errors.

- [ ] **Step 2: Run focused non-browser regression tests**

```bash
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-builder-spatial.R", reporter="summary"); testthat::test_file("tests/testthat/test-builder-plan-readiness.R", reporter="summary"); testthat::test_file("tests/testthat/test-builder-stage-server.R", reporter="summary"); testthat::test_file("tests/testthat/test-builder-ui-contract.R", reporter="summary")'
```

Expected: all files report DONE with zero failures/errors/warnings.

- [ ] **Step 3: Commit only if verification required a corrective change**

If no corrective change was needed, do not create an empty commit. Otherwise,
commit only the minimal correction with a message describing that behavior.

### Task 5: Rebuild and compare the two real exports

- [ ] **Step 1: Clear only the current export targets**

Move existing `builder_export` and `cli_export` into timestamped directories
under the user's Trash, recreate an empty
`/Users/nuioi/Downloads/anna_lena/builder_export`, and do not touch `data/`.

- [ ] **Step 2: Regenerate the CLI reference**

```bash
Rscript /Users/nuioi/Downloads/anna_lena/export_cli_app.R
```

Expected: two CRBs, one complete App, and 14 files under `spatial-assets/`.

- [ ] **Step 3: Export through the actual Builder UI**

Launch the updated Builder from the worktree. Upload the two real RDS files,
select all supported metadata and all eight Groups, retain UMAP/t-SNE,
Spatial, and Trekker, choose external image storage, add and save all 14 images,
select the complete App output, and build into
`/Users/nuioi/Downloads/anna_lena/builder_export`.

- [ ] **Step 4: Verify data and topology parity**

Run an R verification script that loads paired CRBs and asserts equal
expression matrices, metadata values/columns, Groups, projections, spatial
coordinates, and Trekker core content. Assert both App configurations declare
14 external images and that all image hashes match their source files. Compare
the Viewer trees while allowing only intentional Builder filenames/report
metadata to differ.

- [ ] **Step 5: Start both Apps and verify HTTP**

Run the Builder and CLI Apps on separate local ports, request `/`, and require
HTTP 200 plus `<title>CerebroNexus</title>` from both.

- [ ] **Step 6: Run the final project checks**

Run the smallest complete package check used by this branch, followed by:

```bash
git diff --check
git status --short
```

Expected: package checks pass, no whitespace errors, and only intentional
commits/changes remain.

- [ ] **Step 7: Record the verified outcome**

Update project-local memory only after verification, recording the final
commit, output paths, parity result, and any remaining intentional differences.
