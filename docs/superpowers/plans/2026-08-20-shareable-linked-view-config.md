# Shareable Linked View Configuration Implementation Plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**Goal:** Let Linked views users export, copy, and restore a versioned JSON configuration that reproduces a selected cohort and its coordinated visual context.

**Architecture:** The browser remains the interactive-state authority and exposes a narrow capture/apply adapter. A self-contained R module validates, normalizes, fingerprints, and canonically encodes the exchange document; the Shiny server transports only validated documents between that module and a small dialog controller.

**Tech stack:** R, Shiny, jsonlite, testthat, vanilla JavaScript, Canvas, CSS, shinytest2, Node syntax checks.

---

## File Map

- Create `inst/viewer/coordinated_views/config.R`: pure versioned JSON contract, limits, fingerprint, validation, and canonical encoding.
- Modify `inst/viewer/coordinated_views/server.R`: add bundle fingerprint and nonce-based copy/download/upload transport.
- Modify `inst/viewer/coordinated_views/UI.R`: add Save/share trigger, accessible dialog, upload input, status region, and hidden download link.
- Modify `inst/viewer/www/coordviews.js`: capture/apply adapter over the existing coordinated state.
- Create `inst/viewer/www/coordviews-config.js`: dialog and Shiny transport controller.
- Modify `inst/viewer/www/coordviews.css`: styles for the compact dialog and actions.
- Modify `R/shiny_UI.R`: include the new browser controller in generated and package-launched viewers.
- Create `tests/testthat/test-coordinated-views-config.R`: pure contract and bundle-safe integration tests.
- Create `tests/testthat/test-coordinated-views-config-browser.R`: real-browser state and dialog tests.
- Modify `tests/testthat/test-smoke-production.R`: assert the generated App contains all configuration runtime assets.

### Task 1: Versioned R contract and dataset identity

**Files:**
- Create: `tests/testthat/test-coordinated-views-config.R`
- Create: `inst/viewer/coordinated_views/config.R`

- [ ] **Step 1: Write failing fingerprint and valid-round-trip tests**

```r
test_that("linked-view fingerprints ignore cell order", {
  expect_identical(
    cv_config_cell_fingerprint(c("cell-b", "cell-a")),
    cv_config_cell_fingerprint(c("cell-a", "cell-b"))
  )
})

test_that("a version-one configuration round-trips canonically", {
  normalized <- cv_config_normalize(valid_config(), cells = fixture_cells)
  decoded <- cv_config_decode(
    cv_config_encode(normalized),
    cells = fixture_cells
  )
  expect_identical(decoded$selection$cells, c("cell-a", "cell-c"))
  expect_identical(decoded$schema, "cerebronexus-linked-view")
  expect_identical(decoded$version, 1L)
})
```

- [ ] **Step 2: Run the focused test and verify RED**

Run: `Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`

Expected: FAIL because `inst/viewer/coordinated_views/config.R` and `cv_config_cell_fingerprint()` do not exist.

- [ ] **Step 3: Implement the fingerprint, envelope validator, and encoder**

```r
cv_config_cell_fingerprint <- function(cells) {
  cells <- sort(enc2utf8(as.character(cells)), method = "radix")
  stream <- paste0(nchar(cells, type = "bytes"), ":", cells, collapse = "")
  path <- tempfile("coordviews-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw(stream), path)
  paste0("md5-cell-set-v1:", unname(tools::md5sum(path)))
}

cv_config_decode <- function(text, cells) {
  cv_config_check_json_size(text)
  value <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  cv_config_normalize(value, cells = cells)
}
```

Implement strict helpers for records, scalars, unique arrays, finite bounded numbers, maximum depth 10, and canonical field ordering. Accept no unknown keys.

- [ ] **Step 4: Run the focused test and verify GREEN**

Run the command from Step 2.

Expected: PASS with no warnings.

- [ ] **Step 5: Commit the first contract slice**

```bash
git add inst/viewer/coordinated_views/config.R tests/testthat/test-coordinated-views-config.R
git commit -m "feat: add linked view config contract"
```

### Task 2: Reject unsafe and incompatible documents

**Files:**
- Modify: `tests/testthat/test-coordinated-views-config.R`
- Modify: `inst/viewer/coordinated_views/config.R`

- [ ] **Step 1: Write a failing validation matrix**

```r
test_that("configuration validation is strict and transactional", {
  cases <- list(
    unsupported_schema = within(valid_config(), schema <- "other"),
    future_version = within(valid_config(), version <- 2L),
    wrong_count = within(valid_config(), dataset$cell_count <- 99L),
    wrong_fingerprint = within(valid_config(), dataset$cell_fingerprint <- "md5-cell-set-v1:00000000000000000000000000000000"),
    missing_cell = within(valid_config(), selection$cells <- "missing"),
    duplicate_cell = within(valid_config(), selection$cells <- c("cell-a", "cell-a")),
    unknown_field = c(valid_config(), list(secret = "no"))
  )
  for (name in names(cases)) {
    expect_error(cv_config_normalize(cases[[name]], fixture_cells), class = "cv_config_error", info = name)
  }
})
```

Add individual tests for a document above 5 MiB, depth above 10, strings above their byte limits, non-finite values, out-of-range display values, duplicate lenses, invalid spatial image identifiers, and canonical output privacy.

- [ ] **Step 2: Run the focused test and verify RED**

Expected: the new rejection cases fail because the first slice does not enforce every bound.

- [ ] **Step 3: Complete strict normalization**

```r
cv_config_abort <- function(code, message) {
  stop(structure(
    list(message = message, call = NULL, code = code),
    class = c("cv_config_error", "error", "condition")
  ))
}
```

Normalize every version-one field into a fresh allowlisted list, compare both dataset identity fields, require every selected barcode to be present, and map internal failures to stable safe error codes.

- [ ] **Step 4: Run the focused test and verify GREEN**

Run the Task 1 test command.

Expected: PASS; canonical JSON contains no path, data URI, expression matrix, metadata payload, token, or credentials.

- [ ] **Step 5: Commit validation**

```bash
git add inst/viewer/coordinated_views/config.R tests/testthat/test-coordinated-views-config.R
git commit -m "feat: validate linked view configs"
```

### Task 3: Add server-side transport

**Files:**
- Modify: `tests/testthat/test-coordinated-views-config.R`
- Modify: `inst/viewer/coordinated_views/server.R`

- [ ] **Step 1: Write failing server contract tests**

```r
test_that("the coordinated server fingerprints bundles and exposes config transport", {
  text <- readLines(viewer_file("coordinated_views", "server.R"), warn = FALSE)
  expect_match(paste(text, collapse = "\n"), "cv_config_cell_fingerprint\\(b\\$cells\\)")
  expect_match(paste(text, collapse = "\n"), "coordviews_config_request")
  expect_match(paste(text, collapse = "\n"), "coordviews_config_upload")
  expect_match(paste(text, collapse = "\n"), "coordviews_config_download")
})
```

Add a `shiny::testServer()` case that sends a valid capture request and verifies the canonical JSON reactive; send an invalid request and verify a safe error result with no canonical replacement.

- [ ] **Step 2: Run the focused test and verify RED**

Expected: missing transport symbols and outputs.

- [ ] **Step 3: Implement nonce-based requests and bounded upload**

```r
coordviews_config_json <- shiny::reactiveVal(NULL)

shiny::observeEvent(input$coordviews_config_request, {
  req <- input$coordviews_config_request
  normalized <- cv_config_normalize(req$config, cells = coordviews_bundle()$cells)
  canonical <- cv_config_encode(normalized)
  coordviews_config_json(canonical)
  session$sendCustomMessage("coordviews_config_result", list(
    nonce = req$nonce,
    action = req$action,
    ok = TRUE,
    json = if (identical(req$action, "copy")) canonical else NULL
  ))
})
```

Source `config.R`, attach `dataset_fingerprint` to each successful bundle, add `downloadHandler`, read uploads only after checking `.json` extension and `size <= 5 * 1024^2`, and never send raw R condition messages.

- [ ] **Step 4: Run contract and existing coordinated-view tests**

Run:

```bash
Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-coordinated-views-config.R"); testthat::test_file("tests/testthat/test-coordinated-views.R")'
```

Expected: both files PASS without warnings.

- [ ] **Step 5: Commit transport**

```bash
git add inst/viewer/coordinated_views/server.R tests/testthat/test-coordinated-views-config.R
git commit -m "feat: add linked view config transport"
```

### Task 4: Capture and transactionally restore browser state

**Files:**
- Create: `tests/testthat/test-coordinated-views-config-browser.R`
- Modify: `inst/viewer/www/coordviews.js`

- [ ] **Step 1: Write a failing real-browser round-trip test**

```r
test_that("browser adapter restores a selected cohort and view context", {
  driver <- config_browser_driver()
  driver$run_js("window.__savedConfig = window.cerebroLinkedViewsState.capture()")
  driver$run_js("window.cerebroLinkedViewsState.apply(window.__emptyConfig)")
  driver$run_js("window.cerebroLinkedViewsState.apply(window.__savedConfig)")
  expect_equal(driver$get_js("window.cerebroLinkedViewsState.summary().selectedCells"), 2)
  expect_equal(driver$get_value(input = "coordviews_selection")$value, c("cell-a", "cell-c"))
})
```

Use the real coordinated-view bundle and browser engine; do not mock `setSelection()` or canvas events.

- [ ] **Step 2: Run the focused browser test and verify RED**

Run: `Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-coordinated-views-config-browser.R")'`

Expected: FAIL because `window.cerebroLinkedViewsState` is undefined.

- [ ] **Step 3: Implement capture, prepare, commit, and summary**

```js
window.cerebroLinkedViewsState = Object.freeze({
  ready: () => Boolean(D && D.dataset_fingerprint),
  capture: captureConfigState,
  apply: applyConfigState,
  summary: configStateSummary
});
```

Capture only allowlisted JSON data. During apply, resolve all fields into temporary Sets, Maps, panel selections, lens viewports, rotations, spatial image alignments, and Trekker values. Commit control and panel state only after all references pass capability checks, then invoke existing `setSelection()` once with resolved cell indexes.

- [ ] **Step 4: Extend the test across every version-one state family**

Set projections, Spatial sections, active Spatial section, colour controls, filters, hidden levels, display controls, focus, lens viewport, 3-D rotation, spatial background/alignment, and Trekker controls before capture. After restore, assert the adapter summary and visible controls match.

- [ ] **Step 5: Run focused browser test and verify GREEN**

Run the command from Step 2.

Expected: PASS; applying a capability-mismatched document throws and leaves the pre-apply summary unchanged.

- [ ] **Step 6: Commit state adapter**

```bash
git add inst/viewer/www/coordviews.js tests/testthat/test-coordinated-views-config-browser.R
git commit -m "feat: restore linked view state"
```

### Task 5: Accessible Save/share dialog and client transport

**Files:**
- Modify: `tests/testthat/test-coordinated-views-config.R`
- Modify: `tests/testthat/test-coordinated-views-config-browser.R`
- Modify: `inst/viewer/coordinated_views/UI.R`
- Create: `inst/viewer/www/coordviews-config.js`
- Modify: `inst/viewer/www/coordviews.css`
- Modify: `R/shiny_UI.R`

- [ ] **Step 1: Write failing UI and browser interaction tests**

```r
test_that("Save/share markup is accessible and runtime assets are bundled", {
  ui_text <- paste(readLines(viewer_file("coordinated_views", "UI.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_text, "coordviews-config-dialog")
  expect_match(ui_text, "aria-live")
  expect_match(ui_text, "coordviews_config_upload")
  expect_match(ui_text, "coordviews_config_download")
  expect_true(file.exists(viewer_file("www", "coordviews-config.js")))
})
```

In the browser test, tab to Save/share, open it, assert focus enters the dialog, Escape returns focus to the trigger, and assert a stale nonce result does not replace the current status.

- [ ] **Step 2: Run focused tests and verify RED**

Expected: missing dialog and controller failures.

- [ ] **Step 3: Add native dialog markup and styling**

```html
<dialog id="coordviews-config-dialog" aria-labelledby="coordviews-config-title">
  <h2 id="coordviews-config-title">Save or share this view</h2>
  <p>Your configuration includes cell barcodes and view settings, but no expression data or local paths.</p>
  <div class="cv-config-actions">...</div>
  <p id="coordviews-config-status" role="status" aria-live="polite"></p>
</dialog>
```

Use Shiny tag builders in `UI.R`, keep the real `fileInput()` visually integrated and keyboard reachable, and place a hidden `downloadLink()` outside the dialog.

- [ ] **Step 4: Implement the controller**

`coordviews-config.js` must open/close the dialog, capture adapter state, generate monotonic nonces, send `coordviews_config_request`, activate the real download link only after server acknowledgement, copy canonical JSON with Clipboard API plus a safe fallback, relay uploads through Shiny, reject stale results, and render only server-provided safe status text.

- [ ] **Step 5: Include the controller in every Viewer build path**

Add `tags$script(src = "coordviews-config.js")` next to the existing `coordviews.js` inclusion in `R/shiny_UI.R`; verify generated Apps copy the file through the existing `inst/viewer/www` bundling path.

- [ ] **Step 6: Run focused R and browser tests and verify GREEN**

Run:

```bash
node --check inst/viewer/www/coordviews.js
node --check inst/viewer/www/coordviews-config.js
Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-coordinated-views-config.R"); testthat::test_file("tests/testthat/test-coordinated-views-config-browser.R")'
```

Expected: syntax checks and both test files PASS.

- [ ] **Step 7: Commit the user experience**

```bash
git add R/shiny_UI.R inst/viewer/coordinated_views/UI.R inst/viewer/www/coordviews-config.js inst/viewer/www/coordviews.css tests/testthat/test-coordinated-views-config.R tests/testthat/test-coordinated-views-config-browser.R
git commit -m "feat: add linked view sharing dialog"
```

### Task 6: Generated-App coverage and final verification

**Files:**
- Modify: `tests/testthat/test-smoke-production.R`
- Modify: `docs/superpowers/plans/2026-08-20-shareable-linked-view-config.md`

- [ ] **Step 1: Write a failing generated-App asset assertion**

```r
expect_true(file.exists(file.path(app_dir, "www", "coordviews-config.js")))
expect_match(
  paste(readLines(file.path(app_dir, "coordinated_views", "server.R")), collapse = "\n"),
  "config.R"
)
```

- [ ] **Step 2: Run the focused smoke test and verify RED if copying is incomplete**

Run: `Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-smoke-production.R")'`

Expected: either the new assertion fails and identifies a missing bundle path, or passes because the existing recursive copy already handles the new files; in the latter case, retain the assertion as regression coverage without changing production copy logic.

- [ ] **Step 3: Make only the required bundling correction**

If the focused smoke test proves a missing path, update its existing explicit runtime file list to include `coordinated_views/config.R` and `www/coordviews-config.js`. Do not add a second copy mechanism.

- [ ] **Step 4: Run the complete verification ladder**

```bash
node --check inst/viewer/www/coordviews.js
node --check inst/viewer/www/coordviews-config.js
Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-coordinated-views-config.R"); testthat::test_file("tests/testthat/test-coordinated-views-config-browser.R"); testthat::test_file("tests/testthat/test-coordinated-views-browser.R"); testthat::test_file("tests/testthat/test-coordinated-views.R"); testthat::test_file("tests/testthat/test-smoke-production.R")'
scripts/precheck.sh fast
scripts/precheck.sh
```

Expected: all checks exit 0, with no unexpected warning or skip introduced by this feature.

- [ ] **Step 5: Inspect the real UI at desktop and narrow widths**

Launch the Viewer, open Linked views, create a box/lasso cohort, exercise Download/Copy/Open, capture screenshots at desktop and narrow widths, and verify readable spacing, keyboard focus, success/error status, and no overlap with existing topbar controls.

- [ ] **Step 6: Run specification and quality reviews**

Compare the final diff to `docs/superpowers/specs/2026-08-20-shareable-linked-view-config-design.md`, then review for correctness, self-contained generated-App behavior, privacy leakage, transactionality, accessibility, and test quality. Fix any blocking findings and rerun the smallest affected tests.

- [ ] **Step 7: Mark the plan complete and create the final commit**

Mark every completed checkbox in this file, run `git diff --check`, and commit the remaining test/documentation changes:

```bash
git add tests/testthat/test-smoke-production.R docs/superpowers/plans/2026-08-20-shareable-linked-view-config.md
git commit -m "test: cover shared linked view config"
```

