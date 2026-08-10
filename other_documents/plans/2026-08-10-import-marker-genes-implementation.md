# Import precomputed Marker genes implementation plan

> **For the implementing agent:** Required subskills: use `superpowers-zh:subagent-driven-development` (recommended) or `superpowers-zh:executing-plans` to implement this plan task by task. Track the steps below with checkboxes.

**Goal:** Import precomputed Marker genes from CSV, TSV, and XLSX into a generated CRB as independently selectable Viewer methods.

**Architecture:** A pure marker-import domain inventories files, confirms cluster mappings, and emits safe normalized tables with the selected group as first column. Editable records live in `settings$marker_imports`; freeze copies only ready records into BuildPlan; build merges those records into `object@misc$marker_genes` before export. The Viewer remains unchanged.

**Tech stack:** R, Shiny, testthat, `readxl` (runtime XLSX reader), `writexl`
(test-only XLSX fixture writer), existing Builder state/plan/build contracts.

---

## File structure

- Create `inst/builder/marker_import.R`: inventory, mapping, coverage, frozen-record validation, and Seurat merge.
- Create `inst/builder/ui/marker_import.R`: accessible Enhance controls and mapping/coverage UI.
- Create `tests/testthat/test-builder-marker-import.R`: pure-domain and temporary-file tests.
- Modify `DESCRIPTION`, `inst/builder/app.R`, `inst/builder/ui/enhance_stage.R`, `inst/builder/server/enhancements.R`, `inst/builder/server/review.R`, `inst/builder/www/builder.js`, `inst/builder/www/builder.components.css`, `inst/builder/plan/freeze.R`, `inst/builder/build.R`.
- Modify `tests/testthat/test-builder-stage-enhance.R`, `tests/testthat/test-builder-ui-contract.R`, `tests/testthat/test-builder-stage-server.R`, `tests/testthat/test-builder-plan-content.R`, `tests/testthat/test-builder-build.R`, `tests/testthat/test-generated-app-pages-analysis.R`, `vignettes/build_a_data_set_by_pointing.Rmd`, and `README.md`.

### Task 1: Inventory sources and add the XLSX dependency

**Files:**
- Create: `inst/builder/marker_import.R`
- Create: `tests/testthat/test-builder-marker-import.R`
- Modify: `DESCRIPTION`, `inst/builder/app.R`

- [ ] **Step 1: Write a failing inventory test**

```r
test_that("marker import inventories delimited files and XLSX sheets", {
  csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(cluster = "B", gene = "MS4A1"), csv, row.names = FALSE)
  xlsx <- builder_marker_import_xlsx(list(
    all_cells = data.frame(cluster = c("B", "T"), gene = c("MS4A1", "CD3D")),
    NK = data.frame(gene = "NKG7")
  ))
  got <- builder_marker_import_inventory(c(csv, xlsx))
  expect_identical(vapply(got, `[[`, character(1), "source_name"),
                   c(basename(csv), "all_cells", "NK"))
  expect_true(all(vapply(got, `[[`, logical(1), "valid")))
})
```

- [ ] **Step 2: Confirm red**

Run: `Rscript -e 'devtools::test(filter = "builder-marker-import")'`

Expected: FAIL because the inventory function and XLSX fixture helper are absent.

- [ ] **Step 3: Implement the minimal inventory**

Add `readxl` to `Imports` and `writexl` to `Suggests`; define
`builder_marker_import_xlsx()` in the test file with `writexl::write_xlsx()`.
In `inst/builder/marker_import.R` implement:

```r
builder_marker_import_inventory <- function(paths, filenames = basename(paths)) {
  unlist(Map(builder_marker_import_file_inventory, paths, filenames),
         recursive = FALSE, use.names = FALSE)
}

builder_marker_import_file_inventory <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("csv", "tsv")) return(list(
    builder_marker_import_source(path, filename, NULL,
      builder_marker_import_read_delimited(path, ext))))
  if (identical(ext, "xlsx")) return(lapply(readxl::excel_sheets(path), function(sheet)
    builder_marker_import_source(path, filename, sheet,
      as.data.frame(readxl::read_excel(path, sheet)))))
  list(builder_marker_import_error(filename, NULL, "unsupported_format"))
}
```

`builder_marker_import_source()` retains only plain `source_name`, `sheet`, `table`, `columns`, `rows`, `valid`, and `error`; it uses existing table-safety limits. Rendered summaries use basename/sheet only. Source this file in `app.R` before `extras.R`.

- [ ] **Step 4: Confirm green**

Run: `Rscript -e 'devtools::test(filter = "builder-marker-import")'`

Expected: PASS for CSV, TSV, each XLSX sheet, empty input, unreadable input, and unsupported input.

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION inst/builder/app.R inst/builder/marker_import.R tests/testthat/test-builder-marker-import.R
git commit -m "feat(builder): inventory marker gene imports"
```

### Task 2: Normalize mappings and calculate coverage

**Files:**
- Modify: `inst/builder/marker_import.R`
- Modify: `tests/testthat/test-builder-marker-import.R`

- [ ] **Step 1: Write failing mapping tests**

```r
test_that("single and multi cluster imports normalize to the Viewer contract", {
  levels <- c("B", "T", "NK")
  single <- builder_marker_import_map_single(
    builder_marker_import_source("", "NK.csv", NULL, data.frame(gene = "NKG7")),
    group = "cell_type", level = "NK", known_levels = levels
  )
  multiple <- builder_marker_import_map_multiple(
    builder_marker_import_source("", "markers.csv", NULL,
      data.frame(label = c("B", "T"), gene = c("MS4A1", "CD3D"))),
    group = "cell_type", column = "label", known_levels = levels
  )
  expect_identical(names(single$table)[[1]], "cell_type")
  expect_identical(single$table$cell_type, "NK")
  expect_identical(names(multiple$table)[[1]], "cell_type")
  expect_identical(builder_marker_import_coverage(list(single, multiple), levels)$missing,
                   character())
})
```

- [ ] **Step 2: Confirm red**

Run: `Rscript -e 'devtools::test(filter = "builder-marker-import")'`

Expected: FAIL because mapping and coverage helpers are absent.

- [ ] **Step 3: Implement exact inference and normalization**

```r
builder_marker_import_infer_level <- function(source_name, sheet, known_levels) {
  candidates <- unique(c(tools::file_path_sans_ext(basename(source_name)), sheet %||% ""))
  hits <- known_levels[tolower(known_levels) %in% tolower(candidates)]
  if (length(hits) == 1L) hits else NULL
}

builder_marker_import_normalize <- function(table, group, values) {
  table[[group]] <- as.character(values)
  table <- table[c(group, setdiff(names(table), group))]
  rownames(table) <- NULL
  table
}
```

Reject blank/duplicate method names, excluded groups, unknown/blank labels, empty/unsafe tables, and overlapping labels among sources. Do not fuzzy-match. `builder_marker_import_coverage()` returns plain `known`, `present`, and `missing`; missing is warning-only.

- [ ] **Step 4: Confirm green**

Run: `Rscript -e 'devtools::test(filter = "builder-marker-import")'`

Expected: PASS for inferred/overridden single labels, designated multi-label columns, first-column normalization, overlap rejection, and partial coverage.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/marker_import.R tests/testthat/test-builder-marker-import.R
git commit -m "feat(builder): normalize imported marker tables"
```

### Task 3: Freeze and merge imported methods

**Files:**
- Modify: `inst/builder/plan/freeze.R`, `inst/builder/build.R`, `inst/builder/marker_import.R`
- Modify: `tests/testthat/test-builder-plan-content.R`, `tests/testthat/test-builder-build.R`

- [ ] **Step 1: Write failing freeze/build tests**

```r
test_that("freeze preserves ready imports and rejects method collisions", {
  entry <- builder_task6_entry()
  entry$settings$marker_imports <- list(builder_marker_import_ready_fixture())
  plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
  expect_identical(plan$items[[1]]$marker_imports[[1]]$method, "Scanpy Wilcoxon")
  entry$settings$marker_imports[[1]]$method <- "cerebro_seurat"
  expect_identical(builder_freeze_plan(list(entry), tempdir(), FALSE)$error_code,
                   "marker_import_method_conflict")
})

test_that("merge preserves source and imported marker methods", {
  object <- builder_fixture_object()
  object@misc$marker_genes <- list(cerebro_seurat = list(cell_type = data.frame()))
  got <- builder_attach_marker_imports(object, list(builder_marker_import_ready_fixture()))
  expect_setequal(names(got@misc$marker_genes), c("cerebro_seurat", "Scanpy Wilcoxon"))
})
```

- [ ] **Step 2: Confirm red**

Run: `Rscript -e 'devtools::test(filter = "builder-(plan-content|build)")'`

Expected: FAIL because frozen items lack `marker_imports` and merge is absent.

- [ ] **Step 3: Implement plan/build boundaries**

In `freeze.R`, validate imports against included groups, `entry$levels`, retained source methods, and `cerebro_seurat`; deep-copy only ready inert records:

```r
item$marker_imports <- .builder_plan_deep_copy(
  builder_marker_imports_validate(settings$marker_imports %||% list(),
    included_groups[[index]], entry$levels %||% list(), "cerebro_seurat")
)
```

In `.builder_build_prepare()`, after group-factor normalization and before analyses:

```r
object <- builder_attach_marker_imports(object, item$marker_imports %||% list())
```

The merge must write `object@misc$marker_genes[[method]][[group]] <- table` only after checking it cannot overwrite a retained or computed method.

- [ ] **Step 4: Confirm green**

Run: `Rscript -e 'devtools::test(filter = "builder-(plan-content|build)")'`

Expected: PASS; partial imports freeze, invalid imports fail before build, and retained plus imported methods survive preparation.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/marker_import.R inst/builder/plan/freeze.R inst/builder/build.R tests/testthat/test-builder-plan-content.R tests/testthat/test-builder-build.R
git commit -m "feat(builder): freeze imported marker methods"
```

### Task 4: Add the accessible Enhance workbench

**Files:**
- Create: `inst/builder/ui/marker_import.R`
- Modify: `inst/builder/app.R`, `inst/builder/ui/enhance_stage.R`, `inst/builder/server/enhancements.R`, `inst/builder/server/review.R`, `inst/builder/www/builder.js`, `inst/builder/www/builder.components.css`
- Modify: `tests/testthat/test-builder-stage-enhance.R`, `tests/testthat/test-builder-ui-contract.R`, `tests/testthat/test-builder-stage-server.R`

- [ ] **Step 1: Write failing UI/server tests**

```r
test_that("Enhance exposes an accessible precomputed Marker genes workbench", {
  html <- builder_stage_html(builder_enhance_stage_ui("enhance", model))
  expect_match(html, "Import precomputed Marker genes", fixed = TRUE)
  expect_match(html, 'id="enhance-marker_import_files"', fixed = TRUE)
  expect_match(html, 'accept=".xlsx,.csv,.tsv"', fixed = TRUE)
  expect_match(html, "Method name", fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
})
```

```r
test_that("Enhance saves an inferred mapping and accepts an override", {
  shiny::testServer(app_env$server, {
    session$setInputs(`enhance-marker_import_method` = "Scanpy Wilcoxon")
    session$setInputs(`enhance-marker_import_group` = "cell_type")
    session$setInputs(`enhance-marker_import_files` = builder_marker_upload(csv))
    session$flushReact()
    expect_identical(sets()[[1]]$settings$marker_imports[[1]]$method, "Scanpy Wilcoxon")
  })
})
```

- [ ] **Step 2: Confirm red**

Run: `Rscript -e 'devtools::test(filter = "builder-(stage-enhance|ui-contract|stage-server)")'`

Expected: FAIL because controls, action protocol, and observers are absent.

- [ ] **Step 3: Implement UI and server translation**

Create `builder_marker_import_ui()` with method name, Groups select, multi-file input, mapping rows, and textual coverage. A row includes basename/sheet, shape, mapping control, visible status, and remove button. Use a live region for status.

Add delegated JS events carrying only `{action, import_id, source_id, value, nonce}` to `enhance-marker_import_action`; never include temporary paths. Observers call pure helpers, change only `entry$settings$marker_imports`, then call `replace_entry(entry)`. Support exactly `set_method`, `set_group`, `set_multi_column`, `set_single_level`, `remove_source`, and `remove_import`. Recompute coverage from `entry$levels[[group]]` and render the same safe summary in Review. Add scoped `.marker-import-*` styles with a mobile one-column layout.

- [ ] **Step 4: Confirm green**

Run: `Rscript -e 'devtools::test(filter = "builder-(stage-enhance|ui-contract|stage-server)")'`

Expected: PASS; mappings are dataset-isolated, Review repeats partial coverage, and rendered HTML contains no local path.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/app.R inst/builder/ui/marker_import.R inst/builder/ui/enhance_stage.R inst/builder/server/enhancements.R inst/builder/server/review.R inst/builder/www/builder.js inst/builder/www/builder.components.css tests/testthat/test-builder-stage-enhance.R tests/testthat/test-builder-ui-contract.R tests/testthat/test-builder-stage-server.R
git commit -m "feat(builder): map imported marker files"
```

### Task 5: Verify the unchanged Viewer contract and document it

**Files:**
- Modify: `tests/testthat/test-generated-app-pages-analysis.R`, `vignettes/build_a_data_set_by_pointing.Rmd`, `README.md`

- [ ] **Step 1: Write a failing generated-app test**

```r
test_that("generated Viewer selects an imported marker method", {
  generated_app_e2e_activate_tab("marker_genes")
  generated_app_e2e_wait_input("marker_genes_selected_method")
  generated_app_e2e_set_input("marker_genes_selected_method", "Scanpy Wilcoxon")
  generated_app_e2e_set_input("marker_genes_selected_table", "cell_type")
  expect_match(generated_app_e2e_table_text("marker_genes_table"), "NKG7", fixed = TRUE)
})
```

- [ ] **Step 2: Confirm red**

Run: `Rscript -e 'devtools::test(filter = "generated-app-pages-analysis")'`

Expected: FAIL because the fixture lacks a Builder-imported method.

- [ ] **Step 3: Implement fixture and docs**

Extend the fixture through production freeze/build behavior rather than Viewer source. Document manual method names, accepted files, mapping confirmation, and partial-coverage warnings in the Builder vignette; add a narrow README link. Do not document generic imports or enriched-pathway imports.

- [ ] **Step 4: Confirm green**

```bash
Rscript -e 'devtools::test(filter = "generated-app-pages-analysis")'
Rscript -e 'pkgdown::build_articles(lazy = FALSE)'
```

Expected: Viewer test PASS and article build has no RMarkdown error.

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test-generated-app-pages-analysis.R vignettes/build_a_data_set_by_pointing.Rmd README.md
git commit -m "docs(builder): explain imported marker methods"
```

### Task 6: Full regression and quality gate

**Files:** Modify only a file implicated by a failing regression.

- [ ] **Step 1: Run focused regression suites**

```bash
Rscript -e 'devtools::test(filter = "builder")'
Rscript -e 'devtools::test(filter = "generated-app-pages-analysis")'
```

Expected: PASS with no missing-`readxl` skip.

- [ ] **Step 2: Run install and static checks**

```bash
R CMD INSTALL .
git diff --check HEAD~5..HEAD
```

Expected: installation succeeds and no whitespace error is reported.

- [ ] **Step 3: Final specification review**

Verify no Viewer file changed; no UI/state summary includes a temporary path; collisions never overwrite; status is textual as well as coloured; partial coverage warns but does not block; and every acceptance criterion in `other_documents/plans/2026-08-10-import-marker-genes-design.md` has a passing test.

- [ ] **Step 4: Commit only a concrete regression correction, if one exists**

```bash
git status --short
git add inst/builder/marker_import.R tests/testthat/test-builder-marker-import.R
git commit -m "fix(builder): harden marker import release"
```

Use the displayed paths only when that concrete regression changed those files;
otherwise substitute the exact changed paths shown by
`git status --short`. Before that commit, rerun the focused test that exposed
it. If nothing changed, do not create an empty commit.
