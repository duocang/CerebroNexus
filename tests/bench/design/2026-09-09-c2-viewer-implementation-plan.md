# C2 Million-Cell Viewer Gate Implementation Plan

> **For the implementing agent:** Use `superpowers:test-driven-development`
> task by task. Keep every step checked with a fresh command result.

**Goal:** Add four mandatory end-to-end Viewer validations to the C2 phase of
the publication-full benchmark.

**Architecture:** A small benchmark helper drives the existing standalone App
through `shinytest2`. C2 invokes one browser process for build repeat 1 of each
source/backend pair, the ordinary phase validator rejects incomplete or failed
Viewer rows, and the combined report publishes the four raw diagnostic rows.

**Technical stack:** R, Bash, testthat, shinytest2/Chromote, existing Cerebro
Canvas Viewer and benchmark CSV/reporting utilities.

---

## File Structure

- Create `tests/bench/lib/viewer_validation.R`: reusable browser actions and
  strict Viewer-row validation.
- Create `tests/bench/src/21_measure_viewer.R`: CLI process that builds and
  drives one standalone App and appends one result row.
- Modify `tests/bench/run_sweep.sh`: initialize `21_viewer.csv` and invoke the
  driver for C2 build repeat 1 before scratch removal.
- Modify `tests/bench/src/30_check_measurements.R`: require the exact C2 Viewer
  grid and successful rows.
- Modify `tests/bench/src/42_write_panel_c_report.R`: publish
  `viewer_metrics.csv` and include functional results in `summary.md`.
- Modify `tests/bench/src/50_check_outputs.R`: require C2 Viewer output.
- Modify `tests/testthat/test-bench-cli-contract.R`: cover CLI and shell wiring.
- Modify `tests/testthat/test-bench-reporting.R`: cover Viewer row validation
  and combined publication.
- Create `tests/testthat/test-bench-viewer-validation.R`: exercise the browser
  helper against a small generated App when Chrome is available.
- Modify benchmark README/methodology/results, vignette, and `NEWS.md`: state
  the end-to-end gate and its interpretation boundary.

## Task 1: Define and validate the four-row Viewer contract

- [ ] Add failing tests to `test-bench-reporting.R` that source
  `viewer_validation.R`, construct the expected C2 schedule, and require:

```r
expected <- bench_viewer_schedule(bench_panel_c_schedule(BENCH_SOURCES, "c2"))
expect_equal(nrow(expected), 4L)
expect_true(all(expected$export_repeat == 1L))
expect_silent(bench_validate_viewer_results(expected, successful_rows))
expect_error(
  bench_validate_viewer_results(expected, successful_rows[-1, ]),
  "does not cover"
)
```

- [ ] Run the focused test and confirm failure because the helper does not
  exist:

```bash
Rscript -e 'devtools::test(filter = "bench-reporting")'
```

- [ ] Create `lib/viewer_validation.R` with the minimum schedule selector and
  validator. The key is `source`, `n_cells`, `backend`, and `export_repeat`;
  rows must be unique, finite for all timing columns, share the phase run ID,
  and have `status == "OK"` and `correctness == "OK"`.

- [ ] Re-run the focused test and confirm zero failures.

- [ ] Commit:

```bash
git add tests/bench/lib/viewer_validation.R \
  tests/testthat/test-bench-reporting.R
git commit -m "test(bench): define C2 viewer gate"
```

## Task 2: Drive the production Viewer path

- [ ] Add a failing `test-bench-viewer-validation.R` test which builds a small
  standalone App from an existing test CRB, calls
  `bench_run_viewer_validation()`, and expects these completed stages:

```r
expect_identical(result$correctness, "OK")
expect_true(all(c(
  "bundle_secs", "launch_secs", "hover_secs", "selection_secs",
  "zoom_secs", "gene_secs"
) %in% names(result)))
expect_true(all(unlist(result[grepl("_secs$", names(result))]) >= 0))
```

  Skip only when `shinytest2` or a Chrome executable is unavailable.

- [ ] Run the focused test and confirm failure because
  `bench_run_viewer_validation()` does not exist.

- [ ] Implement `bench_run_viewer_validation()` in
  `lib/viewer_validation.R`. Reuse existing production selectors and Chromote
  mouse events:

```r
canvas <- "#overview_projection_cell_view_host canvas:not(.cv-mini)"
app$wait_for_js(sprintf("document.querySelector('%s') !== null", canvas))
# scan bounded points for a visible tooltip
# click the box tool and drag across the Canvas
# click #overview_projection_zoom_to_selection
# open Gene expression and request the frozen first gene
```

  Each stage uses a bounded wait and returns elapsed seconds. The helper must
  stop the AppDriver with `on.exit()` and reject browser/App error logs.

- [ ] Re-run the browser-focused test and confirm zero failures.

- [ ] Commit:

```bash
git add tests/bench/lib/viewer_validation.R \
  tests/testthat/test-bench-viewer-validation.R
git commit -m "feat(bench): drive C2 viewer actions"
```

## Task 3: Add the one-run CLI and C2 shell wiring

- [ ] Add failing CLI-contract assertions which require
  `21_measure_viewer.R`, the `21_viewer.csv` header, and a C2-only invocation
  guarded by `export_repeat = 1`.

- [ ] Run `devtools::test(filter = "bench-cli-contract")` and confirm those
  assertions fail.

- [ ] Create `src/21_measure_viewer.R`. It accepts:

```text
<source> <n_cells> <backend> <export_repeat> <crb> <result> <query_plan>
```

  It creates one row, calls `bench_run_viewer_validation()`, appends success or
  `FAILED(<stage>): <message>`, and exits without publishing partial evidence.

- [ ] Modify `run_sweep.sh` to create `21_viewer.csv`, call the driver only for
  `panel_c2`/repeat 1 after backend access, and preserve its process exit code in
  `crashes.csv` just like build/access failures.

- [ ] Run the CLI-contract test and `bash -n tests/bench/run_sweep.sh`; confirm
  both pass.

- [ ] Commit:

```bash
git add tests/bench/src/21_measure_viewer.R tests/bench/run_sweep.sh \
  tests/testthat/test-bench-cli-contract.R
git commit -m "feat(bench): run C2 viewer gate"
```

## Task 4: Make Viewer success a publication requirement

- [ ] Extend synthetic validator/report fixtures with four successful Viewer
  rows, then add failing assertions for a missing row, failed interaction, and
  missing study-level `viewer_metrics.csv`.

- [ ] Run the benchmark reporting/CLI tests and confirm failure before changing
  production validators.

- [ ] Source `viewer_validation.R` from `30_check_measurements.R`; for
  `panel_c2`, read `21_viewer.csv` and call the strict validator. Other profiles
  remain unchanged.

- [ ] Update `50_check_outputs.R` so C2 output requires `21_viewer.csv`.

- [ ] Update `42_write_panel_c_report.R` to validate C2 rows, copy them to
  `viewer_metrics.csv`, and add a concise Viewer table and single-run caveat to
  `summary.md`.

- [ ] Run the focused benchmark tests and confirm zero failures.

- [ ] Commit:

```bash
git add tests/bench/src/30_check_measurements.R \
  tests/bench/src/42_write_panel_c_report.R \
  tests/bench/src/50_check_outputs.R \
  tests/testthat/test-bench-cli-contract.R \
  tests/testthat/test-bench-reporting.R
git commit -m "feat(bench): require viewer evidence"
```

## Task 5: Align publication documentation

- [ ] Add contract assertions that documentation names all four browser
  actions, states that four rows are single-run diagnostics, and no longer says
  C2 excludes Viewer UX.

- [ ] Run focused tests and confirm documentation assertions fail.

- [ ] Update `tests/bench/README.md`, `METHODOLOGY.md`, `RESULTS.md`,
  `vignettes/expression_backend_benchmark.Rmd`, and `NEWS.md`. Preserve the
  boundary that cross-machine and Vitessce claims are unsupported.

- [ ] Run focused tests and confirm zero failures.

- [ ] Commit:

```bash
git add tests/bench/README.md tests/bench/METHODOLOGY.md \
  tests/bench/RESULTS.md vignettes/expression_backend_benchmark.Rmd NEWS.md \
  tests/testthat/test-bench-cli-contract.R
git commit -m "docs(bench): add million-cell viewer gate"
```

## Task 6: Verify, review, and push

- [ ] Run formatting and syntax checks:

```bash
scripts/precheck.sh air
bash -n tests/bench/run_sweep.sh tests/bench/run_publication_full.sh
git diff --check origin/paper/real-data-benchmark...HEAD
```

- [ ] Run the complete benchmark-focused suite:

```bash
Rscript -e 'devtools::test(filter = "bench")'
```

- [ ] Run the repository fast precheck once:

```bash
scripts/precheck.sh fast
```

- [ ] Review the final diff against
  `C2_VIEWER_VALIDATION_DESIGN.md`; record any large-run limitation rather than
  claiming million-cell success without evidence.

- [ ] Confirm a clean worktree and push the exact head:

```bash
git status --short --branch
git push origin paper/real-data-benchmark
git rev-parse HEAD
git rev-parse origin/paper/real-data-benchmark
```
