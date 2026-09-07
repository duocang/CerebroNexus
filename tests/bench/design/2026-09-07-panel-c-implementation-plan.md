# Panel C Implementation Plan

> **For the implementing agent:** Required skill: use
> `superpowers:executing-plans` in this session. Track each checkbox and use
> test-driven development for every behavior change.

**Goal:** Add an incremental C1 three-backend scale bridge and a C2 full-source
out-of-core benchmark without rerunning or modifying the existing A/B run.

**Architecture:** Reuse the current sweep for C1 through a dedicated protocol
profile and result root. Add one focused out-of-core library and runner for C2,
then derive a combined report from three immutable run directories. Keep source
files in the existing verified external cache.

**Technical stack:** Bash, base R, Matrix, BPCells, rhdf5, HDF5Array, testthat.

---

### Task 1: Define the Panel C schedules

**Files:**
- Modify: `tests/bench/lib/protocol.R`
- Modify: `tests/bench/src/03_plan_runs.R`
- Test: `tests/testthat/test-bench-protocol.R`

- [ ] Add failing tests asserting that C1 contains exactly mouse 400k and human
  300k with three backends, 18 builds, and two access repeats; C2 contains the
  two exact full-source cell counts with only `bpcells` and `h5`, 12 builds,
  and two access repeats.
- [ ] Run `Rscript -e 'testthat::test_file("tests/testthat/test-bench-protocol.R")'`
  and confirm the new profile/schedule lookup fails.
- [ ] Add `panel_c1` and `panel_c2` profiles plus a small
  `bench_panel_c_schedule(specs, part)` helper. The helper must derive full
  counts from source metadata, reject unknown parts, and return the existing
  schedule column schema.
- [ ] Rerun the focused test and confirm it passes.

### Task 2: Stream full-source backend artifacts

**Files:**
- Create: `tests/bench/lib/full_source.R`
- Create: `tests/bench/src/11_build_full_backend.R`
- Test: `tests/testthat/test-bench-full-source.R`

- [ ] Add failing fixture tests that open tiny 10x and AnnData HDF5 sources as
  lazy BPCells matrices, preserve genes-by-cells orientation and names, and
  reject a source passed as `dgCMatrix`.
- [ ] Run the focused test and confirm `bench_open_full_source()` is missing.
- [ ] Implement `bench_open_full_source(spec, path)` using only
  `BPCells::open_matrix_10x_hdf5()` and
  `BPCells::open_matrix_anndata_hdf5(group = "X")`; validate dimensions and
  non-empty dimnames without coercing the full matrix.
- [ ] Add failing fixture tests for `bench_write_full_backend()`: BPCells output
  must reopen genes-by-cells; H5 output must reopen through
  `HDF5Array::TENxMatrix(path, group = "expression")` and transpose back to
  the same matrix.
- [ ] Implement BPCells output with `write_matrix_dir()`. Implement H5 output
  with `write_matrix_10x_hdf5(t(source))`, then rename `/matrix` to
  `/expression` using `rhdf5::H5Lmove()` with handles closed by `on.exit()`.
- [ ] Add the one-process build CLI. It records build seconds, stored bytes,
  process RSS, peak RSS, dimensions, status, and fingerprints in one CSV row.
- [ ] Rerun the focused test and confirm both fixture backends round-trip.

### Task 3: Create the portable full-cell Cerebro shell and access plan

**Files:**
- Modify: `tests/bench/lib/full_source.R`
- Modify: `tests/bench/lib/access_metrics.R`
- Modify: `tests/bench/src/11_build_full_backend.R`
- Reuse: `tests/bench/src/20_measure_backend.R`
- Test: `tests/testthat/test-bench-full-source.R`

- [ ] Add a failing test that creates a shell with all fixture cell IDs,
  deterministic sample/cluster factors, deterministic two-dimensional
  projection, `expression = NULL`, and a relative sibling backend path.
- [ ] Implement `bench_make_full_shell()` using the public `Cerebro` methods;
  avoid Seurat and do not store source expression values in the `.crb`.
- [ ] Add a failing test for a lazy query plan built from row nonzero counts and
  only the selected row/block values.
- [ ] Implement `bench_build_lazy_query_plan()` so only 12 selected genes are
  materialised and the existing access/fingerprint contract is preserved.
- [ ] Round-trip both fixture shells through the real
  `.attachExternalExpression()` runtime and `bench_measure_backend()`.

### Task 4: Add incremental C1/C2 orchestration and publication

**Files:**
- Create: `tests/bench/run_panel_c.sh`
- Create: `tests/bench/src/31_check_panel_c2.R`
- Modify: `tests/bench/run_sweep.sh`
- Modify: `tests/bench/src/50_check_outputs.R`
- Modify: `tests/bench/src/60_publish_results.R`
- Test: `tests/testthat/test-bench-cli-contract.R`
- Test: `tests/testthat/test-bench-publication.R`

- [ ] Add failing contract tests proving `run_panel_c.sh` supports
  `BENCH_PANEL_C_PART=c1|c2`, points C1 to `result/panel-c1`, points C2 to
  `result/panel-c2`, never selects the A/B publication schedule, and reuses
  `BENCH_SOURCE_CACHE`.
- [ ] Add failing publication tests proving each part updates only its own
  `CURRENT` pointer and a failed part preserves the previous pointer.
- [ ] Parameterise the existing sweep only where needed for the C1 profile and
  result root; do not fork its exporter or access implementation.
- [ ] Implement the C2 shell runner with the same scratch marker, startup
  isolation, thread variables, source-cache validator, fresh process per build
  and access, cleanup, validation, and pointer-last publication conventions.
- [ ] Implement `run_panel_c.sh` as a thin sequential wrapper. With no part it
  runs C1 then C2; with a part it runs only that part.
- [ ] Rerun focused CLI/publication tests and confirm they pass.

### Task 5: Derive the combined A/B/C report

**Files:**
- Create: `tests/bench/src/42_write_panel_c_report.R`
- Create: `tests/bench/src/43_draw_panel_c_figure.R`
- Test: `tests/testthat/test-bench-reporting.R`

- [ ] Add failing tests that reject the wrong A/B run ID, Git SHA, source hash,
  required column set, query-plan fingerprint, or query-panel size.
- [ ] Add a failing fixture test expecting a manifest containing the exact A/B,
  C1, and C2 run IDs and Git SHAs plus a two-facet PNG.
- [ ] Implement a base-R loader for the three `CURRENT` pointers and exact A/B
  guard. Write `result/panel-c/manifest.csv` and `summary.md` atomically.
- [ ] Draw one C1 scale/access facet and one C2 full-source feasibility facet
  with existing ggplot2 helpers; label embedded full-source points as
  `not representable`.
- [ ] Run focused reporting tests and confirm they pass.

### Task 6: Documentation, verification, commit, and push

**Files:**
- Modify: `tests/bench/README.md`
- Modify: `tests/bench/METHODOLOGY.md`
- Modify: `tests/bench/RESULTS.md`

- [ ] Document cache reuse, incremental result locations, part selection,
  expected process counts, and the exact background command.
- [ ] Run all benchmark-focused tests:
  `Rscript -e 'testthat::test_dir("tests/testthat", filter = "bench")'`.
- [ ] Run `bash -n tests/bench/run_sweep.sh tests/bench/run_panel_c.sh` and
  `git diff --check`.
- [ ] Inspect the final diff for accidental changes to
  `tests/bench/result/runs/20260907T172844Z-9c6ab101e4a5-publication`.
- [ ] Commit with an English Conventional Commit message and push
  `paper/real-data-benchmark` to `origin`.
