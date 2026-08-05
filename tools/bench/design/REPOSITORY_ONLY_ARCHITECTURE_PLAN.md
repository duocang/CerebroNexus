# Repository-only benchmark architecture plan

> **For AI workers:** Execute each task with test-driven development. Keep the
> benchmark in this repository, but keep research-only code, tests, dependencies,
> and evidence outside the installed R package.

**Goal:** Retain the complete real-data benchmark in CerebroNexus while giving
the package a clean boundary and a trustworthy user-facing backend guide.

**Architecture:** Move the harness and its contract tests together under
`tools/bench/`, which `.Rbuildignore` excludes from the source package. Keep the
vignette focused on backend selection and make every numerical claim depend on
a validated immutable publication run.

**Technical stack:** base R, Matrix, rhdf5, Seurat, Bash, testthat, Nix.

**Status:** Implemented locally on 2026-08-05. Contract tests pass; no
publication benchmark run was performed as part of this change.

---

### Task 1: Lock the review findings into failing contracts

**Files:**
- Modify: `tests/testthat/test-bench-protocol.R`
- Modify: `tests/testthat/test-bench-resources.R`
- Modify: `tests/testthat/test-bench-cli-contract.R`

- [x] Add a result-validation test that rejects duplicate, missing, and
  unscheduled `access_repeat` identities.
- [x] Add a resource-policy test that permits unsafe overrides only for the
  `stress` profile.
- [x] Change the profile contract so `standard` excludes scale tiers.
- [x] Add a static export contract that requires `slot = spec$slot`.
- [x] Run the focused tests and confirm they fail for the intended reasons.

### Task 2: Implement the protocol fixes

**Files:**
- Modify: `tests/bench/lib/protocol.R`
- Modify: `tests/bench/lib/resource_planning.R`
- Modify: `tests/bench/src/04_check_resources.R`
- Modify: `tests/bench/src/10_export_backend.R`
- Modify: `tests/bench/src/30_check_measurements.R`

- [x] Validate the exact scheduled access-repeat identity set.
- [x] Restrict `BENCH_ALLOW_UNSAFE=1` to `stress` at both preflight and final
  validation.
- [x] Remove scale tiers from `standard`.
- [x] Pass the source registry slot to `exportFromSeurat()`.
- [x] Run the focused tests and confirm they pass.

### Task 3: Establish the repository-only boundary

**Files:**
- Move: `tests/bench/` to `tools/bench/`
- Move: `tests/testthat/test-bench-*.R` to `tools/bench/tests/testthat/`
- Create: `tools/bench/tests/testthat.R`
- Create: `tools/bench/run_contract_tests.R`
- Modify: `.Rbuildignore`
- Modify: `DESCRIPTION`
- Modify references in benchmark scripts and documentation.

- [x] Move the harness without deleting design documents or archived evidence.
- [x] Give the benchmark an independent test entry point.
- [x] Remove `rhdf5` from package `Suggests`; document it as a benchmark-only
  dependency supplied by the pinned environment.
- [x] Verify that the source-package file list contains neither the harness nor
  its dedicated tests.

### Task 4: Separate measured evidence from user guidance

**Files:**
- Rewrite: `vignettes/expression_backend_benchmark.Rmd`
- Modify: `tools/bench/src/41_draw_figures.R`
- Modify: `tools/bench/src/50_check_outputs.R`
- Modify: `tools/bench/README.md`
- Modify: `tools/bench/METHODOLOGY.md`
- Modify: `tools/bench/RESULTS.md`
- Modify: `README.md`
- Modify: `NEWS.md`

- [x] Remove pilot-backed rankings and recommendations from the package
  vignette while retaining the archived pilot under `tools/bench/result/archive/`.
- [x] Explain backend trade-offs using implementation properties rather than
  unqualified performance claims.
- [x] Rename the ceiling figure and language as a host-specific estimate; reserve
  observed boundaries for stress measurements.
- [x] Document the immutable-run contract: exact code SHA, dependency manifest,
  source checksums, raw rows, generated tables, generated figures, and release
  link.

### Task 5: Verify the complete change

**Files:** all files above.

- [x] Run every repository-only benchmark contract test.
- [x] Run the relevant package version, vignette, DESCRIPTION, and documentation
  tests.
- [x] Build the source archive and inspect its file list for boundary leaks.
- [x] Parse/render the rewritten vignette and run `git diff --check`.
- [x] Review every mihem finding against the final tree.
- [x] Create local logical commits only after fresh verification. Do not push or
  contact the maintainer.
