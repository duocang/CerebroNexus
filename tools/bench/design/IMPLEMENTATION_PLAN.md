# Benchmark safety and organization implementation plan

> **For AI workers:** Execute each task with test-driven development. Keep structural changes separate from resource-policy behavior changes.

**Goal:** Make the benchmark understandable, resource-aware, reproducible, and safe to run on a 32 GiB Mac while retaining every design and historical record.

**Architecture:** Plain-language numbered scripts form a staged pipeline. Resource planning is a pure R library used by a small command-line checker. Measurement data, reports, and figures are all validated before immutable publication.

**Technical stack:** base R, Matrix, rhdf5, Seurat, Bash, testthat.

---

### Task 1: Organize stages and documentation

**Files:** `tests/bench/src/*.R`, `tests/bench/run_sweep.sh`, `tests/bench/README.md`, `tests/bench/METHODOLOGY.md`, `tests/bench/RESULTS.md`, `tests/testthat/test-bench-cli-contract.R`.

- [ ] Add a failing contract test for the plain-language script names and required orchestration order.
- [ ] Rename scripts and update every code/document reference.
- [ ] Split operation, methodology, and result-reading documentation.
- [ ] Move pilot CSVs to `result/archive/pilot-2026-07-30/` and label them as superseded evidence.
- [ ] Run focused contract and reporting tests, then commit the structural change without behavior changes.

### Task 2: Reject unsafe plans before data transfer

**Files:** `tests/bench/lib/resource_planning.R`, `tests/bench/src/04_check_resources.R`, `tests/bench/lib/protocol.R`, `tests/bench/config/sources.R`, `tests/bench/run_sweep.sh`, `tests/testthat/test-bench-resources.R`.

- [ ] Write failing pure-function tests for memory, sparse-index, and disk rejection and for actionable safe/unsafe rows.
- [ ] Write a failing CLI test proving an unsafe plan exits non-zero before a source download is attempted.
- [ ] Implement conservative estimates with injectable host limits.
- [ ] Add `stress`; remove large boundary tiers from normal profiles and move human 150k out of the default repeated comparison set.
- [ ] Write `resource_check.csv` and require an explicit override for unsafe schedules.
- [ ] Run the resource and protocol tests, then commit the behavior change.

### Task 3: Validate complete evidence before publication

**Files:** `tests/bench/src/40_write_report.R`, `tests/bench/src/41_draw_figures.R`, `tests/bench/src/50_check_outputs.R`, `tests/bench/src/60_publish_results.R`, `tests/bench/run_sweep.sh`, `tests/testthat/test-bench-publication.R`.

- [ ] Add a failing publication test for a missing report or required figure.
- [ ] Generate report and publication figures inside the staged tree.
- [ ] Validate staged artifacts after measurement validation.
- [ ] Publish only after both validation phases pass; update `CURRENT` last.
- [ ] Fault-inject report, artifact-check, and pre-pointer failures and prove the previous run remains current.

### Task 4: Verify and record

**Files:** benchmark documentation, `NEWS.md`, branch review records.

- [ ] Run all benchmark tests and deliberate resource/fingerprint mutations.
- [ ] Run `R CMD INSTALL .`, the full test suite, `scripts/precheck.sh`, documentation checks, formatting checks, and `git diff --check`.
- [ ] Update the branch dossier and adversarial-review record.
- [ ] Review the final diff and commit history. Do not push or notify upstream.
