# Benchmark measurement and figure revision plan

> **For AI workers:** Execute each task with test-driven development. Use
> `superpowers:executing-plans` and keep existing published result directories
> immutable. Steps use checkboxes so progress is explicit.

**Goal:** Correct the lazy-block timing contract and produce a richer,
Nature-style publication figure set without overstating host capacity.

**Architecture:** Measurement helpers return separate native-view preparation
and dense-materialization timings. Reporting helpers normalize old and new
result schemas, while a dedicated figure library builds article and
supplementary panels from raw independent-process rows. Publication validation
requires every vector and print figure plus finalized provenance.

**Technical stack:** base R, Matrix, ggplot2, patchwork, Bash, testthat.

---

### Task 1: Measure the materialized block fairly

**Files:** `tools/bench/lib/access_metrics.R`,
`tools/bench/src/20_measure_backend.R`,
`tools/bench/tests/testthat/test-bench-access-metrics.R`.

- [x] Add a lazy block fixture whose `as.matrix()` method fails unless invoked
  inside the injected timer; assert separate preparation, materialization, and
  combined timings.
- [x] Run the focused test and confirm it fails because the existing
  fingerprint materializes outside the timer.
- [x] Time `getExpressionBlock()`, then time `as.matrix()` separately; compute
  `block_ready_secs` and fingerprint the materialized value.
- [x] Extend the access CSV row with `block_prepare_secs`,
  `block_materialize_secs`, and `block_ready_secs`; remove the ambiguous
  `block_secs` claim from new rows.
- [x] Run the focused access tests and confirm they pass.

### Task 2: Make provenance and source identity fail closed

**Files:** `tools/bench/src/02_record_environment.R`,
`tools/bench/src/30_check_measurements.R`, `tools/bench/config/sources.R`,
`tools/bench/run_sweep.sh`,
`tools/bench/tests/testthat/test-bench-cli-contract.R`.

- [x] Add failing tests requiring an unambiguous `repository_version` key and,
  for publication, a matching non-empty `package_CerebroNexus` value.
- [x] Add failing source-registry contracts for exact byte counts and SHA-256.
- [x] Remove the accidental `.Version` suffix by un-naming DESCRIPTION values;
  re-record the manifest after the isolated package installation.
- [x] Pin all three observed source hashes, refresh MSSM bytes, and reject a
  downloaded file before measurement when bytes or SHA-256 differ.
- [x] Run CLI/source contract tests and confirm they pass.

### Task 3: Replace capacity claims with observed scaling

**Files:** `tools/bench/lib/reporting.R`,
`tools/bench/src/40_write_report.R`,
`tools/bench/tests/testthat/test-bench-reporting.R`.

- [x] Add a failing report test proving an infinite R vector limit cannot be
  replaced with `1.1 * max(observed)` or described as capacity.
- [x] Add a schema normalizer that marks legacy `block_secs` as preparation-only
  and excludes it from materialized-read claims.
- [x] Report observed bytes/non-zero descriptively and state that no maximum
  capacity is inferred without a stress failure boundary.
- [x] Run reporting tests and confirm they pass.

### Task 4: Build the publication figure family

**Files:** `tools/bench/lib/figures.R`,
`tools/bench/src/41_draw_figures.R`,
`tools/bench/src/50_check_outputs.R`,
`tools/bench/tests/testthat/test-bench-reporting.R`,
`tools/bench/tests/testthat/test-bench-publication.R`.

- [x] Add failing output tests requiring six named figures in SVG, PDF, and PNG.
- [x] Implement a shared Nature-style theme, stable colour/shape mappings,
  raw-point plus median/range layers, and safe legacy-block labelling.
- [x] Generate the six figures: article overview, observed scaling, repeat/order,
  query latency, memory/storage Pareto, and correctness audit.
- [x] Render a preview from the immutable current run into a temporary
  directory and inspect representative PNG/SVG outputs.
- [x] Run every benchmark contract test, shell syntax checks, and
  `git diff --check`. Do not commit or push in this side conversation.
