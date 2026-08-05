# Scientific expression-backend benchmark design

## Goal

Produce correctness-checked, reproducible evidence for the `embedded`, `bpcells`, and `h5` expression backends without asking a workstation to run tiers that cannot fit its memory or disk budget.

## Plain-language pipeline

The numbered scripts use verbs that describe what they do:

1. `01_inspect_data.R` records how large each public source is.
2. `02_record_environment.R` records the code and machine being tested.
3. `03_plan_runs.R` lists every backend run requested by the profile.
4. `04_check_resources.R` rejects a plan that is unsafe on this machine.
5. `10_export_backend.R` and `20_measure_backend.R` collect measurements.
6. `30_check_measurements.R` rejects missing or incorrect measurements.
7. `40_write_report.R` and `41_draw_figures.R` create staged evidence.
8. `50_check_outputs.R` checks the complete staged evidence package.
9. `60_publish_results.R` publishes it immutably and updates `CURRENT` last.

## Resource policy

Resource checking happens before source downloads and exports. For every scheduled source/tier, it records estimated non-zero entries, estimated peak R heap, the configured safety budget, source-download bytes, and free disk.

The default estimate is deliberately conservative: 64 bytes per non-zero plus 1 GiB fixed overhead, compared with 70% of the smaller of physical RAM and R's vector-memory limit. It also enforces the sparse-index limit and a disk margin. The estimate is a scheduling guard, not a scientific memory model.

Unsafe plans fail with an actionable table. They run only under the `stress` profile when the operator sets the explicit unsafe override. Quick, standard, and publication runs reject that override. Normal publication runs never silently drop tiers, because silently changing the experiment would make runs incomparable.

## Profiles

| Profile | Repeats | Large boundary tiers | Intended use |
|---|---:|:---:|---|
| `quick` | 1 export, 1 access | no | harness smoke check |
| `standard` | 3 exports, 1 access | no | local review |
| `publication` | 3 exports, 2 accesses | no | article evidence |
| `stress` | 1 export, 1 access | yes | explicit memory-boundary experiment |

Large tiers are isolated in `stress`; publication evidence measures backends on tiers expected to complete. On a 32 GiB host, the default repeated tiers are mouse 50k/150k and human 50k. Human 150k, mouse 400k/800k, and human 300k are stress tiers and are expected to be refused unless the host is large enough.

## Scientific controls

- Backend order rotates across independent export processes.
- A deterministic 12-gene panel spans expression density.
- The first query is the first backend getter call in a fresh R process.
- Source and backend row/block fingerprints must match.
- Independent processes, not repeated queries within one process, are the statistical unit.
- Reports show medians, ranges, and sample counts and keep raw rows.
- Source SHA-256, Git state, versions, CPU, memory, and operating system travel with every published run.
- The operating-system cache is uncontrolled, so no metric is called cold disk.

The 12-gene block has two deliberately separate timings. `block_prepare_secs`
measures construction of the backend-native block view. `block_materialize_secs`
measures conversion of that view to an ordinary dense R matrix. Scientific
backend comparisons use `block_ready_secs`, the sum of the two. This avoids
comparing lazy H5/BPCells view construction with eager embedded-matrix reading.

## Figure system

Publication runs produce one article figure, one observed-scaling figure, and
four supplementary figures. They use a restrained colour-blind-safe palette,
raw process points, median/range overlays, direct units, and lower-case panel
letters. Every figure is emitted as editable SVG, print PDF, and 600 dpi PNG.

The article figure answers six questions: export time, stored size, backend
readiness, resident memory, warmed single-gene latency, and materialized
12-gene latency. The supplementary figures expose independent repeats and
backend order, first-versus-warmed latency, memory/storage trade-offs, and the
correctness audit rather than hiding these controls in CSV files.

The scaling figure is descriptive. It plots only observed peak-heap points and
an observed-range trend. It never invents a capacity boundary from the largest
successful run. Physical memory, R's vector limit, and the `dgCMatrix` 32-bit
index limit remain separate operational constraints in the report.

## Publication contract

Measurement validation precedes report generation. Reports and figures are then checked as part of the staged package. Only that complete package is copied into `result/runs/<run-id>/`; `result/CURRENT` changes last. A failed or interrupted run cannot replace current evidence.

The environment manifest is finalized after the branch is installed into the
isolated benchmark library. A publication result is rejected unless the
installed CerebroNexus version is present and equals the repository version.
Downloaded sources must match both the registry byte count and pinned SHA-256.

## Documentation retention

This design and its implementation plans are tracked under `tools/bench/design/`. The entire harness, including its tests and `rhdf5` dependency, is repository-only and excluded from the R source package. Raw quick-run experiments remain local and ignored. Historical pilot results and figures are retained under `result/archive/` with an explicit superseded label.

## Out of scope

- Cross-host confidence intervals.
- Privileged operating-system cache eviction.
- Streaming export that avoids the current `dgCMatrix` construction limit.
- Treating a profile named `publication` as academic peer review.
