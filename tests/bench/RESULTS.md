# Reading full-source benchmark results

This guide defines how to interpret one completed publication run. It contains no fixed performance conclusion.

## Select one immutable run

Read `result/publication-full/CURRENT`, then open the matching directory under `result/publication-full/runs/<run-id>/`. Do not combine files from different runs.

Start with:

1. `summary.md` for the descriptive tables;
2. `run_manifest.csv` and `source_manifest.csv` for provenance;
3. `05_schedule.csv` and `resource_check.csv` for the exact complete-source grid;
4. `10_export.csv` for build observations;
5. `20_access.csv` for hydrated startup and expression access;
6. `21_viewer.csv` for the 12 standalone Viewer observations;
7. `query_plan_manifest.csv` and `query_panel.csv` for the frozen workload and correctness references;
8. `figures/expression_backend_benchmark_overview.png` for the publication overview.

## Interpret metrics

A value such as `2.4 [2.2-2.9], n=6` is a median of 2.4, an observed process range of 2.2 to 2.9, and six independent processes. It is not a confidence interval.

| field | meaning |
|---|---|
| `read_secs` | lazy complete-source opening |
| `export_secs` | streaming backend construction |
| `shell_secs` | synthetic complete-cell Cerebro shell construction |
| `serialize_secs` | default qs2 CRB serialization |
| `total_mb` | CRB plus relative backend sibling |
| build `r_peak_mb`, `peak_rss_mb` | R heap and whole build-process high-water RSS |
| `startup_secs` | fresh-process public `readCerebro()` hydration |
| access `rss_mb`, `peak_rss_mb` | hydrated-process resident and peak memory |
| `first_query_secs` | first full-cell getter call; not controlled cold disk |
| `hot_p50_secs`, `hot_p95_secs` | warmed full-cell single-gene distribution |
| `block_secs` | deterministic 12-gene-by-all-cells read |
| `subset_row_secs`, `subset_block_secs` | reverse-ordered non-contiguous access over up to 100,000 cells |
| Viewer `bundle_secs`, `launch_secs` | standalone App construction and first complete overview |
| `hover_secs`, `selection_secs`, `zoom_secs`, `gene_secs`, `linked_secs` | required browser interaction steps |
| `js_heap_mb` | browser JavaScript heap after the workload |

## Correctness requirements

Every successful access row must have `status = OK`, `correctness = OK`, and matching full-row, full-block, subset-row, subset-block, and query-plan fingerprints. Every Viewer row must render exactly `n_cells` points with `renderer_backend = webgpu`, no context loss, no renderer error, and successful interaction checks.

`embedded` has no complete-source observation because neither source is representable as `dgCMatrix`. This is a feasibility boundary, not zero performance or a failed attempt.

## Unsupported claims

- true cold-disk latency;
- universal or cross-machine performance;
- statistical significance from three builds;
- biological-method quality;
- concurrent-user or cross-browser performance;
- comparison with Vitessce;
- full-source `embedded` support.

The study supports the narrower claim that two complete public million-scale matrices were processed and viewed through CerebroNexus's production out-of-core paths on one fully recorded host.
