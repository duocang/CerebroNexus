# Reading benchmark results

This guide defines how to interpret a completed publication study. It contains
no fixed performance conclusion.

## Select one immutable study

Read `result/publication-full/CURRENT`, then open the matching directory under
`result/publication-full/runs/<study-id>/`. Do not combine files from different
study IDs. The final bundle contains the exact raw phase inputs under
`phases/ab`, `phases/c1`, and `phases/c2` plus all derived tables and figures.

Start with:

1. `summary.md` for the descriptive result;
2. `study_manifest.csv` and `environment_comparison.csv` for comparability;
3. `source_provenance.csv` for dataset identity and hashes;
4. `correctness.csv` for the publication gate;
5. `combined_metrics.csv` and `backend_ratios.csv` for quantitative analysis;
6. `query_plan_metrics.csv` for preparation cost kept outside timed builds.
7. `query_panel.csv` for the exact gene workload and reference fingerprints.
8. `viewer_metrics.csv` for the four C2 standalone Viewer outcomes.

## Interpret values and ratios

A value such as `2.4 [2.2-2.9], n=6` is a median of 2.4, observed process range
2.2 to 2.9, and six independent processes. It is not a confidence interval.

`backend_ratios.csv` compares medians within one source, cell tier, phase, and
metric. A ratio below 1 means the named backend used less time, space, or memory
than its reference (`embedded` in A/B and C1; `bpcells` in C2). Never compare
absolute timing or memory across different study IDs or environments.

| field | meaning |
|---|---|
| `export_secs` | timed backend construction |
| `total_mb` | CRB plus relative backend sibling |
| build `peak_rss_mb` | whole build-process high-water RSS |
| `load_secs`, `attach_secs` | fresh-process CRB load and backend attachment |
| access `rss_mb`, `peak_rss_mb` | attached-process resident and peak memory |
| `first_query_secs` | first getter call; not controlled cold disk |
| `hot_p50_secs`, `hot_p95_secs` | warmed single-gene timing distribution |
| `block_secs` | deterministic 12-gene block read |

For C2, build peak RSS includes lazy source opening, backend writing, and shell
serialization, but not the separately measured query-plan preparation.

`viewer_metrics.csv` contains one row for each complete-source/backend pair.
All four must pass `createShinyApp()`/`runApp()`, WebSocket-backed Canvas hover,
box selection, zoom, and frozen-gene switching. The six elapsed values are
single-run diagnostics from the recorded browser, not medians, confidence
intervals, or replicated browser-performance estimates.

## Correctness and exclusions

Every successful access row must have `status = OK`, `correctness = OK`, and
matching row, block, and query-plan fingerprints. The combined report is not
created when any scheduled process is absent or failed. The same rule applies
to the four C2 Viewer rows and their browser, frozen-gene, and timing checks.

`embedded` has no C2 observation because each complete source exceeds the
`dgCMatrix` non-zero index limit. This is reported as `not representable`, not
as zero performance or a failed attempt.

## Files

At the study root:

| file | purpose |
|---|---|
| `study_manifest.csv` | frozen phase paths, study ID, acquisition/report SHAs, and core environment |
| `environment_comparison.csv` | explicit A/B–C1 and A/B–C2 compatibility result |
| `source_provenance.csv` | stable identifiers, landing pages, and acquired SHA-256 values |
| `query_plan_metrics.csv` | untimed preparation time, memory, and fingerprints |
| `query_panel.csv` | exact ordered genes, roles, densities, and reference fingerprints |
| `combined_metrics.csv` | median, range, and `n` for every absolute metric |
| `backend_ratios.csv` | matched within-tier median ratios |
| `correctness.csv` | passed/total access processes by phase |
| `viewer_metrics.csv` | four C2 Viewer outcomes, browser provenance, and raw step timings |
| `summary.md` | generated publication-facing narrative and tables |
| `figures/expression_backend_benchmark_publication_full.png` | combined scaling and full-source figure |
| `phases/<phase>/` | exact raw schedule, environment, source, build, access, and correctness evidence |

## Unsupported claims

- true cold-disk latency;
- universal or cross-machine performance;
- statistical significance from three builds;
- biological-method quality;
- replicated Viewer latency, visual quality, concurrent-user behavior, or
  cross-machine interaction quality;
- comparison with Vitessce;
- full-source `embedded` support.

The study supports a narrower claim: on one fully recorded host and two public
datasets, it measures the reproducible engineering trade-offs and feasibility
of Cerebro's three expression-storage modes over the stated scale range. A
completed study also establishes that the four specified million-cell Viewer
paths finished on that recorded host and browser.
