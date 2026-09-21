# Real-data expression-backend benchmark

This directory compares the `embedded`, `bpcells`, and `h5` backends on public single-cell matrices. This page explains how to run it. Read [METHODOLOGY.md](METHODOLOGY.md) for the experimental design and [RESULTS.md](RESULTS.md) before interpreting any number.

## Million-cell page readiness

The page harness uses the official 1M CRB at 100% cells. Because that data set has no immune repertoire, HLA typing, or trajectory, first create a deterministic benchmark-only CRB in the same directory so it can share the expression sidecar:

```bash
Rscript tests/bench/prepare_viewer_1m_pages.R . /path/to/cerebro_mouse_brain_1m.crb /path/to/cerebro_mouse_brain_1m_pages.crb
Rscript tests/bench/benchmark_viewer_1m_pages.R current=. /path/to/cerebro_mouse_brain_1m_pages.crb /path/to/viewer_1m_pages.tsv 3
```

The harness checks every page exposed by the data set. The enforced first-visit budget is strictly `<2,000 ms` for ordinary pages and `<3,000 ms` for Trajectory and HLA; every repeat visit must be strictly `<500 ms`. Overview, Groups, Gene Expression, Immune Repertoire, Trajectory, HLA, and Coordinated Views are required. Data-dependent pages such as Spatial are optional when unavailable, but every available page with a successful observation must pass its budget. The synthetic additions are performance fixtures only and must not be used for biological conclusions. The fixture does not contain valid Spatial data, so a skipped or historical Spatial row is not evidence about Spatial performance.

Every candidate/page/visit row runs in its own Shiny R process and Chrome process. A repeat observation warms that page once inside its otherwise fresh process, returns to Data Info, then measures the second visit. When an event is required, the harness increments a generation, records `performance.now()`, attaches the event listener, and clicks within one synchronous browser script; the event must carry a timestamp from that click generation. Specialist pages wait for the production `cerebro:specialist-state` event emitted after drawing plus their canvas `data-point-count` on both visits. Coordinated Views ends `elapsed_ms` at the post-click `primaryReady` event, when the correct primary projection is visible; `complete_elapsed_ms` separately records when its progressive supplement has merged. A repeat observation uses the persisted bundle and surface after its warm visit. Specialist pages and Coordinated Views do not wait for global Shiny idle; Groups and non-specialist optional pages continue to do so. The HLA primary frame intentionally does not wait for unrelated secondary Shiny work, but it must still emit the post-click specialist event.

The dedicated Ren Linked transport benchmark reports three distinct endpoints. `primary_logical_ms` and `complete_logical_ms` use the browser's monotonic clock from the tab click to the corresponding page event; the `driver_*` fields retain the surrounding automation wait for diagnosing polling overhead. `complete_logical_ms` means the supplement has been merged; `visual_ready_ms` additionally requires every visible GPU renderer to become idle, a clonal canvas when receptor data exist, and two consecutive animation frames with identical panel geometry and opacity; `interactive_ready_ms` then waits for Shiny to become idle. Use the latter two when comparing against perceived page readiness. Coordinated Views transport reports `projectionBytes` and `metadataBytes` separately because validated Viewer Pack coordinates and initial categorical codes use HTTP resource paths rather than the Shiny websocket. Projection resource time is further split into download and decode, while renderer initialization and apply times distinguish prewarming from per-data-set construction.

The script writes the raw observation TSV, a pre-generated `_schedule.tsv`, and a `_manifest.tsv`. Raw rows include the page correctness result/detail, rendered point count when available, effective renderer backend and adapter diagnostics, a post-timing visible-pixel check, sampled peak RSS for the Shiny R process tree and Chrome process tree at 50 ms intervals, a CDP `JSHeapUsedSize` snapshot, and browser-side WebSocket sent/received payload bytes. The elapsed timer ends at the observable primary-ready boundary. RSS/WebSocket collection covers the click-to-ready interval and stops immediately after the heap snapshot taken at that boundary; renderer and pixel diagnostics run afterward and do not affect elapsed time. The observation-local meter wraps the active Shiny socket's `send` method and listens for `message` events, counts encoded string, ArrayBuffer, typed-view, and Blob payload sizes without copying payloads over CDP, then restores the socket. These byte counts exclude WebSocket framing and TCP/TLS overhead. Specialist and Coordinated Views primary-ready means the post-draw page event and renderer-state check, not global Shiny idle. Selection keys, hover data, and other progressive auxiliary payloads are intentionally outside the primary gate and byte/resource window, so these fields must not be described as complete-interaction resources; Coordinated Views records its later supplement boundary separately in `complete_elapsed_ms`. The correctness contract is limited to Data Info reporting 1,000,000 cells plus observable page state: the four million-cell background canvases must each report exactly 1,000,000 points and a dataset fingerprint, HLA and Spatial canvases must report a positive point count and fingerprint, Groups must expose a visible nonempty Plotly graph, and Coordinated Views must expose a primary-ready summary with a nonempty dataset fingerprint. Canvas pages must also produce non-background pixels in the central rendered area. It does not validate complete scientific identity or statistics. Million-cell page budgets apply only when their effective backend is WebGPU; Canvas fallback rows remain correctness diagnostics and are not comparable WebGPU performance observations. A publication profile refuses to publish when required WebGPU timing is unavailable. The manifest records candidate Git SHA and dirty state, CRB control-file SHA-256, host/OS, R, CerebroNexus, shinytest2, chromote, and Chrome versions. Browser/server errors, failed correctness, missing metrics, and every applicable successful available-page budget are publication gates.

For a balanced comparison, pass additional `LABEL=REPO_ROOT` candidates after the round count. Candidate position rotates by round. Set `VIEWER_BENCH_PROFILE=publication` and use at least five rounds; shorter publication runs are rejected before launching an app.

```bash
VIEWER_BENCH_PROFILE=publication Rscript tests/bench/benchmark_viewer_1m_pages.R baseline=/path/to/baseline /path/to/cerebro_mouse_brain_1m_pages.crb /path/to/viewer_1m_pages.tsv 5 candidate=/path/to/candidate
```

On Windows, the reproducible wrapper resolves Git refs to immutable commits,
creates or reuses clean detached worktrees, runs a balanced three-candidate
comparison, and writes raw plus aggregated results outside the source tree:

```powershell
pwsh tests/bench/run_viewer_page_comparison.ps1 `
  -Artifact C:\data\cerebro_mouse_brain_1m_pages.crb `
  -Profile publication
```

The defaults compare the merge base before PR0 (`892097a1`), the fixed PR5
reference (`6c9718d7`), and the current `temp/pr5-dead-code-prune-v2` tip. Refs
are recorded together with their resolved SHA in `run-config.tsv`. The default
output root is the `CerebroNexus-benchmarks` directory beside the repository.
Each run writes `raw.tsv`, `raw_schedule.tsv`, `raw_manifest.tsv`, `run.log`,
`page-summary.tsv`, `comparison.tsv`, and `page-summary.md`. Use `-Profile quick`
for a one-round preflight, `-Pages overview,trajectory,spatial` to restrict a
diagnostic run, and explicit `-BaselineRef`, `-ReferenceRef`, or `-CandidateRef`
values to reproduce historical comparisons. Add `-Visits first` for a faster
first-open smoke test. Publication runs should keep the default of measuring
both `first` and `repeat` visits.

For a reproducible first-open comparison in which each revision builds and
uses its own Viewer Pack, run:

```powershell
pwsh tests/bench/run_viewer_release_comparison.ps1 `
  -Artifact C:\data\cerebro_mouse_brain_1m_pages.crb `
  -Rounds 3
```

This creates isolated hard-linked fixtures, builds one pack from the fixed PR5
revision and one from the latest candidate revision, runs every available page
three times with `VIEWER_VISITS_ONLY=first`, and stores raw data, manifests,
logs, resolved Git SHAs, and summaries below the sibling
`CerebroNexus-benchmarks` directory.

> **Current status:** a complete five-round `publication` profile comparison between clean PR4 and PR5 revisions is recorded in `results/million_cell_pages_pr4_pr5_4_6_3.tsv`. All 140 observations passed status and correctness checks. The overall comparison does not pass the publication gate because both candidates contain budget failures. PR5 itself passes 11/14 page/visit gates; only its first-visit Gene Expression, Immune Repertoire, and Coordinated Views remain over budget. Use the run as a complete diagnostic comparison under its historical completion-ready Coordinated Views contract, not as a current primary-frame result. The archived pilot is retained for provenance only.

## Quick start

```bash
# Smallest correctness and harness check
BENCH_PROFILE=quick tests/bench/run_sweep.sh

# Repeated local review
BENCH_PROFILE=standard tests/bench/run_sweep.sh

# Repeated evidence plus staged figures
BENCH_PROFILE=publication tests/bench/run_sweep.sh

# Explicit memory-boundary experiment; normally rejected on a 32 GiB host
BENCH_PROFILE=stress tests/bench/run_sweep.sh
```

Limit a run to one source when developing the harness:

```bash
BENCH_SOURCES_ONLY=mouse_brain_e18 \
  BENCH_PROFILE=quick tests/bench/run_sweep.sh
```

`BENCH_ALLOW_UNSAFE=1` bypasses the resource gate. Use it only for an intentional stress run. Normal runs must not silently skip unsafe tiers.

## What happens before a download

The command first:

1. inspects source dimensions;
2. records the machine and Git revision;
3. creates the requested run plan; and
4. checks estimated memory, sparse-index, and free-disk limits.

If the plan is unsafe, it stops with the source, cell tier, estimated memory, safe budget, and reason. No complete source file or backend export has started at that point.

## Profiles

| profile | purpose | large boundary tiers |
|---|---|:---:|
| `quick` | verify the harness and correctness gate | no |
| `standard` | repeated local comparison | no |
| `publication` | repeated article evidence and figures | no |
| `stress` | opt-in host memory-boundary experiment | yes |

## Outputs

Validated runs are immutable under `result/runs/<run-id>/`. `result/CURRENT` contains the run used by report and plotting tools. A failed or interrupted run leaves the previous pointer unchanged.

The 2026-07-30 single-run pilot is retained under `result/archive/pilot-2026-07-30/`; it is superseded and cannot support current performance claims.

## Plain-language script map

| script | meaning |
|---|---|
| `01_inspect_data.R` | find out how large the sources are |
| `02_record_environment.R` | record the code and machine under test |
| `03_plan_runs.R` | list the requested backend runs |
| `04_check_resources.R` | stop before running a plan that will not fit |
| `10_export_backend.R` | export one backend in a fresh process |
| `20_measure_backend.R` | measure and correctness-check one backend |
| `30_check_measurements.R` | reject incomplete or incorrect measurements |
| `40_write_report.R` | generate the Markdown result report |
| `41_draw_figures.R` | generate publication figures |
| `50_check_outputs.R` | ensure the report package is complete |
| `60_publish_results.R` | publish immutably and update `CURRENT` last |

The two default public sources are 10x mouse brain E18 (4.2 GB) and the HBCC human prefrontal-cortex atlas (14.2 GB). The MSSM cohort is opt-in through `BENCH_SOURCES_EXTRA=human_pfc_mssm`.
