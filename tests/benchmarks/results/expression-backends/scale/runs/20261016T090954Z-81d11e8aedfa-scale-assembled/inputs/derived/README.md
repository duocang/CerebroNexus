# Scale curves with reused full-dataset endpoints

This derived visualization combines the two completed original runs with new 300k measurements and one separately marked Human embedded 500k feasibility measurement. Full endpoints were reused, never remeasured.

- Full: `20261015T213433Z-81d11e8aedfa-full` (20 successful builds, 40 read processes).
- Scale: `20261016T002447Z-81d11e8aedfa-scale` (165 successful builds, 330 read processes; 5 resource skips).
- Additional 300k: `20261016T061232Z-81d11e8aedfa-scale-300k` (30 successful builds, 60 read processes; both datasets, all three backends, five builds per combination).
- Human 500k feasibility: `20261016T061232Z-81d11e8aedfa-human500k-embedded-99G` (one build, two read processes; successful under a 99 GiB cap).
- Branch: `paper/real-data-benchmark`; both manifests record clean commit `81d11e8aedfae380516f16e4f95c29728fbad02a`.
- Environment: R 4.6.1, CerebroNexus 5.0.0, Matrix 1.7.6, rhdf5 2.56.1, Seurat 5.6.0, SeuratObject 5.4.0, BPCells 0.3.1, HDF5Array 1.40.0; Linux 6.18.54.1-microsoft-standard-WSL2, Intel i9-12900K, one benchmark thread.

## Backup and preservation

Before this supplemental update, the entire results directory was backed up to `C:/Users/wangx/Downloads/CerebroNexus-results-before-300k-supplement-20261016-081413/results`. All 724 files (11,102,798 bytes) in both source and backup were rechecked by SHA256 before editing. This backup includes the previous derived figure. The earlier 691-file original inventory described below remains unchanged after this update.

Before any derived files were created, the entire worktree `tests/benchmarks/results` directory was copied to `C:/Users/wangx/Downloads/CerebroNexus-results-before-full-endpoints-20261016-074602/results`.
All 691 files (8,127,849 bytes) were checked for matching size and SHA256. The backup's `backup-manifest.json` preserves the full inventory. `backup-reference.json` records the post-render recheck.
The benchmark runner had already activated the completed new runs. This derivation does not change either CURRENT, any original raw CSV, any historical run, or the runner's original figures. It does not copy results into the separate primary checkout.

## How to read the figure

Common scale tiers are 1k, 10k, 50k, 100k, **300k**, 500k and 1m. Actual x coordinates for the full endpoints are Mouse 1,306,127 and Human PFC 1,486,324 cells. Rings and dashed extensions identify measurements reused from Full. No full-size scale experiment was launched.

Filled points have five independent builds per source/tier/backend, with two read processes per build. Access metrics use the median of two process values within each build, then median and min-max across five builds; storage uses five build values directly. These are descriptive ranges, not confidence intervals. The original overview pooled ten process values; this summary retains their nested structure. CSV values retain original profile/run ID plus `source_run_id` and `tier_type` (`scale`, `full`, `scale_supplement`, or `feasibility`).

**Disconnected hollow diamonds are the exception:** Human embedded 500k has only one build and two independent read processes. Its access point is the median of those two processes, with no between-build error bar. It establishes feasibility, not five-build repeatability. The matrix has 34,176 genes and 1,982,146,320 nonzero entries. Its query plan fingerprint `47fca3dda5ed5d1c8b7ddeb57c94d078` and all four read fingerprints match the original HDF5/BPCells 500k records. Cgroup peak was approximately 97.3 GiB; task swap was zero.

The panels show warmed single-gene latency, materialized 12-gene block latency, RSS after load/attach, and total disk footprint. All 16,416 combined raw timing records use CLOCK_MONOTONIC; endpoints, finite nonnegative durations, resolution, sequence coverage and summary agreement pass the repository validator. All 432 read processes pass materialized-v2 correctness. There are 216 successful builds and five preserved resource skips (221 outcome rows). Original Full/Scale and additional 300k inputs also passed the unmodified CLI validator; the single-build feasibility experiment was separately validated and does not claim formal Scale preflight approval. Input checksums are verified.

Both profiles use the same 12-gene selection strategy, three hot repetitions and the same operation/materialization boundaries. The selected genes vary by tier. Therefore the line is a descriptive dataset/backend curve, not a fixed-query complexity experiment. In particular, decreases at full size must not be interpreted as proof that larger matrices are intrinsically faster. Read `combined_query_panel.csv` for the actual genes.

The five original Human PFC embedded 500k preflight skips remain intact, with no invented values. The new hollow diamonds are a separate run: the standalone build calls the existing scale-prefix reader, Seurat builder and exporter outside formal preflight, followed by two actual unmodified CLI access processes. The prior attempt's query plan was reused; `query_plan_source_run_id` records that origin in the combined query table. Embedded 1m/full was not scheduled. Existing original-run figures remain unchanged.

Supplemental resource policies differ from the original runs: 300k had a 90 GiB cap, 500k a 99 GiB cap; both disabled task swap and used Nice=10/IOWeight=10. Operation definitions match, but host/cache state and memory limits may affect cross-run latency. These added points are not a controlled causal comparison of cell count or proof of repeatable 500k performance. The unchanged installed branch library and R executable were reused; no benchmark package was upgraded.

## Files and reproduction

- `scale_with_full_endpoints.pdf`, `.png`: final 240 x 260 mm figure; PNG at 350 dpi, PDF contains editable text. Optional SVG export is omitted because svglite is absent; no package was installed.
- `replot.R`, `measure_rendered_panels.R`: source and measured viewport geometry helper.
- `combined_exports.csv`, `combined_access.csv`, `combined_query_panel.csv`: derived concatenation with explicit provenance.
- `access_per_build.csv`, `access_summary.csv`, `storage_summary.csv`, `full_endpoints.csv`, `resource_skips.csv`: transparent summary stages and exclusions.
- `data-validation.json`, `input-sha256.json`, `execution.log`, QA JSON/logs: verification evidence.
- `supplements/20261016-300k-and-human500k-99G/`: raw supplemental CSVs, frozen query plan, scripts, logs, validations and SHA256 inventory; all inputs retain their acquisition identity.

From the worktree's declared Nix shell, run:

```sh
BENCH_PROFILE=panel_c2 Rscript tests/benchmarks/expression-backends/scripts/benchmark_cli.R validate tests/benchmarks/results/expression-backends/full/runs/20261015T213433Z-81d11e8aedfa-full
BENCH_PROFILE=scale Rscript tests/benchmarks/expression-backends/scripts/benchmark_cli.R validate tests/benchmarks/results/expression-backends/scale/runs/20261016T002447Z-81d11e8aedfa-scale
FIGURE_SKILL=/mnt/c/Users/wangx/.codex/skills/nature-figure Rscript tests/benchmarks/results/expression-backends/derived/20261016-scale-with-full/replot.R .
```

The plotting script uses ggplot2, patchwork, dplyr, jsonlite and digest already present in the existing Nix environment. It reads the explicitly named original runs and archived supplement, never choosing inputs via CURRENT or launching measurements. svglite remains optional and unavailable. It uses the backend-neutral alignment auditor and installed Windows Python/PyMuPDF for final QA. The supplemental measurements were newly authorized acquisitions; plotting did not repeat them. No dataset download, cache cleanup, commit or push was performed.
