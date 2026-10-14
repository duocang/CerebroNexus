# H5 / BPCells / embedded benchmark

Compare expression-storage backends by build time, file size, memory, startup and gene-read speed. Use the commands below from the repository root on Linux or WSL, with `nix-shell` installed and a clean Git checkout. The full run needs a machine with substantial memory and disk space; the runner checks capacity before downloading the data.

## Start and monitor

```bash
# Small correctness run: 1,000 cells per source, all three backends.
bash tests/benchmarks/expression-backends/run.sh quick

# Full measurements: full datasets, then the size comparison.
bash tests/benchmarks/expression-backends/run.sh

# Check progress.
bash tests/benchmarks/expression-backends/run.sh status

# Follow the log (default cache location).
tail -f "${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark/runner/benchmark.log"
```

Both commands start in the background and enter the project Nix environment. Run one at a time. `quick` builds and opens each backend once; it checks correctness and is not enough for performance conclusions. It still downloads the complete source files on first use (about 4.2 GB mouse and 14.2 GB human). To try only mouse data, use `BENCH_SOURCES_ONLY=mouse_brain_e18 bash tests/benchmarks/expression-backends/run.sh quick`. Downloads are checked and reused. Temporary work and logs use `${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark/` by default.

If measurements finished but validation or reporting failed, retry those final steps using the scratch directory printed in the log. This does not restart an interrupted measurement run:

```bash
bash tests/benchmarks/expression-backends/run.sh resume /path/to/cerebro-bench.XXXXXX
```

## What runs

| Study | Data | Backends |
|---|---|---|
| Full datasets | 10x mouse brain: 1,306,127 cells; PsychAD human PFC: 1,486,324 cells | H5 and BPCells |
| Size comparison | The same sources at 1k, 10k, 50k, 100k, 500k and 1m cells | H5 and BPCells at every size; embedded attempted through 500k |

The complete matrices exceed the sparse-index limit of embedded storage, so full-dataset embedded results are unavailable. They are not replaced with smaller samples.

In full and scale runs, each source, size and backend gets five independent builds. Each successful build is opened in two fresh R processes. Runs use the same cells, gene names and fixed 12-gene queries, including full-cell reads and non-contiguous subsets of up to 100,000 cells. Reading a lazy matrix fully into memory is included in the timer; value checks happen afterward. Processes run sequentially with one thread by default.

Incorrect values, crashes or unexpected build failures stop publication. An explicit embedded memory or sparse-index limit is recorded as a resource limit. A new result becomes `CURRENT` only after all required checks pass.

Scale checks embedded separately because it loads the matrix into memory. If its conservative estimate exceeds 70% of available memory or the sparse-index limit, that tier is recorded as `SKIPPED(resource)` while H5 and BPCells continue. The check uses both the source-wide estimate and the actual tier's nonzero count. These skips describe this machine's resource budget, not a measured universal size limit.

## Reruns and saved files

Every run gets a timestamp-and-commit directory under `results/expression-backends/<quick|full|scale>/runs/`. Existing runs are never overwritten. The CSV measurements, source checksums, environment, report, stage logs and generated report figures stay together. `CURRENT` selects the latest successful run in each study independently: if full succeeds and scale fails, full advances while scale keeps its previous result.

Verified source downloads stay in the external cache. Failed runs keep their scratch directory by default; `resume` retries validation, reporting and publication only. It does not resume measurements or start the next study. Generated CRB/backend files are temporary and normally removed after measurement; start with `BENCH_KEEP=1 bash tests/benchmarks/expression-backends/run.sh` if you need to retain them too. Starting another run archives the previous launcher log under `runner/logs/` in the cache.

## Figures

Full runs automatically save `figures/expression_backend_benchmark_overview.png`. Scale runs also save `figures/expression_backend_benchmark_ceiling.png`. These 150 dpi PNGs are report previews, not final submission artwork. Quick runs produce measurements and a report without figures.

For a full-dataset paper figure with vector PDF, 350 dpi TIFF, legend and source data, follow [paper figure commands](../../../paper/figures/README.md). Exporting paper figures is a separate step and preserves the benchmark results. Old timing records, incomplete runs and quick/scale inputs are rejected by that exporter. Final submission still needs review of the fresh figure at its printed size.

## Read results

Results are stored in [../results/expression-backends/](../results/expression-backends/), with separate `quick/`, `full/` and `scale/` directories. Quick runs do not replace full or scale results. To open the selected full-dataset report:

```bash
RESULTS=tests/benchmarks/results/expression-backends/full
RUN="$RESULTS/runs/$(cat "$RESULTS/CURRENT")"
cat "$RUN/summary.md"
```

Use `quick` or `scale` instead of `full` for those reports. `10_export.csv` contains build measurements; `20_access.csv` contains startup and query measurements. `run_manifest.csv` and `source_manifest.csv` identify the code, host, packages and input files. Keep files from each run together.

Numbers are medians with observed ranges, not confidence intervals. The two access processes share one built artifact. The first read may benefit from the operating-system cache. Storage and memory fields ending in `_mb` use MiB. Embedded `export_secs` includes the full exporter, while H5/BPCells measure the backend writer; do not compare that column alone as equal work.

**Existing results are historical.** The September 22 runs predate the corrected timer, and their embedded size-comparison reads failed correctness checks. Use fresh `materialized-v2` measurements for backend comparisons; changing the scripts does not update old results.

## Keep it simple

Use `run.sh` for normal work. The implementation stays in three files: `scripts/run.sh` runs the stages, `scripts/benchmark.R` holds shared functions, and `scripts/benchmark_cli.R` runs one stage. Reuse verified downloads and the fixed query plan. Keep Viewer checks separate, and avoid adding overlapping launchers or configuration layers.
