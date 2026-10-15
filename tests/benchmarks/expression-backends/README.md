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

Every run gets a timestamp-and-commit directory under `results/expression-backends/<quick|full|scale>/runs/`. Existing runs are never overwritten by a later run. The CSV measurements, source checksums, environment, report, stage logs and figures stay together. After a full or scale run passes validation and is archived, publication replaces that study's `figures/` with copies of the archived images and updates `CURRENT`. The two studies advance independently: if full succeeds and scale fails, full advances while scale keeps its previous result.

Publication stages the new images before switching and rolls back both the images and `CURRENT` on an error or ordinary interrupt. If the publisher is killed, the runner restores the old pair; if the entire runner or host stops, the next publication recovers it before retrying. Successful publication removes its temporary rollback directory. Run only one publisher per study at a time.

Verified source downloads stay in the external cache. Failed runs keep their scratch directory by default; `resume` retries validation, reporting and publication only. It does not resume measurements or start the next study. Generated CRB/backend files are temporary and normally removed after measurement; start with `BENCH_KEEP=1 bash tests/benchmarks/expression-backends/run.sh` if you need to retain them too. Starting another run archives the previous launcher log under `runner/logs/` in the cache.

## Figures

Full and scale runs save `figures/expression_backend_benchmark_overview.png` (350 dpi) and an editable vector PDF. The full comparison uses backend categories, shows every process observation plus median and range, and includes numeric medians below the backend names. It uses logarithmic axes for warmed reads and storage, and an expanded linear axis for loaded memory. Scale overviews use logarithmic cell-count and value axes for all four panels, including memory and storage; storage is drawn as a trend rather than bars. Both memory and storage use explicit binary units. Skipped embedded tiers are identified in the caption. Read processes are nested within builds; ranges are not confidence intervals. Scale runs also save a 350 dpi embedded build-memory plot (legacy filename `expression_backend_benchmark_ceiling.png`): measured peak R heap in GiB against nonzero elements in millions, with no extrapolated memory ceiling. Quick runs produce measurements and a report without figures.

The same layout applies to full and scale:

```text
full/ or scale/
  CURRENT                     latest successful run ID
  figures/                    latest images, replaced after successful publication
    *.png / *.pdf
  runs/<run-id>/figures/       images archived with that run
    *.png / *.pdf
```

Open the [latest full figures](../results/expression-backends/full/figures/) or
[latest scale figures](../results/expression-backends/scale/figures/). These directories
contain image exports only: no run-ID subdirectories, QA files or README files.
Their images are byte-for-byte copies of the run selected by `CURRENT`. Plotting
does not rerun measurements. Export dimensions prioritize readability; check text
at the intended final size before reducing them.

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

Use `run.sh` for normal work; it runs both studies and publishes their images automatically.
All implementation lives in `scripts/`:

- `run.sh`: environment, cached downloads, independent measurement processes and finalization.
- `benchmark.R`: source/configuration settings at the top, shared measurement and publication functions below.
- `benchmark_cli.R`: command-line stages; selects the appropriate plotting function.
- `figure_full.R` and `figure_scale.R`: full and scale plots; write image exports only.

Reuse verified downloads and the fixed query plan. Keep Viewer checks separate,
and avoid adding overlapping launchers or configuration layers.
