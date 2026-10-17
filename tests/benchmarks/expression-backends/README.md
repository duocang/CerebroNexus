# H5 / BPCells / embedded benchmark

Compare expression-storage backends by build time, file size, memory, startup and gene-read speed. Use the commands below from the repository root on Linux or WSL, with `nix-shell` installed and a clean Git checkout. The full run needs a machine with substantial memory and disk space; the runner checks capacity before downloading the data.

## Start and monitor

```bash
# Small correctness run: 1,000 cells per source, all three backends.
bash tests/benchmarks/expression-backends/run.sh quick

# Independent size comparison, including both full-dataset endpoints.
bash tests/benchmarks/expression-backends/run.sh scale
# No argument (or run) also runs Scale only. Optional separate Full study:
# bash tests/benchmarks/expression-backends/run.sh full

# Check progress.
bash tests/benchmarks/expression-backends/run.sh status

# Follow the log (default cache location).
tail -f "${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark/runner/benchmark.log"
```

Both commands start in the background and enter the project Nix environment. The runner ignores your personal R setup (`R_LIBS`, `R_LIBS_SITE`, user library, `~/.Renviron`, `~/.Rprofile`), so only the Nix packages are loaded. Run one at a time. `quick` builds and opens each backend once; it checks correctness and is not enough for performance conclusions. It still downloads the complete source files on first use (about 4.2 GB mouse and 14.2 GB human). To try only mouse data, use `BENCH_SOURCES_ONLY=mouse_brain_e18 bash tests/benchmarks/expression-backends/run.sh quick`. Downloads are checked and reused. Temporary work and logs use `${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark/` by default.

If measurements finished but validation or reporting failed, retry those final steps using the scratch directory printed in the log. This does not restart an interrupted measurement run:

```bash
bash tests/benchmarks/expression-backends/run.sh resume /path/to/cerebro-bench.XXXXXX
```

## What runs

| Study | Data | Backends |
|---|---|---|
| Full datasets | 10x mouse brain: 1,306,127 cells; PsychAD human PFC: 1,486,324 cells | H5 (compressed), H5 (uncompressed) and BPCells |
| Size comparison | The same sources at 1k, 10k, 50k, 100k, 300k, 500k, 1m, then each exact full-dataset size | Both H5 variants and BPCells at every size; embedded attempted through 500k |

The complete matrices exceed the sparse-index limit of embedded storage, so full-dataset embedded results are unavailable. They are not replaced with smaller samples.

`h5` is the 10x H5 sidecar at the package exporter's compression level (`HDF5Array::getHDF5DumpCompressionLevel()`, 6 by default), i.e. what CerebroNexus ships. `h5_uncompressed` is the same file written at gzip level 0, a storage/read trade-off reference that the package does not produce by default; both are opened through the package's `h5` backend. Both are built in the same run and interleaved with BPCells (and embedded in Scale), so they share host conditions. The manifest records both levels (`h5_gzip_level`, `h5_uncompressed_gzip_level`). Full/scale runs without these keys predate the fix: their `h5` rows were written at gzip level 0 and are uncompressed results; `paper/figure-drafts/expression-backends/rebuild-figures.R` labels them accordingly. Uncompressed H5 needs about 12 bytes per nonzero on disk, so the preflight requires the cached source plus 18 bytes per nonzero of free scratch space (about 125 GB for the full human matrix) within 80% of free disk.

In full and scale runs, each source, size and backend gets five independent builds. Each successful build is opened in two fresh R processes. Runs use the same cells, gene names and fixed 12-gene queries, including full-cell reads and non-contiguous subsets of up to 100,000 cells. Reading a lazy matrix fully into memory is included in the timer; value checks happen afterward. Processes run sequentially with one thread by default.

Elapsed durations use `CLOCK_MONOTONIC`; system-clock timestamps are retained only as provenance. A small native clock reader is compiled once per R process using `R CMD SHLIB` before the first timed operation (the existing Nix toolchain supplies the compiler). Clock initialization fails closed; there is no wall-clock fallback. `21_access_timings.csv` retains every startup, first read, warmed read, block and subset read, including monotonic endpoints, wall-clock endpoints and clock resolution. CSV writes and value checks occur outside timed operations. Publication requires finite nonnegative durations, complete raw observations and summaries matching those observations.

Incorrect values, crashes or unexpected build failures stop publication. An explicit embedded memory or sparse-index limit is recorded as a resource limit. A new result becomes `CURRENT` only after all required checks pass.

Scale snapshots Linux `MemAvailable` at launch, capped by the remaining memory allowance of every cgroup-v2 ancestor. It uses 99% of that available RAM, not 99% of installed RAM. The runner enforces this byte limit with a systemd user scope (`MemoryMax`), with task swap disabled; failure to create the scope stops the run. The snapshot, byte limit and policy are recorded in the manifest. This is a fixed launch-time limit, not a moving target during measurements. A working systemd user manager with memory-controller delegation is required.

Scale checks embedded separately because it loads the matrix into memory. Its estimate is 1 GiB plus 58 bytes per nonzero (`BENCH_EMBEDDED_BYTES_PER_NNZ`; measured peaks were 52.6-57 bytes per nonzero from 50k cells up, so at least 10% headroom; runs before this change used 64), checked against that budget and the R vector limit, first using source density and then actual tier nonzeros. Exceeding the budget or 32-bit sparse-index limit records `SKIPPED(resource)` while H5 and BPCells continue; an embedded build that the kernel kills at the cap is also recorded as `SKIPPED(resource)`, but only when the scope's cgroup `oom_kill` count rose during that build (any other death remains a crash that blocks publication); increasing the budget does not guarantee a formerly skipped tier will pass the estimator. No automatic cell-count reduction occurs. Historical runs retain their original policies (including the old 70%-of-total-RAM preflight); they are not retroactively labelled as 99% runs.

Scale now measures its own full endpoints and does not read Full results. The optional `full` command remains available, but default `run` does not run both studies or measure the endpoints twice.

## Reruns and saved files

Every run gets a timestamp-and-commit directory under `results/expression-backends/<quick|full|scale>/runs/`. Existing runs are never overwritten by a later run. The CSV measurements, source checksums, environment, report, stage logs and figures stay together. After a full or scale run passes validation and is archived, publication replaces that study's `figures/` with copies of the archived images and updates `CURRENT`. The two studies advance independently: if full succeeds and scale fails, full advances while scale keeps its previous result.

Publication stages the new images before switching and rolls back both the images and `CURRENT` on an error or ordinary interrupt. If the publisher is killed, the runner restores the old pair; if the entire runner or host stops, the next publication recovers it before retrying. Successful publication removes its temporary rollback directory. Run only one publisher per study at a time.

Verified source downloads stay in the external cache. Failed runs keep their scratch directory by default; `resume` retries validation, reporting and publication only; if the run was already archived before activation failed, it skips regeneration and only re-verifies and activates the archive. It does not resume measurements or start the next study. Generated CRB/backend files are temporary and normally removed after measurement; start with `BENCH_KEEP=1 bash tests/benchmarks/expression-backends/run.sh` if you need to retain them too. Starting another run archives the previous launcher log under `runner/logs/` in the cache.

## Figures

Full and scale runs save `figures/expression_backend_benchmark_overview.png` (340 dpi) and an editable vector PDF on the Cairo device. Both figures share one style: white background, Okabe-Ito colours (H5 compressed `#0072B2` filled square, H5 uncompressed `#56B4E9` open square and dashed line, BPCells `#E69F00`, Embedded `#D55E00`; defined once in `bench_backend_style()`), lowercase panel letters a-h at the top-left of each panel, and no title or caption inside the file. The full comparison uses backend categories, shows every read process as a small dot plus the build median and observed min-max whiskers. The scale overview plots medians across builds against cell count on a log10 x axis. Timing and storage use logarithmic axes; loaded memory is linear (GiB). Read processes are nested within builds and ranges are descriptive min-max, not confidence intervals. Scale runs also save a 340 dpi embedded build-memory plot (legacy filename `expression_backend_benchmark_ceiling.png`): measured peak R heap in GiB against nonzero elements in millions, with no extrapolated memory ceiling. Quick runs produce measurements and a report without figures.

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

### Assembled Scale evidence (2026-10-16)

The latest Scale collection may have `result_kind=assembled` in its manifest.
It consolidates the verified October 16 Scale, 300k supplement, original Full
endpoints and the completed Human 500k embedded series (one feasibility build
plus four prospective repeats) without new measurements.
It is self-contained: original acquisition CSVs, logs, environments and checksums
are copied under `inputs/`; replotting does not read the external Full directory.
Original `run_id` and `profile` values remain unchanged in merged measurement
tables, with `collection_id`, `source_run_id`, `source_profile` and `tier_type`
recording provenance. It is not a new acquisition under the 99% memory policy.

All 44 configured dataset/tier/backend combinations have real observations with
five builds and ten reads each; Human 500k embedded is one feasibility build plus
four repeats. Original five resource skips are retained separately, never counted
as successes or silently replaced by invented repeats. The collection's schedule
describes included successful builds rather than the prospective five-repeat
protocol. Its `summary.md` explains these limitations. Each collection ships its
own archived `validate.R`/`code`; run it inside the collection directory to check
every origin, raw timing and exact row conservation. The repo-side CLI no longer
carries the assembled validator. Use the collection's archived `replot.R` for
figures; the ordinary single-run report/figure commands deliberately reject
assembled collections to avoid pooling acquisition identities.

**Existing results are historical.** The September 22 runs predate materialized reads, and the October measurements at `b64f8df3` contain four negative subset-block durations (one full, three scale). Their timing evidence remains provisional. Full and scale H5 files written before the compression fix used BPCells' default `gzip_level = 0`; the H5 builder now uses the package exporter's level (`HDF5Array::getHDF5DumpCompressionLevel()`, 6 by default), so earlier H5 size, build and read figures are not comparable with fresh runs. Fresh runs must satisfy both `materialized-v2` and `monotonic-v1`. Retain the original CSVs; do not clip, take absolute values or delete invalid observations. `resume` cannot certify a legacy run without raw monotonic observations. Repeat full and scale on the same 12900K WSL host; local Mac smoke checks do not replace those performance measurements.

## Keep it simple

Use `run.sh scale` (or the default `run.sh`) for normal work; it runs the independent Scale study and publishes its images automatically.
All implementation lives in `scripts/`:

- `run.sh`: environment, cached downloads, independent measurement processes and finalization.
- `benchmark.R`: source/configuration settings at the top, shared measurement and publication functions below.
- `benchmark_cli.R`: command-line stages; selects the appropriate plotting function.
- `figure_full.R` and `figure_scale.R`: full and scale plots; write image exports only.

Reuse verified downloads and the fixed query plan. Keep Viewer checks separate,
and avoid adding overlapping launchers or configuration layers.
