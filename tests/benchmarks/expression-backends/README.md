# H5 / BPCells / embedded benchmark

Compare expression-storage backends by build time, file size, memory, startup and gene-read speed. Use the commands below from the repository root on Linux or WSL, with `nix-shell` installed and a clean Git checkout. The full run needs a machine with substantial memory and disk space; the runner checks capacity before downloading the data.

## Start and monitor

```bash
# Small correctness run: 1,000 cells per source, all three backends.
bash tests/benchmarks/expression-backends/run.sh quick

# Independent size comparison, including both full-dataset endpoints.
bash tests/benchmarks/expression-backends/run.sh scale
# No argument (or run) also runs Scale.

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
| Scale | 10x mouse brain (1,306,127 cells) and PsychAD human PFC (1,486,324 cells) at 1k, 10k, 50k, 100k, 300k, 500k, then each exact full-dataset size (no 1M tier) | H5 (compressed), H5 (uncompressed) and BPCells at every size; embedded attempted through 500k |

The complete matrices exceed the sparse-index limit of embedded storage, so full-dataset embedded results are unavailable. They are not replaced with smaller samples.

`h5` is the 10x H5 sidecar at the package exporter's compression level (`HDF5Array::getHDF5DumpCompressionLevel()`, 6 by default), i.e. what CerebroNexus ships. `h5_uncompressed` is the same file written at gzip level 0, a storage/read trade-off reference that the package does not produce by default; both are opened through the package's `h5` backend. Both are built in the same run and interleaved with BPCells (and embedded in Scale), so they share host conditions. The manifest records both levels (`h5_gzip_level`, `h5_uncompressed_gzip_level`). Full/scale runs without these keys predate the fix: their `h5` rows were written at gzip level 0 and are uncompressed results; `paper/figure-drafts/expression-backends/rebuild-figures.R` labels them accordingly. Uncompressed H5 needs about 12 bytes per nonzero on disk, so the preflight requires the cached source plus 18 bytes per nonzero of free scratch space (about 125 GB for the full human matrix) within 80% of free disk.

In Scale runs, each source, size and backend gets five independent builds. Each successful build is opened in two fresh R processes. Runs use the same cells, gene names and fixed 12-gene queries, including full-cell reads and non-contiguous subsets of up to 100,000 cells. Reading a lazy matrix fully into memory is included in the timer; value checks happen afterward. Processes run sequentially with one thread by default.

Elapsed durations use `CLOCK_MONOTONIC`; system-clock timestamps are retained only as provenance. A small native clock reader is compiled once per R process using `R CMD SHLIB` before the first timed operation (the existing Nix toolchain supplies the compiler). Clock initialization fails closed; there is no wall-clock fallback. `21_access_timings.csv` retains every startup, first read, warmed read, block and subset read, including monotonic endpoints, wall-clock endpoints and clock resolution. CSV writes and value checks occur outside timed operations. A full R garbage collection runs outside the timers before the first read, before the warmed loop and before each block and subset read (`bench_settle_heap`). Without it, R's periodic full collection (0.3-0.7 s, set by the whole heap rather than the read) lands in whichever read reaches that point of the process's allocation history; which read that is changes with tier, source and even the size of the harness scripts. Startup is not settled and still includes the collections triggered while loading the shell. Benchmark processes do not load Seurat unless they build an embedded export: earlier harness versions attached it in every process, which added about 5 s per process and about 350 MB to every backend's memory after load in results measured before that change. Publication requires finite nonnegative durations, complete raw observations and summaries matching those observations.

Incorrect values, crashes or unexpected build failures stop publication. An explicit embedded memory or sparse-index limit is recorded as a resource limit. A new result becomes `CURRENT` only after all required checks pass.

Scale snapshots Linux `MemAvailable` at launch, capped by the remaining memory allowance of every cgroup-v2 ancestor. It uses 99% of that available RAM, not 99% of installed RAM. The runner enforces this byte limit with a systemd user scope (`MemoryMax`), with task swap disabled; failure to create the scope stops the run. The snapshot, byte limit and policy are recorded in the manifest. This is a fixed launch-time limit, not a moving target during measurements. A working systemd user manager with memory-controller delegation is required.

Scale checks embedded separately because it loads the matrix into memory. Its estimate is 1 GiB plus 58 bytes per nonzero (`BENCH_EMBEDDED_BYTES_PER_NNZ`; measured peaks were 52.6-57 bytes per nonzero from 50k cells up, so at least 10% headroom; runs before this change used 64), checked against that budget and the R vector limit. The launch preflight uses the source-wide density; the frozen query plan then supplies the tier's exact nonzeros, which decide memory and the sparse-index limit (the density can overstate a prefix: Human 500k is 112 GiB by density but 108 GiB exact). A preflight failure for any other reason, such as disk, still skips. Exceeding the budget or 32-bit sparse-index limit records `SKIPPED(resource)` while H5 and BPCells continue; an embedded build that the kernel kills at the cap is also recorded as `SKIPPED(resource)`, but only when the scope's cgroup `oom_kill` count rose during that build (any other death remains a crash that blocks publication); increasing the budget does not guarantee a formerly skipped tier will pass the estimator. No automatic cell-count reduction occurs. Historical runs retain their original policies (including the old 70%-of-total-RAM preflight); they are not retroactively labelled as 99% runs.

Scale has no 1M tier: 1M sits next to the 1.31M and 1.49M endpoints on the log axis, added little and was the costliest tier.

## Reruns and saved files

Every run gets a timestamp-and-commit directory under `results/expression-backends/<quick|scale>/runs/`. Existing runs are never overwritten by a later run. The CSV measurements, source checksums, environment, report and stage logs stay together. After a run passes validation and is archived, publication points that study's `CURRENT` at it; a failed run leaves `CURRENT` unchanged.

Verified source downloads stay in the external cache. Failed runs keep their scratch directory by default; `resume` retries validation, reporting and publication only; if the run was already archived before activation failed, it skips regeneration and only re-verifies and activates the archive. It does not resume measurements. Generated CRB/backend files are temporary and normally removed after measurement; start with `BENCH_KEEP=1 bash tests/benchmarks/expression-backends/run.sh` if you need to retain them too. Starting another run archives the previous launcher log under `runner/logs/` in the cache.

## Figures

Runs do not draw figures. The article and paper figures are drawn from a run's `10_export.csv` and `20_access.csv` by `paper/figure-drafts/expression-backends/rebuild-figures.R` (`render-figures.sh` supplies the fonts); the full-size figure uses the Scale run's own full-size endpoints. Plotting does not rerun measurements.

## Read results

Results are stored in [../results/expression-backends/](../results/expression-backends/), with separate `quick/` and `scale/` directories. Quick runs do not replace Scale results. To open the selected Scale report:

```bash
RESULTS=tests/benchmarks/results/expression-backends/scale
RUN="$RESULTS/runs/$(cat "$RESULTS/CURRENT")"
cat "$RUN/summary.md"
```

Use `quick` instead of `scale` for quick reports. `10_export.csv` contains build measurements; `20_access.csv` contains startup and query measurements. `run_manifest.csv` and `source_manifest.csv` identify the code, host, packages and input files. Keep files from each run together.

Numbers are medians with observed ranges, not confidence intervals. The two access processes share one built artifact. The first read may benefit from the operating-system cache. Storage and memory fields ending in `_mb` use MiB. Embedded `export_secs` includes the full exporter, while H5/BPCells measure the backend writer; do not compare that column alone as equal work.

### Current Scale: one complete four-backend run (2026-10-19)

`CURRENT` Scale is `20261018T101740Z-1ed063d363a0-scale`, measured on the 128 GB host (Intel i7-14700, 128 GB, Ubuntu, 99% cap 120,950,935,142 bytes) with every fix described here: compressed and uncompressed H5, the heap settled outside each timed read, Seurat loaded only for embedded export, 58 bytes per nonzero for the embedded estimate decided by the exact plan, and no 1M tier. All 270 builds and 540 access processes succeeded, including Human 500k embedded (peak R heap 96.9 GiB). The paper figures are drawn from this run alone; the full-size figure uses its two full-size endpoints. Settling the heap removed the 1k BPCells spikes of earlier runs, where one full R garbage collection landed inside the timed read (Human block read median 0.36 s to 0.0055 s, Mouse subset-row read 0.34 s to 0.009 s). Memory after load fell by 75-135 MB per backend at 1k once Seurat was no longer loaded; CerebroNexus's own dependencies already account for the rest of the earlier estimate. The remaining read ranges wider than about 0.25 s are embedded reads at 100k cells and above, whose slicing allocates enough to trigger collections of its own; these show in the min-max whiskers, not the medians. Earlier runs are kept as recorded in their own run directories.

**Existing results are historical.** The September 22 runs predate materialized reads, and the October measurements at `b64f8df3` contain four negative subset-block durations (one Full, three Scale). Their timing evidence remains provisional. H5 files written before the compression fix used BPCells' default `gzip_level = 0`; the H5 builder now uses the package exporter's level (`HDF5Array::getHDF5DumpCompressionLevel()`, 6 by default), so earlier H5 size, build and read figures are not comparable with fresh runs. Fresh runs must satisfy both `materialized-v2` and `monotonic-v1`. Retain the original CSVs; do not clip, take absolute values or delete invalid observations. `resume` cannot certify a legacy run without raw monotonic observations. Repeat Scale on the same 12900K WSL host; local Mac smoke checks do not replace those performance measurements.

## Keep it simple

Use `run.sh scale` (or the default `run.sh`) for normal work; it runs the Scale study and publishes it automatically.
All implementation lives in `scripts/`:

- `run.sh`: environment, cached downloads, independent measurement processes and finalization.
- `benchmark.R`: source/configuration settings at the top, shared measurement and publication functions below.
- `benchmark_cli.R`: command-line stages.

Reuse verified downloads and the fixed query plan. Keep Viewer checks separate,
and avoid adding overlapping launchers or configuration layers.
