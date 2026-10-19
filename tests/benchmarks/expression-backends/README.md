# H5 / BPCells / embedded benchmark

Compare expression-storage backends by build time, file size, memory, startup and gene-read speed. Use the commands below from the repository root on Linux or WSL, with `nix-shell` installed and a clean Git checkout. The full run needs a machine with substantial memory and disk space; the runner checks capacity before downloading the data.

## Start and monitor

```bash
# Small correctness run: 1,000 cells per source, all three backends.
bash tests/benchmarks/expression-backends/run.sh quick

# Independent size comparison, including both full-dataset endpoints.
bash tests/benchmarks/expression-backends/run.sh scale
# No argument (or run) also runs Scale only. Scale already measures both full-size endpoints;
# a separate Full study (`run.sh full`) repeats exactly those cells and is optional.

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
| Size comparison | The same sources at 1k, 10k, 50k, 100k, 300k, 500k, then each exact full-dataset size (no 1M tier) | Both H5 variants and BPCells at every size; embedded attempted through 500k |

The complete matrices exceed the sparse-index limit of embedded storage, so full-dataset embedded results are unavailable. They are not replaced with smaller samples.

`h5` is the 10x H5 sidecar at the package exporter's compression level (`HDF5Array::getHDF5DumpCompressionLevel()`, 6 by default), i.e. what CerebroNexus ships. `h5_uncompressed` is the same file written at gzip level 0, a storage/read trade-off reference that the package does not produce by default; both are opened through the package's `h5` backend. Both are built in the same run and interleaved with BPCells (and embedded in Scale), so they share host conditions. The manifest records both levels (`h5_gzip_level`, `h5_uncompressed_gzip_level`). Full/scale runs without these keys predate the fix: their `h5` rows were written at gzip level 0 and are uncompressed results; `paper/figure-drafts/expression-backends/rebuild-figures.R` labels them accordingly. Uncompressed H5 needs about 12 bytes per nonzero on disk, so the preflight requires the cached source plus 18 bytes per nonzero of free scratch space (about 125 GB for the full human matrix) within 80% of free disk.

In full and scale runs, each source, size and backend gets five independent builds. Each successful build is opened in two fresh R processes. Runs use the same cells, gene names and fixed 12-gene queries, including full-cell reads and non-contiguous subsets of up to 100,000 cells. Reading a lazy matrix fully into memory is included in the timer; value checks happen afterward. Processes run sequentially with one thread by default.

Elapsed durations use `CLOCK_MONOTONIC`; system-clock timestamps are retained only as provenance. A small native clock reader is compiled once per R process using `R CMD SHLIB` before the first timed operation (the existing Nix toolchain supplies the compiler). Clock initialization fails closed; there is no wall-clock fallback. `21_access_timings.csv` retains every startup, first read, warmed read, block and subset read, including monotonic endpoints, wall-clock endpoints and clock resolution. CSV writes and value checks occur outside timed operations. A full R garbage collection runs outside the timers before the first read, before the warmed loop and before each block and subset read (`bench_settle_heap`). Without it, R's periodic full collection (0.3-0.7 s, set by the whole heap rather than the read) lands in whichever read reaches that point of the process's allocation history; which read that is changes with tier, source and even the size of the harness scripts. Startup is not settled and still includes the collections triggered while loading the shell. Benchmark processes do not load Seurat unless they build an embedded export: earlier harness versions attached it in every process, which added about 5 s per process and about 350 MB to every backend's memory after load in results measured before that change. Publication requires finite nonnegative durations, complete raw observations and summaries matching those observations.

Incorrect values, crashes or unexpected build failures stop publication. An explicit embedded memory or sparse-index limit is recorded as a resource limit. A new result becomes `CURRENT` only after all required checks pass.

Scale snapshots Linux `MemAvailable` at launch, capped by the remaining memory allowance of every cgroup-v2 ancestor. It uses 99% of that available RAM, not 99% of installed RAM. The runner enforces this byte limit with a systemd user scope (`MemoryMax`), with task swap disabled; failure to create the scope stops the run. The snapshot, byte limit and policy are recorded in the manifest. This is a fixed launch-time limit, not a moving target during measurements. A working systemd user manager with memory-controller delegation is required.

Scale checks embedded separately because it loads the matrix into memory. Its estimate is 1 GiB plus 58 bytes per nonzero (`BENCH_EMBEDDED_BYTES_PER_NNZ`; measured peaks were 52.6-57 bytes per nonzero from 50k cells up, so at least 10% headroom; runs before this change used 64), checked against that budget and the R vector limit. The launch preflight uses the source-wide density; the frozen query plan then supplies the tier's exact nonzeros, which decide memory and the sparse-index limit (the density can overstate a prefix: Human 500k is 112 GiB by density but 108 GiB exact). A preflight failure for any other reason, such as disk, still skips. Exceeding the budget or 32-bit sparse-index limit records `SKIPPED(resource)` while H5 and BPCells continue; an embedded build that the kernel kills at the cap is also recorded as `SKIPPED(resource)`, but only when the scope's cgroup `oom_kill` count rose during that build (any other death remains a crash that blocks publication); increasing the budget does not guarantee a formerly skipped tier will pass the estimator. No automatic cell-count reduction occurs. Historical runs retain their original policies (including the old 70%-of-total-RAM preflight); they are not retroactively labelled as 99% runs.

Scale now measures its own full endpoints and does not read Full results. It has no 1M tier: 1M sits next to the 1.31M and 1.49M endpoints on the log axis, added little and was the costliest tier. The full-size comparison figure is drawn from the Scale endpoints (`paper/figure-drafts/expression-backends/rebuild-figures.R`), which are the same cells, builds and reads a Full study measures, so the optional `full` command is not needed; default `run` does not measure the endpoints twice.

## Reruns and saved files

Every run gets a timestamp-and-commit directory under `results/expression-backends/<quick|full|scale>/runs/`. Existing runs are never overwritten by a later run. The CSV measurements, source checksums, environment, report, stage logs and figures stay together. After a full or scale run passes validation and is archived, publication replaces that study's `figures/` with copies of the archived images and updates `CURRENT`. The two studies advance independently: if full succeeds and scale fails, full advances while scale keeps its previous result.

Publication stages the new images before switching and rolls back both the images and `CURRENT` on an error or ordinary interrupt. If the publisher is killed, the runner restores the old pair; if the entire runner or host stops, the next publication recovers it before retrying. Successful publication removes its temporary rollback directory. Run only one publisher per study at a time.

Verified source downloads stay in the external cache. Failed runs keep their scratch directory by default; `resume` retries validation, reporting and publication only; if the run was already archived before activation failed, it skips regeneration and only re-verifies and activates the archive. It does not resume measurements or start the next study. Generated CRB/backend files are temporary and normally removed after measurement; start with `BENCH_KEEP=1 bash tests/benchmarks/expression-backends/run.sh` if you need to retain them too. Starting another run archives the previous launcher log under `runner/logs/` in the cache.

### Supplements

To measure only some scheduled cells, set `BENCH_SCHEDULE_FILTER` to `key=value` terms joined by `;` (keys `source`, `n_cells`, `backend`; values may be comma-separated):

```bash
BENCH_SCHEDULE_FILTER='source=human_pfc_hbcc;n_cells=500000;backend=embedded' \
  bash tests/benchmarks/expression-backends/run.sh scale
```

The kept cells retain their repeats, order positions and access repeats, and the run is validated, reported and drawn like a full study. It is published under `results/expression-backends/supplements/<study>/` with a `-supplement` run id and the filter in its manifest (`schedule_filter`), so it never replaces the study's `CURRENT`; `resume` follows the same rule. Run it on the host of the study it supplements, after that study has finished. Combining it with the study is a separate, explicit assembly step that keeps both run ids.

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

After a run, the latest figures are in `results/expression-backends/full/figures/` and
`results/expression-backends/scale/figures/`. These directories
contain image exports only: no run-ID subdirectories, QA files or README files.
Their images are byte-for-byte copies of the run selected by `CURRENT`. Plotting
does not rerun measurements. Export dimensions prioritize readability; check text
at the intended final size before reducing them.

## Read results

Results are stored in `results/expression-backends/`, with separate `quick/`, `full/` and `scale/` directories. Quick runs do not replace full or scale results. To open the selected full-dataset report:

```bash
RESULTS=tests/benchmarks/results/expression-backends/full
RUN="$RESULTS/runs/$(cat "$RESULTS/CURRENT")"
cat "$RUN/summary.md"
```

Use `quick` or `scale` instead of `full` for those reports. `10_export.csv` contains build measurements; `20_access.csv` contains startup and query measurements. `run_manifest.csv` and `source_manifest.csv` identify the code, host, packages and input files. Keep files from each run together.

Numbers are medians with observed ranges, not confidence intervals. The two access processes share one built artifact. The first read may benefit from the operating-system cache. Storage and memory fields ending in `_mb` use MiB. Embedded `export_secs` includes the full exporter, while H5/BPCells measure the backend writer; do not compare that column alone as equal work.

### Current Scale: one complete four-backend run (2026-10-19)

`CURRENT` Scale is `20261018T101740Z-1ed063d363a0-scale`, measured on the 128 GB host (Intel i7-14700, 128 GB, Ubuntu, 99% cap 120,950,935,142 bytes) with every fix described here: compressed and uncompressed H5, the heap settled outside each timed read, Seurat loaded only for embedded export, 58 bytes per nonzero for the embedded estimate decided by the exact plan, and no 1M tier. All 270 builds and 540 access processes succeeded, including Human 500k embedded (peak R heap 96.9 GiB), so no supplement or assembly is involved. The paper figures are drawn from this run alone; the Full figure uses its two full-size endpoints, and no separate Full study was run with this code. Settling the heap removed the 1k BPCells spikes described below (Human block read median 0.36 s to 0.0055 s, Mouse subset-row read 0.34 s to 0.009 s). Memory after load fell by 75-135 MB per backend at 1k once Seurat was no longer loaded; CerebroNexus's own dependencies already account for the rest of the earlier estimate. The remaining read ranges wider than about 0.25 s are embedded reads at 100k cells and above, whose slicing allocates enough to trigger collections of its own; these show in the min-max whiskers, not the medians. The runs below are kept as recorded and superseded.

### Earlier four-backend Scale on the 128 GB host (2026-10-18, superseded)

This collection was `CURRENT` before the run above: `20261018T091308Z-58e778efc30a-scale-assembled`, built with `benchmark_cli.R assemble` from the complete four-backend study `20261016T224351Z-63d5337c3647-scale` and the same-host supplement `20261018T072255Z-30a6295c85f5-scale-supplement` (Human 500k embedded, five builds). The supplement replaces only the study's five Human 500k embedded resource skips, which remain in `resource_skips.csv`; all 300 builds and 600 access processes are real measurements. Full is `20261017T174502Z-63d5337c3647-full` from the same host. Run ids containing `63d5337c` name a commit that was later rewritten on this branch to remove commit-message trailers; the identical code is `c9677606`. The supplement was measured at `30a6295c` and finalized at `ad79130b`, which only changed validation rules. Human 1k BPCells 12-gene block reads take about 0.3-0.5 s in these runs (also reproduced on the 12900K with the same code) against about 0.003 s in the October 16 runs, and Mouse 1k BPCells subset-row reads show the same excess. Neither is a BPCells read cost: it is one full (level 2) R garbage collection that lands inside that read, measured with `gcinfo()` and `gc.time()` to account for the whole excess. The harness grew between `81d11e8a` and these runs, which moved that collection into the measured reads; padding the October 16 harness with 300 unused lines moves it into the first read instead. Other one-off excesses of about 0.35-0.5 s in these Scale results, such as 100k first reads and some 300k block reads, are probably the same collection; only a rerun with `bench_settle_heap` separates them. Results measured before that change are left as recorded.

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
