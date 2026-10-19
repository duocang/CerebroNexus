# Viewer benchmarks

Measure page speed or check that a CRB opens and responds correctly. Run from the repository root in Linux or WSL. You need the project R environment, Chrome/Chromium and the renderer required by the data (WebGPU by default for the million-cell page benchmark).

```bash
nix-shell default.nix -A shell
```

## Choose a command

```bash
# Quick page comparison: pre-PR0, fixed PR5 and committed HEAD.
bash tests/benchmarks/viewer/run.sh pages --artifact /data/brain-pages.crb

# Five rounds for a more complete comparison.
bash tests/benchmarks/viewer/run.sh pages --artifact /data/brain-pages.crb --profile publication

# Compare PR5 and HEAD, each using its own Viewer Pack.
bash tests/benchmarks/viewer/run.sh release --artifact /data/brain-pages.crb --rounds 3

# Check loading, rendering, selection, zoom and gene switching.
bash tests/benchmarks/viewer/run.sh smoke /data/brain.crb
```

Page comparisons use the million-cell fixture and compare committed revisions in separate clean worktrees. Uncommitted changes are not measured. Use `--baseline`, `--reference` and `--candidate` to select other revisions. For example:

The default pre-PR0 and PR5 revisions are fixed historical reference points, not the current `master`. Select revisions explicitly when comparing a new release.

```bash
bash tests/benchmarks/viewer/run.sh pages --artifact /data/brain-pages.crb --candidate master --pages overview,trajectory
bash tests/benchmarks/viewer/run.sh pages --help
```

## Prepare the page fixture

The official mouse-brain CRB lacks repertoire, HLA and trajectory data. Create a test fixture beside it so both files can use the same expression sidecar:

```bash
Rscript tests/benchmarks/viewer/scripts/prepare_viewer_1m_pages.R . /data/brain.crb /data/brain-pages.crb
```

The added data are only for testing page behavior, not biological analysis. Use [smoke checks](scripts/smoke/README.md) for other CRB files.

## Existing artifact diagnostics

The general H5 / BPCells / embedded comparison lives in [CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code) (`topics/expression-backends`). Two older diagnostics remain useful for specific questions: `scripts/benchmark_ren_expression_backends.R` compares reads from two already prepared Ren CRBs, without rebuilding them; `scripts/benchmark_bpcells_storage_order.R` compares column and row storage on the matching million-cell fixture. For example, from the same Nix shell:

```bash
Rscript tests/benchmarks/viewer/scripts/benchmark_ren_expression_backends.R /data/ren-bpcells.crb /data/ren-h5.crb /tmp/ren-backends 3
Rscript tests/benchmarks/viewer/scripts/benchmark_bpcells_storage_order.R /data/brain-col.crb /data/brain-row.crb /tmp/storage-order.tsv 3
```

## Narrow diagnostics

These scripts answer one question each. They are run by hand when that part of the Viewer changes; none of them runs in CI.

- **Server hot paths and Linked bundle, two revisions.** `scripts/run_viewer_1m_benchmark.sh BEFORE AFTER [OUTPUT_DIR]` checks out both revisions in temporary worktrees, prepares the million-cell CRB with `prepare_viewer_1m_data.R`, and runs `viewer_1m_hot_paths.R` (filtering, cell indexing and expression reads, with equality checks) and `viewer_1m_bundle.R` (Linked bundle build, JSON encoding and size). It produced `inst/extdata/benchmarks/million_cell_backend_hot_paths.tsv` and `million_cell_backend_bundle.tsv`. `REPEATS` sets the repetitions; `CEREBRO_LARGE_CACHE` reuses prepared data.
- **Startup.** `scripts/benchmark_million_cell_startup.R LABEL=CRB [LABEL=CRB ...]` starts the app for each CRB, opens it in headless Chrome and times three steps: the server is ready, the page has loaded, and Data info reports all one million cells. `CEREBRO_STARTUP_REPEATS` sets the repetitions (default 3). It produced the `million_cell_startup_*.csv` files in the [saved results](https://github.com/duocang/CerebroNexus_paper_code/tree/main/topics/viewer-performance/data).
- **Renderer.** `scripts/benchmark_million_cell_renderer.R [points] [repeats] [output.csv]` draws synthetic points (one million by default) in the small app in `scripts/million_cell_renderer_app/` with headless Chrome and times renderer setup, building the colour buffers and drawing, on the real renderer code in `inst/`. Set `CEREBRO_RENDERER_BACKEND=webgl2` to force the WebGL2 fallback instead of WebGPU.

```bash
REPEATS=3 bash tests/benchmarks/viewer/scripts/run_viewer_1m_benchmark.sh 27303f21 HEAD
Rscript tests/benchmarks/viewer/scripts/benchmark_million_cell_startup.R before=/data/old.crb after=/data/new.crb
Rscript tests/benchmarks/viewer/scripts/benchmark_million_cell_renderer.R 1000000 15 /tmp/renderer.csv
```

## Results

Page comparisons write to `CerebroNexus-benchmarks/` beside the repository. Start with `page-summary.md`; `raw.tsv` contains individual observations and `run-config.tsv` records the revisions. Failed runs keep their measurements and return a nonzero exit code.

Page timing starts after the dataset has loaded and ends when the correct primary result is drawn. First visits must be under 2 seconds, or 3 seconds for HLA and Trajectory; repeat visits must be under 500 ms. `settled_ms` is an additional diagnostic, not the page-speed score. Run `--mode memory` separately: memory sampling changes timing, so those times are excluded from performance comparisons.

[Saved Viewer results](https://github.com/duocang/CerebroNexus_paper_code/tree/main/topics/viewer-performance/data) are historical. Check each report's revision and timing definition before comparing numbers. The detailed event checks are implemented in `scripts/viewer_1m_page_protocol.R`; the other scripts are listed above.

The script that draws the older three-launch, 11-page figure is in [CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code) (`topics/viewer-performance`). Historical reports and backend recommendations remain evidence from their recorded revisions, not conclusions from the current benchmark.

## Load mode

The browser benchmarks (`benchmark_viewer_1m_pages.R`, `benchmark_million_cell_viewer.R`, `benchmark_ren_linked_transport.R`) share `scripts/lib/viewer_launch.R`. By default (`VIEWER_BENCH_LOAD=installed`) each candidate is installed into a temporary library with `R CMD INSTALL` and launched with `library(CerebroNexus)`, as deployments run it. `VIEWER_BENCH_LOAD=load_all` uses `devtools::load_all()` instead, which also attaches every DESCRIPTION Imports package; the much larger R heap makes each full garbage collection take seconds and shifts pauses onto whichever page step allocates next, so use it only to profile an uninstalled source tree. The page benchmark manifest records the mode as `viewer_load_mode`.
