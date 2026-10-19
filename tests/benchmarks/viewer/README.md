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

For the general H5 / BPCells / embedded comparison, use [expression-backends](../expression-backends/README.md). Two older diagnostics remain useful for specific questions: `scripts/benchmark_ren_expression_backends.R` compares reads from two already prepared Ren CRBs, without rebuilding them; `scripts/benchmark_bpcells_storage_order.R` compares column and row storage on the matching million-cell fixture. For example, from the same Nix shell:

```bash
Rscript tests/benchmarks/viewer/scripts/benchmark_ren_expression_backends.R /data/ren-bpcells.crb /data/ren-h5.crb /tmp/ren-backends 3
Rscript tests/benchmarks/viewer/scripts/benchmark_bpcells_storage_order.R /data/brain-col.crb /data/brain-row.crb /tmp/storage-order.tsv 3
```

## Results

Page comparisons write to `CerebroNexus-benchmarks/` beside the repository. Start with `page-summary.md`; `raw.tsv` contains individual observations and `run-config.tsv` records the revisions. Failed runs keep their measurements and return a nonzero exit code.

Page timing starts after the dataset has loaded and ends when the correct primary result is drawn. First visits must be under 2 seconds, or 3 seconds for HLA and Trajectory; repeat visits must be under 500 ms. `settled_ms` is an additional diagnostic, not the page-speed score. Run `--mode memory` separately: memory sampling changes timing, so those times are excluded from performance comparisons.

[Saved Viewer results](../results/viewer/) are historical. Check each report's revision and timing definition before comparing numbers. The detailed event checks are implemented in `scripts/viewer_1m_page_protocol.R`; other files in `scripts/` prepare fixtures or investigate specific bottlenecks.

`scripts/plot_latest_viewer_performance.R` reproduces the older three-launch, 11-page figure; it is not the general page-report command. It requires standalone `panel_alignment.R` and `audit_panel_alignment.py` tools, Python and the R packages listed in the script. Supply their directory explicitly and choose a new figure directory:

```bash
FIGURE_QA_TOOLS=/path/to/figure-qa-tools Rscript tests/benchmarks/viewer/scripts/plot_latest_viewer_performance.R /data/historical-latest.tsv /tmp/viewer-figure-new
```

Its automatic checks do not replace visual review. Historical reports and backend recommendations remain evidence from their recorded revisions, not conclusions from the current benchmark.

## Load mode

The browser benchmarks (`benchmark_viewer_1m_pages.R`, `benchmark_million_cell_viewer.R`, `benchmark_ren_linked_transport.R`) share `scripts/lib/viewer_launch.R`. By default (`VIEWER_BENCH_LOAD=installed`) each candidate is installed into a temporary library with `R CMD INSTALL` and launched with `library(CerebroNexus)`, as deployments run it. `VIEWER_BENCH_LOAD=load_all` uses `devtools::load_all()` instead, which also attaches every DESCRIPTION Imports package; the much larger R heap makes each full garbage collection take seconds and shifts pauses onto whichever page step allocates next, so use it only to profile an uninstalled source tree. The page benchmark manifest records the mode as `viewer_load_mode`.
