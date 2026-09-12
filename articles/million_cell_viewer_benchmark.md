# Benchmark the million-cell Viewer

## Purpose

This guide reproduces the million-cell workload used to profile the
CerebroNexus Viewer. It covers four separate operations:

1.  download the official 10x Genomics E18 mouse-brain matrix;
2.  build a reproducible one-million-cell Seurat object and
    BPCells-backed CRB;
3.  compare the pre-GPU Viewer with the current WebGPU Viewer;
4.  launch the same data interactively through the bundled demo app.

The expensive chunks are deliberately not evaluated while building the
vignette. A package build must never download a 3.93 GiB source matrix
or start a browser benchmark.

## Requirements and disk use

Run the commands from the root of a CerebroNexus checkout. Data
preparation requires `BPCells`, `Seurat`, `SeuratObject`, and `rhdf5`.
The browser benchmark also requires `shinytest2` and a Chromium build
with WebGPU support.

The source used here contains 27,998 genes and 1,306,127 cells. The
preparation script selects exactly 1,000,000 cells at evenly spaced
indexes, replaces Ensembl identifiers with unique gene symbols, and
computes a sketch-based UMAP with seed 123.

Allow at least 16 GiB of free disk space for the source file, the Seurat
and CRB BPCells directories, and transactional staging during export.
The default cache is below `tools::R_user_dir("CerebroNexus", "cache")`;
pass `cache_dir` to put it on a larger volume.

## Download and prepare the data

The preparation script downloads the official 10x HDF5 matrix only when
it is absent, then reuses every complete intermediate on later runs.

``` r
devtools::load_all(quiet = TRUE)
source("tests/bench/prepare_viewer_1m_data.R")

crb <- prepareViewer1mBenchmarkData()
crb
```

To choose another cache root:

``` r
crb <- prepareViewer1mBenchmarkData(
  cache_dir = "/Volumes/large-data/CerebroNexus-benchmark"
)
```

The original download is:

``` text
https://cf.10xgenomics.com/samples/cell-exp/1.3.0/1M_neurons/1M_neurons_filtered_gene_bc_matrices_h5.h5
```

The resulting cache contains:

``` text
1m/
├── source/1M_neurons_filtered_gene_bc_matrices_h5.h5
├── seurat/mouse_brain_1m.rds
├── seurat/mouse_brain_1m.bpcells/
├── cerebro/cerebro_mouse_brain_1m.crb
└── cerebro/cerebro_mouse_brain_1m.bpcells/
```

The `.crb` and adjacent `.bpcells/` directory are one portable dataset.
Keep them together when moving or deploying it.

## Run the regression tests

The focused regression set covers expression export and integrity, the
compact BPCells CRB contract, the million-cell payload, and renderer
integration.

``` bash
Rscript -e 'devtools::test(
  filter = paste0(
    "million-cell-renderer|million-cell-viewer|",
    "seurat-v5-split-layers|export-data-integrity|",
    "exportFromSeurat|r-functions"
  ),
  stop_on_failure = TRUE
)'
```

The real BPCells bundle relocation check can be run separately:

``` bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file(
  "tests/testthat/test-createShinyApp-sibling.R",
  desc = "a bundled real BPCells backend attaches with exact data",
  stop_on_failure = TRUE
)'
```

On 12 September 2026 these commands produced 308 passes, zero failures,
zero warnings, and one conditional HDF5 skip, followed by 7/7 passes for
the real BPCells bundle check.

## Compare the Viewer before and after

### Create the baseline checkout

Commit `5f509edd` contains the optional one-million-cell demo
immediately before the GPU renderer work. Create a detached worktree so
both implementations can be measured against the same CRB and browser
installation:

``` bash
git worktree add --detach ../CerebroNexus-before-1m 5f509edd
```

Use an old-format BPCells CRB for this Viewer comparison. The current
Viewer loads that format too, so both candidates receive exactly the
same serialized input. This keeps the CRB-format reduction separate from
browser rendering.

### Run the full Viewer benchmark

The harness alternates candidate order on each round. It checks that
Data Info reports 1,000,000 cells, waits for Overview and Linked Views,
measures a single-gene Gene expression plot and a three-gene RGB plot,
requires the current candidate to report `webgpu`, performs ten
alternating zoom operations, and rejects GPU errors or context loss.
Renderer-ready events must report the expected number of points, so a
fast but incomplete plot fails the run.

Replace `CRB` below with the path returned by the preparation step.

``` bash
CRB=/path/to/cerebro_mouse_brain_1m.crb

Rscript tests/bench/benchmark_million_cell_viewer.R \
  ../CerebroNexus-before-1m . "$CRB" 3 100

Rscript tests/bench/benchmark_million_cell_viewer.R \
  ../CerebroNexus-before-1m . "$CRB" 3 10
```

The 100% run draws all one million cells. The 10% run draws 100,000
cells and matches the bundled demo’s initial setting. Timings below are
medians of three fresh app processes per candidate on the same host.

| Initial display | Metric | Before (`canvas2d`) | After (`webgpu`) | Change |
|----|----|---:|---:|---:|
| 100% | Data ready | 17.425 s | 17.106 s | 1.8% faster |
| 100% | First Overview | 63.572 s | 12.354 s | 80.6% faster |
| 100% | Data + Overview | 80.997 s | 29.460 s | 63.6% faster |
| 100% | Zoom median | 361.2 ms | 8.7 ms | 41.5x faster |
| 100% | Zoom p95 | 421.2 ms | 11.5 ms | 36.6x faster |
| 10% | Data ready | 16.247 s | 16.889 s | effectively tied |
| 10% | First Overview | 50.027 s | 2.424 s | 95.2% faster |
| 10% | Data + Overview | 66.274 s | 19.313 s | 70.9% faster |
| 10% | Zoom median | 122.9 ms | 3.5 ms | 35.1x faster |
| 10% | Zoom p95 | 134.5 ms | 5.2 ms | 25.9x faster |

The first row group shows why storage and rendering are reported
separately: loading and attaching the same old CRB barely changed, while
the first plot and subsequent camera operations changed substantially.

### Verify the three-second navigation target

The later hot-path pass starts from commit `5a915962`, where WebGPU was
already enabled. This comparison therefore measures transport,
first-frame scheduling, group-label calculation, and large-dataset
bundle preparation without counting the earlier Canvas-to-WebGPU
replacement as a second time.

``` bash
git worktree add --detach ../CerebroNexus-before-wire 5a915962

Rscript tests/bench/benchmark_million_cell_viewer.R \
  ../CerebroNexus-before-wire . "$CRB" 3 100

Rscript tests/bench/benchmark_million_cell_viewer.R \
  ../CerebroNexus-before-wire . "$CRB" 3 10
```

Both candidates used WebGPU. Results are medians of three alternating
fresh processes on 12 September 2026:

| Display | Metric | Before | After | Change |
|----|----|---:|---:|---:|
| 100% (1,000,000 points) | Data ready | 14.798 s | 14.185 s | 4.1% faster |
| 100% (1,000,000 points) | First Overview | 11.819 s | 1.789 s | 84.9% faster |
| 100% (1,000,000 points) | Linked Views ready | 4.519 s | 2.608 s | 42.3% faster |
| 100% (1,000,000 points) | Gene expression ready | 6.830 s | 2.176 s | 68.1% faster |
| 100% (1,000,000 points) | RGB ready | 11.934 s | 2.096 s | 82.4% faster |
| 100% (1,000,000 points) | Zoom median | 7.9 ms | 8.2 ms | effectively tied |
| 100% (1,000,000 points) | Zoom p95 | 16.4 ms | 15.7 ms | effectively tied |
| 10% (100,000 points) | Data ready | 13.693 s | 14.081 s | 2.8% slower |
| 10% (100,000 points) | First Overview | 2.582 s | 1.526 s | 40.9% faster |
| 10% (100,000 points) | Linked Views ready | 4.942 s | 1.187 s | 76.0% faster |
| 10% (100,000 points) | Gene expression ready | 1.018 s | 0.869 s | 14.6% faster |
| 10% (100,000 points) | RGB ready | 1.795 s | 1.470 s | 18.1% faster |
| 10% (100,000 points) | Zoom median | 4.6 ms | 3.8 ms | 17.4% faster |
| 10% (100,000 points) | Zoom p95 | 6.6 ms | 5.3 ms | 19.7% faster |

All four measured million-point page paths now meet the three-second
navigation target. Integer vectors use the narrowest lossless wire type,
reducing this dataset’s initial Linked Views bundle from 28,001,836 to
15,001,832 bytes (46.4% smaller). Hover columns remain in their grouped
representation and are expanded only for the point actually under the
pointer. Non-render-critical hover data is sent after the first-frame
work rather than competing with it.

`Data ready` remains about 14 seconds because it includes opening the
CRB, attaching the BPCells sidecar, and starting the Shiny session. It
is reported separately and is not included in the page-navigation
target. Requiring a cold launch to finish within three seconds would be
a separate storage and startup project; it is not a browser-rendering
optimization.

### Measure the renderer alone

The synthetic harness isolates buffer construction, upload, the first
frame, zoom, and RGB recolouring from Shiny and CRB startup:

``` bash
Rscript tests/bench/benchmark_million_cell_renderer.R \
  1000000 15 renderer.csv renderer.png
```

The same run used Apple Metal 3 through WebGPU and produced:

| Metric                   |    Result |
|--------------------------|----------:|
| GPU buffer size          | 15.26 MiB |
| Renderer initialization  |    5.9 ms |
| Categorical buffer build |    7.6 ms |
| Categorical upload       |   10.7 ms |
| First frame              |   30.6 ms |
| Pan/zoom median          |    8.6 ms |
| Pan/zoom p95             |   12.2 ms |
| RGB buffer build         |    9.0 ms |
| RGB upload               |    7.8 ms |
| RGB frame median         |    9.5 ms |
| RGB frame p95            |   13.0 ms |

## Compare the CRB representation

The compact BPCells CRB no longer serializes the live matrix handle or
repeated cell names. At runtime the Viewer reopens the adjacent BPCells
directory, validates its cell-name checksum, and restores the omitted
metadata and projection row names.

Measure an old and newly exported CRB after warming both files once.
Alternating the order limits systematic page-cache bias:

``` r
files <- c(
  before = "/path/to/cerebro_mouse_brain_1m_before.crb",
  after = "/path/to/cerebro_mouse_brain_1m_after.crb"
)
invisible(lapply(files, readRDS))

rows <- list()
for (round in seq_len(5L)) {
  order <- if (round %% 2L) names(files) else rev(names(files))
  for (candidate in order) {
    rows[[length(rows) + 1L]] <- data.frame(
      candidate = candidate,
      round = round,
      read_rds_ms = unname(
        system.time(readRDS(files[[candidate]]))["elapsed"] * 1000
      )
    )
  }
}

raw <- do.call(rbind, rows)
aggregate(raw["read_rds_ms"], raw["candidate"], median)
file.info(files)$size / 1024^2
```

| Metric | Before | After | Change |
|----|---:|---:|---:|
| CRB size | 26.83 MiB | 13.92 MiB | 48.1% smaller |
| Warm [`readRDS()`](https://rdrr.io/r/base/readRDS.html) median | 1,535 ms | 113 ms | 13.6x faster |

These numbers describe only the serialized `.crb`. The 3.5 GiB BPCells
expression sidecar is unchanged and remains the authoritative expression
and cell-name store.

## Launch the interactive one-million-cell demo

Point `CEREBRO_1M_DEMO_CRB` at the prepared CRB and run the bundled app:

``` bash
CEREBRO_1M_DEMO_CRB=/path/to/cerebro_mouse_brain_1m.crb \
  Rscript -e 'shiny::runApp("inst", host = "127.0.0.1", port = 7451)'
```

Open `http://127.0.0.1:7451`, then select **10x E18 mouse brain (1M)**.
The dataset starts at 10% sampling with point size 1 and opacity 0.5.
Increasing the sampling control to 100% renders all one million points.
When the environment variable is unset, the bundled application behaves
exactly as before and does not show this dataset.

For a direct launch without the bundled dataset switcher:

``` r
devtools::load_all(quiet = TRUE)
launchCerebro(
  mode = "closed",
  crb_file_to_load = c("10x E18 mouse brain (1M)" = crb),
  percentage_cells_to_show = 10,
  point_size = 1,
  point_opacity = 0.5,
  projections_show_hover_info = TRUE
)
```

## Interpretation limits

- The Viewer values are process medians, not confidence intervals.
- [`readRDS()`](https://rdrr.io/r/base/readRDS.html) is a warm
  operating-system-cache measurement, not cold storage.
- The full Viewer includes R loading, Shiny transport, JavaScript
  decoding, and drawing; the synthetic renderer table measures only the
  browser renderer.
- WebGPU results depend on the browser, driver, and adapter. The
  recorded run used Headless Chrome 152 and Apple Metal 3.
- The one-million-cell subset is suitable for engineering measurements,
  not a new biological analysis of the source experiment.

Remove the detached baseline checkout when it is no longer needed:

``` bash
git worktree remove ../CerebroNexus-before-1m
```
