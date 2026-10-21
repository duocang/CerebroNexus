# Large demo data sets

These scripts download and prepare the two large demo data sets that `run-demo.R` opens in the Viewer: the one-million-cell mouse brain E18 data set and the Ren et al. COVID-19 atlas (1.46M cells with paired TCR/BCR data). They are not shipped in `inst/extdata/examples/`, `data-raw/` is excluded from package builds, and they are not part of the test suite. `tests/testthat/test-run-demo.R`, `test-ren-demo.R`, `test-million-cell-viewer.R` and `test-demo-h5-transpose.R` test them with small synthetic data.

## How to run

You normally do not run these scripts yourself. From the repository root, run the launcher:

```bash
Rscript run-demo.R                  # both data sets, then the Viewer in the browser
Rscript run-demo.R --ren-only       # only the Ren atlas
Rscript run-demo.R --prepare-only   # download and convert, do not start the Viewer
Rscript run-demo.R --no-browser     # start the Viewer without opening a browser
```

The launcher loads this checkout with `pkgload`, sources the scripts below, prepares each data set (a complete cache is reused, so only the first run downloads and converts), builds the Viewer Pack, and starts the app on `http://127.0.0.1:7451`. It needs `pkgload`, `rhdf5`, `HDF5Array`, `DelayedArray`, `readxl` and `qs2`, plus the packages the Viewer itself uses.

## What `Rscript run-demo.R --ren-only` does the first time

There is no prompt: if the cache has no finished Ren data, the launcher starts downloading and converting right away, then opens the Viewer. The options only change which data sets are prepared (`--ren-only` skips the one-million-cell mouse data set; the small example data sets of the app are always there). The steps, in order:

1. **Check the cache.** If a finished Ren data set is already there, skip to step 7.
2. **Download five files** into `large-examples/ren/source/`: the 14 GB `.h5ad` archive, the cell annotation table, the TCR table, the BCR table and the clinical sample table. Each file is checked against its expected size and SHA256; a file that is already complete is not downloaded again. The log says `Preparing Ren source: <file>`.
3. **Unpack** the archive into a 61 GB `.h5ad` file.
4. **Convert the counts to an H5 file** (`Converting the Ren count matrix to an H5 sidecar...`). This is the long step. The log first counts up the cells it has read (`read cells 20,000 of 1,462,702`), then the gene blocks it has written (`wrote gene block 1 of 24`). Temporary files are used on the way and removed at the end. The result is a 28 GB `.h5` file.
5. **Build the data set object**: read the cell annotations and clinical data, the published UMAP, and the 503,432 paired TCR/BCR records, and save the `.crb` file and the immune repertoire file next to the `.h5` file. This takes a few minutes.
6. **Publish.** Everything is built in a temporary folder first and only moved to its final name when it is complete, so a run that is stopped halfway leaves no half-finished data set behind. Starting again continues from the downloaded files, but redoes the conversion.
7. **Build the Viewer Pack**, the extra files that make the first page of the Viewer open quickly.
8. **Start the Viewer** at `http://127.0.0.1:7451`. The first data set in the list is still the small PBMC example; choose "Ren et al. COVID-19 atlas" in the data set menu.

The next run finds the finished data set in step 1 and starts the Viewer without downloading or converting anything. You need about 105 GB of free disk for the Ren data set (the archive and the `.h5ad` together are 75 GB, the converted files 28 GB). The one-million-cell data set needs more on top of that.

To prepare a data set from an R session without starting the Viewer, source the helper and the script, then call its entry function:

```r
pkgload::load_all(".")
source("data-raw/large-demo/h5_transposed_write.R")
source("data-raw/large-demo/prepare_viewer_ren_data.R")
crb <- prepareViewerRenDemoData()   # returns the path of the prepared .crb
```

`prepareViewer1mBenchmarkData()` does the same for the mouse brain data set, and `prepareDemoViewerPack(crb)` (from `prepare_viewer_pack.R`) builds the Viewer Pack of a prepared `.crb`.

## Files

- `prepare_viewer_1m_data.R`: downloads the 10x 1M neurons matrix and builds the one-million-cell mouse brain E18 `.crb`. Entry function: `prepareViewer1mBenchmarkData()`.
- `prepare_viewer_ren_data.R`: downloads the Ren et al. atlas (about 61 GB as `.h5ad`), the cell annotation, the clinical sample table and the TCR/BCR tables, and builds the Ren `.crb` with its immune repertoire. Entry function: `prepareViewerRenDemoData()`.
- `prepare_viewer_pack.R`: builds or reuses the Viewer Pack of a prepared `.crb`. Entry function: `prepareDemoViewerPack()`.
- `h5_transposed_write.R`: a helper, not a script to run. Both `prepare_viewer_*_data.R` scripts call its `.demoWriteH5Transposed()` to write the expression matrix as the cells x genes H5 sidecar, so it has to be sourced before them (`run-demo.R` does this). It exists because `HDF5Array::writeTENxMatrix()` can only write one gene after the other, and a source stored by cells is then scanned from start to end for every block of genes, which takes days for these atlases. The helper reads the cells once in chunks (directly from the CSR arrays for an AnnData file) on several processes, then assembles blocks of genes from the chunk files and appends each block to the H5 file. It is kept in its own file because two scripts share it.

## Where the data goes

Downloads and prepared files are written to the user cache directory (`tools::R_user_dir("CerebroNexus", "cache")`, under `large-examples/`), shared by all checkouts and worktrees. Set `CEREBRO_LARGE_CACHE` to use another folder, for example on a cluster. The Ren data set alone needs about 105 GB. The conversion of the counts to H5 runs on 2 R processes by default; set `CEREBRO_DEMO_WORKERS` to use more (each process needs about 3 to 5 GB of memory, an estimate, not a measurement). On a 24-core workstation, reading the 1.46M Ren cells took about 80 seconds on 8 processes.

Benchmarks and smoke checks live in [CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code) (`viewer/` and `benchmark/`).
