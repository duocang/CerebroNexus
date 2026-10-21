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
- `h5_transposed_write.R`: a helper, not a script to run. Both `prepare_viewer_*_data.R` scripts call its `.demoWriteH5Transposed()` to write the expression matrix as the cells x genes H5 sidecar, so it has to be sourced before them (`run-demo.R` does this). It exists because `HDF5Array::writeTENxMatrix()` can only write one gene after the other, and a source stored by cells is then scanned from start to end for every block of genes, which takes days for these atlases. The helper reads the cells once in chunks, splits them into gene blocks on disk and appends each block to the H5 file. It is kept in its own file because two scripts share it.

## Where the data goes

Downloads and prepared files are written to the user cache directory (`tools::R_user_dir("CerebroNexus", "cache")`, under `large-examples/`), shared by all checkouts and worktrees. Set `CEREBRO_LARGE_CACHE` to use another folder, for example on a cluster. Both data sets together need about 100 GB. The Ren conversion took about 50 minutes on a 24-core workstation with 128 GB of memory, and its process used about 9 GB of memory.

Benchmarks and smoke checks live in [CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code) (`viewer/` and `benchmark/`).
