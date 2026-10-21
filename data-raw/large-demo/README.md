# Large demo data sets

`run-demo.R` sources these scripts to download and prepare the two large demo
data sets, then starts the Viewer on them. They are not shipped with the package
(`data-raw/` is excluded from builds) and are not part of the test suite;
`tests/testthat/test-run-demo.R`, `test-ren-demo.R`, `test-million-cell-viewer.R`
and `test-demo-h5-transpose.R` test them with small synthetic data.

- `prepare_viewer_1m_data.R`: the one-million-cell mouse brain E18 CRB.
- `prepare_viewer_ren_data.R`: the Ren et al. COVID-19 atlas (1.46M cells) with
  paired TCR/BCR data.
- `prepare_viewer_pack.R`: builds or reuses the Viewer Pack of a prepared CRB.
- `h5_transposed_write.R`: writes a cell-major source (AnnData, TENx) as the
  cells x genes H5 sidecar in two passes. `HDF5Array::writeTENxMatrix()` scans
  the whole source once per block of genes, which takes days for these atlases.
  Source it before the two `prepare_*` scripts above.

## Where the data goes

Downloads and prepared files are written to the user cache directory
(`tools::R_user_dir("CerebroNexus", "cache")`, under `large-examples/`), shared by
all checkouts and worktrees. Set `CEREBRO_LARGE_CACHE` to use another folder,
for example on a cluster. The two data sets need about 100 GB together.

Benchmarks and smoke checks live in
[CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code)
(`viewer/` and `benchmark/`).
