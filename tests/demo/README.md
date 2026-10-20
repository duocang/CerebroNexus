# Demo data preparation

`run-demo.R` sources these scripts to download and prepare the large demo datasets; `test-run-demo.R`, `test-ren-demo.R` and `test-million-cell-viewer.R` test them.

- `prepare_viewer_1m_data.R`: the one-million-cell mouse brain E18 CRB.
- `prepare_viewer_ren_data.R`: the Ren COVID-19 CRB with immune repertoire data.
- `prepare_viewer_pack.R`: builds or reuses the Viewer Pack of a prepared CRB.

Keep only what the demo and its tests need here. Benchmarks and smoke checks live in [CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code) (`viewer/` and `benchmark/`). This directory is excluded from package builds.
