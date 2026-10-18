# Scale: study filled with same-host supplements

Collection `20261018T091308Z-58e778efc30a-scale-assembled`. **No new measurements.**

- Study: `20261016T224351Z-63d5337c3647-scale` (complete Scale run).
- Supplements: `20261018T072255Z-30a6295c85f5-scale-supplement`.
- 5 study builds that were skipped for resources are replaced by supplement builds of the same cells:
  human_pfc_hbcc 500,000 cells, embedded.
  The skipped rows remain in `resource_skips.csv`; no successful study build is replaced.
- Totals: 300 builds (300 successful), 600 access processes, 22800 raw timings.

Every row keeps its acquisition `run_id`; `source_run_id` and `collection_id` identify
origin and assembly. Study and supplements share host, R and package versions
(checked by `bench_assemble_supplement`); `acquisitions.csv` lists each run, its code
version and memory cap. Per-run reports, logs and manifests remain in the run
directories named there. Figures in `figures/` are drawn from the assembled rows.
