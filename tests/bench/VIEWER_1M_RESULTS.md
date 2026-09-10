# 1M-cell Viewer benchmark

Measured on 2026-09-10 with the official 10x 1M neurons dataset.

## Compared revisions

| Version | Revision | Notes |
| --- | --- | --- |
| Baseline | `892097a1` | Reference revision |
| Candidate | `f3358c1d` | Optimized revision |

The measured detached worktrees differed only by the optimized changes. The browser benchmark
passes a named CRB path, so clean checkouts of both revisions run unchanged.

## Dataset preparation

| Property | Result |
| --- | ---: |
| Cells | 1,000,000 |
| Genes | 27,998 |
| Clusters | 33 |
| UMAP rows | 1,000,000 |
| End-to-end preparation time | 1,342.12 s (22 min 22 s) |
| Maximum resident memory | 8.47 GiB |
| Official source H5 | 4,216,018,749 bytes |
| Reusable Seurat output | 3,778,378,648 bytes |
| CRB + BPCells output | 3,765,141,671 bytes |
| CRB metadata file alone | 28,132,278 bytes |

All source and generated data are stored in the R user cache, outside Git.

## Viewer hot paths

Medians use three repetitions on the same 1M-cell CRB. Allocations are R
allocations reported by `Rprofmem`; correctness checks passed for every row.

| Operation | Scale | Before | After | Time | Before alloc. | After alloc. | Allocation |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Full projection selection | 1M cells, all groups, 100% | 222 ms | 4 ms | -98.2% | 206.7 MiB | 11.4 MiB | -94.5% |
| Filtered projection selection | 1M cells, 17/33 clusters, 25% (134,557 cells) | 356 ms | 30 ms | -91.6% | 170.6 MiB | 43.3 MiB | -74.6% |
| Hover preparation | 1M loaded, 100K displayed, 3 groups | 32,666 ms | 2,385 ms | -92.7% | 1,328.7 MiB | 56.3 MiB | -95.8% |
| Single-gene expression | 1 gene x 1M cells, BPCells | 4,041 ms | 4,073 ms | +0.8% | 154.1 MiB | 169.3 MiB | +9.9% |
| RGB expression | 3 genes x 1M cells, 3 reads vs 1 | 12,285 ms | 4,067 ms | -66.9% | 461.4 MiB | 238.3 MiB | -48.4% |
| Multi-panel expression | 9 genes x 1M cells, transpose removed | 4,336 ms | 4,272 ms | -1.5% | 562.3 MiB | 463.2 MiB | -17.6% |
| Mean expression | 100 genes x 1M cells, dense vs backend-native | 5,540 ms | 3,978 ms | -28.2% | 1,916.8 MiB | 112.8 MiB | -94.1% |

The single-gene path is effectively time-neutral in this run and allocates
15.3 MiB more R memory. The main expression gains come from batching RGB
reads and keeping aggregate computation backend-native.

## Browser loading

The browser benchmark launches the real 1M CRB, verifies the displayed cell
count, opens Overview, waits for Canvas, checks at least 1,000 sampled non-white
pixels, rejects browser errors, and measures the Shiny process RSS. The Viewer
displays 10% (100K cells), matching the default large-data operating mode.

| Metric | Before median | After median | Change |
| --- | ---: | ---: | ---: |
| Data ready | 16,315 ms | 15,981 ms | -2.0% |
| Overview Canvas ready | 46,609 ms | 19,102 ms | -59.0% |
| Launch through painted Overview | 62,460 ms | 35,418 ms | -43.3% |
| Shiny process RSS | 3,759.6 MiB | 2,160.3 MiB | -42.5% |

These values are medians of three alternating-order runs. A later one-round
rerun from clean detached checkouts measured 64,033 ms and 3,530.4 MiB before
versus 34,105 ms and 2,699.5 MiB after; both painted the same 1301 x 594 Canvas
with more than 31,000 sampled non-white pixels.

The optimized revision was also tested at 100% display. Two runs loaded
and painted all 1M UMAP points in 72.4 and 73.5 seconds, using 4,616.7 and
4,368.5 MiB RSS respectively. This is an acceptance check for chunked Canvas
painting, not a baseline/candidate comparison.

## Environment

- Apple M1 Pro, 32 GiB RAM
- macOS 27.0 (26A5421a)
- R 4.6.1, aarch64
- BPCells 0.3.1, Seurat 5.5.1, Shiny 1.14.0, shinytest2 0.5.1

## Reproduce

```sh
Rscript tests/bench/viewer_1m_hot_paths.R BEFORE_ROOT AFTER_ROOT CRB 3
Rscript tests/bench/viewer_1m_browser.R BEFORE_ROOT AFTER_ROOT CRB 3 10
```
