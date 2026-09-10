# 1M-cell Viewer benchmark

Measured on 2026-09-10 with the official 10x 1M neurons dataset.

## Compared revisions

| Stage | Public identifier | Purpose |
| --- | --- | --- |
| Release | `v4.4.2` | Unoptimized reference |
| Backend | PR #167 | Backend hot paths |
| Viewer | Viewer interaction candidate | Browser and interaction hot paths |

The three detached worktrees formed one ancestry chain. The browser benchmark passes a named CRB path, so clean checkouts run unchanged. The Viewer row should use its PR number after that pull request is opened; this document deliberately does not use branch names or short commit IDs as public identifiers.

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

Medians use three repetitions on the same 1M-cell CRB. Allocations are R allocations reported by `Rprofmem`; correctness checks passed for every row.

| Operation | Scale | Before | After | Time | Before alloc. | After alloc. | Allocation |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Full projection selection | 1M cells, all groups, 100% | 228 ms | 5 ms | -97.8% | 206.7 MiB | 11.4 MiB | -94.5% |
| Filtered projection selection | 1M cells, 17/33 clusters, 25% (134,557 cells) | 351 ms | 30 ms | -91.5% | 170.6 MiB | 43.3 MiB | -74.6% |
| Hover preparation | 1M loaded, 100K displayed, 3 groups | 33,877 ms | 2,438 ms | -92.8% | 1,328.7 MiB | 56.3 MiB | -95.8% |
| Single-gene expression | 1 gene x 1M cells, BPCells | 4,060 ms | 4,227 ms | +4.1% | 154.1 MiB | 169.3 MiB | +9.9% |
| RGB expression | 3 genes x 1M cells, 3 reads vs 1 | 12,963 ms | 4,170 ms | -67.8% | 461.4 MiB | 238.3 MiB | -48.4% |
| Multi-panel expression | 9 genes x 1M cells, transpose removed | 4,336 ms | 4,311 ms | -0.6% | 562.3 MiB | 463.2 MiB | -17.6% |
| Mean expression | 100 genes x 1M cells, dense vs backend-native | 5,622 ms | 3,988 ms | -29.1% | 1,916.8 MiB | 112.8 MiB | -94.1% |

The single-gene path regressed slightly in this run and allocates 15.3 MiB more R memory; it is not reported as an improvement. The main expression gains come from batching RGB reads and keeping aggregate computation backend-native.

## Viewer interaction hot paths

| Operation | Before | After | Change |
| --- | ---: | ---: | ---: |
| Configuration fingerprint, 500K cells | 139 ms | 128 ms | -7.9% |
| Hover payload and JSON, 50K cells | 1,224 ms | 37 ms | -97.0% |
| Hover allocation | 56.2 MiB | 12.8 MiB | -77.2% |
| Hover payload | 8.65 MiB | 1.44 MiB | -83.3% |
| Spatial hulls, five style changes | 413 ms | 69 ms | -83.3% |
| Hit testing, 500K cells | 1.229 ms/event | 0.0042 ms/event | -99.7% |

The hit-test index took 13.34 ms to build and broke even after approximately 11 pointer events. Five cached reads of an 8 MiB Spatial image completed below the timer's 1 ms resolution, compared with 281 ms without reuse.

## Browser loading

The browser benchmark launches the real 1M CRB, verifies the displayed cell count, opens Overview, waits for Canvas, checks at least 1,000 sampled non-white pixels, rejects browser errors, and measures the Shiny process RSS. The Viewer displays 10% (100K cells), matching the default large-data operating mode.

| Comparison | Data ready | Overview Canvas | Total | Shiny RSS |
| --- | ---: | ---: | ---: | ---: |
| Release to backend | 16.71 to 15.59 s (-6.7%) | 45.38 to 18.28 s (-59.7%) | 62.08 to 33.87 s (-45.4%) | 3,464.7 to 2,435.3 MiB (-29.7%) |
| Backend to Viewer | 15.38 to 14.72 s (-4.3%) | 18.25 to 1.52 s (-91.7%) | 33.68 to 16.30 s (-51.6%) | 2,154.6 to 1,913.9 MiB (-11.2%) |
| Release to Viewer | 15.49 to 15.10 s (-2.5%) | 44.58 to 1.59 s (-96.4%) | 59.70 to 16.69 s (-72.0%) | 3,513.1 to 1,904.7 MiB (-45.8%) |

These values are medians of three alternating-order runs. Every run loaded 1,000,000 cells, painted the same 1301 x 594 Canvas and passed the browser-log check. Absolute medians vary slightly between pairwise runs; use each pair only for its stated comparison.

The Viewer revision was also tested at 100% display. Two runs loaded and painted all 1M UMAP points in 72.4 and 73.5 seconds, using 4,616.7 and 4,368.5 MiB RSS respectively. This is an acceptance check for chunked Canvas painting, not a revision comparison.

## Environment

- Apple M1 Pro, 32 GiB RAM
- macOS 27.0 (26A5421a)
- R 4.6.1, aarch64
- BPCells 0.3.1, Seurat 5.5.1, Shiny 1.14.0, shinytest2 0.5.1

## Reproduce

```sh
Rscript "$BACKEND_ROOT/tests/bench/viewer_1m_hot_paths.R" \
  "$BASELINE_ROOT" "$BACKEND_ROOT" "$CRB" 3
Rscript "$VIEWER_ROOT/tests/bench/viewer_interaction_hot_paths.R" "$VIEWER_ROOT"
node "$VIEWER_ROOT/tests/bench/viewer_interaction_hit_test.js" "$VIEWER_ROOT"
Rscript "$VIEWER_ROOT/tests/bench/viewer_1m_browser.R" \
  "$BASELINE_ROOT" "$BACKEND_ROOT" "$CRB" 3 10
Rscript "$VIEWER_ROOT/tests/bench/viewer_1m_browser.R" \
  "$BACKEND_ROOT" "$VIEWER_ROOT" "$CRB" 3 10
Rscript "$VIEWER_ROOT/tests/bench/viewer_1m_browser.R" \
  "$BASELINE_ROOT" "$VIEWER_ROOT" "$CRB" 3 10
```

The vignette defines the revision variables, prepares all three detached worktrees and documents dataset preparation, output capture and cleanup.
