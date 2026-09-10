# Viewer interaction performance results

## Scope

- The browser comparison uses the backend revision as the control and the stacked Viewer revision as the candidate.
- The cumulative comparison uses the release as the control and the stacked Viewer revision as the candidate.
- Runtime: Apple M1 Pro, 32 GiB RAM, macOS 27.0, R 4.6.1 and Node 26.5.1.
- No asynchronous runtime, cross-session cache or new dependency is used.

## Real Viewer on 1M cells

The Viewer loaded the same 1M-cell BPCells-backed CRB, displayed 100,000 cells and alternated execution order for three rounds.

| Comparison | Data ready | Overview Canvas | Total | Shiny RSS |
| --- | ---: | ---: | ---: | ---: |
| Backend to Viewer | 15.38 to 14.72 s (-4.3%) | 18.25 to 1.52 s (-91.7%) | 33.68 to 16.30 s (-51.6%) | 2,154.6 to 1,913.9 MiB (-11.2%) |
| Release to Viewer | 15.49 to 15.10 s (-2.5%) | 44.58 to 1.59 s (-96.4%) | 59.70 to 16.69 s (-72.0%) | 3,513.1 to 1,904.7 MiB (-45.8%) |

Every run reported 1,000,000 cells, painted a 1301 x 594 Canvas with sufficient non-white pixels and produced no browser error logs.

## Interaction microbenchmarks

| Path | Fixture | Before | After | Time change | Allocation change |
| --- | --- | ---: | ---: | ---: | ---: |
| Configuration fingerprint | 500,000 cells | 139 ms | 128 ms | -7.9% | 36.43 to 29.75 MiB (-18.3%) |
| Hover payload and JSON | 50,000 cells, 3 groups | 1,224 ms | 37 ms | -97.0% | 56.15 to 12.79 MiB (-77.2%) |
| External Spatial image reuse | 8 MiB image, 5 renders | 281 ms | below 1 ms | timer resolution | 105.23 to 0 MiB |
| Spatial hull reuse | 500,000 cells, 20 groups, 5 style changes | 413 ms | 69 ms | -83.3% | 945.44 to 189.09 MiB (-80.0%) |
| Browser hit test | 500,000 cells, 200 events | 1.229 ms/event | 0.0042 ms/event | -99.7% | not measured |

The hover payload fell from 8.65 MiB to 1.44 MiB (-83.3%). The hit-test index took 13.34 ms to build and broke even after approximately 11 pointer events. Moving interaction marks to the overlay removes 6,000,000 cell-loop iterations and 2,000,000 bytes of visibility-mask allocation per hover event across four 500,000-cell panels.

## Reproduce

Run from the Viewer revision root:

```sh
Rscript tests/bench/viewer_interaction_hot_paths.R .
node tests/bench/viewer_interaction_hit_test.js .
```

For the complete 1M-cell preparation, three-revision browser commands and cleanup, follow [`large_example_data_benchmark.Rmd`](../vignettes/large_example_data_benchmark.Rmd).

Reported times are medians. R allocation figures come from `Rprofmem`; cached image reuse records no new R allocation at its timer resolution. The scripts verify fingerprint equality, image equality, hull equality and indexed-versus-linear hit-test equality before reporting results.
