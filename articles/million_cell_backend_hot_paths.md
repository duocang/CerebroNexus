# Million-cell Viewer backend hot paths

## Purpose

This benchmark isolates the R-side paths that prepare million-cell
Viewer data before JSON serialization or browser rendering. Its purpose
is to answer a practical question: when a user filters a projection or
switches expression colouring, how much server time and temporary R
allocation can be removed without changing the CRB schema, the selected
cells, or the expression values?

The comparison deliberately stops before Canvas or WebGPU rendering. It
measures metadata filtering and sampling, canonical cell-index handling,
BPCells expression reads, RGB and multi-panel batching, and
backend-native mean expression. Browser payload and renderer work belong
to the following Viewer/WebGPU layer.

## Version and data contract

| Role | Revision | Version | Contract |
|----|----|----|----|
| PR \#165 | `69893a2b` | CerebroNexus 4.4.3 | Legacy RDS CRB and original Viewer backend paths |
| Thin CRB/qs2 | `27303f21` | CerebroNexus 4.5.0 | Thin CRB v1, qs2 default, hydration, and canonical BPCells cell index |
| Backend hot paths | `1ad8abc0` | CerebroNexus 4.5.1 | Same CRB contract plus lower-copy Viewer backend paths, storage-order reads, and compact Linked Views bundles |

The Thin CRB and backend stages read the same artifact under
`large-examples/1m/cerebro/`.
[`readCerebro()`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md)
hydrates the qs2 or RDS Thin CRB, opens the unchanged BPCells sidecar,
verifies the `cell_names` checksum, and restores omitted barcode and
projection row names. The benchmark does not create a second data
layout.

``` text
large-examples/1m/
├── source/
├── seurat/
└── cerebro/
    ├── cerebro_mouse_brain_1m_thin_qs2.crb
    └── cerebro_mouse_brain_1m.bpcells/
```

## What changed

Projection filtering now returns original integer row indices directly
instead of copying the complete metadata into a temporary data frame.
Expression access keeps those canonical indices through
`getExpressionRow()` and `getExpressionBlock()`, so a million indices
are no longer converted to barcode strings and matched back to the same
BPCells columns. RGB reads are batched, multi-panel extraction avoids a
full transpose, and mean expression stays in the backend-native matrix
representation.

The output is unchanged: sampling still uses the same seed and eligible
cells, requested order is preserved, missing names still fail
explicitly, and character barcode callers remain supported.

## CRB lifecycle result

The CRB lifecycle benchmark uses five alternating warm-cache rounds on
the same BPCells sidecar. The 4.5.0 and 4.5.1 values come from separate
runs of the same script; their small differences show run-to-run and
serialized-method variation rather than a new persistence algorithm.

``` r
knitr::kable(
  crb_lifecycle,
  col.names = c(
    "Resource",
    "PR #165 / 4.4.3",
    "Thin CRB / 4.5.0",
    "Backend / 4.5.1",
    "Latest vs PR #165",
    "Latest vs Thin CRB"
  ),
  row.names = FALSE,
  digits = 1
)
```

| Resource | PR \#165 / 4.4.3 | Thin CRB / 4.5.0 | Backend / 4.5.1 | Latest vs PR \#165 | Latest vs Thin CRB |
|:---|---:|---:|---:|:---|:---|
| CRB control payload (MiB) | 26.8 | 13.2 | 13.2 | 50.7% smaller | +0.02% |
| Write median (ms) | 8001.0 | 231.0 | 238.0 | 33.6x faster | 3.0% slower |
| Decode median (ms) | 1296.0 | 41.0 | 40.0 | 32.4x faster | 2.4% faster |
| Hydrated read median (ms) | 1479.0 | 297.0 | 298.0 | 5.0x faster | 0.3% slower |

## Viewer backend result

The PR \#165 and Thin CRB Viewer hot-path implementations are identical.
The code between those revisions changes serialization and hydration, so
their Viewer columns intentionally share the same measured baseline. The
latest column was measured from backend implementation `3fb88b8a` on
2026-09-12. Each value is a warm-cache median of three repetitions;
every row first verifies identical selected cells or expression values.

``` r
display_hot_paths <- hot_paths
display_hot_paths$latest_ms <- ifelse(is.na(display_hot_paths$latest_ms), "<1", format(display_hot_paths$latest_ms, trim = TRUE))
knitr::kable(
  display_hot_paths[, c(
    "operation",
    "scale",
    "pr165_ms",
    "thin_crb_ms",
    "latest_ms",
    "latest_vs_pr165",
    "latest_vs_thin_crb",
    "pr165_alloc_mib",
    "thin_crb_alloc_mib",
    "latest_alloc_mib",
    "allocation_change_vs_both"
  )],
  col.names = c(
    "Operation",
    "Scale",
    "PR #165 (ms)",
    "Thin CRB (ms)",
    "Backend (ms)",
    "Latest vs PR #165",
    "Latest vs Thin CRB",
    "PR #165 allocation (MiB)",
    "Thin CRB allocation (MiB)",
    "Backend allocation (MiB)",
    "Allocation vs both"
  ),
  row.names = FALSE,
  digits = 1
)
```

| Operation | Scale | PR \#165 (ms) | Thin CRB (ms) | Backend (ms) | Latest vs PR \#165 | Latest vs Thin CRB | PR \#165 allocation (MiB) | Thin CRB allocation (MiB) | Backend allocation (MiB) | Allocation vs both |
|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| Full projection selection | 1M cells, all groups, 100% | 197 | 197 | 31 | -84.3% | -84.3% | 199.1 | 199.1 | 19.1 | -90.4% |
| Filtered projection selection | 1M cells, 17/33 clusters, 25% | 329 | 329 | 28 | -91.5% | -91.5% | 166.5 | 166.5 | 39.5 | -76.3% |
| Cell-index resolution | 1M shuffled indices | 79 | 79 | \<1 | \>98.7% faster | \>98.7% faster | 27.1 | 27.1 | 0.0 | -100.0% |
| Single-gene expression | 1 gene x 1M cells, canonical order | 4063 | 4063 | 4022 | -1.0% | -1.0% | 154.1 | 154.1 | 157.1 | +1.9% |
| Shuffled single-gene expression | 1 gene x 100k cells, random order | 7886 | 7886 | 5881 | -25.4% | -25.4% | 33.4 | 33.4 | 22.3 | -33.2% |
| RGB expression | 3 genes x 1M cells | 12498 | 12498 | 4021 | -67.8% | -67.8% | 461.4 | 461.4 | 226.1 | -51.0% |
| Multi-panel expression | 9 genes x 1M cells | 4309 | 4309 | 4189 | -2.8% | -2.8% | 562.3 | 562.3 | 451.0 | -19.8% |
| Mean expression | 100 genes x 1M cells | 5912 | 5912 | 3931 | -33.5% | -33.5% | 1916.8 | 1916.8 | 65.4 | -96.6% |

Full projection selection falls from 197 to 31 ms and filtered selection
from 329 to 28 ms, moving both operations toward immediate feedback. RGB
expression falls from 12.50 to 4.02 seconds, saving 8.48 seconds per
update. Mean expression falls from 5.91 to 3.93 seconds while cumulative
R allocation drops from 1,916.8 to 65.4 MiB. A canonical-order
single-gene scan remains I/O-bound and effectively unchanged; a
100,000-cell random-order stress read improves from 7.89 to 5.88 seconds
after reading BPCells columns in storage order and restoring the
requested output order.

One explanatory pass through the six full-scale operations, excluding
index resolution and the 100,000-cell stress row, falls from 27.308 to
16.222 seconds and from 3,460.2 to 958.2 MiB of cumulative R
allocations. That is 40.6% less elapsed time and 72.3% less allocation,
but it is not an end-to-end Viewer timing because the operations have
different UI frequencies and the total excludes transport and rendering.

## Linked Views bundle construction

The final backend audit found two repeated operations outside the
expression benchmark: the full bundle calculated a second million-cell
fingerprint and copied the default projection coordinates into both
`projections` and `spaces`. The optimized bundle reuses the fingerprint
already cached for saved views and keeps coordinates only in
`projections`, from which the existing client reconstructs its
lightweight space descriptor.

``` r
knitr::kable(
  linked_bundle,
  col.names = c(
    "Resource",
    "PR #165 and Thin CRB",
    "Backend 4.5.1",
    "Change"
  ),
  row.names = FALSE,
  digits = 2
)
```

| Resource | PR \#165 and Thin CRB | Backend 4.5.1 | Change |
|:---|---:|---:|:---|
| Bundle construction (ms) | 3473.00 | 253.00 | 92.7% faster; 13.7x |
| JSON encoding (ms) | 3625.00 | 1961.00 | 45.9% faster; 1.85x |
| R bundle object (MiB) | 135.88 | 120.62 | 11.2% smaller |
| JSON payload (MiB) | 62.94 | 48.78 | 22.5% smaller |

These are three-round warm medians on the same one-million-cell Thin qs2
CRB. Bundle construction falls from 3.473 seconds to 253 milliseconds,
JSON encoding from 3.625 to 1.961 seconds, the R bundle from 135.88 to
120.62 MiB, and the JSON payload from 62.94 to 48.78 MiB. The benchmark
normalizes only the intentionally removed duplicate fields and verifies
that every remaining before/after value is equal.

The remaining 48.78 MiB is the intentional all-cell Linked Views
workspace. Sampling it, loading columns on demand, or introducing
typed-array transport changes Viewer behavior or its wire protocol and
therefore belongs to the separate Viewer/WebGPU layer rather than this
backend branch.

## Resource interpretation

Timing columns are warm-cache wall-clock medians. Allocation columns are
cumulative allocations recorded by `Rprofmem`; they are not peak
resident memory. The preparation host reached 8.47 GiB maximum resident
memory while creating the reusable 1M artifacts, but that preparation
cost is outside the interactive hot-path comparison.

| Resource                            |      Recorded value |
|-------------------------------------|--------------------:|
| Cells                               |           1,000,000 |
| Genes                               |              27,998 |
| Clusters                            |                  33 |
| End-to-end data preparation         |          1,342.12 s |
| Preparation maximum resident memory |            8.47 GiB |
| Official source H5                  | 4,216,018,749 bytes |
| Reusable Seurat output              | 3,778,378,648 bytes |
| CRB plus BPCells output             | 3,765,141,671 bytes |
| Thin qs2 CRB control payload        |           13.21 MiB |

## Reproduce the comparison

The wrapper defaults to the exact Thin CRB baseline and current
checkout. It creates detached temporary worktrees, records the PR \#165,
Thin CRB, and backend implementation SHAs, reuses complete artifacts,
and writes a tab-separated comparison table. Existing 1M data are not
downloaded or regenerated when the expected files and sidecar are
complete.

``` bash
export CEREBRO_LARGE_CACHE=/path/to/cerebro-large-examples
tests/bench/run_viewer_1m_benchmark.sh 27303f21 1ad8abc0 tests/bench/scratch/backend-4.5.1
```

The output directory contains the exact comparison identity and measured
rows:

``` bash
column -t -s $'\t' tests/bench/scratch/backend-4.5.1/run_manifest.tsv
column -t -s $'\t' tests/bench/scratch/backend-4.5.1/hot_paths.tsv
column -t -s $'\t' tests/bench/scratch/backend-4.5.1/bundle.tsv
```

To reuse a prepared CRB directly, run the lower-level benchmark with
explicit source roots. The CRB may use qs2 or RDS because the script
loads it through
[`readCerebro()`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md).

``` bash
Rscript tests/bench/viewer_1m_hot_paths.R /path/to/thin-crb-source /path/to/backend-source /path/to/cerebro_mouse_brain_1m_thin_qs2.crb 3
```

The current benchmark records a separate barcode-to-index row before the
expression workloads. This exposes the work removed by retaining
canonical indices instead of hiding it inside a BPCells read.

## Evidence policy

Published values must come from `hot_paths.tsv` and `bundle.tsv`
together with `run_manifest.tsv`; revision labels copied by hand are
insufficient. A row is accepted only when its before/after correctness
check passes. Cold-cache results, browser transfer, JSON parsing,
Canvas/WebGPU rendering, GPU memory, and end-to-end interaction latency
are intentionally outside this backend-only benchmark and must not be
inferred from it.
