# Panel C Incremental Benchmark Design

## Status

Approved direction: retain the published Panel A/B evidence and add both a
three-backend scale bridge and a two-backend full-source out-of-core study.

## Goal

Extend the real-data expression-backend evidence without rerunning or mutating
the existing publication run:

- Panel C1 compares `embedded`, `bpcells`, and `h5` at 400,000 mouse cells and
  300,000 human cells.
- Panel C2 compares `bpcells` and `h5` on all 1,306,127 mouse cells and all
  1,486,324 human cells without constructing a full `dgCMatrix`.
- One command runs C1 and C2 sequentially while reusing the existing verified
  source cache.

## Non-goals

- Do not rerun, rewrite, or append rows to the immutable Panel A/B run.
- Do not include `embedded` in the full-source comparison. Both full sources
  exceed the 32-bit sparse-index limit of `dgCMatrix`.
- Do not claim that Panel C2 measures every cost of a complete biological
  analysis. It measures construction and runtime access for the expression
  backend plus a synthetic full-cell Cerebro metadata shell.
- Do not change the production `exportFromSeurat()` contract merely to create
  benchmark evidence.

## Published baseline

The combined report accepts this exact Panel A/B baseline:

- run: `20260907T172844Z-9c6ab101e4a5-publication`
- Git SHA: `9c6ab101e4a5a2ae79ada5d5fab99bf05952a0f4`
- sources: the recorded mouse and human SHA-256 values in its
  `source_manifest.csv`

The baseline remains under `tests/bench/result/runs/`; its files are never
modified. A combined report must reject a different baseline Git SHA, source
hash, required CSV-column set, query-plan fingerprint, or 12-gene query-panel
size.

## Result layout and publication model

Each extension publishes independently so that a C2 failure cannot invalidate
a completed C1 run:

```text
tests/bench/result/
├── CURRENT                         # existing A/B pointer
├── runs/<AB_RUN_ID>/               # existing immutable A/B evidence
├── panel-c1/
│   ├── CURRENT
│   └── runs/<C1_RUN_ID>/
├── panel-c2/
│   ├── CURRENT
│   └── runs/<C2_RUN_ID>/
└── panel-c/
    ├── summary.md                  # derived A/B/C interpretation
    ├── manifest.csv                # exact three input run IDs and Git SHAs
    └── figures/
        └── expression_backend_benchmark_panel_c.png
```

Publication remains pointer-last and immutable within each sub-run. The
combined report is regenerated only after both C1 and C2 validate successfully.

## Source-cache behavior

`BENCH_SOURCE_CACHE` keeps the two complete public source files outside Git.
The Panel C wrapper calls the existing cache validator before either run. A file
is reused only when its byte size and SHA-256 sidecar match. Pulling new code or
publishing result CSVs does not alter the cache. A missing or invalid cache
entry is downloaded normally and then validated.

## Panel C1: three-backend scale bridge

C1 uses the existing source reader, Seurat shell, exporter, access measurement,
and correctness paths. Its schedule contains only:

| Source | Cells | Backends | Export repeats | Access repeats |
|---|---:|---|---:|---:|
| mouse brain E18 | 400,000 | embedded, bpcells, h5 | 3 | 2 |
| human PFC HBCC | 300,000 | embedded, bpcells, h5 | 3 | 2 |

This produces 18 export processes and, if all exports succeed, 36 access
processes. Mouse 800,000 is deliberately excluded because the current host's
70% safety budget is below its conservative peak-memory estimate.

C1 reports the same fields and uses the same 12-gene query-plan semantics as
the baseline. It is a scale extension, not a replacement for A/B.

## Panel C2: full-source out-of-core construction

### Lazy source adapters

The full path opens each cached source as a BPCells `IterableMatrix` in
genes-by-cells orientation:

- mouse: `BPCells::open_matrix_10x_hdf5()`
- human: `BPCells::open_matrix_anndata_hdf5(group = "X")`

Dimension, names, and deterministic numeric fingerprints are checked against
the source inventory before writing either backend.

### BPCells artifact

`BPCells::write_matrix_dir()` consumes the lazy source matrix and writes the
full genes-by-cells directory without materialising a `dgCMatrix`.

### H5 artifact

The lazy source matrix is transposed and passed to
`BPCells::write_matrix_10x_hdf5()`, producing the cells-by-genes orientation
expected by Cerebro's H5 runtime. The writer's `/matrix` group is renamed to
`/expression` with `rhdf5::H5Lmove()` after validating dimensions and names;
the file and root-group handles are always closed before access measurement.
Cerebro then attaches it through the existing
`HDF5Array::TENxMatrix(..., group = "expression")` path and lazily transposes
it back to genes-by-cells.

### Portable Cerebro shell

Each backend receives its own small `.crb` containing:

- source and run provenance;
- all cell identifiers;
- deterministic synthetic sample and cluster factors;
- a deterministic two-dimensional synthetic projection;
- `expression = NULL`;
- a relative `expression_backend` tag pointing to the sibling artifact.

The shell makes load and attach measurements representative of a full-cell
Cerebro artifact while ensuring that the expression matrix stays on disk.

### Schedule

| Source | Cells | Backends | Build repeats | Access repeats |
|---|---:|---|---:|---:|
| mouse brain E18 | 1,306,127 | bpcells, h5 | 3 | 2 |
| human PFC HBCC | 1,486,324 | bpcells, h5 | 3 | 2 |

This produces 12 build processes and, if all builds succeed, 24 access
processes. Processes run sequentially and remove each staged backend after its
access repeats complete.

## Metrics and correctness

C1 retains the existing metrics. C2 records:

- backend construction seconds and stored bytes;
- `.crb` shell bytes;
- peak process RSS during construction;
- fresh-process shell load and backend attach seconds;
- attached-process RSS and peak RSS;
- first single-gene latency;
- warmed single-gene p50 and p95 over the deterministic panel;
- one 12-gene by all-cell block latency;
- source, row, block, and query-plan fingerprints.

The full block may materialise only the requested 12 rows, not the source
matrix. At 1.5 million cells this is roughly 18 million numeric values and is
well below the host-memory budget.

## Combined Panel C figure

The derived figure contains two facets:

1. C1 scale bridge: peak RSS and the two access latencies from 50k/150k through
   400k mouse or 300k human, retaining all three backends.
2. C2 full-source feasibility: build time, stored size, peak RSS, and access
   latency for BPCells and H5 at the exact full cell count.

The figure visually separates three-backend comparable evidence from
two-backend out-of-core evidence. Missing `embedded` full-source points are
labelled `not representable`, not treated as crashes or zero values.

## Command-line interface

The public entry point is:

```bash
BENCH_SOURCE_CACHE=/persistent/cache \
  tests/bench/run_panel_c.sh
```

Optional `BENCH_PANEL_C_PART=c1` or `c2` runs one part. The wrapper inherits
`BENCH_THREADS`, `BENCH_SCRATCH_PARENT`, and the R-startup isolation used by the
existing sweep. It never runs the A/B schedule.

## Failure behavior

- Resource and disk preflight runs before backend construction.
- Every build and access attempt runs in a fresh process.
- A killed process is recorded explicitly.
- Correctness mismatch, source-hash drift, protocol drift, missing scheduled
  rows, or excessive memory invalidates that sub-run.
- A failed sub-run leaves its previous `CURRENT` pointer unchanged.
- The wrapper stops before regenerating the combined report when either C1 or
  C2 is incomplete.
- Source cache entries are never removed by scratch cleanup.

## Verification

Automated coverage must include:

- exact C1 and C2 schedule sizes and backend membership;
- rejection of mouse 800k from C1;
- fixture-based lazy 10x and AnnData source adapters;
- streamed BPCells and H5 artifact dimensions, orientation, names, and values;
- a guard proving the full path never coerces the source to `dgCMatrix` or a
  dense full matrix;
- C2 runtime attach through the real Cerebro backend code;
- row and 12-gene block fingerprints for both full backends;
- independent pointer-last publication for C1 and C2;
- combined-report rejection of baseline or source drift;
- shell syntax, focused benchmark tests, and the complete benchmark test set.
