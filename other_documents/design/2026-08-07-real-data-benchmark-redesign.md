# Simplified real-data expression-backend benchmark design

**Status:** implemented in CerebroNexus 4.2; validated full-scale evidence pending

**Date:** 2026-08-07

**Branch:** `perf/real-data-benchmark`
**Baseline:** CerebroNexus v4.0.0, `ee502d94`

## 1. Purpose

Rebuild the real-data benchmark as a small, readable research harness that
answers two related but different questions:

1. At predeclared real-data tiers up to a 500,000-cell comparison target, how
   do `embedded`, `bpcells`, and `h5` compare for export, storage, loading,
   memory, and expression access when all three receive the same materialized
   matrix?
2. How does the BPCells path scale from that common comparison point through
   the complete 4,140,453-cell MSSM matrix?

The full MSSM data is a primary benchmark endpoint. It is not an appendix or a
smoke test. It cannot be a three-backend row under the current implementation
because its approximately 15.6 billion non-zero values exceed the 2,147,483,647
non-zero limit of `Matrix::dgCMatrix`. That representation limit is independent
of installed RAM.

## 2. Decisions

- Use one pinned real source for every measured tier: the MSSM human prefrontal
  cortex H5AD.
- Use deterministic, nested cell subsets. Dataset identity must not be
  confounded with cell count.
- Compare all three backends at 125k, 250k, and a predeclared comparison
  target capped at 500k cells. Freeze `common_target_actual` only after the
  selected cells' exact nnz has been measured.
- Extend BPCells through 1M, 2M, and all 4,140,453 cells. The frozen
  `common_target_actual` appears in both panels as a linkage point, but the two
  BPCells observations use different input routes and are not pooled.
- Run three fresh-process technical export/access pairs per Panel A
  `(tier, backend)` cell and four per Panel B tier. This is 27 pairs / 54 worker
  processes in Panel A and 16 pairs / 32 worker processes in Panel B.
- Use fixed, separately rotated export and access orders. Process repeats are
  consistency observations on one host and source, not statistically
  independent biological or system replicates.
- Do not impose a 32 GiB-oriented memory gate. The target common tier may be
  reduced only for the exact `dgCMatrix` nnz representation limit. A runtime
  memory failure is a recorded failure, never a reason to change a tier.
- Keep source download outside timed code. A run accepts a local H5AD and
  validates its byte size and SHA-256 before measuring anything.
- Keep the harness under `tests/bench/`, which is excluded from package builds.
- Do not restore profiles, generic schedulers, resource planners, publication
  pipelines, `CURRENT`, or generated figure systems.

## 3. Pinned source

The configuration records one source:

| field | value |
|---|---|
| key | `human_pfc_mssm` |
| format | AnnData H5AD, sparse `/X` |
| cells | 4,140,453 |
| expected non-zero values | approximately 15.6 billion; recomputed exactly |
| expected bytes | 36,092,176,654 |
| SHA-256 | `c62456941372b90bcf0df38e8cb1c34dd060bc5a507270ab1d068cbe6f1dfd54` |
| URL | `https://datasets.cellxgene.cziscience.com/0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad` |
| expression semantics | normalized floating-point `data` |

The runner refuses a byte-size or hash mismatch. Network time is never part of
a benchmark metric.

## 4. Deterministic nested sampling

The pre-freeze target ladder is:

```text
125,000 ⊂ 250,000 ⊂ 500,000 ⊂ 1,000,000 ⊂ 2,000,000 ⊂ 4,140,453
```

The measured ladder replaces 500,000 with `common_target_actual <= 500000`.
No result labels a reduced tier as 500k.

Divide the source cell axis into four fixed contiguous strata with boundaries
`B_s = floor(s * N / 4)`. For requested size `n`, stratum `s` receives the
integer quota

```text
q_s(n) = floor(s * n / 4) - floor((s - 1) * n / 4).
```

Take a centered contiguous window of exactly that quota within each stratum.
This handles every integer tier, including the non-divisible full size
4,140,453, while preserving source order and expanding around the same four
centers. It covers four regions of the source cell axis; it makes no claim that
the file is donor-ordered or that the sample is biologically representative.

```r
bench_stratified_blocks <- function(n_total, n_take) {
  stopifnot(n_total >= 1L, n_take >= 1L, n_take <= n_total)
  n_strata <- 4L
  out <- vector("list", n_strata)
  for (s in seq_len(n_strata)) {
    lo <- floor((s - 1) * n_total / n_strata) + 1L
    hi <- floor(s * n_total / n_strata)
    quota <- floor(s * n_take / n_strata) -
      floor((s - 1) * n_take / n_strata)
    start <- lo + floor(((hi - lo + 1L) - quota) / 2L)
    out[[s]] <- data.frame(
      stratum = s,
      start = start,
      end = start + quota - 1L,
      n = quota
    )
  }
  do.call(rbind, out)
}

bench_stratified_indices <- function(n_total, n_take) {
  blocks <- bench_stratified_blocks(n_total, n_take)
  unlist(Map(
    function(start, end, n) if (n == 0L) integer() else seq.int(start, end),
    blocks$start,
    blocks$end,
    blocks$n
  ), use.names = FALSE)
}
```

The protocol fixes exactly four strata; these helpers are not advertised as a
generic `k`-stratum sampler. For the benchmark's large tiers every quota is
positive. The implementation
still treats a zero quota as an empty block so the helper remains correct for
small test fixtures. The quota sequence is monotone within each stratum, so
the centered windows give exact sizes and actual set inclusion for arbitrary
increasing `n` under this fixed four-stratum rule; when `n == N`, the four
quotas equal the four stratum sizes and
every source cell is selected. Contract tests check identities and set
inclusion, not just lengths. Each run records block boundaries and a SHA-256
fingerprint of the ordered source-index vector in `sampling.csv`.

## 5. Panel A: controlled three-backend comparison

### 5.1 Matrix contract

For 125k, 250k, and the frozen `common_target_actual` at or below 500k:

1. From the pinned local H5AD, read only `/X/indptr` with 64-bit values retained
   as exact doubles. Require CSR encoding, length `n_cells + 1`, finite
   integer-valued entries, zero first entry, monotonicity, final value no
   greater than `2^53`, and exact equality of the final pointer with the
   `/X/data` and `/X/indices` lengths. Derive non-negative integer per-cell nnz
   with `diff()`.
2. Before either measured schedule is frozen, sum exact per-cell nnz over each
   selected index set. If the 500k target is not legal, binary-search all
   integer sizes `n <= 500000` using the same nested sampler and freeze the
   largest legal `n` as `common_target_actual`. This is the largest admissible
   tier at or below the predeclared 500k target, not a claim about the largest
   matrix possible under another sampling design.
3. Abort the protocol before any measured export/access worker starts if
   `common_target_actual <= 250000`, if 125k or 250k is not legal, or if any
   scheduled Panel A tier's exact nnz exceeds `.Machine$integer.max`. Never
   reduce a tier after the schedule is frozen, including after a memory or disk
   failure.
4. Open the H5AD expression matrix with
   `BPCells::open_matrix_anndata_hdf5(path, group = "X")`.
5. Select the deterministic tier cells in source order.
6. Materialize that tier once per export process as the same genes-by-cells
   `dgCMatrix`.
7. Build the same minimum Seurat object and export it as
   `embedded`, `bpcells`, or `h5`.

The expected 500k nnz is about 1.88 billion. The estimate is informational;
only the exact `/X/indptr` result can freeze the schedule. Every table uses the
actual frozen cell count and `common_target_actual`, never silently relabeling
a reduced tier as `500k`.

### 5.2 Repetition and order

Each backend has three separate fresh-process technical export/access pairs
per tier. Export order is fixed as follows:

| repeat | export tier order | export backend order within each tier |
|---:|---|---|
| 1 | 125k → 250k → common | embedded → bpcells → h5 |
| 2 | 250k → common → 125k | bpcells → h5 → embedded |
| 3 | common → 125k → 250k | h5 → embedded → bpcells |

Access order deliberately differs from the matching export order:

| repeat | access tier order | access backend order within each tier |
|---:|---|---|
| 1 | 250k → common → 125k | h5 → embedded → bpcells |
| 2 | common → 125k → 250k | embedded → bpcells → h5 |
| 3 | 125k → 250k → common | bpcells → h5 → embedded |

An export process is the experimental unit for source preparation, export
time, stored bytes, R heap, and peak sampled RSS. A fresh access process tied
to one export is the experimental unit for loading, attaching, and queries.
Repeated warmed calls inside an access process are not replicates.

Panel A therefore contains 27 export workers and 27 access workers: 54 worker
processes forming 27 technical pairs. Execution is batched by repeat. Within a
repeat, all nine exports finish, then those nine artifacts are accessed in the
separate access order, then every marked artifact is safely removed after its
outcome and log are preserved. This avoids a
same-job immediate read-after-write pattern without retaining all 27 exports
at once. `schedule.csv` has separate integer `export_order` and `access_order`
columns, and validation proves that the order vectors differ within every
repeat. OS page cache remains uncontrolled and is recorded as a limitation.

## 6. Panel B: BPCells extended scaling

Panel B measures BPCells at `common_target_actual`, 1M, 2M, and 4,140,453
cells. It uses four separate fresh-process technical pairs per tier. Panel B
therefore contains 16 export workers and 16 access workers: 32 worker processes
forming 16 technical pairs.

The export and access tier orders are two different four-order Latin rotations:

| repeat | export tier order | access tier order |
|---:|---|---|
| 1 | common → 1M → 2M → full | 2M → full → common → 1M |
| 2 | 1M → 2M → full → common | full → common → 1M → 2M |
| 3 | 2M → full → common → 1M | common → 1M → 2M → full |
| 4 | full → common → 1M → 2M | 1M → 2M → full → common |

As in Panel A, each repeat completes its four exports, then its four accesses,
then cleanup. Every tier occupies every position once in each phase, and no
repeat has identical export and access order.

For every tier:

1. Open the local H5AD as a BPCells `IterableMatrix`.
2. Apply the deterministic nested cell selection without converting the
   expression matrix to `dgCMatrix`.
3. Build the same deterministic synthetic shell generator used by Panel A.
   Its one complete `data` layer is the real expression matrix, while two
   grouping variables, `nUMI`/`nGene`, and two-dimensional coordinates are
   synthetic functions of the selected source indices.
4. Call the public `exportFromSeurat(..., slot = "data",
   expression_matrix_mode = "bpcells")` path without injecting the private
   `.expression_resolution` argument.
5. Stream it to a BPCells sidecar with `BPCells::write_matrix_dir()`.
6. Reload the CRB in a fresh process, attach its relative sidecar through
   `inst/viewer/utility_functions.R`, and run the same query protocol.

The real source attributes in scope are normalized expression values,
dimensions, and sparsity. Export, storage, loading, memory, and access timings
are performance observations on a hybrid fixture: real MSSM expression plus a
deterministic synthetic shell with non-representative compression
characteristics. CRB and total stored bytes are not estimates of a full MSSM
application file. The full-scale result does not claim to validate original MSSM metadata,
biological groupings, reductions, spatial content, immune repertoires, or a
complete real Seurat object.

The Panel A common BPCells job starts from the shared materialized
`dgCMatrix`; the Panel B common job starts directly from the H5AD-backed
`IterableMatrix`. They are labeled as separate bridge observations and are not
combined into one homogeneous scaling curve.

Panel B itself contains only scheduled BPCells measurements. A separate
eligibility table records `embedded` and `h5` at the common tier as
`NOT_APPLICABLE_PROTOCOL` because Panel B is the direct-streaming protocol. At
larger tiers it records `UNSUPPORTED_DGCMATRIX_INDEX` when exact nnz exceeds
the 32-bit sparse index limit; otherwise it records
`NOT_APPLICABLE_PROTOCOL`. These are structural eligibility states, not failed
jobs or zero measurements.

## 7. Query-panel correctness protocol

Build the gene panel once from the 125k source selection, outside timed jobs and
before either measured schedule is executed:

- Compute per-gene non-zero counts in one streaming pass over 125k.
- Select five expressed genes spanning the observed non-zero distribution,
  breaking density ties by source row index.
- Put the gene nearest the median density first.
- Use that gene for the first and warmed single-gene queries.
- Use all five genes for the block query.

Freeze that ordered five-gene panel for every larger tier. Each tier recomputes
only those genes' tier-specific densities, expected values, and source
fingerprints. Panel B imports the same five genes from Panel A; it may not
reselect them. Query-plan setup necessarily reads and warms the local source,
so no source-side timing is presented as cold I/O.

Each access process measures:

1. CRB `readRDS()` time.
2. External backend attach time.
3. First single-gene query, returning a fully materialized numeric vector.
4. Five warmed calls for the same gene; retain every raw duration and report
   their within-process median only as a descriptive metric.
5. Five-gene block view preparation.
6. Explicit `as.matrix()` materialization of that block.

Correctness is checked outside the timed expressions. The source and exported
results must have identical SHA-256 fingerprints for the single-gene vector
and materialized five-gene block. Identity hashing rejects missing, duplicate,
or invalidly encoded IDs, applies `enc2utf8()` to the ordered IDs, serializes
them with XDR/version 3, and stores only `cell_identity_sha256`. The numeric
payload then contains its schema tag, dimensions, ordered gene IDs, that cell
identity hash, and column-major double values; it is separately serialized
with XDR/version 3 and SHA-256 hashed. Thus reordered or changed cell names,
gene names, dimensions, or values fail the gate without persisting millions of
names. The common tier's
source query-plan hash must be identical across Panel A and Panel B.
Fingerprint mismatch makes the job invalid; timing from that job is not
accepted.

This is a query-panel correctness gate, not a streaming digest of all matrix
values. Real evidence supports exact agreement for the frozen five-gene panel
only. A separate small product fixture verifies full-matrix round-trip behavior
for the public exporter; no full 4M matrix equality claim is made.

`queries.csv` records, per tier, the source SHA, subset/index SHA, gene, role,
density, tie-break rank, expected row/block fingerprints, and query-plan SHA.
The required `query-plan.rds` stores the typed plan metadata and expected
hashes consumed by workers, not the full expected numeric block. It does not
repeat millions of cell names: ordered cell identity is represented by source
identity, exact tier size, block boundaries, the ordered source-index
fingerprint, and a source-derived ordered cell-ID fingerprint.

Five genes across 4,140,453 cells materialize to roughly 158 MiB of double
values, large enough to exercise the path while remaining a bounded query.

## 8. Metrics

### Export process

- source open time;
- selected-tier materialization time (`Panel A` only);
- Seurat object or shell preparation time;
- export time;
- CRB bytes;
- sidecar bytes for external backends; this is `NA`, not zero, for embedded;
- total stored bytes;
- maximum R heap reported after a reset at measurement start;
- peak process RSS sampled by the parent every 500 ms;
- final status, failure stage, error text, and process exit status.

### Access process

- CRB load time;
- backend attach time for external backends; embedded records `NA` plus an
  explicit non-applicability flag/reason;
- first-gene materialized query time;
- five raw warmed single-gene times and within-process median;
- block-view preparation time;
- block materialization time;
- total time to a materialized block;
- peak process RSS sampled every 500 ms;
- source and result fingerprints;
- correctness result;
- final status, failure stage, error text, and process exit status.

The benchmark does not call first access a cold-disk measurement. OS page cache
is not controlled. The warmed metric means five repeated calls for the same
median-density gene; it is not a general estimate of warm-gene performance.
Peak RSS is explicitly a 500 ms sampled peak, not an exact kernel high-water
mark; it covers the child lifetime including package startup. The R-heap peak
is reset at worker-stage entry and therefore excludes earlier R startup.
Source-open time measures construction of the source handle, not source read
throughput. Stored bytes should be deterministic; repeats verify
consistency and do not supply inferential uncertainty intervals.

Total stored bytes is the comparable complete-artifact footprint within this
synthetic-shell protocol. External sidecar bytes isolate expression storage;
embedded expression cannot be separated from its CRB and therefore records
`sidecar_bytes = NA`, `sidecar_bytes_applicable = FALSE`, and reason
`embedded_has_no_sidecar`. No zero is used for a structurally inapplicable
metric, and sidecar-only values are never compared across all three backends.

### 8.1 Descriptive aggregation

Raw worker rows remain primary evidence. For each `(panel, tier, backend)`
condition, the technical-pair count is `n = 3` in Panel A or `n = 4` in Panel
B. Continuous timing and memory metrics are reported as median `[min, max]`
with that `n`; no confidence interval or hypothesis test is computed. Panel A
also reports within-repeat backend-to-embedded differences or ratios before
summarizing their median and range. The five warmed calls are summarized only
inside their one access worker and never counted as `n = 5` replicates.

Panel B lists the four observed tiers with median/range only. It does not fit a
line, infer complexity, or extrapolate capacity. Deterministic stored-byte
values are shown as exact consistency observations; any disagreement across
repeats is flagged rather than converted into an uncertainty estimate.

## 9. Process and failure model

The parent runner uses `callr::r_bg()` for one worker at a time. Serial workers
avoid resource contention and make the recorded order meaningful. While a
worker runs, the parent samples `ps -o rss= -p <pid>` every 500 ms.

Before reference/query manifests are finalized, the current Git tree is
installed into a marked run-local library. An unmeasured setup worker using
that library first, user/system profiles disabled, and the same dependency
library path validates the source, freezes sampling, selects/imports the gene
panel, and produces source query/shell fingerprints. Its loaded package
versions form a runtime fingerprint in `manifest.csv`. Setup and validation
workers are protocol preparation and are not included in the 54/32 measured
worker counts.

The runtime fingerprint hashes R version/platform plus sorted
`package=version` entries for CerebroNexus, Matrix, SeuratObject, Seurat,
BPCells, HDF5Array, rhdf5, digest, and callr. `manifest.csv` also records each
normalized library path for diagnosis, but paths are not part of the
cross-panel hash because the two run-local roots intentionally differ. Each
run's CerebroNexus path must be inside its marked run-local library; measured
workers record and validate that same run-local origin.

Before each stage the worker atomically writes its current stage to a tiny state
file. If R exits normally, the returned measurement row is written to CSV. If
it errors, is killed, or is OOM-terminated, the parent records the last stage,
exit status, sampled peak RSS when available, and log path. Peak RSS remains
missing when no finite sample was captured; it is never converted to zero.

Static eligibility uses only `SCHEDULED`, `NOT_APPLICABLE_PROTOCOL`, and
`UNSUPPORTED_DGCMATRIX_INDEX`. Export/access outcomes use `OK` or
`FAILED_<stage>`; an access row whose export failed uses
`NOT_RUN_EXPORT_FAILED`. `MISSING_RESULT` is created only by the validator when
an expected outcome row is absent. Any scheduled failure, missing row,
duplicate, unscheduled row, or fingerprint mismatch makes that panel invalid.
All four full-tier export/access pairs must be `OK` for the 4M endpoint to be a
valid core result.

Each job writes into a marked scratch child owned by the run. Only that exact
child may be recursively removed. Per-repeat export/access batching caps live
complete artifacts at nine for Panel A and four for Panel B. Failed partial
artifacts are removed immediately after their outcome and log are preserved;
successful exports are removed after their access attempt, whether that access
succeeds or fails. Scratch is a marked child of the new output directory, so
the operator selects its filesystem by selecting the output path. There is no
automatic memory or disk resource gate; a disk-full/OOM event is a recorded
failure and never changes a tier.
Logs and raw CSV remain. The harness has no generic resume, publication, or
mutable-current mechanism.

## 10. Provenance and outputs

Panel A requires a non-existent output path and creates it as a marked run
directory. Its nearest existing parent must be outside the Git worktree:

```text
Rscript --vanilla tests/bench/run_comparison.R <local-mssm.h5ad> <new-panel-a-dir>
```

Panel B consumes the frozen Panel A protocol and requires a different,
non-existent output path for its own marked run directory:

```text
Rscript --vanilla tests/bench/run_full_scale.R \
  <local-mssm.h5ad> <panel-a-dir> <new-panel-b-dir>
```

Panel B's output path is also required outside the worktree. Git cleanliness is
checked before either runner creates its path, so the runner cannot dirty its
own provenance target.

Before any Panel B worker starts, it strictly matches source SHA, Git SHA,
schema/config version, `common_target_actual`, sampling/index hash, synthetic
shell fingerprint, common query-plan hash, and runtime fingerprint to Panel A.
It also requires a well-formed Panel A `validation.csv` with exactly one final
`VALID` row, 27 expected `OK` exports, 27 expected `OK` accesses, and no failed,
not-run, missing, duplicate, unscheduled, or fingerprint-mismatched gate.
After installing its run-local tree, Panel B recomputes the common source,
sampling, shell, cell-identity, and query hashes with a setup worker before any
measured worker starts. `--dry-run` is the only mode that needs neither source
nor manifests; it prints an explicitly unqualified target schedule.

Each panel output directory receives:

```text
manifest.csv       Git, R, package, OS, CPU, memory, command and timestamps
source.csv         URL, expected/actual bytes and SHA-256, dimensions and nnz
sampling.csv       tier sizes, four block boundaries, exact nnz and index hash
eligibility.csv    protocol applicability and structural N/A reasons
queries.csv        frozen gene roles/densities and expected fingerprint hashes
query-plan.rds     canonical hashed query payload used for panel linkage
schedule.csv       expected pair IDs plus separate export/access order
export.csv         one raw export outcome per scheduled job
access.csv         one outcome per expected access, including not-run rows
validation.csv     expected/observed coverage gates and final VALID/INVALID
logs/              one export/access log per job
```

Measured values and configuration-derived estimates have separate columns and
must never share a field. Validation checks expected IDs, duplicates,
unscheduled rows, missing outcomes, source/sampling/shell/query fingerprints,
and all required full-tier pairs. The runner writes raw failure evidence and
`validation.csv`, then exits non-zero when the panel is invalid. The exact Git
SHA and dirty state are recorded; a real evidence run requires a clean
worktree.

## 11. Repository architecture

```text
tests/bench/
├── README.md
├── config.R
├── helpers.R
├── run_comparison.R
├── run_full_scale.R
└── results/
    └── README.md
```

- `config.R` contains the one pinned source, exact tiers, repetitions, query
  size, RSS interval, and backend order.
- `helpers.R` contains pure sampling/scheduling/fingerprint functions plus the
  source, Seurat-shell, worker, process-monitor, provenance, and CSV helpers.
- `run_comparison.R` is the complete Panel A entry point.
- `run_full_scale.R` is the complete Panel B entry point.
- `results/` contains only deliberately reviewed evidence, not scratch output
  or a publication pointer.

Focused package tests live in:

- `tests/testthat/test-exportFromSeurat.R` for true IterableMatrix streaming;
- `tests/testthat/test-benchmark-contract.R` for the small-data harness
  contracts when running from a repository checkout.

`tests/bench/` remains excluded by `.Rbuildignore`. Therefore every test in
`test-benchmark-contract.R` begins with a shared `skip_unless_bench_tree()`
guard before sourcing harness files. These contracts run fully under
checkout-level `devtools::test()` and are expected to skip under `R CMD check`
of the built tarball. Product exporter tests do not depend on `tests/bench` and
must still execute in the built package.

## 12. Minimal product change

The current expression resolver in `R/seurat_utils.R` rejects disk-backed
assays, and the `bpcells` branch in `R/exportFromSeurat.R` later assumes every
non-dense input can eventually be treated as `dgCMatrix` and accesses
`expression_data@x`. A normal public export therefore cannot stream a
BPCells-backed Seurat layer.

The product change is deliberately narrow:

- add a narrowly scoped resolver opt-in used only when
  `expression_matrix_mode == "bpcells"`;
- accept one complete BPCells `IterableMatrix` layer through the normal public
  Seurat lookup and cell-coverage validation;
- reject split BPCells layers explicitly rather than joining them or selecting
  the first layer; this rejection occurs while resolving the partition set,
  before the current early return of a disk-backed member;
- apply non-empty dimension checks to an allowed `IterableMatrix`, just as for
  in-memory matrices;
- keep embedded and H5 source handling unchanged and rejecting disk-backed
  input;
- recognize `inherits(expression_data, "IterableMatrix")` in the BPCells
  writer;
- pass it directly to `BPCells::write_matrix_dir()`;
- do not inspect `@x` or coerce it through `CsparseMatrix`;
- retain existing integer bit-packing for ordinary `dgCMatrix` inputs;
- retain the relative sidecar descriptor and staged publication path;
- leave embedded, H5, dense, DelayedArray, and ordinary sparse behavior
  unchanged.

No Temp file is copied wholesale. All paths and runtime checks remain the v4.0
versionless paths.

## 13. Baseline code disposition

### Retain as behavior or extract into the flat harness

- fresh-process export/access isolation;
- backend-order rotation;
- timing, path-size and RSS helpers;
- deterministic gene selection and numeric fingerprints;
- source/Git/environment provenance;
- versionless runtime attach through `inst/viewer/utility_functions.R`.

### Replace

- `tests/bench/README.md`;
- `tests/bench/config/sources.R`;
- the useful portions of `lib/access_metrics.R`, `lib/bench_utils.R`, and
  `lib/make_seurat.R`;
- the numbered source scripts and `run_sweep.sh` with the two entry points;
- all existing benchmark contract files with one focused contract file;
- the benchmark vignette only after validated results exist.

### Remove

- quick / standard / publication / stress profiles;
- `tests/bench/design/`;
- `tests/bench/RESULTS.md` and `tests/bench/METHODOLOGY.md`;
- `lib/protocol.R`, `lib/reporting.R`, `lib/resource_planning.R`, and the ROS3
  remote-reader framework;
- all `tests/bench/src/01_*` through `60_*` scripts;
- `tests/bench/run_sweep.sh`;
- immutable publication, `CURRENT`, archived pilot and plotting pipeline;
- the seven old `tests/testthat/test-bench-*.R` files;
- stale benchmark images and the 2020 hard-coded DelayedArray benchmark.

`rhdf5` is not removed: the new repository-level testthat benchmark contract
reads a tiny H5AD fixture directly, so it remains in `DESCRIPTION` `Suggests`.
`digest` is added to `Suggests` for canonical SHA-256 fingerprints. Both are
declared in `create_env.R` before regenerating `default.nix`.

## 14. Acceptance criteria

### 14.1 Implementation acceptance

Implementation is accepted only when:

1. A focused public API test first fails on v4.0 at the resolver's disk-backed
   rejection. After the narrow opt-in, it reaches the BPCells writer and passes
   without any `@x` access or materialization of the IterableMatrix.
2. Small synthetic contract tests prove exact tier sizes and actual set
   inclusion for arbitrary sizes under the fixed four-stratum protocol and
   full 4,140,453 coverage, four-block boundaries,
   the exact export/access rotations, 27 Panel A pairs / 54 workers, 16 Panel B
   pairs / 32 workers, and four scheduled full-data pairs.
3. Exact nnz freezes the largest admissible integer tier at or below 500k.
   `common_target_actual <= 250000` or any illegal scheduled Panel A tier
   rejects the run before its first measured export/access worker; memory and
   disk size never change a tier.
4. The public API test uses a real BPCells-backed Seurat layer and never passes
   `.expression_resolution`; split BPCells layers fail explicitly.
5. A killed synthetic worker produces a failure row with last stage and exit
   status rather than disappearing.
6. Query tests prove that lazy blocks are explicitly materialized and that
   changed values, vector names/order, matrix dimnames/order, duplicate/missing
   identities, or invalid text encoding fail the five-gene query-panel gate.
   The small public-export fixture separately verifies its full matrix.
7. A small BPCells-backed CRB can be moved with its sibling, attached through
   the versionless Viewer helper, and queried with exact values.
8. Eligibility and runtime outcome vocabularies remain separate.
   `validation.csv` detects duplicates, unscheduled or missing rows, failed
   exports/accesses, and all provenance/fingerprint mismatches, retains raw
   failure rows, and makes the runner exit non-zero.
9. Panel B refuses a Panel A directory unless it has one final `VALID` gate,
   all 27 expected exports and accesses are `OK`, and source, Git revision,
   config/schema, runtime, common tier, sampling, shell, cell-identity, and
   common query-plan hashes all match a post-install local recomputation.
10. Checkout-level `devtools::test()` runs the full repository harness
    contracts; built-tarball `R CMD check --no-manual` skips only those guarded
    repo-only contracts while still executing product API tests. Focused tests,
    the complete checkout suite, package installation, build/check, and
    `git diff --check` all pass.
11. No real 4M download or full benchmark is run during implementation.
12. No Builder branch/worktree, remote branch, PR, backup ref, or Git history
    is modified. Only obsolete tracked benchmark artifacts explicitly listed
    in this design are deleted from the feature branch.

### 14.2 Evidence acceptance

Real evidence is publishable only when Panel A has 27 `OK` export rows and 27
`OK` access rows, Panel B has 16 of each, all required fingerprints match, both
panels are `VALID`, and all four 4,140,453-cell technical pairs pass. Reported
trends are descriptive observations for this pinned normalized MSSM expression
matrix, synthetic shell, software revision, and one host. They do not support
capacity extrapolation, linear-scaling claims, confidence intervals, or
biological inference. Final tables use the frozen median `[min, max]` rules and
technical-pair `n`; the five warmed calls never inflate that `n`.

## 15. Final history

The intended final branch history is:

1. `docs(bench): design simplified real-data benchmark`
2. `feat(bench): implement controlled and full-scale benchmarks`
3. `docs(bench): publish validated benchmark results`

The third commit is created only after the real runs finish on an appropriate
host and their raw evidence has been reviewed.
