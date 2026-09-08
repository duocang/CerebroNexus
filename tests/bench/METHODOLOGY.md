# Expression-backend benchmark methodology

## Research question

For the same public expression matrices and deterministic query workloads, how
do Cerebro's `embedded`, `bpcells`, and `h5` backends trade build time, stored
size, process memory, fresh-process startup, warmed single-gene latency, and a
12-gene block read as cell count grows?

The study is descriptive engineering evidence. It does not compare biological
methods, test statistical significance, control the operating-system page
cache, or claim cross-machine generality.

## One complete publication study

`run_publication_full.sh` acquires three phases under one study ID:

| phase | source tiers | backends |
|---|---|---|
| A/B | mouse and human at 50k and 150k cells | embedded, bpcells, h5 |
| C1 | mouse 400k and human 300k | embedded, bpcells, h5 |
| C2 | complete mouse and human sources | bpcells, h5 |

All phases must share the same Git SHA, clean worktree, R and dependency
versions, CPU, OS, thread count, storage description, and acquired source
SHA-256 values. Any drift aborts the combined report. `embedded` is omitted
from C2 because both full matrices exceed the 32-bit non-zero index limit of
`Matrix::dgCMatrix`; the report labels it `not representable` rather than zero
or failed.

## Sources and sampling

The sources are the 10x 1.3-million-cell mouse brain E18 dataset (`GSE93421`,
`SRP096558`) and the CELLxGENE PsychAD HBCC human prefrontal-cortex dataset
(dataset `d27fb144-f105-46c2-b36f-f51421f74e4e`, collection
`84ce6837-548d-4a1f-919f-0bc0d9a3952f`, DOI
`10.1038/s41597-025-04687-5`). Downloads live outside Git in a persistent
cache and are reused only after byte-size and SHA-256 verification.

Sampled tiers contain four evenly spaced contiguous cell runs. This limits HDF5
hyperslabs while avoiding a prefix-only sample. The samples support storage and
runtime measurement, not biological inference. C2 opens the complete sources
lazily and streams expression to BPCells or TENx HDF5 without constructing a
full `dgCMatrix`.

## Experimental units and order

Each backend build runs in a fresh process and is an independent observation
for build time, stored size, R heap, and process peak RSS. Each access repeat
runs in a fresh process and is an independent observation for load, attach,
resident memory, peak RSS, and query timing. Calls repeated inside one access
process describe that process's warmed-query distribution and are not treated
as independent replicates.

Every tier has three builds and two access processes per build. Backend order
rotates deterministically across the three builds, so every backend occupies
each order position once when three backends are compared. Processes run
sequentially with a fixed thread count on an exclusive node.

## Query plan and cache semantics

For each source/tier, a separate process reads or lazily opens the source,
selects 12 expressed genes across the observed density range, computes source
fingerprints, and atomically freezes one query plan. Its preparation time and
peak RSS are stored in `query_plan_manifest.csv` but excluded from all timed
backend builds. `query_panel.csv` retains the exact gene order, roles, density,
and reference fingerprints used by every process.

Every build and access process reads this exact plan. Validation requires one
matching fingerprint across preparation, all builds, and all access processes.
The first getter call is a fresh-process first query, not a cold-disk query:
source preparation and earlier runs may warm the operating-system cache. The
remaining genes are warmed once, then queried in deterministic repeated passes.
Publication access rows contain 33 warmed observations per process.

## Metrics

| metric | interpretation |
|---|---|
| `source_prepare_secs`, `query_plan_secs` | untimed study preparation, reported separately |
| `read_secs` | sampled source construction or lazy full-source open |
| `seurat_secs` | sampled Seurat shell construction; zero for C2 |
| `export_secs` | backend build only |
| `crb_mb`, `sibling_mb`, `total_mb` | stored artifact size |
| `r_peak_mb`, `peak_rss_mb` | R heap and whole-process peak during the build process |
| `load_secs`, `attach_secs` | fresh-process CRB load and backend attachment |
| `rss_mb`, `peak_rss_mb` | attached-process resident and peak memory |
| `first_query_secs` | first getter call in that process |
| `hot_p50_secs`, `hot_p95_secs` | within-process warmed single-gene distribution |
| `block_secs` | one deterministic 12-gene by all-cell read |

For C2, build peak RSS includes lazy source opening, backend writing, and shell
serialization, but excludes query-plan preparation. Reports retain raw rows and
show median, observed minimum/maximum, and independent-process `n`. Matched
backend ratios are computed only within the same source, tier, phase, and
metric.

## Correctness and provenance gates

Each access process recomputes deterministic fingerprints for the first gene
and 12-gene block outside timed expressions. Any failed status, value mismatch,
missing scheduled row, duplicate tier plan, source-hash drift, protocol drift,
or environment drift invalidates publication.

`run_manifest.csv` records the study/run IDs, clean Git SHA, package version,
R platform, key dependency versions, OS, CPU, allocated threads, scheduler
metadata, RAM and vector limits, scratch filesystem, and operator-supplied
storage description. Stable source identifiers and hashes are preserved in the
final `source_provenance.csv`.

## Resource and publication safety

Sampled phases are rejected when their conservative memory, sparse-index, or
disk estimates exceed the recorded host budget. C2 has a separate out-of-core
resource gate. Unsafe tiers are never silently removed.

Each phase first publishes immutably inside marked study work. The final bundle
freezes all three raw phase directories, validates them together, creates the
tables and figure, and is then copied atomically to
`result/publication-full/runs/<study-id>/`; `CURRENT` changes last. A failed or
interrupted study cannot replace previous evidence. A stopped study can resume
by reusing the same explicit `BENCH_STUDY_ID`.

## Interpretation boundaries

- Results apply to the recorded host, filesystem, code, dependencies, sources,
  cell tiers, query plan, and fixed thread count.
- First-query results are warm-cache-compatible, not controlled cold-disk I/O.
- Median and range describe observed process variation; they are not confidence
  intervals and no significance test is performed.
- Synthetic metadata and projection in C2 exercise the expression backend and
  portable Cerebro shell, not end-to-end biological analysis or Viewer UX.
- A second host is required before claiming cross-machine generality; browser
  experiments are required before claiming million-cell interactive usability.

## Reproduction

```bash
BENCH_THREADS=1 \
  BENCH_SOURCE_CACHE=/persistent/cache \
  BENCH_SCRATCH_PARENT=/local/scratch \
  BENCH_STORAGE_DESCRIPTION="local NVMe; ext4; model=<model>" \
  tests/bench/run_publication_full.sh
```

Use `BENCH_KEEP=1` only to retain a failed phase scratch directory for diagnosis.
Use `BENCH_KEEP_STUDY_WORK=1` to retain the completed study work after final
publication.
