# Expression backend benchmark: embedded vs bpcells vs h5 at scale

## Status

The CerebroNexus 4.2 real-data benchmark protocol and harness are
available for review. No validated full-scale result is currently
committed, so this article makes no performance ranking or capacity
claim.

## Protocol

The benchmark uses one pinned local H5AD from the MSSM human prefrontal
cortex atlas. Its sparse `/X` matrix supplies normalized floating-point
expression (`data`), not raw counts. Source bytes and SHA-256 are
verified before setup, and download time is outside the benchmark.

Panel A compares `embedded`, `bpcells`, and `h5` using the same
materialized `dgCMatrix` at 125,000 cells, 250,000 cells, and a
predeclared common target capped at 500,000 cells. Exact sparse-index
counts may reduce only that common target. Three fresh-process technical
pairs per condition produce 27 export/access pairs and 54 measured
workers.

Panel B measures the public BPCells streaming export path at the frozen
common tier, 1 million cells, 2 million cells, and all 4,140,453 cells.
Four fresh-process pairs per tier produce 16 pairs and 32 measured
workers. Panel B starts only after strict validation of Panel A’s frozen
source, sampling, runtime, shell, cell-identity, and five-gene query
manifests.

Each repeat batches exports before accesses and uses different fixed
rotations for those phases. The fixture keeps real expression,
dimensions, and sparsity, but its sample/cluster fields, zero QC fields,
and two-dimensional sine/cosine projection are deterministic synthetic
shell data. Results therefore do not describe original MSSM metadata or
biology.

## Measurements and limits

Workers record export, CRB load, external attach, first and warmed
single-gene access, block preparation and explicit materialization,
stored bytes, R heap, and sampled peak RSS. Five frozen genes provide a
bounded correctness gate; this is not a full-matrix digest. Embedded
sidecar bytes are structurally inapplicable and remain `NA`, never zero.

The OS page cache is uncontrolled, setup warms the source, and all
repeats are technical observations on one host. Analysis is limited to
raw outcomes and descriptive median `[min, max]` summaries. It must not
infer confidence intervals, biological effects, general capacity, or
scaling beyond measured tiers.

The complete operator guide, commands, schemas, validation rules, and
pinned source identity live in `tests/bench/README.md`. Evidence will be
added only after the complete runs pass validation and receive manual
review.
