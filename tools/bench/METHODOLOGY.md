# Expression-backend benchmark methodology

## Research question

For the same Cerebro expression matrix and the same public source cells, how do the `embedded`, `bpcells`, and `h5` backends differ in export cost, stored size, fresh-process startup, process memory, single-gene access, and a fully materialized 12-gene block read? A separate exploratory sweep asks where the current in-memory Seurat export path stops working as the non-zero count grows.

The benchmark does not compare biological methods, remote-network throughput, or true cold-disk behaviour.

## Experimental unit and replication

An export process is the experimental unit for export time, disk size, and peak R heap. A fresh access process is the experimental unit for startup, resident memory, and query timings. Repeated getter calls inside one access process estimate its warmed-query distribution; they are not counted as independent replicates.

The backend order rotates deterministically across three export repeats:

1. `embedded`, `bpcells`, `h5`;
2. `bpcells`, `h5`, `embedded`;
3. `h5`, `embedded`, `bpcells`.

Each backend therefore occupies every order position once on a repeated tier. The schedule is written to `05_schedule.csv` before data transfer begins. The resource checker then evaluates every distinct source/tier against the recorded host memory, R vector limit, free disk, and sparse-index limit.

## Run profiles

| profile | comparison-tier exports | accesses per export | use |
|---|---:|---:|---|
| `quick` | 1 | 1 | smoke-check the harness |
| `standard` | 3 | 1 | local review and debugging |
| `publication` | 3 | 2 | evidence for the pkgdown article |
| `stress` | 1 | 1 | explicit host memory-boundary experiment |

The `quick` profile runs only the smallest comparison tier for each selected source. Normal profiles exclude large tiers that exist only to locate a pass/fail boundary. Those tiers belong to `stress`, and resource preflight rejects them unless the recorded host has a safe budget or the operator uses an explicit unsafe override. Only `publication` results may regenerate the article figures.

## Resource preflight

The scheduling guard estimates peak R heap as 64 bytes per expected non-zero plus 1 GiB fixed overhead. It permits at most 70% of the smaller of physical RAM and R's vector-memory limit, and at most 80% of currently free disk for a source download. The constants are conservative operational limits, not fitted scientific results. A plan also fails when its expected non-zero count exceeds the 32-bit sparse-index limit.

Unsafe tiers are never silently removed. The run stops before bulk transfer and prints an actionable reason. `BENCH_ALLOW_UNSAFE=1` is reserved for an intentional `stress` run.

## Data and sampling

The default sources are a 10x mouse-brain H5 matrix and a CELLxGENE human-PFC H5AD matrix. Both are downloaded once per run, read locally, hashed with SHA-256, and deleted with the scratch tree. The source URL, byte size, and hash are stored in `source_manifest.csv`.

Each source registry entry declares the expression slot passed to `exportFromSeurat()`. The mouse source exercises raw integer `counts`; the HBCC and opt-in MSSM sources exercise normalized floating-point `data`. This prevents a benchmark of normalized input from silently taking the counts-only path.

Each tier contains four evenly spaced contiguous cell runs. Contiguity keeps each source read to a small number of HDF5 hyperslabs; spacing avoids measuring only the first donors in a donor-ordered file. The subsets are suitable for storage and access measurements, not biological inference.

## Query panel and cache semantics

The export process counts non-zero entries per gene and deterministically picks 12 expressed genes spanning the observed density range. The gene nearest the median density is reserved for the first query; the rest form the warmed-query and block-read panel.

The plan and its reference fingerprints are written before the CRB is loaded in an access process. Consequently, the first backend getter call is the timed first query. It is called a **fresh-process first query**, not a cold-disk query: the benchmark cannot evict the operating-system page cache without privileged, platform-specific operations. After the first query, every hot-panel gene is warmed once and then measured in deterministic repeated passes. Publication runs make three observations per warmed gene (33 warmed observations per process). The independent process, not each within-process query, is the statistical unit.

The block measurement separates backend-native view construction from dense
materialization. `getExpressionBlock()` is timed first as preparation; then
`as.matrix()` is timed around the returned block. Their sum is the comparable
time-to-materialized-block metric. This distinction matters because H5 and
BPCells can return lazy objects while the embedded backend returns an eager
subset.

## Correctness gate

Speed is not accepted without value equality. The source matrix supplies an XDR serialized fingerprint for the first row and for the full 12-gene block. Every access process recomputes both fingerprints outside the timed expressions. A mismatch marks the process as failed, and validation refuses to publish the run.

## Metrics

| metric | experimental meaning |
|---|---|
| `read_secs` | construct the sampled source `dgCMatrix` |
| `seurat_secs` | wrap that matrix in the minimum exportable Seurat object |
| `export_secs` | call `exportFromSeurat()` for one backend |
| `crb_mb`, `sibling_mb`, `total_mb` | stored bytes after export |
| `r_peak_mb` | maximum R heap observed across read, Seurat construction, and export |
| `load_secs`, `attach_secs` | fresh-process CRB load and backend attachment |
| `rss_mb` | process RSS immediately after load and attach |
| `first_query_secs` | first backend getter call in that R process |
| `hot_p50_secs`, `hot_p95_secs` | within-process warmed single-gene distribution |
| `block_prepare_secs` | construct the backend-native 12-gene block view |
| `block_materialize_secs` | convert that view to a dense ordinary R matrix |
| `block_ready_secs` | preparation plus materialization; the comparable block-read metric |

Reports show median, minimum, maximum, and the number of independent processes. They retain raw per-process rows in the CSVs rather than treating within-process queries as independent samples.

The report also derives two server-side readiness proxies. `backend ready` is `load_secs + attach_secs`; `first gene ready` additionally includes `first_query_secs`. These connect backend mechanics to a user-relevant wait without pretending to measure the Shiny session handshake, browser rendering, or network latency.

## Provenance and publication

`run_manifest.csv` records the run ID, profile, exact Git SHA and dirty state, the `default.nix` Git blob identity, repository and installed package versions, R and dependency versions, operating system, CPU, logical cores, RAM, and R vector-memory limit. The manifest is finalized after installing the branch into the isolated library, and validation requires the repository and installed CerebroNexus versions to match. The exact clean Git SHA therefore pins both the CerebroNexus code and its dependency-environment definition. A publication-profile run is rejected when the Git worktree is dirty.

Every source registry entry pins both byte size and SHA-256. A changed download
is rejected before any backend measurement rather than being silently compared
with an older run.

All files are written to scratch. Measurements are validated before report and figure generation; the complete staged package is checked again before it is copied into `result/runs/<run-id>/`. `result/CURRENT` is replaced last. A crash before that pointer update leaves the previous evidence current and keeps every older immutable run available.

## Interpretation boundaries

- Runtime comparisons apply to the recorded host, dependencies, data hashes, cell tiers, and query panel. They are not cross-platform confidence intervals.
- The first-query metric does not control the operating-system disk cache.
- The observed-scaling figure connects successful source/tier points only. It does not estimate capacity. An observed stopping boundary requires a stress run with recorded failures.
- A full source exceeding the `Matrix::dgCMatrix` 32-bit non-zero limit cannot enter the current Seurat exporter, but that statement does not apply to every sparse representation available in R.
- A profile named `publication` is an internal evidence gate, not a claim of academic peer review.

## Reproduction

```bash
BENCH_PROFILE=quick tools/bench/run_sweep.sh
BENCH_PROFILE=standard tools/bench/run_sweep.sh
BENCH_PROFILE=publication tools/bench/run_sweep.sh
BENCH_PROFILE=stress tools/bench/run_sweep.sh

# Run repository-only contracts without downloading real data.
bash tools/bench/run_contract_tests.sh
```

Set `BENCH_KEEP=1` to retain the unique scratch directory for diagnosis. Set `BENCH_SOURCES_EXTRA=human_pfc_mssm` to include the opt-in 4.1-million-cell source, or `BENCH_SOURCES_ONLY=mouse_brain_e18` to limit a diagnostic run to one configured source. `BENCH_SCRATCH_PARENT` selects a parent directory; the harness always creates and deletes its own marked child rather than deleting a caller-provided path.
