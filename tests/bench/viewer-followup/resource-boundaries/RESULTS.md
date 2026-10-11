# Resource boundaries and external-request robustness

2026-10-11. Branch `perf/pr6-ui-accessibility-polish`. Baseline:
`066df6f05059ba5bda4d09ef70f5ab904fe7d879`. The workspace was clean: all prior
IR work was already committed, so no empty baseline commit was created.
The candidate is the commit containing this report. No branch/worktree change
or push; no Viewer performance benchmark, new profile, or browser acceptance
claim in this task.

## Changes

### Delayed expression export

Removed whole-matrix `as.matrix()` from both external export paths. H5 now
passes the lazy transpose directly to HDF5Array's block writer. BPCells uses
a temporary block-written TENx HDF5 bridge in the private export staging
directory, streams that into the existing gene-major writer, then removes
the bridge. Export-stage cleanup also covers failures. Delayed BPCells export
now additionally requires HDF5Array; this dependency is documented and fails
explicitly when absent instead of attempting dense realization.

A blockwise exact integer check preserves the existing uint32 packing for
non-negative integer counts within the existing range. Fractional values stay
double. Existing IterableMatrix streaming is unchanged. This trades temporary
disk space and additional passes for avoiding a full dense RAM allocation;
it is not a claim of faster export or a fixed total RSS ceiling.

### CRB prototypes and prefetch

The process retains at most two cached prototypes, evicting the least recently
used entry. Startup cataloguing likewise retains only the first two prototypes;
all dataset information is still catalogued. Launch closure/options references
are released after handing ownership to the server, and the one-time handoff
is cleared after seeding the cache. Active session clones remain valid after
eviction. The small backend-identity registry survives eviction so a changed
configuration cannot silently reuse an old path.

Prefetch tasks are now shared by path across sessions in the same process,
with at most two outstanding tasks. Process-owned completion publishes the
prototype and releases the task's retained result, including when its initiating
session disconnects or no longer selects that dataset. Errors release the task
and allow retry; the existing synchronous loading fallback remains available.
Dataset publication continues through the existing generation/epoch checks.

These are retention/concurrency limits, **not a byte or whole-process memory
guarantee**. Individual datasets, active session clones, other caches and R/worker
allocation still contribute to RSS. R6 `object.size()` is not used to pretend
to measure referenced payloads. Evicted datasets may require another read.

### Gene-set queries

On public msigdbr APIs supporting collection enumeration, catalogue construction
queries one collection at a time and retains only distinct name/collection
pairs. Older APIs retain the existing compatibility fallback. The package's
own internal allocation/cache behavior is outside this change.

The most recent small collection can retain a compact set-to-gene mapping,
capped at 8 MiB; wide query tables are discarded. Resolved individual sets have
a separate 4 MiB / 128-entry FIFO retention limit. Queries for another set in
the retained collection reuse the mapping; species remains part of identity.
Oversized results are returned without retention. Removed explicit synchronous
`gc()` calls. No private msigdbr API or persistent full-database cache was added.
Initial query latency and live MSigDB service behavior were not benchmarked.

### Enrichr

Submission, share and each database export have a 60-second per-request timeout
and HTTP success checking before parsing. Invalid gene inputs stop before any
request, and text connections close even on parsing errors. This is not a
60-second whole-query budget across all databases, nor an asynchronous Viewer
change. The existing future configuration and result semantics are retained.

## Validation

Windows checkout, WSL R 4.6.1. Installed versions: Shiny 1.14.0, mirai 2.7.3,
HDF5Array 1.40.0, DelayedArray 0.38.2, BPCells 0.3.1, msigdbr 26.1.1,
httr 1.4.9, testthat 3.3.2.

`run-tests.R`, `tests.log`, `tests.csv` record the final seven-file run:

| Scope | Passed assertions | Failed | Skipped |
|---|---:|---:|---:|
| New resource-boundary regressions | 60 | 0 | 0 |
| Utility, export, launch, Viewer review and sync compatibility | 706 | 0 | 1 |
| Expanded Builder sibling suite | 509 | 8 | 0 |
| Total | 1275 | 8 | 1 |

Zero unexpected warnings or test errors. The skipped test exercises absence of
HDF5Array, which is installed here. The runner intentionally exits nonzero for
the eight failures; this is not reported as a fully green suite.

The eight Builder assertions belong to three existing tests: duplicate CRB
paths under two labels (six assertions), and label-versus-identity wording
(two assertions). `check-existing-failures.R` restores the baseline definition
of `.datasetCatalogFromFiles` in a disposable R process—the only function
changed in `R/createShinyApp.R`—and evaluates those same three tests. It
reproduces all eight failures (`baseline-existing-failures.log`). No checkout
or production behavior was changed to accommodate obsolete Builder assertions.

New coverage includes:

- Exact delayed/Rle export round trips through both backends, dimnames/order,
  double precision, uint32 integer packing and gene-major storage. A delayed
  matrix subclass rejects whole-input `as.matrix()` calls.
- Cache eviction with a still-live clone; backend identity rejection after
  eviction; bounded startup prototype retention.
- Two real Shiny mock session domains sharing one ExtendedTask/mirai read,
  the two-task limit, initiating-session disconnect, completion cleanup,
  failed-read cleanup/retry and completed-cache reuse.
- Collection-wise catalogue calls, same-collection reuse, species separation
  and small-result cache limits; legacy malformed-response/retry tests.
- Timeout configuration on all three HTTP stages and rejection of a 503 at
  each stage before later requests/parsing. HTTP and MSigDB fixtures are local
  mocks; no claim of live external service acceptance.

## Deliberately unchanged

Diversity still computes synchronously on an uncached request. The existing
20-bootstrap measurement (~268 ms) does not establish a multi-second bottleneck
requiring a new worker lifecycle; no unmeasured asynchronous rewrite was added.
Rarefaction's renderer exists but its normal page entry is commented out.
Their synchronous nature is a remaining conditional responsiveness limitation,
not something this patch claims to eliminate. The prior grouped Homeostasis
fix and its acceptance remain intact.

Spatial Moran's I already has its 2,000-cell cap, page gate and cache, and was
not changed. Viewer expression reading, geometry, RGB, wire precision,
ready/RAF and the existing dataset-generation architecture remain unchanged.
