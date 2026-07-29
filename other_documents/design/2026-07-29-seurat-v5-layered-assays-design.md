# Seurat v5 Layered Assays: Complete Resolution Design

**Status:** Approved for implementation in pull request #101
**Date:** 2026-07-29
**Target release:** CerebroNexus 3.0.3
**Upstream pull request:** `duocang:fix/v5-split-layers` → `mihem:master`

**Companion:** [Implementation plan](../plans/2026-07-29-seurat-v5-layered-assays.md)

## Executive summary

Seurat v5 assays may store one logical expression layer as several physical
layers, commonly one per sample. CerebroNexus must turn that representation into
one expression matrix before it exports data or performs calculations that span
the complete object.

This is not primarily a naming problem. Names such as `data.s1` may describe a
real sample partition, but names such as `data.imputed`, `dataBackup`, or
`data.batch_corrected` may describe unrelated custom matrices. Seurat's own
`JoinLayers()` uses prefix matching, so joining `data` can consume all of these
names. Conversely, relying on a fixed list of known roots misses valid custom
split roots such as `ambient.s1` and `ambient.s2`.

The complete solution is a request-driven expression-layer resolver:

1. honour an exact layer request without rewriting it;
2. when the requested root does not exist, inspect the same prefix candidate set
   that Seurat would inspect;
3. identify a real split partition from cell membership, not from suffixes;
4. use a linear fast path for normal partitions and an indexed exact-cover
   fallback only for ambiguous candidate sets;
5. isolate unrelated prefix-matching custom layers on a local object before
   calling `JoinLayers()`;
6. validate and reorder the resolved matrix against the complete object cell set;
7. route every CerebroNexus expression-matrix consumer through this resolver.

All work will remain in the existing upstream pull request #101. Temporary
branches in the `duocang` fork may be used to prove tests red and green, but they
are CI workspaces, not additional upstream review units.

## Why this work exists

### The Seurat v5 representation

In an unsplit assay, one logical layer generally has one physical layer:

```text
RNA
├── counts
└── data
```

After `split(assay, f = sample)`, the same logical content may be represented as:

```text
RNA
├── counts.sample_a
├── counts.sample_b
├── data.sample_a
└── data.sample_b
```

Each split layer covers only its sample's cells. A complete matrix exists only
after selecting the correct partition and joining its members.

### The original failure

`Seurat::GetAssayData()` cannot return a multi-layer Assay5 matrix. Earlier
CerebroNexus fallback code tried available layers one by one and accepted the
first non-empty matrix. On a split object this could return one sample while the
metadata, projections, and grouping tables still represented every cell.

The embedded output backend often detected the mismatch later by accident.
External `h5` and `bpcells` output modes did not populate the embedded expression
slot and could therefore write an internally inconsistent `.crb`.

### Why the apparent fix kept growing

Several distinct problems share the same visible symptom:

| Problem | Example | Consequence |
| --- | --- | --- |
| Suffix assumption | only recognising `data.1`, not `data.patient_a` | real partitions are missed |
| Name-only classification | treating `data.imputed` as a split layer | custom data is joined or substituted |
| Prefix mismatch | protecting dotted names but not `dataBackup` | Seurat still consumes a custom layer |
| Unrelated partial layer | true `data.s1`/`data.s2` plus partial `data.imputed` | the real partition is hidden |
| Fixed root whitelist | split `ambient.s1`/`ambient.s2` | the original partial-matrix bug reappears |
| Quadratic search | repeated full-cell `%in%` scans | large split objects become unusable |
| Multiple entry points | direct `GetAssayData()` in public functions | export works while preprocessing fails |
| Backend-specific validation | embedded catches a mismatch, external modes do not | behaviour depends on storage choice |

Patching these independently creates inconsistent semantics. They must be
resolved by one shared contract.

## Terminology

**Requested layer**
: The value requested by a caller, historically passed as `slot`, for example
  `data`, `counts`, `data.sample_b`, or `ambient`.

**Exact layer**
: A physical layer whose name exactly equals the request.

**Requested root**
: A requested logical layer for which no exact physical layer exists and a
  partition may need to be assembled. The request itself is the root; it is not
  inferred by stripping a suffix.

**Prefix candidate**
: A layer returned by
  `SeuratObject::Layers(assay, search = requested_root)`. This is the candidate
  set Seurat may consume during `JoinLayers()`. It is intentionally broader
  than dotted names and is used for protection, not by itself as proof.

**Partition candidate**
: A non-empty `<requested_root>.*` child covering fewer than all assay cells.
  Names such as `dataBackup` remain protected but cannot prove a `data`
  partition.

**Exact-cover partition**
: At least two partition candidates whose memberships are pairwise disjoint and
  whose union equals the assay cell set.

**Unrelated custom layer**
: A prefix candidate that is not selected for the exact-cover partition,
  regardless of its spelling.

**Cross-semantic fallback**
: An explicit compatibility mode in which a complete matrix from another
  semantic class may replace a missing request, for example `data` → `counts`.
  It always emits a warning.

## Goals

1. Return all and only the cells implied by a request.
2. Never silently substitute a custom prefix layer for a missing logical root.
3. Never let an unrelated prefix layer alter joined expression values.
4. Keep normal split-layer resolution approximately linear in total membership
   size.
5. Support arbitrary requested roots without a global root whitelist.
6. Preserve exact named-layer requests, including intentionally partial layers.
7. Produce identical cell-coverage failures for embedded, HDF5, and BPCells
   output modes.
8. Make all exported preprocessing functions work consistently on Seurat v5
   split assays.
9. Preserve Seurat v3/v4 and unsplit Seurat v5 behaviour.
10. Leave the caller's Seurat object unchanged.

## Non-goals

1. Supporting BPCells- or DelayedArray-backed source assays. CerebroNexus may
   write BPCells output, but source materialisation remains an explicit user
   step.
2. Reimplementing Seurat's layer join operation.
3. Guessing biological semantics from arbitrary custom names.
4. Silently choosing between multiple valid exact-cover partitions.
5. Changing the public meaning of `slot` in `exportFromSeurat()`.
6. Modifying marker calculations that Seurat already handles internally.
7. Persistently joining or deleting layers in the user's object.

## Behavioural contract

### Resolution outcomes

| Assay shape | Request | Required outcome |
| --- | --- | --- |
| exact full `data` exists | `data` | return exact `data`; do not inspect siblings |
| exact partial `data.s2` exists | `data.s2` | return it exactly; do not join |
| only `data.s1` + `data.s2` form a partition | `data` | join those layers and return all cells |
| true partition plus full `data.imputed` | `data` | exclude `data.imputed`, join the partition |
| true partition plus partial overlapping `data.imputed` | `data` | find the independent true partition |
| true partition plus `dataBackup` | `data` | exclude `dataBackup` using Seurat's prefix set |
| only full `data.imputed`, no exact `data` | `data` | error; do not reinterpret imputed values as `data` |
| split `ambient.s1` + `ambient.s2` | `ambient` | resolve and join without whitelist support |
| two distinct valid partitions under one root | root | error as ambiguous; name both solutions |
| no complete same-root matrix | strict | error with candidates and cell coverage |
| no `data`, complete `counts` exists | compatibility | warn, then return `counts` |
| resolved matrix misses object cells | full-object consumer | error before storage or calculation |
| disk-backed source layer | any | error with executable materialisation guidance |

### Exact requests are authoritative

If a physical layer exactly matches the request, the resolver must return that
layer. It must not join sibling layers, even if the exact layer is partial.

This rule allows advanced callers to request `data.sample_b` deliberately. A
full-object consumer such as `exportFromSeurat()` then applies its own coverage
contract and rejects the partial matrix. The resolver and consumer therefore
remain separate:

- resolution answers "what did the caller request?";
- validation answers "is that result valid for this operation?"

### Strict and compatibility consumers

Public preprocessing functions use strict semantics:

| Consumer | Required layer | Cross-semantic fallback |
| --- | --- | --- |
| `calculatePercentGenes()` | `counts` | no |
| `getMostExpressedGenes()` | `counts` | no |
| `addPercentMtRibo()` | `counts` | no |
| `performGeneSetEnrichmentAnalysis()` | `data` | no |

Top-level export and conversion retain the documented compatibility behaviour:
if the requested semantic class is unavailable but another complete class is
used, CerebroNexus emits a warning that names both the request and replacement.
No fallback may return a partial matrix.

## Target architecture

### Component overview

```text
public consumer
    |
    v
.getExpressionMatrix()
    |
    +-- Seurat v3/v4 adapter
    |
    `-- .resolve_seurat_v5_layer()
            |
            +-- exact request
            +-- requested-root candidate discovery
            +-- .find_layer_partition()
            +-- isolated JoinLayers()
            `-- optional complete cross-semantic fallback
    |
    v
.validate_expression_cells()
    |
    v
consumer calculation or export backend
```

### `.getExpressionMatrix()`

The existing helper remains the compatibility facade. Its signature stays
stable:

```r
.getExpressionMatrix(
  seurat,
  assay = "RNA",
  slot = "data",
  join_samples = TRUE,
  allow_cross_semantic_fallback = FALSE,
  verbose = FALSE,
  return_resolution = FALSE
)
```

By default the function returns a matrix, preserving its existing contract and
exact partial-layer requests. Full-object consumers set
`return_resolution = TRUE` and receive `data`, `requested`, `resolved`,
`joined`, and `fallback` fields. They pass the returned matrix and physical
layer name to the shared cell validator.

### `.resolve_seurat_v5_layer()`

This helper performs request resolution and returns:

```r
list(
  data = <matrix-like>,
  requested = "data",
  resolved = "data",
  joined = TRUE,
  candidates = c("data.s1", "data.s2"),
  failure = NULL
)
```

The list is internal. It keeps error and warning construction independent of
matrix extraction. The compatibility facade adds a Boolean `fallback` field
when `return_resolution = TRUE`. An incomplete same-root candidate set returns
`data = NULL` plus a structured `failure` record instead of stopping inside the
resolver. This lets strict consumers raise the detailed same-root error while
export compatibility can still try a complete alternative semantic class.
Ambiguity remains an immediate error and is never hidden by fallback.

Resolution order:

1. Validate assay and request.
2. If the exact request exists, read it directly.
3. If joining is enabled, obtain prefix candidates using
   `Layers(assay, search = request)`.
4. Restrict partition proof to dotted children of the requested root. Prefer a
   unique cover made from direct children; if the only cover shares a deeper
   root, fail closed because the custom-root interpretation is equally valid.
5. Find a unique exact-cover partition among the eligible partial candidates.
6. If found, isolate every broad prefix match outside that partition and join
   only the selected layers on a local object.
7. If no same-root result exists and compatibility fallback is enabled, inspect
   complete alternative semantic classes in a deterministic order.
8. Otherwise fail with a structured diagnostic.

The resolver never discovers roots globally. It resolves the root the caller
asked for.

### `.find_layer_partition()`

This is a pure helper:

```r
.find_layer_partition(
  assay_cells,
  memberships,
  max_solutions = 2L,
  max_search_nodes = 100000L,
  max_conflict_work = 5000000L
)
```

Inputs:

- `assay_cells`: unique cell names in canonical order;
- `memberships`: a named list mapping candidate layer names to cell names.

Output:

```r
list(
  status = "unique", # "none" or "ambiguous"
  layers = c("data.s1", "data.s2"),
  solutions = list(...)
)
```

#### Linear fast path

Convert every membership to integer cell IDs once. Count ownership over all
candidates once.

If:

- every assay cell has ownership count one;
- every candidate is non-empty and partial; and
- at least two candidates exist;

then all candidates form the unique normal partition and are returned
immediately.

Complexity is `O(N + M)`, where `N` is the assay cell count and `M` is total
candidate membership size.

#### Indexed exact-cover fallback

The fast path fails when unrelated custom layers overlap the true partition.
The fallback must not return to repeated full-vector `%in%` scans.

It precomputes:

- candidate membership integer vectors;
- cell → candidate adjacency;
- candidate conflict sets;
- deterministic candidate ordering.

The search chooses an uncovered cell with the fewest compatible candidates,
adds one candidate, and eliminates conflicts. It stops after finding two
solutions:

- zero solutions → no partition;
- one solution → unique partition;
- two solutions → ambiguity error.

The normal case never enters this search. Exact cover remains exponential in
the worst case, so two deterministic budgets bound it: the number of
cell-claim pairs used to construct conflicts and the number of recursive search
nodes. Exceeding either budget is an actionable error, never a partial result.

### Isolated joining

Seurat's join candidate semantics are authoritative:

```r
join_candidates <- SeuratObject::Layers(assay, search = root)
```

The selected partition is a subset of this result. Every other candidate is
unrelated for this resolution.

On the resolver's local Seurat value:

1. remove `setdiff(join_candidates, partition)`;
2. call `JoinLayers(..., layers = root, new = root)`;
3. extract the joined root;
4. discard the local object.

The unrelated layers do not need to be copied and restored because:

- the resolver returns only a matrix;
- R copy-on-modify leaves the caller's object unchanged;
- the local object is discarded immediately.

A regression test must compare the caller's layer names and custom layer values
before and after resolution. If an observed Seurat version mutates the caller,
the implementation must switch to an explicit isolated assay copy rather than
reintroducing full-layer backup copies.

### `.validate_expression_cells()`

Coverage validation is a shared, backend-independent operation:

```r
.validate_expression_cells(
  expression_data,
  object_cells,
  assay,
  requested_layer,
  resolved_layer = requested_layer
)
```

It:

1. requires non-null column names;
2. reports missing and unexpected cells separately;
3. reports requested and resolved layer names;
4. reorders columns to `object_cells` when the sets match;
5. returns the validated matrix.

`exportFromSeurat()` calls it before expression storage modes diverge.
The four preprocessing functions call it before calculating metadata or tables.
When `convertSeuratToCerebro()` needs expression data for an optional
calculation, it validates that matrix before the calculation and hands the same
resolution to `exportFromSeurat()`. Export revalidates cell coverage but does
not join and materialise the split root a second time. Failures propagate to the
caller instead of being printed and swallowed.

The validated matrix and its physical layer name are also passed into
`.getSpatialData(expression_data =, expression_layer =)`, so spatial extraction
does not resolve and join the same assay a second time. A spatial payload keeps
both the requested layer and the actual resolved layer; a `data -> counts`
compatibility fallback must never serialize counts while labelling them as
normalized data.

## Root discovery without a whitelist

The current implementation loops over:

```r
c("scale.data", "counts", "data")
```

That list remains useful for:

- deterministic compatibility fallback priority;
- user-facing descriptions of standard Seurat semantics.

It must not gate correctness.

For a request such as `ambient`, candidate discovery is simply:

```r
Layers(assay, search = "ambient")
```

Cell membership decides whether `ambient.s1` and `ambient.s2` form a partition.
No suffix stripping is necessary, so roots containing dots remain valid.

## Public entry-point integration

The following direct calls to `Seurat::GetAssayData()` must be removed:

- `R/calculatePercentGenes.R`
- `R/getMostExpressedGenes.R`
- `R/performGeneSetEnrichmentAnalysis.R`
- `R/addPercentMtRibo.R`

Each function calls `.getExpressionMatrix()` with:

- its exact required semantic layer;
- `join_samples = TRUE`;
- `allow_cross_semantic_fallback = FALSE`;
- `return_resolution = TRUE`.

The returned matrix is then passed through `.validate_expression_cells()`.

`getMarkerGenes()` is outside this migration because it delegates to Seurat's
marker machinery, which already handles its own assay preparation.

## Error and warning contract

Messages must answer four questions:

1. What was requested?
2. What physical layers were found?
3. Why was no unique complete matrix produced?
4. What can the user do next?

### No exact cover

```text
Could not resolve layer `ambient` in assay `RNA` for all 48,210 cells.
Prefix candidates: ambient.s1 (12,003 cells), ambient.s2 (11,998 cells),
ambient.corrected (24,000 cells).
No unique disjoint partition covers the assay.
Inspect with SeuratObject::Layers(object[["RNA"]]) and
SeuratObject::Cells(object[["RNA"]], layer = "<layer>").
```

### Ambiguous exact cover

```text
Layer `data` in assay `RNA` has more than one valid cell partition.
Partition 1: data.s1, data.s2
Partition 2: data.batch_a, data.batch_b
Request an exact layer or join the intended layers before calling CerebroNexus.
```

### Coverage mismatch

```text
Resolved layer `data.s2` covers 10 of 20 object cells.
Missing examples: c1, c3, c5.
This operation requires a complete expression matrix.
```

### Cross-semantic compatibility fallback

```text
Layer `data` is unavailable in assay `RNA`; using complete layer `counts`.
Values are raw counts rather than normalized expression.
```

Warnings must not be used for partial matrices. Partial data is an error for
full-object consumers.

## Performance contract

### Required properties

1. The normal split path performs one membership encoding and one ownership
   count per requested root.
2. No recursive step performs `cell %in% membership_vector` across all cells.
3. Only the requested root is joined. Resolving `data` must not also join
   `counts`.
4. Full custom matrices are not copied merely to protect them from a temporary
   join.

### Regression budget

The remote CI scale fixture uses 50,000 cells, eight genuine split layers, and
one requested root. On the standard Linux CI runner:

- partition detection must finish within 15 seconds;
- output must contain all 50,000 cells;
- the test is skipped on CRAN but not in project CI.

The threshold intentionally has wide headroom. The old quadratic implementation
is expected to exceed it substantially, while the target fast path should finish
well below it.

Correctness tests remain authoritative; timing is a guard against accidental
algorithmic regression, not a microbenchmark.

## Compatibility matrix

| Environment | Behaviour |
| --- | --- |
| Seurat 3/4 | existing slot-based extraction |
| Seurat 5 unsplit | exact layer read |
| Seurat 5 standard split | automatic unique partition join |
| Seurat 5 arbitrary split root | request-driven partition join |
| Seurat 5 exact custom layer | exact read |
| Seurat 5 ambiguous candidates | explicit error |
| Seurat 5 BPCells source | explicit materialisation error |
| output `embedded` | validated before storage |
| output `h5` | same validation before storage |
| output `bpcells` | same validation before storage |

The package continues to declare Seurat `>= 3.0.0`. Seurat v5-specific fixtures
must skip when Seurat or SeuratObject is unavailable or older than version 5.
The project CI currently installs one contemporary dependency set, so this PR
proves v5 behaviour and preserves the existing v3/v4 adapter by inspection; it
does not claim a separate end-to-end Seurat 3/4 CI run. A legacy dependency job
is a repository-wide CI improvement, not a layered-assay runtime requirement.

## Testing strategy

### Pure partition tests

- normal two-, eight-, and sixteen-layer partitions;
- non-numeric sample suffixes;
- arbitrary root names;
- roots containing dots;
- full-cell custom candidate excluded;
- empty candidate excluded;
- partial overlapping custom candidate ignored;
- no exact cover;
- two valid exact covers reported as ambiguous;
- deterministic result independent of input list order;
- 50,000-cell scale regression.

### Seurat integration tests

- exact full root;
- exact named split layer;
- standard split `counts`, `data`, and `scale.data`;
- custom split `ambient`;
- `data.imputed`, `data_imputed`, and `dataBackup`;
- partial custom prefix layer crossing sample boundaries;
- incomplete requested-prefix noise plus a complete compatibility fallback;
- caller object unchanged;
- only full `data.imputed`, no `data`, produces an error;
- disk-backed source guidance remains executable;
- a mixed in-memory/disk partition is refused regardless of which layer sorts
  first.

### Consumer tests

- all four preprocessing functions work on a split Assay5;
- each function sees every object cell;
- strict consumers refuse semantic substitution;
- `convertSeuratToCerebro()` fails before optional calculations when they need
  the matrix, and always propagates final export failures;
- embedded, HDF5, and BPCells export modes share one coverage message.

### Remote red/green discipline

The user requested that tests run in CI rather than locally.

For each implementation batch:

1. push a test-only temporary branch to `duocang`;
2. record the expected failing assertions from GitHub Actions;
3. add the implementation on the real #101 branch;
4. push the identical test tree plus implementation;
5. require R tests, R CMD check, and pkgdown to pass;
6. delete temporary verification branches after the evidence is captured.

Only #101 is opened against `mihem`; temporary fork branches do not create new
upstream reviews.

## Documentation deliverables

1. This design document.
2. A task-by-task implementation plan.
3. `vignettes/seurat_v5_layered_assays.Rmd`.
4. Four accessible, self-contained SVG diagrams:
   - failure mechanism;
   - membership-based partition selection;
   - resolver decision flow;
   - shared entry-point architecture.
5. A pkgdown article entry.
6. A concise `NEWS.md` entry under unreleased version 3.0.3.

The vignette distinguishes released behaviour from implementation internals and
does not ask users to understand the exact-cover algorithm to export data.

## Pull-request structure

Everything lands in upstream PR #101. To keep review readable, implementation is
grouped into three additional commits:

1. `fix(seurat): resolve layered assays linearly`
2. `fix(seurat): share layered matrix resolution`
3. `docs(seurat): explain layered assay handling`

The existing commit remains the historical first implementation. If the final
history becomes difficult to review, the branch may be rewritten once into
these three coherent commits after all remote checks are green. It must not
accumulate one commit per review comment.

## Risks and mitigations

### Multiple valid partitions

**Risk:** an assay contains two legitimate but different partition schemes under
the same prefix.

**Mitigation:** detect a second solution and stop. Silent first-match behaviour
is not acceptable.

### Seurat prefix semantics change

**Risk:** a future SeuratObject release changes `Layers(search = root)`.

**Mitigation:** use Seurat's public candidate lookup directly and maintain an
integration test against the installed Seurat version.

### Caller mutation

**Risk:** local layer removal leaks through an unexpected reference-like object.

**Mitigation:** assert complete before/after equality of the caller's layer names
and custom layer values. If needed, isolate the assay explicitly.

### Timing-test instability

**Risk:** a loaded CI runner exceeds a narrow wall-clock threshold.

**Mitigation:** use a generous 15-second budget with an expected order-of-
magnitude margin, and keep structural tests for the algorithm.

### Large matrix materialisation

**Risk:** `JoinLayers()` materialises the requested root.

**Mitigation:** join only the requested root, document the memory peak, and
continue to reject disk-backed source layers with materialisation instructions.
Avoid joining unrelated roots or backing up full custom layers.

## Acceptance criteria

PR #101 is complete when:

1. every behavioural-contract row has a regression test;
2. the normal partition path is linear and passes the scale test;
3. arbitrary requested roots work without the standard-root whitelist;
4. unrelated custom layers cannot affect joined values;
5. ambiguous partitions fail clearly;
6. all four public preprocessing functions use the shared resolver;
7. export and conversion validate the complete cell set;
8. the caller's object remains unchanged;
9. embedded, HDF5, and BPCells outputs share the same guard;
10. the vignette and all four SVGs build in pkgdown;
11. remote R tests, R CMD check, and pkgdown are green;
12. no additional upstream PR is required for Seurat v5 layered-assay support.
