# Seurat v5 Layered Assays Implementation Plan

> **For AI agents:** Use the repository's test-driven development, code-review
> response, and verification skills. The user has explicitly required remote CI
> instead of local test execution.

**Goal:** Complete Seurat v5 layered-assay support in the existing upstream pull
request #101, including scalable partition resolution, arbitrary requested
roots, shared public-function behaviour, backend-independent validation, and
complete user documentation.

**Architecture:** Keep `.getExpressionMatrix()` as the compatibility facade.
Move Seurat v5 work into a request-driven resolver backed by a pure partition
engine with a linear fast path and indexed exact-cover fallback. Join only the
requested root on a local object after removing unrelated Seurat-prefix
candidates. Route every full-object expression consumer through the resolver and
one shared cell-coverage validator.

**Tech stack:** R, Seurat/SeuratObject v5, Matrix, testthat, knitr/rmarkdown,
pkgdown, SVG, GitHub Actions on the `duocang` fork.

**Design specification:** [Seurat v5 Layered Assays: Complete Resolution
Design](../design/2026-07-29-seurat-v5-layered-assays-design.md)

---

## Constraints

- Use the existing branch `fix/v5-split-layers` and upstream PR #101.
- Do not create additional pull requests against `mihem`.
- Temporary branches in `duocang` are allowed only for remote red/green evidence.
- Do not run the local R test suite or `scripts/precheck.sh`.
- Do not modify unrelated dirty worktrees.
- Keep repository artifacts in English.
- Keep version `3.0.3` synchronized across `DESCRIPTION`, `inst/app.R`, and
  `NEWS.md`; it is already the unreleased version assigned to #101.
- Preserve Seurat v3/v4 behaviour and skip v5 fixtures on older installations.

## Remote verification convention

For each red phase:

```bash
git switch fix/v5-split-layers
git switch -c ci/v5-layers-<task>-red
git add <test files>
git commit -m "test(seurat): expose <failure>"
git push origin HEAD
```

Wait for the `duocang` GitHub Actions R-test run and record the failing
assertions. Do not open an upstream PR.

For the green phase:

```bash
git switch fix/v5-split-layers
git cherry-pick <test-commit>
# implement the production change
git add <production and test files>
git commit -m "<planned commit subject>"
git push origin fix/v5-split-layers
```

Require all three upstream checks:

- R tests;
- R CMD check;
- pkgdown.

Delete temporary fork branches after their failure evidence is no longer needed:

```bash
git push origin --delete ci/v5-layers-<task>-red
git branch -D ci/v5-layers-<task>-red
```

## Task 1: Extract a pure partition engine

**Files:**

- Modify: `R/seurat_utils.R`
- Modify: `tests/testthat/test-seurat-v5-split-layers.R`

### 1.1 Write pure correctness tests

Add tests for `.find_layer_partition()` using named character-vector
memberships without constructing Seurat objects:

- two-layer exact cover;
- eight-layer exact cover;
- full-cell candidate ignored;
- empty candidate ignored;
- unrelated partial overlapping candidate ignored;
- no cover;
- two valid covers return `status = "ambiguous"`;
- shuffled input order produces the same status and selected layer set.

Example fixture:

```r
cells <- paste0("c", seq_len(12))
memberships <- list(
  "data.s1" = cells[1:6],
  "data.s2" = cells[7:12],
  "data.imputed" = cells[c(2, 3, 8, 9)]
)
```

Expected red result: `.find_layer_partition` does not exist.

### 1.2 Write the scale regression

Create 50,000 cell names and eight disjoint membership vectors. Measure only the
pure helper:

```r
elapsed <- system.time(
  result <- .find_layer_partition(cells, memberships)
)[["elapsed"]]

expect_identical(result$status, "unique")
expect_setequal(result$layers, names(memberships))
expect_lt(elapsed, 15)
```

Add `skip_on_cran()`, but do not skip in repository CI.

Expected red result: helper missing. The final test would also reject the old
quadratic implementation if it were wired into this interface.

Add a second scale-shaped regression with 2,000 singleton layers:

- the ordinary disjoint partition must stay on the linear fast path;
- adding one overlapping noise layer must enter exact-cover search;
- search must stop with the depth-budget diagnostic before R raises
  `node stack overflow`.

### 1.3 Implement membership encoding and the linear fast path

In `R/seurat_utils.R`, implement:

```r
.find_layer_partition <- function(
  assay_cells,
  memberships,
  max_solutions = 2L,
  max_search_nodes = 100000L,
  max_search_depth = 128L,
  max_conflict_work = 5000000L
)
```

Requirements:

- validate unique assay cells and named memberships;
- convert membership names to integer IDs exactly once with `match()`;
- discard empty, full-cell, and invalid candidates;
- use `tabulate()` for the all-candidate fast path;
- return `status`, `layers`, and `solutions`;
- require at least two selected layers.

### 1.4 Implement the indexed exact-cover fallback

Precompute:

- cell-to-layer adjacency;
- layer conflict sets;
- deterministic layer ordering.

Search by the least-ambiguous uncovered cell. Carry integer IDs and logical
coverage; never call `%in%` against full membership character vectors inside
recursion.

Stop after `max_solutions`, default two. Return ambiguity rather than the first
solution.

Bound conflict-index work, visited search nodes, and recursion depth
independently. Check the depth budget before making the next recursive call;
budget exhaustion must produce an actionable error rather than a partial
partition or an R call-stack failure.

### 1.5 Push green implementation

Commit:

```text
fix(seurat): resolve layered assays linearly
```

Remote expectation:

- pure correctness tests pass;
- 50,000-cell scale fixture stays below 15 seconds;
- existing split-layer integration tests remain green.

## Task 2: Replace global root detection with request-driven resolution

**Files:**

- Modify: `R/seurat_utils.R`
- Modify: `tests/testthat/test-seurat-v5-split-layers.R`

### 2.1 Add failing request-driven root tests

Construct an Assay5 with a custom split layer root `ambient`:

```text
ambient.sample_a
ambient.sample_b
```

Test:

- `.getExpressionMatrix(slot = "ambient", join_samples = TRUE)` returns all
  cells;
- values match the manually joined reference;
- no standard-root whitelist entry is added.

Add a root containing a dot, for example:

```text
ambient.corrected.sample_a
ambient.corrected.sample_b
```

Request `ambient.corrected` and require the same result.

Expected red result: current `.split_layer_groups()` never analyses these roots.

### 2.2 Add full custom-prefix refusal

Create an assay with only a full-cell `data.imputed` layer and no exact `data`.
Request `data`.

Expected result after implementation:

- an error states that exact `data` is absent;
- `data.imputed` is listed as a prefix candidate;
- its values are never returned as normalized `data`.

This is the highest-value regression for the semantic-substitution bug.

### 2.3 Add ambiguity coverage

Create two independent valid partitions under one requested prefix:

```text
data.s1 + data.s2
data.batch_a + data.batch_b
```

Require an ambiguity error naming both partitions.

### 2.4 Implement `.resolve_seurat_v5_layer()`

Resolution order:

1. exact physical layer;
2. request-driven `Layers(search = request)`;
3. unique membership partition;
4. optional complete cross-semantic fallback;
5. structured failure.

Do not enumerate roots globally. Retain `.cerebro_layer_roots` only as a
deterministic compatibility-fallback priority.

Return the internal resolution record described in the design. Incomplete
same-root candidates must be represented as structured failure data, not thrown
before the compatibility facade has applied its policy. Ambiguous exact covers
still fail immediately.

### 2.5 Turn `.getExpressionMatrix()` into a facade

Preserve the existing arguments and return type. Add:

```r
return_resolution = FALSE
```

Seurat v3/v4 continues through the existing adapter. Seurat v5 calls the new
resolver. The default remains a matrix; full-object consumers request the
resolution record and pass its matrix to the shared validator.

Update or remove:

- `.split_layer_groups()`;
- global root-looping logic;
- suffix-driven correctness decisions.

Keep `.layer_semantic_root()` only where legacy warning/fallback descriptions
still need standard semantic classes.

### 2.6 Verify exact-request precedence

Retain and extend the existing test for `slot = "data.s2"`:

- only sample 2 cells are returned;
- no join message appears;
- the caller's layer names are unchanged.

### 2.7 Keep incomplete noise from short-circuiting compatibility

Remove the complete `data.*` partition, add one partial `data.imputed` layer,
and retain a complete `counts.*` partition.

Require:

- strict resolution to fail with per-layer cell counts, missing-cell examples,
  and executable inspection commands;
- export-compatible resolution to warn and return the complete `counts`
  replacement;
- the replacement to contain every object cell.

Push the Task 2 changes as part of the Task 1 core commit if still local.
Otherwise amend the same logical commit before upstream review.

## Task 3: Isolate prefix-matching custom layers without backing them up

**Files:**

- Modify: `R/seurat_utils.R`
- Modify: `tests/testthat/test-seurat-v5-split-layers.R`

### 3.1 Preserve all prefix spelling regressions

Keep integration coverage for:

- `data.imputed`;
- `data_imputed`;
- `dataBackup`;
- partial `data.imputed` crossing true sample memberships.

Each custom layer should contain values offset by `+100`, making contamination
visible in the returned matrix.

### 3.2 Add caller-immutability coverage

Before resolution, capture:

```r
before_names <- Layers(obj[["RNA"]])
before_custom <- LayerData(obj[["RNA"]], layer = custom_name)
```

After resolution, require identical names and custom values on `obj`.

Expected red result after removing backup/restore but before confirming copy
semantics: any leaked mutation is detected.

### 3.3 Replace backup and restore

Use exactly:

```r
join_candidates <- SeuratObject::Layers(
  local_object[[assay]],
  search = requested_root
)
protected <- setdiff(join_candidates, partition)
```

Remove `protected` only from the local resolver value, join the selected root,
extract the matrix, and discard the local object.

Do not retain `protected_data`, which duplicates full custom matrices in memory.

If the immutability test fails on any supported Seurat version, explicitly copy
the assay before removal. Do not silently weaken the test.

### 3.4 Join only the requested root

Add an object with both `counts.*` and `data.*`. Instrument the result or
messages so resolving `data` proves `counts.*` was not joined.

Required result:

- returned `data` is complete;
- work is scoped to `data`;
- the caller still has its original split `counts.*` and `data.*`.

## Task 4: Centralize cell coverage validation

**Files:**

- Modify: `R/seurat_utils.R`
- Modify: `R/exportFromSeurat.R`
- Modify: `R/convertSeuratToCerebro.R`
- Modify: `tests/testthat/test-seurat-v5-split-layers.R`

### 4.1 Add validator unit tests

Write tests for `.validate_expression_cells()`:

- identical names return without copying-visible reordering;
- same set, different order returns canonical order;
- missing cells report count and examples;
- unexpected cells report count and examples;
- missing column names fail clearly;
- requested and resolved layer names appear in diagnostics.

### 4.2 Implement `.validate_expression_cells()`

Return the validated and ordered matrix. Keep message construction in this
helper so every backend and consumer reports the same defect.

### 4.3 Replace export's inline guard

Replace `R/exportFromSeurat.R:347-381` with the shared helper. Keep the call
before `expression_matrix_mode` branches.

Set `return_resolution = TRUE` in the export resolver call and validate the
record's `data` before any backend branch.

### 4.4 Never swallow conversion failures

After `convertSeuratToCerebro()` resolves `expr_matrix` for optional
most-expressed-gene calculations, validate it before any group loop.

The final `exportFromSeurat()` call keeps its own validation. This is intentional
defence in depth even though it repeats a cheap cell-name check. Its error
handler must rethrow with the source label; logging an error and returning makes
automation treat a failed conversion, or even a stale output file, as success.

### 4.5 Preserve backend parity tests

Require embedded, HDF5, and BPCells modes to:

- export all cells for a valid split object;
- return byte-identical coverage error text for an invalid object.

Skip optional backend cases when dependencies are unavailable.

## Task 5: Route public preprocessing functions through the resolver

**Files:**

- Modify: `R/calculatePercentGenes.R`
- Modify: `R/getMostExpressedGenes.R`
- Modify: `R/performGeneSetEnrichmentAnalysis.R`
- Modify: `R/addPercentMtRibo.R`
- Add: `tests/testthat/test-seurat-v5-layered-public-functions.R`

### 5.1 Add one red integration fixture

Reuse or extract `make_split_object()` into a test helper if needed. The fixture
must contain:

- split `counts.*` and `data.*`;
- two samples;
- metadata required by all four functions;
- enough genes to exercise mitochondrial/ribosomal and pathway logic without
  network access.

### 5.2 Add a regression for each public function

Require:

- no Seurat "GetAssayData doesn't work for multiple layers" error;
- complete-cell results;
- the function's existing return type and columns;
- caller assay layers remain unchanged.

For gene-set enrichment, use a minimal local gene-set input and disable unrelated
external work.

Expected red result: all four functions fail at their direct
`Seurat::GetAssayData()` calls.

### 5.3 Replace direct matrix access

Use `.getExpressionMatrix()` with:

```r
join_samples = TRUE
allow_cross_semantic_fallback = FALSE
return_resolution = TRUE
```

Layer requirements:

- `counts` for percent and most-expressed functions;
- `data` for gene-set enrichment.

Do not change `getMarkerGenes()`.

### 5.4 Reuse export resolution for spatial extraction

Append `expression_data = NULL` and `expression_layer = NULL` to
`.getSpatialData()` so existing positional calls remain compatible. When export
supplies the validated matrix, skip the internal resolver call and use that
matrix for coordinate intersection.

Return the original request as `requested_layer` and the physical source as
`layer`. Test a counts-only object requested at `data`; its spatial payload must
say `requested_layer = "data"` and `layer = "counts"`.

### 5.5 Verify strict semantics

For each semantic class, create a fixture where the required layer is absent but
another complete class exists.

Require a clear error; preprocessing must never silently use counts as
normalized data or vice versa.

Commit Tasks 4 and 5:

```text
fix(seurat): share layered matrix resolution
```

## Task 6: Keep disk-backed source guidance correct

**Files:**

- Modify if necessary: `R/seurat_utils.R`
- Modify: `tests/testthat/test-seurat-v5-split-layers.R`

### 6.1 Preserve real BPCells execution coverage

Keep the existing test that:

1. receives the materialisation loop from the error;
2. evaluates that loop on a real BPCells split object;
3. resolves the resulting object;
4. confirms all source cells are present.

### 6.2 Apply the same refusal through public functions

At least one public counts consumer and the normalized-data consumer must expose
the shared disk-backed source error rather than Seurat's class-level error.

### 6.3 Inspect every selected partition member

Create a split assay whose first layer is an in-memory `dgCMatrix` and whose
second layer is a real BPCells matrix. Resolution must identify the disk-backed
layer and return the same materialisation guidance. Behaviour must not depend on
the lexical order of sample suffixes.

Do not add BPCells source support in this PR.

## Task 7: Write and integrate the vignette

**Files:**

- Add: `vignettes/seurat_v5_layered_assays.Rmd`
- Add: `vignettes/img/seurat-v5-layer-failure.svg`
- Add: `vignettes/img/seurat-v5-layer-partition.svg`
- Add: `vignettes/img/seurat-v5-layer-resolver.svg`
- Add: `vignettes/img/seurat-v5-layer-entry-points.svg`
- Modify: `_pkgdown.yml`
- Modify: `vignettes/cerebroApp_workflow_Seurat.Rmd`

### 7.1 Explain the user problem

Cover:

- what layers are;
- why split assays exist;
- why one partial layer is dangerous;
- why names alone are insufficient;
- why output backend selection is unrelated to source-assay storage.

### 7.2 Document observable behaviour

Include:

- a compact scenario table;
- automatic resolution examples;
- exact-layer request behaviour;
- custom root behaviour;
- ambiguous and incomplete errors;
- disk-backed source materialisation;
- memory implications of `JoinLayers()`;
- troubleshooting commands.

Keep heavy Seurat code chunks at `eval = FALSE`.

### 7.3 Embed four SVGs

Use `knitr::include_graphics()` and declare all SVGs under `resource_files`.
Every diagram must include:

- a white background;
- accessible contrast;
- labels independent of colour;
- arrow markers and a legend where needed;
- a descriptive `<title>` and `<desc>`.

### 7.4 Register the article

Add `seurat_v5_layered_assays` under the pkgdown "How-to guides" section, near
the Seurat workflow and expression backend guides.

Add a "See also" link from `cerebroApp_workflow_Seurat.Rmd`.

## Task 8: Release notes and version contract

**Files:**

- Modify: `NEWS.md`
- Verify without changing unless wrong: `DESCRIPTION`
- Verify without changing unless wrong: `inst/app.R`

### 8.1 Update unreleased 3.0.3 notes

Describe:

- complete Seurat v5 split-layer export;
- arbitrary roots and custom-layer isolation;
- shared preprocessing support;
- backend-independent cell validation;
- improved diagnostics and performance.

Do not create a new version heading.

### 8.2 Verify version synchronization

All three must state `3.0.3`:

```text
DESCRIPTION
inst/app.R
NEWS.md
```

Do not run a local installation. Let remote R CMD check enforce the package
contract.

Commit Tasks 7 and 8:

```text
docs(seurat): explain layered assay handling
```

## Task 9: Remote full verification

### 9.1 Push the complete branch once

After all planned code and documentation commits are ready:

```bash
git status --short
git log --oneline --decorate upstream/master..HEAD
git push --force-with-lease origin fix/v5-split-layers
```

Use `--force-with-lease` only if the branch was intentionally rewritten after
fetching the current remote tip.

### 9.2 Require all checks

Wait for:

- R tests: zero failures;
- R CMD check: zero errors and zero warnings attributable to this change;
- pkgdown: article and all SVG resources build.

If a job fails, inspect its first project-level failure. Do not infer failure
from later port conflicts or cascading Shiny timeouts.

### 9.3 Review generated pkgdown output

Use the remote artifact or deployed preview to verify:

- all four diagrams render;
- text remains readable on narrow screens;
- table widths do not overflow;
- links to the Seurat workflow and backend guide work;
- the article is present in navigation.

This is document rendering verification, not a local package test.

## Task 10: Final branch hygiene and maintainer reply

### 10.1 Audit the diff

Review:

```bash
git diff --check upstream/master...HEAD
git diff --stat upstream/master...HEAD
git log --oneline upstream/master..HEAD
```

The final diff must contain no unrelated builder, Linked Views, repertoire, or
sibling-backend changes.

### 10.2 Keep a compact history

Target history above upstream master:

```text
fix(seurat): resolve layered assays linearly
fix(seurat): share layered matrix resolution
docs(seurat): explain layered assay handling
```

The existing #101 work may be folded into the first commit once checks are green.
Do not rewrite again after mihem starts reviewing the final architecture unless
technically necessary.

### 10.3 Update the PR body

The PR body should lead with the behavioural contract, not the review history:

- complete matrices for split assays;
- arbitrary requested roots;
- custom prefix isolation;
- all public expression consumers;
- one backend-independent guard;
- scale result;
- complete vignette.

State that all work remains in #101.

### 10.4 Reply to mihem

Keep the reply concise and evidence-based:

```text
Thanks — I expanded #101 into the complete layered-assay resolution path rather
than adding another naming patch.

- Resolution is now request-driven and uses cell membership, with a linear fast
  path and indexed exact-cover fallback.
- Join protection uses the exact Layers(search = root) candidate set, while the
  join runs on a local object and only for the requested root.
- Ambiguous partitions and incomplete matrices now fail explicitly.
- The four public preprocessing functions use the same strict resolver.
- The vignette documents the behaviour and troubleshooting flow.

The 50,000-cell scale regression and all existing integration tests pass in
remote CI. R tests, R CMD check, and pkgdown are green.
```

Only state that checks are green after the remote runs have completed.

## Definition of done

- One upstream PR: #101.
- Three coherent commits at most after cleanup.
- Unique exact-cover resolution is scalable.
- Arbitrary requested roots work.
- Custom prefix layers cannot contaminate a join.
- Ambiguity never resolves silently.
- Four public preprocessing functions share the resolver.
- Export and conversion enforce complete cell coverage.
- Caller objects are unchanged.
- Disk-backed source guidance remains executable.
- Complete vignette and four SVGs are published in pkgdown.
- `NEWS.md` and version contracts are synchronized at 3.0.3.
- Remote R tests, R CMD check, and pkgdown are green.
