# Simplified real-data benchmark implementation plan

> **For AI workers:** Required sub-skill: use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task by task. Track each
> step with the checkboxes below.

**Goal:** Replace the old benchmark framework with a flat, five-gene
query-panel-gated MSSM benchmark that compares three backends at a predeclared
target capped at 500,000 cells and measures BPCells through all 4,140,453
cells.

**Architecture:** Two R entry points share one configuration file and one
helper file. They install the current tree into a run-local library, freeze
source/sampling/query manifests, execute isolated workers with `callr`, and
write raw CSV evidence. Panel B consumes Panel A's frozen protocol manifest
before it runs. The normal public exporter gains one narrow path for a
complete BPCells-backed Seurat layer; no private benchmark-only matrix injection
is used.

**Technical stack:** R 4.6, CerebroNexus, Matrix, Seurat/SeuratObject, BPCells,
HDF5Array, rhdf5 for local H5AD `indptr`, digest for canonical SHA-256, callr,
testthat, base R, Git.

**Design specification:**
`other_documents/design/2026-08-07-real-data-benchmark-redesign.md`

---

## Final file map

### Create

- `tests/bench/config.R` — pinned MSSM identity and fixed protocol constants.
- `tests/bench/helpers.R` — sampling, manifests, fingerprints, synthetic shell,
  workers, process monitor, validation, and safe cleanup.
- `tests/bench/run_comparison.R` — Panel A parent process.
- `tests/bench/run_full_scale.R` — Panel B parent process.
- `tests/bench/results/README.md` — boundary between generated runs and reviewed
  evidence.
- `tests/testthat/test-benchmark-contract.R` — no-network, small-data contracts.

### Modify

- `R/seurat_utils.R` — narrowly allow one complete IterableMatrix layer.
- `R/exportFromSeurat.R` — stream that layer in BPCells mode without `@x`.
- `tests/testthat/test-exportFromSeurat.R` — public streaming and portability.
- `tests/testthat/test-seurat-v5-split-layers.R` — split disk-layer rejection.
- `man/exportFromSeurat.Rd` — regenerated public documentation.
- `tests/bench/README.md` — the complete operator guide.
- `.gitignore` — remove old `result/CURRENT` rules; ignore only scratch/logs.
- `DESCRIPTION` — retain `rhdf5` and add `digest` to `Suggests` for canonical
  SHA-256 fingerprints used by package tests.
- `create_env.R` — declare `rhdf5` and `digest` in the generated repository
  environment.
- `default.nix` — regenerate from `create_env.R`.
- `NEWS.md` — describe streaming-source support and new benchmark status.
- `vignettes/expression_backend_benchmark.Rmd` — remove stale pilot claims and
  document the two-panel protocol pending reviewed results.

### Delete

- `tests/bench/METHODOLOGY.md`
- `tests/bench/RESULTS.md`
- `tests/bench/run_sweep.sh`
- `tests/bench/config/`
- `tests/bench/design/`
- `tests/bench/lib/`
- `tests/bench/src/`
- `tests/bench/result/`
- the seven existing `tests/testthat/test-bench-*.R` files
- `vignettes/img/expression_backend_benchmark_ceiling.png`
- `vignettes/img/expression_backend_benchmark_overview.png`
- `other_documents/benchmark_accessing_DelayedArray.Rmd`
- `other_documents/benchmark_accessing_DelayedArray.html`

Do not modify `/Users/nuioi/projects/shiny/cerebroAppLite`,
`feat/cerebro-builder`, another worktree, a remote branch, or PR #113.

---

## Task 0: Commit the approved specification and implementation plan

**Files:**

- Create: `other_documents/design/2026-08-07-real-data-benchmark-redesign.md`
- Create: `other_documents/plans/2026-08-07-real-data-benchmark-redesign.md`

- [ ] **Step 1: Verify the two documents are the only current changes**

```bash
git status --short
git diff --check
```

Because both files begin untracked, inspect them explicitly; an unstaged
`git diff` does not show their contents.

- [ ] **Step 2: Stage only the two documents and verify the staged patch**

```bash
git add \
  other_documents/design/2026-08-07-real-data-benchmark-redesign.md \
  other_documents/plans/2026-08-07-real-data-benchmark-redesign.md
git diff --cached --check
git diff --cached --stat
git diff --cached --name-only
rg -n '^\^tests/bench\$$' .Rbuildignore
```

- [ ] **Step 3: Create the first logical commit**

```bash
git commit -m "docs(bench): design simplified real-data benchmark"
```

Do not push.

---

## Task 1: Add a public BPCells-backed Seurat export contract

**Files:**

- Modify: `tests/testthat/test-exportFromSeurat.R`
- Modify: `tests/testthat/test-seurat-v5-split-layers.R`

- [ ] **Step 1: Add a helper that creates a small BPCells-backed Seurat object**

Add this test helper near the shared export fixtures in
`tests/testthat/test-exportFromSeurat.R`:

```r
make_bpcells_source_object <- function(root) {
  expression <- Matrix::sparseMatrix(
    i = c(1L, 3L, 2L, 1L, 4L, 2L),
    j = c(1L, 1L, 2L, 3L, 4L, 5L),
    x = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
    dims = c(4L, 5L),
    dimnames = list(paste0("gene", 1:4), paste0("cell", 1:5))
  )
  source_dir <- file.path(root, "source.bpcells")
  BPCells::write_matrix_dir(
    methods::as(expression, "IterableMatrix"),
    dir = source_dir
  )
  assay <- SeuratObject::CreateAssay5Object(
    data = BPCells::open_matrix_dir(source_dir)
  )
  object <- suppressWarnings(
    SeuratObject::CreateSeuratObject(counts = assay, assay = "RNA")
  )
  object$sample <- factor(c("s1", "s1", "s2", "s2", "s3"))
  object$nUMI <- 0
  object$nGene <- 0
  embedding <- cbind(UMAP_1 = 1:5, UMAP_2 = 5:1)
  rownames(embedding) <- colnames(expression)
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embedding,
    key = "UMAP_",
    assay = "RNA"
  )
  list(object = object, expression = expression)
}
```

- [ ] **Step 2: Add the failing public streaming and move/attach test**

```r
test_that("exportFromSeurat streams one complete BPCells-backed layer", {
  skip_if_not_installed("BPCells")
  root <- withr::local_tempdir()
  fixture <- make_bpcells_source_object(root)
  first_dir <- file.path(root, "first")
  moved_dir <- file.path(root, "moved")
  dir.create(first_dir)
  dir.create(moved_dir)
  crb <- file.path(first_dir, "streamed.crb")

  expect_no_error(exportFromSeurat(
    object = fixture$object,
    assay = "RNA",
    slot = "data",
    file = crb,
    experiment_name = "streaming test",
    organism = "hg",
    groups = "sample",
    nUMI = "nUMI",
    nGene = "nGene",
    add_all_meta_data = FALSE,
    expression_matrix_mode = "bpcells"
  ))

  stored <- readRDS(crb)
  expect_identical(
    stored$getExpressionBackend(),
    list(type = "bpcells", location = "streamed.bpcells")
  )
  expect_true(file.rename(crb, file.path(moved_dir, "streamed.crb")))
  expect_true(file.rename(
    file.path(first_dir, "streamed.bpcells"),
    file.path(moved_dir, "streamed.bpcells")
  ))
  attach_env <- new.env(parent = globalenv())
  utility <- system.file(
    "viewer/utility_functions.R",
    package = "CerebroNexus"
  )
  if (!nzchar(utility)) {
    utility <- testthat::test_path("../../inst/viewer/utility_functions.R")
  }
  source(
    utility,
    local = attach_env
  )
  attached <- attach_env$.attachExternalExpression(
    readRDS(file.path(moved_dir, "streamed.crb")),
    file.path(moved_dir, "streamed.crb")
  )
  expect_equal(as.matrix(attached$expression), as.matrix(fixture$expression))
})
```

The test must not pass `.expression_resolution`.

- [ ] **Step 3: Add explicit unsupported-source tests**

Add one test proving the same object fails for `embedded` with a
disk-backed-source message, and a separate H5 test beginning with
`skip_if_not_installed("HDF5Array")`. Keeping them separate ensures a missing H5
dependency does not turn the embedded assertion into a skip. In
`tests/testthat/test-seurat-v5-split-layers.R`, add a test with two BPCells
`data.s1` / `data.s2` layers and require the message to contain both
`split` and `BPCells`; it must not silently select one layer.

```r
expect_disk_source_rejected <- function(mode, fixture, root) {
  expect_error(
    exportFromSeurat(
      object = fixture$object,
      assay = "RNA",
      slot = "data",
      file = file.path(root, paste0(mode, ".crb")),
      experiment_name = "unsupported source",
      organism = "hg",
      groups = "sample",
      nUMI = "nUMI",
      nGene = "nGene",
      expression_matrix_mode = mode
    ),
    "disk-backed|IterableMatrix"
  )
}

test_that("embedded rejects a disk-backed BPCells source", {
  skip_if_not_installed("BPCells")
  root <- withr::local_tempdir()
  fixture <- make_bpcells_source_object(root)
  expect_disk_source_rejected("embedded", fixture, root)
})

test_that("h5 rejects a disk-backed BPCells source", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  fixture <- make_bpcells_source_object(root)
  expect_disk_source_rejected("h5", fixture, root)
})
```

- [ ] **Step 4: Run RED tests**

Run:

```bash
Rscript --vanilla -e 'devtools::test(filter = "exportFromSeurat|seurat-v5-split-layers", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: the new public BPCells test fails with the existing disk-backed assay
rejection; the split test does not yet produce the dedicated split-BPCells
message.

---

## Task 2: Implement the narrow product streaming path

**Files:**

- Modify: `R/seurat_utils.R`
- Modify: `R/exportFromSeurat.R`
- Modify: `man/exportFromSeurat.Rd`
- Test: `tests/testthat/test-exportFromSeurat.R`
- Test: `tests/testthat/test-seurat-v5-split-layers.R`
- Test: `tests/testthat/test-export-data-integrity.R`

- [ ] **Step 1: Add a validated internal resolver opt-in**

Add `allow_iterable_matrix = FALSE` to `.getExpressionMatrix()`, validate it as
a scalar logical, and treat an `IterableMatrix` as supported only when that
flag is true. When a requested root resolves to multiple disk-backed split
layers, stop inside `.resolve_seurat_v5_layer()`'s partition loop before the
current early return of one physical member. After resolution, require every
allowed `IterableMatrix` to have positive row and column dimensions.

Change the condition that enters the existing detailed unsupported-type branch
to use this exact predicate; keep that branch's current error construction
unchanged:

```r
iterable_allowed <- isTRUE(allow_iterable_matrix) &&
  inherits(expr_matrix, "IterableMatrix")
supported_expression <- is.matrix(expr_matrix) ||
  inherits(expr_matrix, "dgCMatrix") ||
  iterable_allowed
```

Do not add DelayedArray to this opt-in.

- [ ] **Step 2: Opt in only from BPCells output mode**

Change the normal resolver call in `exportFromSeurat()` to:

```r
expression_resolution <- .getExpressionMatrix(
  seurat = object,
  assay = assay,
  slot = slot,
  join_samples = TRUE,
  allow_cross_semantic_fallback = TRUE,
  allow_iterable_matrix = identical(expression_matrix_mode, "bpcells"),
  verbose = verbose,
  return_resolution = TRUE
)
```

The private `.expression_resolution` compatibility hook remains internal but is
not used by the new benchmark.

- [ ] **Step 3: Stream IterableMatrix without sparse-slot access**

In the BPCells writer branch, implement:

```r
source_is_bpcells <- inherits(expression_data, "IterableMatrix")
if (!inherits(expression_data, "dgCMatrix") && !source_is_bpcells) {
  if (inherits(expression_data, "matrix")) {
    expression_data <- methods::as(expression_data, "CsparseMatrix")
  } else if (inherits(expression_data, c("RleMatrix", "DelayedMatrix"))) {
    expression_data <- methods::as(
      as.matrix(expression_data),
      "CsparseMatrix"
    )
  }
}

nnz_int_ok <- !source_is_bpcells &&
  length(expression_data@x) > 0L &&
  all(expression_data@x >= 0) &&
  all(expression_data@x == as.integer(expression_data@x)) &&
  all(expression_data@x <= .Machine$integer.max)

bpc_iter <- if (source_is_bpcells) {
  expression_data
} else {
  methods::as(expression_data, "IterableMatrix")
}
```

Only ordinary in-memory integer-valued sparse matrices enter the existing
`uint32_t` conversion. An IterableMatrix retains its source storage type.
Leave staging, permissions, rollback, relative descriptor, and final reopen
unchanged.

- [ ] **Step 4: Document the supported boundary**

Update the roxygen text for `expression_matrix_mode = "bpcells"`: a single
complete BPCells-backed Seurat layer can stream directly; split disk-backed
layers must be joined by a representation-aware upstream operation and are not
silently materialized. Run:

```bash
Rscript --vanilla -e 'devtools::document()'
```

- [ ] **Step 5: Run GREEN product tests**

```bash
Rscript --vanilla -e 'devtools::test(filter = "exportFromSeurat|seurat-v5-split-layers|export-data-integrity|versionless-layout-contract", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: all selected tests pass; the moved CRB reattaches through
`inst/viewer/utility_functions.R` and returns exact values.

Inspect `git status --short` after `devtools::document()` and include any
legitimate regenerated `NAMESPACE` change in scope; do not assume in advance
that only the Rd file can change.

Do not commit yet; the final implementation is one logical feature commit.

---

## Task 3: Define the flat protocol with pure contract tests

**Files:**

- Create: `tests/bench/config.R`
- Create: `tests/bench/helpers.R`
- Create: `tests/testthat/test-benchmark-contract.R`

- [ ] **Step 1: Add failing configuration and sampling tests**

The first tests should source only `config.R` and `helpers.R` and assert:

```r
bench_root <- normalizePath(
  testthat::test_path("../bench"),
  mustWork = FALSE
)

skip_unless_bench_tree <- function() {
  testthat::skip_if_not(
    dir.exists(bench_root),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("real-data tiers are deterministic, stratified, and nested", {
  skip_unless_bench_tree()
  source(file.path(bench_root, "config.R"), local = TRUE)
  source(file.path(bench_root, "helpers.R"), local = TRUE)

  n <- 41L
  tiers <- seq_len(n)
  indices <- lapply(tiers, function(n_take) {
    bench_stratified_indices(n_total = n, n_take = n_take)
  })
  expect_equal(lengths(indices), tiers)
  for (i in seq_len(length(indices) - 1L)) {
    expect_setequal(
      intersect(indices[[i]], indices[[i + 1L]]),
      indices[[i]]
    )
  }
  expect_identical(indices[[length(indices)]], seq_len(n))
  expect_true(all(diff(indices[[length(indices)]]) > 0L))

  blocks <- bench_stratified_blocks(
    BENCH_CONFIG$source$n_cells,
    BENCH_CONFIG$source$n_cells
  )
  expect_s3_class(blocks, "data.frame")
  expect_identical(names(blocks), c("stratum", "start", "end", "n"))
  expect_equal(nrow(blocks), 4L)
  expect_equal(sum(blocks$n), 4140453L)
  expect_equal(blocks$start, c(1, head(blocks$end, -1L) + 1))
  expect_equal(tail(blocks$end, 1L), 4140453L)
})
```

`tests/bench/` remains in `.Rbuildignore`. Every test in this file that reads or
executes harness content must call `skip_unless_bench_tree()` before its first
`source()`, `readLines()`, or subprocess. Never source a harness file at test
file scope. Checkout-level `devtools::test()` exercises all contracts; built
tarball `R CMD check` deliberately skips only this repo-only file's guarded
contracts. Product API tests remain self-contained and run in both contexts.

Add a fixture whose exact per-cell nnz makes 500k illegal but an arbitrary
non-multiple-of-four size legal. Assert the freezer returns the exact largest
legal integer, the selected nnz sum equals its manifest value, and the next
nested size is illegal. Also assert `common_target_actual <= 250000` aborts the
protocol before schedule construction.

- [ ] **Step 2: Add failing schedule and eligibility tests**

```r
test_that("comparison and full schedules have exact separate phase orders", {
  skip_unless_bench_tree()
  source(file.path(bench_root, "config.R"), local = TRUE)
  source(file.path(bench_root, "helpers.R"), local = TRUE)
  comparison <- bench_comparison_schedule(
    tiers = c(tier_125k = 125000L, tier_250k = 250000L, common = 499997L),
    backends = c("embedded", "bpcells", "h5"),
    repeats = 3L
  )
  full <- bench_full_schedule(
    tiers = c(common = 499997L, tier_1m = 1000000L,
      tier_2m = 2000000L, full = 4140453L),
    repeats = 4L
  )
  expect_equal(nrow(comparison), 27L)
  expect_equal(nrow(full), 16L)
  expect_equal(2L * nrow(comparison), 54L)
  expect_equal(2L * nrow(full), 32L)
  expect_equal(sum(full$n_cells == 4140453L), 4L)
  expect_identical(anyDuplicated(comparison$pair_id), 0L)
  expect_identical(anyDuplicated(full$pair_id), 0L)

  expected_a_export <- list(
    c("tier_125k", "tier_250k", "common"),
    c("tier_250k", "common", "tier_125k"),
    c("common", "tier_125k", "tier_250k")
  )
  expected_a_access <- list(
    c("tier_250k", "common", "tier_125k"),
    c("common", "tier_125k", "tier_250k"),
    c("tier_125k", "tier_250k", "common")
  )
  expected_a_backend_export <- list(
    c("embedded", "bpcells", "h5"),
    c("bpcells", "h5", "embedded"),
    c("h5", "embedded", "bpcells")
  )
  expected_a_backend_access <- list(
    c("h5", "embedded", "bpcells"),
    c("embedded", "bpcells", "h5"),
    c("bpcells", "h5", "embedded")
  )
  expected_b_export <- list(
    c("common", "tier_1m", "tier_2m", "full"),
    c("tier_1m", "tier_2m", "full", "common"),
    c("tier_2m", "full", "common", "tier_1m"),
    c("full", "common", "tier_1m", "tier_2m")
  )
  expected_b_access <- list(
    c("tier_2m", "full", "common", "tier_1m"),
    c("full", "common", "tier_1m", "tier_2m"),
    c("common", "tier_1m", "tier_2m", "full"),
    c("tier_1m", "tier_2m", "full", "common")
  )
  for (repeat_id in 1:3) {
    expect_identical(
      bench_tier_order(comparison, repeat_id, "export"),
      expected_a_export[[repeat_id]]
    )
    expect_identical(
      bench_tier_order(comparison, repeat_id, "access"),
      expected_a_access[[repeat_id]]
    )
    for (tier_label in names(c(
      tier_125k = 125000L,
      tier_250k = 250000L,
      common = 499997L
    ))) {
      expect_identical(
        bench_backend_order(
          comparison, repeat_id, tier_label, "export"
        ),
        expected_a_backend_export[[repeat_id]]
      )
      expect_identical(
        bench_backend_order(
          comparison, repeat_id, tier_label, "access"
        ),
        expected_a_backend_access[[repeat_id]]
      )
    }
  }
  for (repeat_id in 1:4) {
    expect_identical(
      bench_tier_order(full, repeat_id, "export"),
      expected_b_export[[repeat_id]]
    )
    expect_identical(
      bench_tier_order(full, repeat_id, "access"),
      expected_b_access[[repeat_id]]
    )
    expect_false(identical(
      bench_tier_order(full, repeat_id, "export"),
      bench_tier_order(full, repeat_id, "access")
    ))
  }
})
```

The schedule schema is exactly `pair_id`, `panel`, `repeat`, `tier_label`,
`n_cells`, `backend`, `export_order`, and `access_order`. Within a Panel A tier,
the expected backend rotations are the design's three explicit sequences; add
the equivalent exact assertions rather than checking only row counts. Also
assert that `bench_eligibility()` uses only `SCHEDULED`,
`NOT_APPLICABLE_PROTOCOL`, and `UNSUPPORTED_DGCMATRIX_INDEX`, and never creates
fake measurement jobs for embedded/H5 in Panel B.

Each order column is a global permutation of `seq_len(nrow(schedule))`, with
numeric repeat blocks contiguous. Runtime interleaves the phases by repeat:
all exports in repeat 1's export rank, then all accesses in repeat 1's access
rank, then repeat 2, and so on. Tests reconstruct and compare the complete
ordered `pair_id` vectors, not only tier summaries.

- [ ] **Step 3: Run RED contract tests**

```bash
Rscript --vanilla -e 'devtools::test(filter = "benchmark-contract", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: FAIL because the new files/functions do not exist.

- [ ] **Step 4: Create the pinned configuration**

`tests/bench/config.R` must define only one top-level object:

```r
BENCH_CONFIG <- list(
  schema_version = 1L,
  source = list(
    key = "human_pfc_mssm",
    url = paste0(
      "https://datasets.cellxgene.cziscience.com/",
      "0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad"
    ),
    expected_bytes = 36092176654,
    expected_sha256 = paste0(
      "c62456941372b90bcf0df38e8cb1c34d",
      "d060bc5a507270ab1d068cbe6f1dfd54"
    ),
    n_cells = 4140453L,
    group = "X",
    organism = "hg38",
    slot = "data"
  ),
  comparison_fixed_tiers = c(tier_125k = 125000L, tier_250k = 250000L),
  common_target = 500000L,
  common_min_exclusive = 250000L,
  full_scale_fixed_tiers = c(
    tier_1m = 1000000L,
    tier_2m = 2000000L,
    full = 4140453L
  ),
  comparison_backends = c("embedded", "bpcells", "h5"),
  comparison_repeats = 3L,
  full_scale_repeats = 4L,
  query_genes = 5L,
  warmed_iterations = 5L,
  rss_poll_ms = 500L,
  sparse_index_limit = .Machine$integer.max
)
```

- [ ] **Step 5: Implement pure sampling and schedule functions**

Add these public-to-the-harness signatures to `helpers.R`:

```r
bench_stratified_blocks <- function(n_total, n_take)
bench_stratified_indices <- function(n_total, n_take)
bench_exact_selected_nnz <- function(n_total, n_take, nnz_per_cell)
bench_freeze_common_tier <- function(target, minimum_exclusive,
                                     nnz_per_cell, limit)
bench_comparison_schedule <- function(tiers, backends, repeats = 3L)
bench_full_schedule <- function(tiers, repeats = 4L)
bench_tier_order <- function(schedule, repeat, phase = c("export", "access"))
bench_backend_order <- function(schedule, repeat, tier_label,
                                phase = c("export", "access"))
bench_eligibility <- function(panel, tiers, exact_nnz, limit)
bench_validate_schedule <- function(schedule, expected_rows)
```

`bench_freeze_common_tier()` receives the exact per-cell nnz vector read from
the H5AD `indptr`; it binary-searches every integer size up to `target` using
the same nested strata and returns
`list(common_target_actual, exact_nnz, target_reduced)`. It aborts if the
result is not greater than `minimum_exclusive`. It must never inspect RAM or
react to a later memory failure. `bench_validate_schedule()` verifies the exact
schema, unique pair IDs, configured rows, both order permutations, and that no
repeat has the same export and access order.

The sampler hard-codes exactly four strata. Do not expose a generic
`n_strata` argument: the nesting proof and protocol tests are for this fixed
four-stratum design.

- [ ] **Step 6: Run GREEN pure contracts**

```bash
Rscript --vanilla -e 'devtools::test(filter = "benchmark-contract", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: the sampling, schedule, and eligibility tests pass without network or
large data.

---

## Task 4: Add source identity, exact nnz, and correctness plans

**Files:**

- Modify: `tests/bench/helpers.R`
- Modify: `tests/testthat/test-benchmark-contract.R`
- Modify: `create_env.R`
- Modify: `default.nix`
- Modify: `DESCRIPTION`

- [ ] **Step 1: Add failing source/fingerprint tests**

Begin source/fingerprint tests with explicit `skip_if_not_installed()` guards
for each direct optional dependency they exercise (`BPCells`, `rhdf5`, and
`digest`). Create a non-square four-gene-by-five-cell local H5AD fixture, then assert that a
wrong byte size/hash fails before `bench_open_source()` is called. Add tests
that fractional pointers, a non-zero first pointer, non-monotone pointers,
values above `2^53`, and disagreement between final pointer and `data` or
`indices` length all fail before schedule construction. Add fingerprint tests
for changed values, gene order, cell identity, and named vectors.

```r
test_that("query fingerprints include values and ordered identities", {
  skip_unless_bench_tree()
  source(file.path(bench_root, "config.R"), local = TRUE)
  source(file.path(bench_root, "helpers.R"), local = TRUE)
  x <- matrix(1:6, nrow = 2,
    dimnames = list(c("g1", "g2"), c("c1", "c2", "c3")))
  base <- bench_numeric_fingerprint(
    values = x,
    gene_ids = rownames(x),
    cell_ids = colnames(x)
  )
  changed <- x
  changed[1, 1] <- 99
  expect_false(identical(base, bench_numeric_fingerprint(
    changed, rownames(changed), colnames(changed)
  )))
  expect_false(identical(base, bench_numeric_fingerprint(
    x[, 3:1], rownames(x), colnames(x)[3:1]
  )))
  expect_false(identical(base, bench_numeric_fingerprint(
    x[2:1, ], rownames(x)[2:1], colnames(x)
  )))

  row <- setNames(as.double(x[1, ]), colnames(x))
  row_hash <- bench_numeric_fingerprint(row, "g1", names(row))
  expect_false(identical(
    row_hash,
    bench_numeric_fingerprint(row[3:1], "g1", names(row)[3:1])
  ))
  renamed <- row
  names(renamed)[1] <- "different-cell"
  expect_false(identical(
    row_hash,
    bench_numeric_fingerprint(renamed, "g1", names(renamed))
  ))
})
```

Also require errors for missing/duplicate gene or cell IDs and invalid byte
sequences. Add a Latin-1/UTF-8 equivalent identifier test to prove `enc2utf8()`
canonicalization rather than rejecting all valid non-UTF-8 source encodings.

- [ ] **Step 2: Add local-source functions**

Implement these exact signatures:

```r
bench_sha256_file <- function(path)
bench_sha256_object <- function(object)
bench_validate_source_file <- function(path, source_spec)
bench_read_h5ad_indptr <- function(path, group = "X")
bench_open_source <- function(path, group = "X")
bench_source_inventory <- function(path, source_spec)
bench_identity_fingerprint <- function(ids)
bench_numeric_fingerprint <- function(values, gene_ids, cell_ids)
bench_select_query_genes <- function(source_matrix, smallest_indices, n = 5L)
bench_build_query_plan <- function(source_matrix, indices, genes)
bench_write_sampling_manifest <- function(path, rows)
bench_write_query_manifest <- function(path, plans)
```

`bench_read_h5ad_indptr()` reads only `/<group>/indptr` from the already local,
SHA-validated H5AD with `rhdf5::h5read(..., bit64conversion = "double")`.
Require length `n_cells + 1`, finite integer-valued entries, first value zero,
monotonicity, and `tail(indptr, 1) <= 2^53`; require `diff(indptr)` to be
non-negative and integer-valued. Inventory also verifies `/X` shape and CSR
encoding, and checks that `tail(indptr, 1)` equals both `/X/data` and
`/X/indices` lengths exactly. Read the exact indptr once in the run's setup
worker and reuse the resulting per-cell vector for manifests and binary search.
This is not a remote ROS3 reader.

`bench_select_query_genes()` computes streaming row non-zero counts only on the
smallest 125k subset and selects five expressed genes spanning density. The
same ordered five genes are used for every larger nested tier.

`bench_build_query_plan()` records source SHA, sampling SHA, dimensions, five
ordered genes with role/density/source-row tie-break, and source row/block
SHA-256 fingerprints. It runs once per tier before timed jobs. Do not store all
ordered cell names at large tiers; source identity, tier, block boundaries, and
ordered-index SHA identify the selection, while an ordered source cell-ID SHA
pins its names without storing them all. Block materialization is bounded to
five rows. Panel A's common plan is written to `queries.csv` and a
canonical `query-plan.rds` payload for Panel B to verify before it runs.

`bench_identity_fingerprint()` rejects missing, duplicate, or invalidly encoded
identities, applies `enc2utf8()`, and hashes their ordered XDR/version-3 payload.
`bench_numeric_fingerprint()` validates dimensions, computes that cell-identity
hash, coerces values to doubles in column-major order, and hashes a named
payload with schema tag, dimensions, ordered gene IDs, the cell hash, and
values via `serialize(..., NULL, xdr = TRUE, version = 3)` plus
`digest::digest(raw, algo = "sha256", serialize = FALSE)`. It does not persist
all cell names in `query-plan.rds`.

- [ ] **Step 3: Declare direct test/harness hashing and HDF5 dependencies**

Retain `rhdf5` in `DESCRIPTION` `Suggests`, because
`tests/testthat/test-benchmark-contract.R` calls `rhdf5::h5read()` and creates a
tiny H5AD fixture. Add `digest` to `Suggests` because that package test also
exercises `digest::digest()`. Add both `rhdf5` and `digest` to the package list
in `create_env.R`, then run
the command documented at the top of that file:

```bash
nix-shell -p rPackages.rix R --run "Rscript create_env.R"
```

This regenerates `default.nix`; do not hand-edit the generated file. Because
the generator resolves a current pinned cache snapshot over the network,
inspect the complete generated diff and record the resolved snapshot. If it
would cause unrelated dependency churn, stop and review rather than hiding
that churn. `HDF5Array` remains a `Suggests`
dependency for the H5 backend.

- [ ] **Step 4: Run GREEN identity tests**

```bash
Rscript --vanilla -e 'devtools::test(filter = "benchmark-contract", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: wrong identity fails before open, exact indptr counts are recovered,
and all value/order mutations change fingerprints.

---

## Task 5: Implement the shared synthetic shell and query measurement

**Files:**

- Modify: `tests/bench/helpers.R`
- Modify: `tests/testthat/test-benchmark-contract.R`

- [ ] **Step 1: Add failing shell parity and materialization tests**

Use the same non-square four-gene-by-five-cell matrix once as `dgCMatrix` and once as
`IterableMatrix`. Assert that both shells have identical cell names, synthetic
groups, zero QC fields, deterministic coordinates, and the same canonical
shell fingerprint. Assert that every Panel A and Panel B tier calls this one
generator. Use a small
`Cerebro_v1.3` object to prove that block preparation and `as.matrix()` are
separate timed operations and correctness is evaluated after timing.

- [ ] **Step 2: Implement the shell**

```r
bench_make_seurat_shell <- function(expression, source_indices) {
  assay <- SeuratObject::CreateAssay5Object(data = expression)
  object <- suppressWarnings(
    SeuratObject::CreateSeuratObject(counts = assay, assay = "RNA")
  )
  object$sample <- factor(paste0("sample_", source_indices %% 8L + 1L))
  object$cluster <- factor(paste0("cluster_", source_indices %% 32L + 1L))
  object$nUMI <- 0
  object$nGene <- 0
  embedding <- cbind(
    UMAP_1 = sin(source_indices / 1000),
    UMAP_2 = cos(source_indices / 1000)
  )
  rownames(embedding) <- colnames(expression)
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embedding,
    key = "UMAP_",
    assay = "RNA"
  )
  object
}
```

All synthetic fields derive from stable source indices, not RNG.
`bench_shell_fingerprint()` hashes the shell schema/version, ordered selected
source-index SHA, metadata columns and levels, coordinate construction rule,
and dimensions. The fingerprint is stored with each sampling row and must
match at the common tier across panels. The expression matrix is fingerprinted
separately by the query plan.

- [ ] **Step 3: Implement query measurement**

Implement a value-returning timer instead of reusing the old numeric-only
benchmark timer:

```r
bench_timed_value <- function(expr) {
  started <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(
    seconds = unname(proc.time()[["elapsed"]] - started),
    value = value
  )
}

bench_measure_queries <- function(object, plan, timer = bench_timed_value) {
  first <- timer(object$getExpressionRow(plan$first_gene))
  warmed <- vapply(seq_len(5L), function(i) {
    timer(object$getExpressionRow(plan$first_gene))$seconds
  }, numeric(1))
  prepared <- timer(object$getExpressionBlock(plan$block_genes))
  materialized <- timer(as.matrix(prepared$value))
  observed <- materialized$value
  list(
    first_query_secs = first$seconds,
    warmed_secs = warmed,
    warmed_median_secs = stats::median(warmed),
    block_prepare_secs = prepared$seconds,
    block_materialize_secs = materialized$seconds,
    block_ready_secs = prepared$seconds + materialized$seconds,
    row_fingerprint = bench_numeric_fingerprint(
      first$value,
      plan$first_gene,
      names(first$value)
    ),
    block_fingerprint = bench_numeric_fingerprint(
      observed,
      rownames(observed),
      colnames(observed)
    )
  )
}
```

Validate the observed fingerprints against the plan outside the timed calls.
The five warmed durations all query the same median-density gene. Do not retain
the old `block_secs` field and do not describe the within-process median as a
replicate or general gene-access estimate.

Call this a five-gene query-panel correctness gate. It does not prove equality
of every source matrix value. The small public export test in Task 1 remains
the full-matrix round-trip contract for its fixture.

- [ ] **Step 4: Run GREEN shell/query tests**

```bash
Rscript --vanilla -e 'devtools::test(filter = "benchmark-contract", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: both source representations create equivalent synthetic shell
metadata, lazy block materialization is timed, and a wrong value fails closed.

---

## Task 6: Implement workers, peak RSS sampling, and crash records

**Files:**

- Modify: `tests/bench/helpers.R`
- Modify: `tests/testthat/test-benchmark-contract.R`

- [ ] **Step 1: Add failing worker lifecycle tests**

Test three synthetic workers: success, ordinary R error after writing stage
`seurat_shell`, and a background worker killed while stage is `export`. Assert
one outcome row in every case and require the killed row to retain stage, exit
status, log, and peak sampled RSS. Add a library-origin probe: a worker records
`normalizePath(find.package("CerebroNexus"))`, and validation rejects it unless
that path is inside the marked `run_context$library`.

- [ ] **Step 2: Implement atomic stage and safe scratch helpers**

```r
bench_write_stage <- function(job_dir, stage)
bench_read_stage <- function(job_dir)
bench_make_job_dir <- function(scratch_root, job_id)
bench_remove_job_dir <- function(job_dir, scratch_root)
bench_r_heap_peak_mb <- function()
bench_safe_r_heap_peak_mb <- function()
bench_rss_mb <- function(pid = Sys.getpid())
bench_write_outcome_atomic <- function(path, row, expected_schema)
```

Every job directory contains `.cerebro-benchmark-job`. Cleanup must verify the
marker, the normalized parent, and the exact scheduled `job_id` before calling
`unlink(..., recursive = TRUE)`.
`bench_make_job_dir()` writes initial stage `startup` before spawning the
child, so an early crash always yields `FAILED_startup` rather than an empty
status.

At the beginning of each worker call `gc(reset = TRUE)`. On both normal return
and caught R error, call `gc()`, require the penultimate column to be
`"max used"`, and compute MiB as
`sum(gc_result[, ncol(gc_result)])` after requiring both values finite. The last column is the
`(Mb)` partner for Ncells/Vcells even though R has duplicate `(Mb)` column
names. This is the within-worker R-heap high-water metric, not process RSS.
`bench_safe_r_heap_peak_mb()` wraps that calculation in `tryCatch()` and
returns `NA_real_` on collection failure.

- [ ] **Step 3: Implement the two worker functions**

```r
bench_export_worker <- function(job, run_context)
bench_access_worker <- function(job, run_context)
bench_worker_entry <- function(worker_name, job, run_context)
```

The export worker stages are exactly:

```text
source_open
source_subset
comparison_materialize   # Panel A only
seurat_shell
export
artifact_sizes
complete
```

The access worker stages are exactly:

```text
crb_load
backend_attach
first_query
warmed_queries
block_prepare
block_materialize
correctness
complete
```

Panel A materializes the selected source to `dgCMatrix` before shell creation.
Panel B keeps the BPCells `IterableMatrix`. Both call the public exporter with
`add_all_meta_data = FALSE`; no worker passes `.expression_resolution`.

`bench_worker_entry()` owns the heap reset and error boundary:

```r
bench_worker_entry <- function(worker_name, job, run_context) {
  gc(reset = TRUE)
  tryCatch(
    {
      row <- get(worker_name, mode = "function")(job, run_context)
      bench_finalize_worker_row(
        row,
        r_heap_peak_mb = bench_safe_r_heap_peak_mb()
      )
    },
    error = function(error) {
      bench_failure_row(
        job = job,
        stage = bench_read_stage(job$job_dir),
        error = conditionMessage(error),
        r_heap_peak_mb = bench_safe_r_heap_peak_mb()
      )
    }
  )
}
```

Both helper constructors return exactly one row in the phase's fixed schema.
If heap collection itself fails, preserve the original outcome and record heap
as `NA_real_` rather than masking the worker error.

- [ ] **Step 4: Implement the monitored callr parent**

```r
bench_run_worker <- function(worker, job, run_context, log_path, poll_ms) {
  process <- callr::r_bg(
    func = function(root, worker_name, job, context) {
      source(file.path(root, "helpers.R"), local = TRUE)
      bench_worker_entry(worker_name, job, context)
    },
    args = list(run_context$bench_root, worker, job, run_context),
    libpath = c(run_context$library, .libPaths()),
    system_profile = FALSE,
    user_profile = FALSE,
    supervise = TRUE,
    stdout = log_path,
    stderr = "2>&1"
  )
  peak_rss <- NA_real_
  sample <- bench_rss_mb(process$get_pid())
  if (is.finite(sample)) peak_rss <- sample
  while (process$is_alive()) {
    Sys.sleep(poll_ms / 1000)
    sample <- bench_rss_mb(process$get_pid())
    if (is.finite(sample)) {
      peak_rss <- if (is.finite(peak_rss)) max(peak_rss, sample) else sample
    }
  }
  process$wait()
  exit_status <- process$get_exit_status()
  result <- tryCatch(
    list(ok = TRUE, value = process$get_result()),
    error = function(error) list(ok = FALSE, error = conditionMessage(error))
  )
  bench_classify_worker_result(
    job = job,
    result = result,
    exit_status = exit_status,
    last_stage = bench_read_stage(job$job_dir),
    peak_rss_mb = peak_rss,
    log_path = log_path
  )
}
```

`run_context` contains only paths, scalar configuration, and ordinary
data-frame/list manifests. Never serialize a BPCells matrix, HDF5 handle, or
Seurat object into `callr`; each worker opens its own source and artifact. At
entry, setup and measured workers resolve `find.package("CerebroNexus")`, store
the normalized path, and fail before scientific work unless it is a descendant
of the marked run-local library.

The child wrapper catches an ordinary R error and returns a fully shaped
`FAILED_<last_stage>` row. On successful `get_result()`, the parent validates
that there is exactly one row for the expected ID before atomically writing it.
For a killed/non-zero process with no result, the parent synthesizes
`FAILED_<last_stage>` with exit status, log and sampled RSS. If the process
exits zero but returns no valid row, or `get_result()` itself cannot recover a
result, the collector leaves the expected ID absent and records a diagnostic;
the validator later creates `MISSING_RESULT` for that ID. Every branch retains
the child exit status. `ps` returning `NA` during exit leaves RSS as
`NA_real_` when no finite sample was ever captured.

- [ ] **Step 5: Use explicit status vocabulary**

Eligibility rows use only `SCHEDULED`, `NOT_APPLICABLE_PROTOCOL`, or
`UNSUPPORTED_DGCMATRIX_INDEX`. Raw export outcomes use `OK` or
`FAILED_<stage>`. Raw access outcomes add `NOT_RUN_EXPORT_FAILED`, written once
for each expected access whose export failed. `MISSING_RESULT` is emitted only
by validation for an expected ID absent from the raw outcome table. Validators
must reject duplicate, unscheduled, missing, failed, not-run, or
fingerprint-mismatched core jobs.

Pin these control schemas before adding metric columns:

```text
eligibility.csv: panel,tier_label,n_cells,backend,exact_nnz,status,reason
schedule.csv:    pair_id,panel,repeat,tier_label,n_cells,backend,
                 export_order,access_order
export.csv:      pair_id,status,failure_stage,error,exit_status,log_path,...
access.csv:      pair_id,status,failure_stage,error,exit_status,log_path,...
validation.csv: check_id,panel,scope,expected,observed,status,detail
```

Every table uses one row per declared entity and a fixed typed schema even when
empty; never infer columns from the first successful worker. `validation.csv`
ends with exactly one `check_id = "panel_valid"` row whose status is `VALID`
only if every preceding required gate is `PASS`, otherwise `INVALID`.

- [ ] **Step 6: Run GREEN lifecycle tests**

```bash
Rscript --vanilla -e 'devtools::test(filter = "benchmark-contract", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: ordinary errors and killed workers both produce machine-readable
outcomes; safe cleanup refuses an unmarked or out-of-root directory.

---

## Task 7: Build the two entry points

**Files:**

- Create: `tests/bench/run_comparison.R`
- Create: `tests/bench/run_full_scale.R`
- Modify: `tests/bench/helpers.R`
- Modify: `tests/testthat/test-benchmark-contract.R`

- [ ] **Step 1: Add failing dry-run CLI tests**

Run each entry point in a child R process with `--dry-run`. Assert exit zero,
Panel A reports 27 technical pairs / 54 workers, Panel B reports 16 pairs / 32
workers, and the full schedule contains 4,140,453 four times. Dry-run output
must say `UNQUALIFIED`: it has not checked source nnz or Panel A linkage. Add a
malformed-argument test with non-zero exit.

Create tiny synthetic Panel A directories and test
`bench_validate_panel_a_evidence()` directly. Only a directory with the exact
27-pair schedule, 27 unique `OK` exports, 27 unique `OK` accesses, matching
hashes, and one final `VALID` row may pass. Mutate each of those gates in turn
and require failure before a setup/measured-worker callback can be invoked.

- [ ] **Step 2: Implement shared run setup**

Add:

```r
bench_parse_args <- function(args, panel)
bench_validate_output_candidate <- function(path, repo, panel_a_dir = NULL)
bench_prepare_output <- function(path)
bench_record_environment <- function(repo, command)
bench_install_tree <- function(repo, library, log)
bench_validate_installed_tree <- function(library, expected_git_sha)
bench_setup_worker <- function(panel, source_path, imported_panel_a, run_context)
bench_run_setup_worker <- function(panel, source_path, imported_panel_a,
                                   run_context, log_path)
bench_validate_panel <- function(schedule, eligibility, exports, access,
                                 sampling, plans, linkage = NULL)
bench_write_validation <- function(path, checks)
bench_validate_panel_a_evidence <- function(panel_a_dir)
bench_validate_panel_a_linkage <- function(panel_a_dir, current_context)
```

Real mode syntax is:

```text
Rscript --vanilla tests/bench/run_comparison.R <local-mssm.h5ad> <new-output-dir>
Rscript --vanilla tests/bench/run_full_scale.R \
  <local-mssm.h5ad> <panel-a-dir> <new-output-dir>
```

`--dry-run` requires no source and prints the target schedule marked
`UNQUALIFIED`; it must never claim the target 500k nnz or cross-panel manifests
were checked. In real mode the new output path must not exist; the runner
creates and marks it without overwriting prior evidence. The Panel A directory
passed to Panel B must be a different existing, immutable input.

`bench_run_setup_worker()` uses the same run-local-first `libpath`, disabled
system/user profiles, and plain serializable context as measured workers. It
writes large reference payloads directly into the marked output tree and
returns only small manifest rows/hashes. Setup time and memory are logged but
are not benchmark metrics or part of the 54/32 measured worker counts.

Its runtime fingerprint uses R version/platform and sorted `package=version`
records for CerebroNexus, Matrix, SeuratObject, Seurat, BPCells, HDF5Array,
rhdf5, digest, and callr. The manifest records normalized package paths for
diagnosis, but excludes paths from the cross-panel hash because Panel A and B
have different run-local roots. The current CerebroNexus path must be under
`run_context$library`; every later worker records the same resolved package
path for its own run.

`bench_install_tree()` receives the already verified clean Git SHA. Only after
a successful install it atomically writes a marker adjacent to the run-local
package containing that SHA, source path, package version, and install-log
SHA-256. `bench_validate_installed_tree()` requires the marker, expected
SHA/version, and a `find.package("CerebroNexus")` descendant of the marked
library before setup proceeds. This marker is run provenance, not a source-tree
modification.

- [ ] **Step 3: Implement Panel A orchestration**

`run_comparison.R` performs, in order:

1. locate repo and source `config.R`/`helpers.R`;
2. parse args, require a non-existent output path whose nearest existing parent
   is outside the Git worktree;
3. require clean Git state for real mode before creating anything;
4. create and mark the output and scratch roots;
5. install the current Git tree into the marked scratch library;
6. launch the unmeasured setup worker in that library to validate source
   bytes/SHA/CSR structure, read exact local `indptr` once, freeze
   `common_target_actual`, reject it when it is at/below 250k, select the fixed
   five-gene panel on 125k, and write source/sampling/eligibility/query/shell
   manifests;
7. finalize `manifest.csv` from the setup worker's actually loaded package and
   dependency versions, and verify the installed CerebroNexus revision matches
   the recorded Git tree;
8. run the fixed-tier exact-nnz gates;
9. freeze and write one 27-row pair schedule containing the exact separate
   `export_order` and `access_order` columns;
10. for each repeat, finish its nine exports, finish its nine accesses in the
    separately frozen access order, and safely remove every marked artifact
    after preserving its outcome/log (failed export partials are removed
    immediately; successful exports live only until their access attempt);
11. write `export.csv` and `access.csv` atomically as outcomes accumulate;
12. write `validation.csv` for exact expected IDs, duplicates, unscheduled and
    missing rows, outcome states, and all source/sampling/shell/query hashes;
13. remove the marked run-local library;
14. retain raw failures and logs, then exit non-zero unless final validation is
    `VALID`.

- [ ] **Step 4: Implement Panel B orchestration**

`run_full_scale.R` repeats Panel A's locate/parse/clean checks, additionally
requires the output candidate to differ from the Panel A input, then performs
a static, no-package gate on the Panel A directory before creating Panel B's
output. Required schemas must exist, `validation.csv` has exactly one final
`VALID`, all 27 expected exports and all 27 expected accesses are unique `OK`
rows, and there is no failed, not-run, missing, duplicate, unscheduled, or
fingerprint-mismatch gate. Failure exits non-zero before installation or a
Panel B worker.

It then creates/marks the Panel B output, installs the current tree, and runs
its own unmeasured setup worker. That worker revalidates the local source,
imports Panel A's frozen ordered five-gene
selection, recomputes the common sampling, shell, cell-identity and query
hashes, builds new source-side plans only for 1M, 2M, and full, and records the
actually loaded runtime fingerprint. Before any measured worker starts, the
runner requires exact equality with Panel A's source SHA, Git SHA,
schema/config, runtime fingerprint, `common_target_actual`, common sampling
hash, common shell hash, cell-identity hash, and common query-plan hash. It does
not accept a merely similar bridge tier. It schedules four BPCells tiers
across four Latin-order repeats; within each repeat it exports four artifacts,
accesses them in the separately frozen order, then cleans them. It writes
structural embedded/H5 eligibility rows without scheduling fake jobs.

Both runners write `manifest.csv`, `source.csv`, `sampling.csv`,
`eligibility.csv`, `queries.csv`, `query-plan.rds`, `schedule.csv`,
`export.csv`, `access.csv`, `validation.csv`, and per-worker logs. A marked
`<new-output-dir>/scratch/` is on the output filesystem, and the run-local
library is another marked child of it. There is no automatic memory/disk gate
or resource planner. README reports the maximum live-artifact counts (nine and
four) so the operator can select an appropriate output filesystem; disk-full
or OOM is a failed run and never triggers tier reduction.

- [ ] **Step 5: Run GREEN CLI tests**

```bash
Rscript --vanilla -e 'devtools::test(filter = "benchmark-contract", reporter = "summary", stop_on_failure = TRUE)'
Rscript --vanilla tests/bench/run_comparison.R --dry-run
Rscript --vanilla tests/bench/run_full_scale.R --dry-run
```

Expected: contracts pass; dry runs report 27/16 unqualified pairs and 54/32
workers, and perform no install, download, manifest read, or data access.

---

## Task 8: Remove the old framework and rewrite operator documentation

**Files:**

- Rewrite: `tests/bench/README.md`
- Create: `tests/bench/results/README.md`
- Modify: `.gitignore`
- Modify: `NEWS.md`
- Modify: `vignettes/expression_backend_benchmark.Rmd`
- Delete: old files listed in the final file map

- [ ] **Step 1: Delete the old framework with `apply_patch`**

Remove every old profile, numbered stage, resource planner, report/figure
generator, immutable publication file, pilot result, and old benchmark contract
test listed above. Do not copy anything from Temp wholesale.

- [ ] **Step 2: Simplify `.gitignore`**

Remove the old `tests/bench/result/`, `CURRENT`, `runs/`, and archive exception
tree. Ignore only generated scratch and logs under the new layout; keep
`tests/bench/results/README.md` tracked. Real evidence is added deliberately in
the later results commit.

- [ ] **Step 3: Write the complete README**

Document:

- the pinned URL, size, and hash;
- manual download and SHA verification;
- the fact that the output path selects the scratch filesystem, the nine/four
  live-artifact bounds, and that there is no automatic memory/disk gate;
- the requirement that new output paths are outside the Git worktree and do
  not already exist;
- `--dry-run` commands;
- the two real commands with positional paths;
- Panel A versus Panel B claims;
- 125k / 250k / `common_target_actual` and `common_target_actual` / 1M / 2M /
  full tiers;
- 27 Panel A pairs / 54 workers and 16 Panel B pairs / 32 workers;
- exact export/access rotations and per-repeat batching;
- synthetic metadata/projection disclosure;
- normalized floating-point `data`, not raw counts;
- external sidecar versus synthetic-shell CRB/total storage interpretation,
  including `NA` rather than zero for embedded sidecar bytes;
- OS cache, warmed-source setup, single-host technical-pair, descriptive-only,
  no-capacity-extrapolation, and no-biological-inference limits;
- every output CSV column, separate eligibility/outcome vocabulary, validation
  gate, and Panel B's strict dependency on Panel A's frozen manifests;
- the fact that there is no profile, resource gate, plot pipeline, `CURRENT`,
  automatic download, or automatic publication.

- [ ] **Step 4: Remove stale result claims**

Rewrite the vignette as a short protocol/status article with no fixed
performance conclusion and no old pilot figures. Delete both old PNGs. Add a
NEWS entry for the new public streaming-source support and the rebuilt harness;
state that reviewed full-scale results are not yet committed.

- [ ] **Step 5: Parse every retained benchmark file**

```bash
Rscript --vanilla -e 'for (f in Sys.glob("tests/bench/*.R")) parse(file = f)'
rg -n 'BENCH_PROFILE|BENCH_ALLOW_UNSAFE|tests/bench/src/|tests/bench/result/|result/CURRENT' \
  tests/bench tests/testthat/test-benchmark-contract.R \
  vignettes/expression_backend_benchmark.Rmd
rg -n 'inst/(extdata|shiny)/v1\.4' \
  R tests/bench tests/testthat/test-benchmark-contract.R \
  vignettes/expression_backend_benchmark.Rmd
```

Expected: all R files parse. Review any precise legacy-control match; prose in
the README may explicitly state that profiles and `CURRENT` no longer exist.
The versioned Viewer-path search must be empty in current code/protocol files.
Do not search broad words such as `quick`, `standard`, or `publication` across
the whole repository, and do not rewrite legitimate historical NEWS entries.

---

## Task 9: Focused verification and the implementation commit

**Files:** all implementation files above.

- [ ] **Step 1: Run preliminary focused tests**

```bash
Rscript --vanilla -e 'devtools::test(filter = "benchmark-contract|exportFromSeurat|seurat-v5-split-layers|export-data-integrity|versionless-layout-contract", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: zero failures.

- [ ] **Step 2: Perform specification and quality reviews**

Run one read-only specification review against every acceptance criterion in
the design and one independent code-quality review. Fix every Critical or
Important finding and any concrete Minor correctness issue before final
verification. Run the smallest affected test while iterating; Steps 3–6 are the
single final verification pass after review fixes are stable.

- [ ] **Step 3: Re-run the final focused set after reviews**

```bash
Rscript --vanilla -e 'devtools::test(filter = "benchmark-contract|exportFromSeurat|seurat-v5-split-layers|export-data-integrity|versionless-layout-contract", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: zero failures after all review fixes.

- [ ] **Step 4: Install the package from the reviewed current tree**

```bash
(
  set -eu
  bench_verify_root="$(mktemp -d "${TMPDIR:-/tmp}/cerebronexus-verify.XXXXXX")"
  bench_repo_root="$(pwd -P)"
  case "$bench_verify_root" in
    ""|/|"$bench_repo_root") exit 2 ;;
  esac
  touch "$bench_verify_root/.cerebronexus-verify-root"
  bench_cleanup_verify() {
    test -n "$bench_verify_root" || return 2
    test "$bench_verify_root" != / || return 2
    test "$bench_verify_root" != "$bench_repo_root" || return 2
    test -f "$bench_verify_root/.cerebronexus-verify-root" || return 2
    rm -rf -- "$bench_verify_root"
  }
  trap bench_cleanup_verify EXIT
  mkdir "$bench_verify_root/library"
  R CMD INSTALL --no-docs --no-byte-compile \
    --library="$bench_verify_root/library" .
)
```

Expected: installation exits zero. The trap can remove only the marked
`mktemp -d` directory after the explicit path checks.

- [ ] **Step 5: Run dry-run contracts against the reviewed tree**

```bash
Rscript --vanilla tests/bench/run_comparison.R --dry-run
Rscript --vanilla tests/bench/run_full_scale.R --dry-run
```

Expected: Panel A prints 27 pairs / 54 workers; Panel B prints 16 pairs / 32
workers; both say `UNQUALIFIED`, and neither touches data or manifests.

- [ ] **Step 6: Run the complete checkout test and built-package check once**

```bash
Rscript --vanilla -e 'devtools::test(reporter = "summary", stop_on_failure = TRUE)'
(
  set -eu
  bench_check_root="$(mktemp -d "${TMPDIR:-/tmp}/cerebronexus-check.XXXXXX")"
  bench_repo_root="$(pwd -P)"
  case "$bench_check_root" in
    ""|/|"$bench_repo_root") exit 2 ;;
  esac
  touch "$bench_check_root/.cerebronexus-check-root"
  bench_cleanup_check() {
    test -n "$bench_check_root" || return 2
    test "$bench_check_root" != / || return 2
    test "$bench_check_root" != "$bench_repo_root" || return 2
    test -f "$bench_check_root/.cerebronexus-check-root" || return 2
    rm -rf -- "$bench_check_root"
  }
  trap bench_cleanup_check EXIT
  cd "$bench_check_root"
  R CMD build --no-manual "$bench_repo_root"
  bench_tarball="$(find . -maxdepth 1 -type f -name 'CerebroNexus_*.tar.gz' -print -quit)"
  test -n "$bench_tarball"
  R CMD check --no-manual "$bench_tarball"
)
```

Expected: checkout-level `devtools::test()` executes all harness contracts.
Build/check finish without errors or warnings introduced by this change;
repo-only benchmark contracts have their documented skips because
`tests/bench` is excluded, while product API tests execute. Review check notes
rather than silently ignoring them. The marked temp root and tarball are
removed by the trap.

- [ ] **Step 7: Run formatting and scope checks**

```bash
git diff --check
git status --short
git diff --stat
git diff --name-only
git diff --cached --stat
git diff --cached --name-only
```

Inspect the complete diff and every untracked path reported by `git status`;
ordinary `git diff` does not include untracked files. Confirm no Builder path,
versioned Viewer path, generated full-scale result, remote operation, or
unrelated file appears. The final search must find the one retained repo-only
benchmark exclusion that the test guard is designed for.

- [ ] **Step 8: Stage exact paths and inspect the verified implementation**

```bash
git add \
  R/exportFromSeurat.R R/seurat_utils.R man/exportFromSeurat.Rd \
  tests/testthat/test-exportFromSeurat.R \
  tests/testthat/test-seurat-v5-split-layers.R \
  tests/testthat/test-benchmark-contract.R \
  tests/bench/README.md tests/bench/config.R tests/bench/helpers.R \
  tests/bench/run_comparison.R tests/bench/run_full_scale.R \
  tests/bench/results/README.md \
  .gitignore DESCRIPTION create_env.R default.nix NEWS.md \
  vignettes/expression_backend_benchmark.Rmd
git add -u -- \
  tests/bench/METHODOLOGY.md tests/bench/RESULTS.md \
  tests/bench/run_sweep.sh tests/bench/config tests/bench/design \
  tests/bench/lib tests/bench/src tests/bench/result \
  tests/testthat/test-bench-access-metrics.R \
  tests/testthat/test-bench-cli-contract.R \
  tests/testthat/test-bench-protocol.R \
  tests/testthat/test-bench-publication.R \
  tests/testthat/test-bench-remote-reader.R \
  tests/testthat/test-bench-reporting.R \
  tests/testthat/test-bench-resources.R \
  vignettes/img/expression_backend_benchmark_ceiling.png \
  vignettes/img/expression_backend_benchmark_overview.png \
  other_documents/benchmark_accessing_DelayedArray.Rmd \
  other_documents/benchmark_accessing_DelayedArray.html
git diff --cached --check
git diff --cached --stat
git diff --cached --name-only
git status --short
```

If `devtools::document()` legitimately changed `NAMESPACE`, add that exact
file after reviewing it. Verify the `DESCRIPTION` diff adds only `digest` and
retains the existing `rhdf5` entry.

- [ ] **Step 9: Commit the implementation as the second logical commit**

```bash
git commit -m "feat(bench): implement controlled and full-scale benchmarks"
```

Deleted paths are intentionally included. Do not push.

---

## Task 10: Run and publish real evidence later

This task is intentionally separate from code implementation and must run on a
host chosen for the real benchmark.

- [ ] **Step 1: Reconfirm the exact clean code revision and source identity**

```bash
git status --porcelain=v1
git rev-parse HEAD
shasum -a 256 /path/to/mssm.h5ad
wc -c /path/to/mssm.h5ad
```

Expected: clean tree, source hash and bytes match `config.R`.

- [ ] **Step 2: Run Panel A without competing workloads**

```bash
Rscript --vanilla tests/bench/run_comparison.R \
  /path/to/mssm.h5ad \
  /new/path/cerebro-benchmark-comparison-<git-sha>
```

- [ ] **Step 3: Review Panel A before Panel B claims**

Require 27 unique `OK` export rows, 27 unique `OK` access rows, matching query
fingerprints, finite non-negative timings where a metric applies, exact
eligibility/sampling/shell/query manifests, and final `VALID` in
`validation.csv`. Sampled RSS may be missing only when explicitly recorded as
unobserved, never zero-filled. Do not continue to Panel B or publication if a
core row is failed, not run, missing, duplicated, or unscheduled.

- [ ] **Step 4: Run Panel B without competing workloads**

```bash
Rscript --vanilla tests/bench/run_full_scale.R \
  /path/to/mssm.h5ad \
  /new/path/cerebro-benchmark-comparison-<git-sha> \
  /new/path/cerebro-benchmark-full-scale-<git-sha>
```

Require 16 unique export rows and 16 access rows, including four successful
4,140,453-cell export/access pairs, a `VALID` final gate, and exact equality of
the imported Panel A linkage hashes. Do not publish a partial full-data result.

- [ ] **Step 5: Review and publish only validated artifacts**

Copy both complete validated evidence directories into
`tests/bench/results/<git-sha>/{panel-a,panel-b}/`, preserving
`manifest.csv`, `source.csv`, `sampling.csv`, `eligibility.csv`, `queries.csv`,
`query-plan.rds`, `schedule.csv`, `export.csv`, `access.csv`, `validation.csv`,
and all setup/worker logs. Add an inventory with file byte sizes and SHA-256
hashes plus a concise result interpretation; do not publish only summary or
timing CSVs. Update the vignette with measured tables and clearly state:

- real MSSM normalized expression plus synthetic metadata/projection shell;
- external sidecar bytes; synthetic-shell CRB and total-artifact bytes with
  non-representative compression characteristics; embedded sidecar bytes are
  structurally inapplicable (`NA`), and CRB/total are not estimates of a full
  MSSM application file;
- one host and separate fresh-process technical pairs, with no inferential
  confidence interval;
- OS cache uncontrolled;
- query-plan setup warms the source and the warmed query repeats one gene;
- Panel A common-input comparison versus Panel B direct-streaming observed
  tiers; the two common BPCells bridge observations are not pooled;
- structural N/A versus failure;
- no capacity, linearity, or performance inference beyond observed tiers.

Aggregate exactly as specified in the design: technical-pair median
`[min, max]` and `n`, Panel A within-repeat backend-to-embedded differences or
ratios, Panel B four observed tiers without a fitted/extrapolated curve, and no
use of the five warmed calls as separate replicates. Report deterministic byte
disagreement as an inconsistency rather than uncertainty.

- [ ] **Step 6: Commit the evidence**

```bash
git add tests/bench/results vignettes/expression_backend_benchmark.Rmd NEWS.md
git commit -m "docs(bench): publish validated benchmark results"
```

Do not push or force-push until this evidence commit has passed review and the
remote lease has been fetched and revalidated.

---

## Plan self-check

- Every design acceptance criterion maps to Tasks 1–9.
- The arbitrary-size sampler selects every one of the 4,140,453 cells at full
  tier, and the full source is scheduled in four export/access pairs.
- Panel A and Panel B use the same source and nested sampling, but their BPCells
  bridge inputs are explicitly labeled `dgCMatrix` versus direct H5AD stream.
- Panel B consumes and validates Panel A's frozen source, Git, schema, common
  tier, sampling, synthetic-shell, and query-plan hashes.
- Panel A contains 27 pairs / 54 workers; Panel B contains 16 pairs / 32
  workers. Every repeat uses separately frozen export and access order, with
  per-repeat artifact batching.
- The plan contains no real-data execution during implementation.
- The plan contains no automatic download, memory-based tier selection,
  generic profile, plotting pipeline, `CURRENT`, push, or PR action.
- Eligibility, raw runtime outcomes, and validator-derived missing results are
  separate schemas, and a failed core row cannot disappear from evidence.
- All code paths use `inst/extdata/examples` and
  `inst/viewer/utility_functions.R`; no Temp v1.4 path is accepted.
- Final history remains three logical commits: design, implementation, results.
