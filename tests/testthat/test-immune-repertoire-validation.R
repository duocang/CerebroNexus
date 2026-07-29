## Tests for the shape check applied to immune repertoire data on export.
##
## The app reads `immune_repertoire` as a list named by sample, each element a
## data.frame carrying at least `barcode` (the key joining a receptor to a cell)
## and `CTgene` (what chain detection scans). Nothing enforced that. A flat
## data.frame passes `is.list()` and `length()` -- its columns become the sample
## names -- so a wrong shape reached the .crb intact and only came apart in the
## running app, where the sample dropdown lists column names and every panel is
## empty.

skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")

## ---------------------------------------------------------------------------
## Fixtures
## ---------------------------------------------------------------------------

make_repertoire_seurat <- function(n_genes = 30, n_cells = 8) {
  set.seed(11)
  counts <- matrix(
    rpois(n_genes * n_cells, 3),
    nrow = n_genes,
    ncol = n_cells,
    dimnames = list(
      paste0("g", seq_len(n_genes)),
      paste0("cell", seq_len(n_cells))
    )
  )
  obj <- Seurat::CreateSeuratObject(
    counts = methods::as(counts, "CsparseMatrix")
  )
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj$sample <- rep(c("s1", "s2"), length.out = n_cells)
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      rnorm(n_cells * 2),
      ncol = 2,
      dimnames = list(colnames(obj), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )
  obj
}

## The shape the app consumes: one data.frame per sample, keyed by barcode.
make_repertoire <- function(object) {
  barcodes <- colnames(object)
  split_by <- object$sample
  lapply(split(barcodes, split_by), function(bc) {
    data.frame(
      barcode = bc,
      CTgene = paste0("TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1"),
      CTaa = paste0("CAVR", seq_along(bc), "F_CASSL", seq_along(bc), "F"),
      CTnt = NA_character_,
      stringsAsFactors = FALSE
    )
  })
}

export_repertoire <- function(object, file = tempfile(fileext = ".crb"), ...) {
  exportFromSeurat(
    object = object,
    file = file,
    experiment_name = "repertoire shape",
    organism = "hg",
    groups = "sample",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    verbose = FALSE,
    ...
  )
  invisible(file)
}

## ---------------------------------------------------------------------------
## The unified slot
## ---------------------------------------------------------------------------

test_that("a flat data.frame in the slot is refused, with the fix in the text", {
  ## `is.list(a_data_frame)` is TRUE and `length()` is its column count, so the
  ## old guard waved this through and `names()` became the column names.
  object <- make_repertoire_seurat()
  object@misc$immune_repertoire <- do.call(rbind, make_repertoire(object))

  expect_error(export_repertoire(object), regexp = "named list")
  expect_error(export_repertoire(object), regexp = "list\\(")
})

test_that("an unnamed list is refused: sample names are the app's labels", {
  object <- make_repertoire_seurat()
  object@misc$immune_repertoire <- unname(make_repertoire(object))

  expect_error(export_repertoire(object), regexp = "name")
})

test_that("an element that is not a data.frame is named in the error", {
  object <- make_repertoire_seurat()
  repertoire <- make_repertoire(object)
  repertoire$s2 <- "not a data frame"
  object@misc$immune_repertoire <- repertoire

  err <- tryCatch(
    export_repertoire(object),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("s2", err, fixed = TRUE))
  expect_true(grepl("character", err, fixed = TRUE))
})

test_that("a missing barcode column is refused: it is the only join to a cell", {
  object <- make_repertoire_seurat()
  repertoire <- lapply(make_repertoire(object), function(df) {
    df$barcode <- NULL
    df
  })
  object@misc$immune_repertoire <- repertoire

  expect_error(export_repertoire(object), regexp = "barcode")
})

test_that("a missing CTgene column warns rather than stops", {
  ## Chain detection scans CTgene, so without it the repertoire loads but no
  ## chain is recognised. That is a degraded page, not an unusable file.
  object <- make_repertoire_seurat()
  repertoire <- lapply(make_repertoire(object), function(df) {
    df$CTgene <- NULL
    df
  })
  object@misc$immune_repertoire <- repertoire

  expect_warning(export_repertoire(object), regexp = "CTgene")
})

test_that("a slot matching no cell warns and still exports", {
  ## An export did not ask for this data, it found it. Subsetting a Seurat
  ## object keeps `@misc`, so "run TCR, then keep one compartment" arrives here
  ## with a stale slot. Refusing would block an export that used to succeed
  ## with an empty repertoire page, so the export says so and carries on.
  object <- make_repertoire_seurat()
  repertoire <- lapply(make_repertoire(object), function(df) {
    df$barcode <- paste0("donorA_", df$barcode)
    df
  })
  object@misc$immune_repertoire <- repertoire

  crb <- tempfile(fileext = ".crb")
  expect_warning(
    export_repertoire(object, file = crb),
    regexp = "shares no barcode"
  )
  expect_true(file.exists(crb))
  ## the repertoire is kept rather than silently dropped
  expect_length(readRDS(crb)$getImmuneRepertoire(), 2)
})

test_that("the no-overlap message names both causes, not just prefixes", {
  ## Blaming combineTCR() sends anyone whose object was subset after the fact
  ## looking for a prefix problem they do not have.
  object <- make_repertoire_seurat()
  repertoire <- lapply(make_repertoire(object), function(df) {
    df$barcode <- paste0("donorA_", df$barcode)
    df
  })
  object@misc$immune_repertoire <- repertoire

  msg <- tryCatch(
    export_repertoire(object),
    warning = function(w) conditionMessage(w)
  )
  expect_true(grepl("RenameCells", msg, fixed = TRUE))
  expect_true(grepl("subset", msg, fixed = TRUE))
  ## and it shows both sides so the mismatch is visible
  expect_true(grepl("donorA_cell", msg, fixed = TRUE))
})

test_that("a stale legacy slot warns rather than blocking the export", {
  ## Same reasoning for the legacy slots, which `getImmuneRepertoire()` merges
  ## when the unified one is empty.
  object <- make_repertoire_seurat()
  object@misc$tcr_data <- lapply(make_repertoire(object), function(df) {
    df$barcode <- paste0("stale_", df$barcode)
    df
  })

  crb <- tempfile(fileext = ".crb")
  expect_warning(
    export_repertoire(object, file = crb),
    regexp = "shares no barcode"
  )
  expect_true(file.exists(crb))
})

test_that("shape errors stay fatal even when overlap is only a warning", {
  ## Tolerance is about the join, not about the shape: data the app cannot
  ## read at all must still stop the export on every path.
  object <- make_repertoire_seurat()
  object@misc$bcr_data <- list(
    s1 = data.frame(CTgene = "TRAV1", stringsAsFactors = FALSE)
  )
  expect_error(export_repertoire(object), "barcode")
})

test_that("one sample matching no cell warns while the others still export", {
  ## Cells being filtered after the repertoire was combined is ordinary, so a
  ## partial match must not fail. A whole sample matching nothing while its
  ## neighbours match is a naming problem worth saying out loud.
  object <- make_repertoire_seurat()
  repertoire <- make_repertoire(object)
  repertoire$s2$barcode <- paste0("mismatched_", repertoire$s2$barcode)
  object@misc$immune_repertoire <- repertoire

  expect_warning(export_repertoire(object), regexp = "match no cell")
})

test_that("a duplicated sample name is refused: the second entry is unreachable", {
  ## `x[["donorA"]]` returns the first match only, so concatenating a TCR list
  ## and a BCR list describing the same samples silently drops one of them.
  object <- make_repertoire_seurat()
  repertoire <- make_repertoire(object)
  object@misc$immune_repertoire <- c(repertoire, repertoire)

  err <- tryCatch(
    export_repertoire(object),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("more than one entry named", err, fixed = TRUE))
  expect_true(grepl("row-bind", err, fixed = TRUE))
})

test_that("the shape the app consumes exports without complaint", {
  object <- make_repertoire_seurat()
  object@misc$immune_repertoire <- make_repertoire(object)

  expect_no_error(expect_no_warning(export_repertoire(object)))
})

## ---------------------------------------------------------------------------
## The legacy slots
## ---------------------------------------------------------------------------

test_that("the legacy tcr_data slot is held to the same shape", {
  ## `getImmuneRepertoire()` falls back to merging `bcr_data` and `tcr_data`,
  ## so a legacy slot that skipped validation would be a way around it.
  object <- make_repertoire_seurat()
  object@misc$tcr_data <- lapply(make_repertoire(object), function(df) {
    df$barcode <- NULL
    df
  })

  expect_error(export_repertoire(object), regexp = "barcode")
})

test_that("the legacy bcr_data slot is held to the same shape", {
  object <- make_repertoire_seurat()
  object@misc$bcr_data <- lapply(make_repertoire(object), function(df) {
    df$barcode <- NULL
    df
  })

  expect_error(export_repertoire(object), regexp = "barcode")
})

test_that("legacy BCR and TCR are unified per sample at export", {
  ## R6 methods are serialized into a .crb, so changing the class getter later
  ## does not repair an object already written. The export boundary has to put
  ## new files into the one-entry-per-sample shape that their serialized getter
  ## already prefers.
  object <- make_repertoire_seurat()
  tcr <- make_repertoire(object)
  bcr <- lapply(make_repertoire(object), function(df) {
    df$CTgene <- "IGHV1.IGHJ4.IGHM_IGKV3.IGKJ1.IGKC"
    df
  })
  object@misc$tcr_data <- tcr
  object@misc$bcr_data <- bcr

  crb_path <- export_repertoire(object)
  loaded <- readRDS(crb_path)
  repertoire <- loaded$getImmuneRepertoire()

  expect_setequal(names(repertoire), names(tcr))
  expect_false(anyDuplicated(names(repertoire)) > 0)
  expect_true(any(grepl("^TRAV", repertoire[["s1"]]$CTgene)))
  expect_true(any(grepl("^IGHV", repertoire[["s1"]]$CTgene)))
  ## Keep the legacy fields for callers of getTCR()/getBCR().
  expect_equal(loaded$getTCR(), tcr)
  expect_equal(loaded$getBCR(), bcr)
})

## ---------------------------------------------------------------------------
## No false positives on shipped data
## ---------------------------------------------------------------------------

test_that("the bundled TCR/BCR demo passes the check", {
  crb_path <- system.file(
    "extdata/v1.4/demo_full_tcr_bcr.crb",
    package = "CerebroNexus"
  )
  if (!nzchar(crb_path)) {
    crb_path <- testthat::test_path(
      "../../inst/extdata/v1.4/demo_full_tcr_bcr.crb"
    )
  }
  skip_if_not(file.exists(crb_path), "demo_full_tcr_bcr.crb not available")

  repertoire <- readRDS(crb_path)$getImmuneRepertoire()
  skip_if(length(repertoire) == 0, "demo carries no immune repertoire")

  expect_no_error(expect_no_warning(
    .validateImmuneRepertoire(
      repertoire,
      source_label = "`@misc$immune_repertoire`"
    )
  ))
})
