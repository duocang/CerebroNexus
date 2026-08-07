## Tests for exportFromSeurat()
##
## Uses the bundled example Seurat object (inst/extdata/examples/pbmc_seurat.rds).
## All tests require Seurat; skipped gracefully when it is not installed.

skip_if_not_installed("Seurat")

## ---------------------------------------------------------------------------
## Load and prepare the shared object once for the whole file
## ---------------------------------------------------------------------------

pbmc_path <- system.file(
  "extdata/examples/pbmc_seurat.rds",
  package = "CerebroNexus"
)
if (!nzchar(pbmc_path)) {
  pbmc_path <- testthat::test_path(
    "../../inst/extdata/examples/pbmc_seurat.rds"
  )
}
obj_raw <- readRDS(pbmc_path)

## Convenience: shared valid call args (no file — added per test)
valid_args <- list(
  object = obj_raw,
  experiment_name = "PBMC test",
  organism = "hg",
  groups = c("sample", "seurat_clusters"),
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA"
)

make_bpcells_source_object <- function(root) {
  expression <- Matrix::sparseMatrix(
    i = c(1L, 3L, 2L, 1L, 4L, 2L),
    j = c(1L, 1L, 2L, 3L, 4L, 5L),
    x = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
    dims = c(4L, 5L),
    dimnames = list(
      paste0("gene", seq_len(4L)),
      paste0("cell", seq_len(5L))
    )
  )
  matrix_dir <- file.path(root, "source.bpcells")
  suppressWarnings(suppressMessages(BPCells::write_matrix_dir(
    methods::as(expression, "IterableMatrix"),
    dir = matrix_dir
  )))
  assay <- SeuratObject::CreateAssay5Object(
    data = BPCells::open_matrix_dir(dir = matrix_dir)
  )
  object <- suppressWarnings(
    Seurat::CreateSeuratObject(counts = assay, assay = "RNA")
  )
  object$sample <- factor(c("s1", "s1", "s2", "s2", "s3"))
  object$nUMI <- 0
  object$nGene <- 0
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      seq_len(10L) / 10,
      nrow = 5L,
      dimnames = list(colnames(object), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )

  list(object = object, expression = expression)
}

expect_disk_source_rejected <- function(mode, fixture, root) {
  expect_error(
    exportFromSeurat(
      object = fixture$object,
      file = file.path(root, paste0("unsupported-", mode, ".crb")),
      experiment_name = "BPCells source rejection",
      assay = "RNA",
      slot = "data",
      organism = "hg",
      groups = "sample",
      nUMI = "nUMI",
      nGene = "nGene",
      add_all_meta_data = FALSE,
      expression_matrix_mode = mode,
      verbose = FALSE
    ),
    regexp = "disk-backed|IterableMatrix"
  )
}

## ---------------------------------------------------------------------------
## Input validation
## ---------------------------------------------------------------------------

test_that("exportFromSeurat: rejects non-Seurat object", {
  expect_error(
    exportFromSeurat(
      object = list(),
      file = tempfile(fileext = ".crb"),
      experiment_name = "test",
      organism = "hg",
      groups = "sample",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA"
    ),
    regexp = "must be of class 'Seurat'"
  )
})

test_that("exportFromSeurat: rejects missing group column", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$groups <- "nonexistent_col"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "Some group columns could not be found"
  )
})

test_that("exportFromSeurat: rejects missing nUMI column", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$nUMI <- "missing_nUMI"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "not found in meta data"
  )
})

test_that("exportFromSeurat: rejects missing nGene column", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$nGene <- "missing_nGene"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "not found in meta data"
  )
})

test_that("exportFromSeurat: rejects missing assay", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$assay <- "SCT"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "could not be found in provided Seurat"
  )
})

test_that("exportFromSeurat: rejects missing cell_cycle column", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$cell_cycle <- "no_such_phase_col"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "Some cell cycle columns could not be found"
  )
})

## ---------------------------------------------------------------------------
## Happy-path integration test
## ---------------------------------------------------------------------------

test_that("exportFromSeurat: produces a valid .crb file from pbmc_seurat.rds", {
  outf <- tempfile(fileext = ".crb")
  args <- valid_args
  args$file <- outf

  expect_no_error(do.call(exportFromSeurat, args))

  ## file must exist and be non-empty
  expect_true(file.exists(outf))
  expect_gt(file.size(outf), 0)

  ## load and inspect the Cerebro object
  cerebro <- readRDS(outf)
  expect_true(inherits(cerebro, "Cerebro_v1.3"))

  ## experiment metadata
  exp <- cerebro$getExperiment()
  expect_equal(exp$experiment_name, "PBMC test")
  expect_equal(exp$organism, "hg")

  ## groups
  groups <- cerebro$getGroups()
  expect_true("sample" %in% groups)
  expect_true("seurat_clusters" %in% groups)

  ## group levels
  expect_true("pbmc_1" %in% cerebro$getGroupLevels("sample"))
  expect_true("pbmc_2" %in% cerebro$getGroupLevels("sample"))

  ## cell count preserved
  expect_equal(nrow(cerebro$getMetaData()), ncol(obj_raw))

  ## projections: UMAP should be present
  projs <- cerebro$availableProjections()
  expect_true(any(grepl("umap|UMAP", projs, ignore.case = TRUE)))

  ## expression matrix: genes x cells
  expr <- cerebro$expression
  expect_false(is.null(expr))
  expect_equal(ncol(expr), ncol(obj_raw))
  expect_equal(nrow(expr), nrow(obj_raw))
})

## ---------------------------------------------------------------------------
## h5 backend round-trip
## ---------------------------------------------------------------------------

test_that("exportFromSeurat: h5 mode writes a TENxMatrix-compatible sibling
           and keeps crb$expression NULL so saveRDS does not embed the
           matrix; round-trips bit-exact via lazy HDF5Array::TENxMatrix", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("Matrix")

  out_dir <- file.path(tempdir(), paste0("h5_rt_", as.integer(Sys.time())))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  outf <- file.path(out_dir, "trip.crb")

  args <- valid_args
  args$file <- outf
  args$expression_matrix_mode <- "h5"
  args$verbose <- FALSE

  expect_no_error(do.call(exportFromSeurat, args))
  expect_true(file.exists(outf))

  ## crb side: expression stays NULL (no in-memory dgCMatrix payload, so
  ## saveRDS does not embed the matrix and the .crb stays small) and the
  ## backend tag points at the sibling .h5.
  cerebro <- readRDS(outf)
  expect_null(
    cerebro$expression,
    label = "crb$expression must be NULL so saveRDS does not embed the matrix"
  )
  be <- cerebro$getExpressionBackend()
  expect_equal(be$type, "h5")
  expect_identical(be$location, "trip.h5")
  h5_path <- file.path(out_dir, be$location)
  expect_true(file.exists(h5_path))

  ## h5 side: TENxMatrix-readable. No direct rhdf5 dependency.
  m <- HDF5Array::TENxMatrix(h5_path, group = "expression")
  expect_s4_class(m, "TENxMatrix")
  ## On-disk layout is cells × genes (TENx column-favoured, optimised for
  ## per-gene column reads). Cerebro's internal layout is genes × cells.
  m_internal <- t(m)
  expect_s4_class(m_internal, "DelayedMatrix")

  ## bit-exact round-trip vs the input matrix
  orig <- SeuratObject::GetAssayData(obj_raw, layer = "data")
  expect_equal(dim(m_internal), dim(orig))
  expect_setequal(rownames(m_internal), rownames(orig))
  expect_setequal(colnames(m_internal), colnames(orig))
  realised <- as.matrix(m_internal[rownames(orig), colnames(orig)])
  delta <- max(abs(realised - as.matrix(orig)))
  expect_equal(delta, 0)
})

test_that("exportFromSeurat: h5 mode errors clearly when HDF5Array is missing", {
  skip_if(requireNamespace("HDF5Array", quietly = TRUE))
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$expression_matrix_mode <- "h5"
  expect_error(do.call(exportFromSeurat, args), regexp = "HDF5Array")
})

test_that("h5 attach is lazy: .attachExternalExpression returns a DelayedMatrix
           seed, not an eagerly materialised dgCMatrix (low RAM, instant attach)", {
  skip_if_not_installed("HDF5Array")

  ## source the runtime attach helper from inst/ — it's a Shiny utility,
  ## not part of the package namespace
  inst_util <- system.file(
    "viewer/utility_functions.R",
    package = "CerebroNexus"
  )
  if (!nzchar(inst_util)) {
    inst_util <- testthat::test_path(
      "../../inst/viewer/utility_functions.R"
    )
  }
  ## load only the symbol we need into a fresh env to avoid namespace pollution
  attach_env <- new.env(parent = globalenv())
  source(inst_util, local = attach_env, echo = FALSE)
  skip_if_not(
    is.function(attach_env$.attachExternalExpression),
    ".attachExternalExpression not found in utility_functions.R"
  )

  out_dir <- file.path(tempdir(), paste0("h5_attach_", as.integer(Sys.time())))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  outf <- file.path(out_dir, "trip.crb")

  args <- valid_args
  args$file <- outf
  args$expression_matrix_mode <- "h5"
  args$verbose <- FALSE
  do.call(exportFromSeurat, args)

  cerebro <- readRDS(outf)
  expect_null(cerebro$expression)

  attached <- attach_env$.attachExternalExpression(cerebro, outf)

  ## the attach must NOT materialise a dgCMatrix in RAM — that defeats the
  ## entire point of the h5 backend (Roman Hillje's vignette
  ## `create_expression_matrix_in_h5_format.Rmd`).
  expect_false(
    inherits(attached$expression, "dgCMatrix"),
    info = "h5 attach must stay lazy; got an in-memory dgCMatrix"
  )
  expect_s4_class(attached$expression, "DelayedMatrix")

  ## but it should still expose Cerebro's genes × cells layout
  orig <- SeuratObject::GetAssayData(obj_raw, layer = "data")
  expect_equal(nrow(attached$expression), nrow(orig))
  expect_equal(ncol(attached$expression), ncol(orig))
  expect_setequal(rownames(attached$expression), rownames(orig))
  expect_setequal(colnames(attached$expression), colnames(orig))
})

test_that("public bpcells export streams a BPCells-backed Seurat layer and
          remains portable when the crb and sibling sidecar move together", {
  skip_if_not_installed("BPCells")

  root <- withr::local_tempdir()
  fixture <- make_bpcells_source_object(root)
  export_dir <- file.path(root, "export")
  moved_dir <- file.path(root, "moved")
  dir.create(export_dir)
  dir.create(moved_dir)
  crb <- file.path(export_dir, "streamed.crb")

  export_ok <- FALSE
  expect_no_error({
    exportFromSeurat(
      object = fixture$object,
      file = crb,
      experiment_name = "BPCells source streaming",
      assay = "RNA",
      slot = "data",
      organism = "hg",
      groups = "sample",
      nUMI = "nUMI",
      nGene = "nGene",
      add_all_meta_data = FALSE,
      expression_matrix_mode = "bpcells",
      verbose = FALSE
    )
    export_ok <- TRUE
  })
  if (!export_ok) {
    return(invisible(NULL))
  }

  crb_exists <- file.exists(crb)
  expect_true(crb_exists)
  if (!crb_exists) {
    return(invisible(NULL))
  }

  cerebro <- readRDS(crb)
  expect_identical(
    cerebro$getExpressionBackend(),
    list(type = "bpcells", location = "streamed.bpcells")
  )

  sidecar <- file.path(export_dir, "streamed.bpcells")
  moved_crb <- file.path(moved_dir, "streamed.crb")
  moved_sidecar <- file.path(moved_dir, "streamed.bpcells")
  crb_moved <- file.rename(crb, moved_crb)
  sidecar_moved <- file.rename(sidecar, moved_sidecar)
  expect_true(crb_moved)
  expect_true(sidecar_moved)
  if (!crb_moved || !sidecar_moved) {
    return(invisible(NULL))
  }

  inst_util <- system.file(
    "viewer/utility_functions.R",
    package = "CerebroNexus"
  )
  if (!nzchar(inst_util)) {
    inst_util <- testthat::test_path(
      "../../inst/viewer/utility_functions.R"
    )
  }
  attach_env <- new.env(parent = globalenv())
  source(inst_util, local = attach_env, echo = FALSE)
  attached <- attach_env$.attachExternalExpression(
    readRDS(moved_crb),
    moved_crb
  )

  expect_equal(as.matrix(attached$expression), as.matrix(fixture$expression))
})

test_that("embedded export rejects a BPCells-backed Seurat source clearly", {
  skip_if_not_installed("BPCells")

  root <- withr::local_tempdir()
  fixture <- make_bpcells_source_object(root)
  expect_disk_source_rejected("embedded", fixture, root)
})

test_that("h5 export rejects a BPCells-backed Seurat source clearly", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  fixture <- make_bpcells_source_object(root)
  expect_disk_source_rejected("h5", fixture, root)
})
