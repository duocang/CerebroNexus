test_that("external modes require a distinct .crb output path", {
  object <- make_ir_seurat(n_cells = 12L)
  output <- file.path(withr::local_tempdir(), "dataset.h5")

  expect_error(
    export_ir(object, output, expression_matrix_mode = "h5"),
    regexp = "same path|\\.crb|sidecar",
    ignore.case = TRUE
  )
  expect_false(file.exists(output))
})

test_that("external export basenames are portable across filesystems", {
  skip_if_not_installed("HDF5Array")

  object <- make_ir_seurat(n_cells = 12L)
  root <- withr::local_tempdir()
  for (name in c(
    "CON.crb",
    "CON .crb",
    "COM1 .crb",
    "bad:name.crb",
    ".crb",
    "bad?.crb"
  )) {
    output <- file.path(root, name)
    expect_error(
      export_ir(object, output, expression_matrix_mode = "h5"),
      regexp = "portable|reserved|file name",
      ignore.case = TRUE
    )
    expect_false(file.exists(output))
  }
})

artifact_mode <- function(path) {
  sprintf("%03o", bitwAnd(as.integer(file.info(path)$mode), 511L))
}

minimal_export <- function(type = "embedded", location = NULL) {
  export <- new.env(parent = emptyenv())
  backend <- list(type = type, location = location)
  export$expression_backend <- backend
  export$getExpressionBackend <- local({
    function() backend
  })
  class(export) <- c("Cerebro", "R6")
  export
}

test_that("the export stage and new artifacts are owner-only", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  stage <- .createPrivateExportStage(output)
  withr::defer(unlink(stage, recursive = TRUE, force = TRUE))
  expect_identical(artifact_mode(stage), "700")

  .publishCerebroExport(
    export = minimal_export(),
    final_file = output,
    stage_dir = stage,
    expression_matrix_mode = "embedded"
  )
  expect_identical(artifact_mode(output), "600")

  sidecar_name <- "external.h5"
  sidecar_output <- file.path(root, "external.crb")
  sidecar_stage <- .createPrivateExportStage(sidecar_output)
  withr::defer(unlink(sidecar_stage, recursive = TRUE, force = TRUE))
  writeLines("private matrix", file.path(sidecar_stage, sidecar_name))
  .publishCerebroExport(
    export = minimal_export("h5", sidecar_name),
    final_file = sidecar_output,
    stage_dir = sidecar_stage,
    expression_matrix_mode = "h5"
  )
  expect_identical(
    artifact_mode(file.path(root, sidecar_name)),
    "600"
  )
})

test_that("replacing a CRB preserves its existing mode", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  saveRDS(minimal_export(), output)
  Sys.chmod(output, mode = "0600")
  stage <- file.path(root, "stage")
  dir.create(stage, mode = "0700")

  .publishCerebroExport(
    export = minimal_export(),
    final_file = output,
    stage_dir = stage,
    expression_matrix_mode = "embedded"
  )

  expect_identical(artifact_mode(output), "600")
})

test_that("publication refuses to replace an unowned sidecar", {
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  saveRDS(minimal_export(), output)

  sidecar_name <- "dataset.h5"
  sidecar <- file.path(root, sidecar_name)
  writeLines("unrelated user file", sidecar)
  stage <- file.path(root, "stage")
  dir.create(stage)
  writeLines("new matrix", file.path(stage, sidecar_name))

  before_crb <- unname(tools::md5sum(output))
  expect_error(
    .publishCerebroExport(
      export = minimal_export("h5", sidecar_name),
      final_file = output,
      stage_dir = stage,
      expression_matrix_mode = "h5"
    ),
    regexp = "already exists|collision|refus",
    ignore.case = TRUE
  )
  expect_identical(readLines(sidecar), "unrelated user file")
  expect_identical(unname(tools::md5sum(output)), before_crb)
})

test_that("backend ownership checks do not execute serialized bindings", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "binding-ran")

  active <- new.env(parent = emptyenv())
  class(active) <- c("Cerebro", "R6")
  makeActiveBinding(
    "expression_backend",
    function(value) {
      writeLines("active", sentinel)
      list(type = "h5", location = "dataset.h5")
    },
    active
  )
  active_path <- file.path(root, "active.crb")
  saveRDS(active, active_path)
  expect_null(.readPublishedExportBackend(active_path))
  expect_false(file.exists(sentinel))

  lazy <- new.env(parent = emptyenv())
  class(lazy) <- c("Cerebro", "R6")
  delayedAssign(
    "expression_backend",
    {
      writeLines("lazy", sentinel)
      list(type = "h5", location = "dataset.h5")
    },
    assign.env = lazy
  )
  lazy_path <- file.path(root, "lazy.crb")
  saveRDS(lazy, lazy_path)
  expect_null(.readPublishedExportBackend(lazy_path))
  expect_false(file.exists(sentinel))

  plain <- minimal_export("h5", "dataset.h5")
  plain$getExpressionBackend <- function() {
    writeLines("getter", sentinel)
    list(type = "h5", location = "wrong.h5")
  }
  plain_path <- file.path(root, "plain.crb")
  saveRDS(plain, plain_path)
  expect_identical(
    .readPublishedExportBackend(plain_path),
    list(type = "h5", location = "dataset.h5")
  )
  expect_false(file.exists(sentinel))

  factor_backend <- minimal_export("h5", "dataset.h5")
  factor_backend$expression_backend$type <- factor("h5")
  factor_path <- file.path(root, "factor.crb")
  saveRDS(factor_backend, factor_path)
  expect_null(.readPublishedExportBackend(factor_path))
})

test_that("publisher accepts only its fixed sidecar name", {
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  stage <- file.path(root, "stage")
  dir.create(stage)
  writeLines("matrix", file.path(stage, "other.h5"))

  expect_error(
    .publishCerebroExport(
      export = minimal_export("h5", "other.h5"),
      final_file = output,
      stage_dir = stage,
      expression_matrix_mode = "h5"
    ),
    regexp = "fixed|expected|dataset\\.h5",
    ignore.case = TRUE
  )
  expect_false(file.exists(output))
  expect_false(file.exists(file.path(root, "other.h5")))
})

test_that("publisher rejects a symbolic-link staged sidecar", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  target <- file.path(root, "private-source.h5")
  writeLines("do not publish", target)
  stage <- file.path(root, "stage")
  dir.create(stage)
  expect_true(file.symlink(target, file.path(stage, "dataset.h5")))

  expect_error(
    .publishCerebroExport(
      export = minimal_export("h5", "dataset.h5"),
      final_file = output,
      stage_dir = stage,
      expression_matrix_mode = "h5"
    ),
    regexp = "symbolic.link|symlink",
    ignore.case = TRUE
  )
  expect_false(file.exists(output))
  expect_false(file.exists(file.path(root, "dataset.h5")))
  expect_identical(readLines(target), "do not publish")
})

test_that("publisher rejects a sidecar with the wrong artifact type", {
  root <- withr::local_tempdir()

  h5_output <- file.path(root, "h5.crb")
  h5_stage <- file.path(root, "h5-stage")
  dir.create(h5_stage)
  dir.create(file.path(h5_stage, "h5.h5"))
  expect_error(
    .publishCerebroExport(
      export = minimal_export("h5", "h5.h5"),
      final_file = h5_output,
      stage_dir = h5_stage,
      expression_matrix_mode = "h5"
    ),
    regexp = "h5.*file|regular file",
    ignore.case = TRUE
  )

  skip_if_not_installed("BPCells")
  bpc_output <- file.path(root, "bpc.crb")
  bpc_stage <- file.path(root, "bpc-stage")
  dir.create(bpc_stage)
  writeLines("not a directory", file.path(bpc_stage, "bpc.bpcells"))
  expect_error(
    .publishCerebroExport(
      export = minimal_export("bpcells", "bpc.bpcells"),
      final_file = bpc_output,
      stage_dir = bpc_stage,
      expression_matrix_mode = "bpcells"
    ),
    regexp = "bpcells.*directory|must be a directory",
    ignore.case = TRUE
  )
})

test_that("H5 replacement updates only its owned portable sibling", {
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  object <- make_ir_seurat(n_cells = 12L)

  export_ir(object, output, expression_matrix_mode = "h5")
  first_location <- readRDS(output)$getExpressionBackend()$location
  first_sidecar <- file.path(root, first_location)
  first_md5 <- unname(tools::md5sum(first_sidecar))

  changed <- SeuratObject::LayerData(object[["RNA"]], layer = "data")
  changed@x <- changed@x + 100
  SeuratObject::LayerData(object[["RNA"]], layer = "data") <- changed
  export_ir(object, output, expression_matrix_mode = "h5")
  second_location <- readRDS(output)$getExpressionBackend()$location
  second_sidecar <- file.path(root, second_location)

  expect_identical(first_location, "dataset.h5")
  expect_identical(second_location, first_location)
  expect_false(identical(unname(tools::md5sum(second_sidecar)), first_md5))
  expect_true(file.exists(second_sidecar))
})

test_that("createShinyApp bundles the sidecar produced by the exporter", {
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  object <- make_ir_seurat(n_cells = 12L)
  export_ir(object, output, expression_matrix_mode = "h5")

  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = c(dataset = output),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  )

  expect_true(file.exists(file.path(app_dir, "private-data", "dataset.crb")))
  expect_true(file.exists(file.path(app_dir, "private-data", "dataset.h5")))
})

test_that("switching to embedded removes the old owned sidecar", {
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  object <- make_ir_seurat(n_cells = 12L)

  export_ir(object, output, expression_matrix_mode = "h5")
  old_location <- readRDS(output)$getExpressionBackend()$location
  old_sidecar <- file.path(root, old_location)
  expect_true(file.exists(old_sidecar))

  export_ir(object, output, expression_matrix_mode = "embedded")

  expect_identical(
    readRDS(output)$getExpressionBackend()$type,
    "embedded"
  )
  expect_false(file.exists(old_sidecar))
})

test_that("a publication error restores the previous BPCells export", {
  skip_if_not_installed("BPCells")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  saveRDS(minimal_export("bpcells", "dataset.bpcells"), output)
  before_crb <- unname(tools::md5sum(output))

  old_matrix <- matrix(
    c(1, 3, 4, 2),
    nrow = 2,
    dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )
  new_matrix <- old_matrix + 10
  write_bpcells <- function(matrix, directory) {
    BPCells::write_matrix_dir(
      methods::as(
        methods::as(matrix, "CsparseMatrix"),
        "IterableMatrix"
      ),
      dir = directory
    )
  }

  old_sidecar_name <- "dataset.bpcells"
  old_sidecar <- file.path(root, old_sidecar_name)
  write_bpcells(old_matrix, old_sidecar)

  stage <- file.path(root, "stage")
  dir.create(stage)
  new_sidecar_name <- "dataset.bpcells"
  staged_sidecar <- file.path(stage, new_sidecar_name)
  write_bpcells(new_matrix, staged_sidecar)

  export <- new.env(parent = emptyenv())
  export$getExpressionBackend <- function() {
    list(type = "bpcells", location = new_sidecar_name)
  }
  export$setExpression <- function(...) {
    stop("injected publication failure", call. = FALSE)
  }

  expect_error(
    .publishCerebroExport(
      export = export,
      final_file = output,
      stage_dir = stage,
      expression_matrix_mode = "bpcells"
    ),
    "injected publication failure"
  )
  expect_identical(unname(tools::md5sum(output)), before_crb)
  restored <- as.matrix(BPCells::open_matrix_dir(dir = old_sidecar))
  expect_equal(restored, old_matrix)
  expect_length(
    list.files(root, all.files = TRUE, pattern = "-backup-"),
    0L
  )
})

test_that("late validation cannot replace an existing H5 export", {
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  object <- make_ir_seurat(n_cells = 12L)
  expect_no_error(export_ir(object, output, expression_matrix_mode = "h5"))
  location <- readRDS(output)$getExpressionBackend()$location
  sidecar <- file.path(root, location)
  before_crb <- unname(tools::md5sum(output))
  before_sidecar <- unname(tools::md5sum(sidecar))

  changed <- SeuratObject::LayerData(object[["RNA"]], layer = "data")
  changed@x <- changed@x + 100
  SeuratObject::LayerData(object[["RNA"]], layer = "data") <- changed
  object@misc$immune_repertoire <- list(
    s1 = data.frame(CTgene = "TRAV1_TRAJ1")
  )

  expect_error(
    export_ir(object, output, expression_matrix_mode = "h5"),
    regexp = "barcode"
  )
  expect_identical(unname(tools::md5sum(output)), before_crb)
  expect_identical(unname(tools::md5sum(sidecar)), before_sidecar)
})
