builder_build_source_runtime <- function(local = parent.frame()) {
  for (file in c(
    "spatial.R",
    "profile.R",
    "inspect.R",
    "analysis.R",
    "extras.R",
    "state.R",
    "app_bundle.R",
    "build.R"
  )) {
    path <- testthat::test_path("..", "..", "inst", "builder", file)
    if (!file.exists(path)) {
      path <- system.file("builder", file, package = "CerebroNexus")
    }
    if (file.exists(path)) {
      sys.source(path, envir = local)
    }
  }
}

builder_build_source_runtime()

test_that("session execution stages artifacts without publishing them", {
  path <- testthat::test_path("..", "..", "inst", "builder", "session.R")
  if (!file.exists(path)) {
    path <- system.file("builder", "session.R", package = "CerebroNexus")
  }
  expect_true(file.exists(path))
  session <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(session, "worker$snapshot_root", fixed = TRUE)
  expect_match(
    session,
    "builder_execute_plan(plan, stage, registry)",
    fixed = TRUE
  )
  expect_false(grepl("builder_publish_batch", session, fixed = TRUE))
})

builder_build_test_plan <- function(analyses = character()) {
  item <- list(
    id = "dataset-a",
    name = "Dataset A",
    filename = "dataset-a.crb",
    analyses = analyses,
    analysis_dependency_graph = builder_analysis_graph(analyses),
    artifact_identity = list(
      schema_version = 2L,
      cells = c("cell-b", "cell-a"),
      features = c("Gene2", "Gene1"),
      group_levels = list(cluster = c("B", "A")),
      projections = "umap",
      metadata = c("cell_barcode", "cluster"),
      spatial_sections = character()
    ),
    expression_backend = "embedded",
    sidecars = character(),
    colors = list(cluster = c(B = "#ffffff", A = "#000000")),
    viewer_page_expectations = list(
      visible_conditional = character(),
      hidden_conditional = character()
    )
  )
  structure(
    list(
      items = list(item),
      dataset_order = "dataset-a",
      make_app = FALSE,
      app_contract_version = 0L,
      app_options = list(
        enabled = FALSE,
        show_upload_ui = FALSE,
        initial_dataset = "dataset-a",
        initial_dataset_mode = "automatic",
        welcome_message = "Welcome to CerebroNexus!",
        point_size = list(overview_projection_point_size = 5),
        variable_to_compare = FALSE,
        host = "127.0.0.1",
        port = 8080L,
        max_request_size = 8000,
        display_mode = "normal",
        launch_browser = TRUE
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

builder_build_test_hooks <- function(fail = NULL, outside = NULL) {
  list(
    open_snapshot = function(snapshot) list(snapshot = snapshot),
    prepare = function(object, item) object,
    run_analyses = function(object, item) {
      if (!is.null(fail) && fail %in% item$analyses) {
        return(list(
          object = object,
          completed = setdiff(item$analyses, fail),
          failed = fail,
          log = paste("failed", fail)
        ))
      }
      list(
        object = object,
        completed = item$analyses,
        failed = character(),
        log = character()
      )
    },
    export = function(object, item, path) {
      target <- if (is.null(outside)) path else outside
      saveRDS(list(ok = TRUE), target)
      target
    },
    attach_extras = function(path, object, item) list(path = path),
    verify = function(path, item) {
      list(
        valid = TRUE,
        path = path,
        page_contract = item$viewer_page_expectations
      )
    }
  )
}

test_that("retry recomputes the failed dependency closure", {
  graph <- builder_analysis_graph(c("marker_genes", "enriched_pathways"))
  expect_identical(
    builder_retry_closure(graph, "enriched_pathways"),
    c("marker_genes", "enriched_pathways")
  )
  expect_identical(
    builder_retry_closure(graph, "marker_genes"),
    c("marker_genes", "enriched_pathways")
  )
})

test_that("selected analysis failure cannot produce publishable output", {
  stage <- tempfile("builder-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  plan <- builder_build_test_plan(c("marker_genes", "enriched_pathways"))

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list(id = "snapshot-a")),
    hooks = builder_build_test_hooks(fail = "enriched_pathways")
  )

  expect_identical(result$state, "needs_decision")
  expect_false(result$publishable)
  expect_identical(result$failed_analyses, "enriched_pathways")
  expect_identical(
    result$retry_closure,
    c("marker_genes", "enriched_pathways")
  )
  expect_false(file.exists(file.path(stage, "dataset-a.crb")))

  state <- builder_build_state()
  state <- builder_reduce_build(
    state,
    list(type = "start", id = "build-a", revision = 1L)
  )
  state <- builder_reduce_build(
    state,
    list(type = "needs_decision", id = "build-a", result = result)
  )
  expect_identical(state$status, "needs_decision")
  expect_identical(state$result$failed_analyses, "enriched_pathways")
})

test_that("worker results map to explicit build-state actions", {
  success <- builder_build_action(
    list(state = "success", publishable = TRUE),
    "build-a"
  )
  expect_identical(success$type, "succeed")

  decision <- builder_build_action(
    list(
      state = "needs_decision",
      publishable = FALSE,
      failed_analyses = "marker_genes"
    ),
    "build-a"
  )
  expect_identical(decision$type, "needs_decision")

  failed <- builder_build_action(
    list(state = "failure", publishable = FALSE, error = "failed"),
    "build-a"
  )
  expect_identical(failed$type, "fail")
  expect_identical(failed$error, "failed")

  expect_error(
    builder_build_action(
      list(state = "success", publishable = FALSE),
      "build-a"
    ),
    "publishable"
  )
})

test_that("only an existing assigned stage can receive artifacts", {
  plan <- builder_build_test_plan()
  missing <- tempfile("missing-builder-stage-")
  expect_error(
    builder_execute_plan(
      plan,
      missing,
      snapshots = list(`dataset-a` = list()),
      hooks = builder_build_test_hooks()
    ),
    "assigned stage"
  )

  stage <- tempfile("builder-stage-")
  dir.create(stage)
  outside <- tempfile("outside-stage-", fileext = ".crb")
  on.exit(unlink(c(stage, outside), recursive = TRUE, force = TRUE), add = TRUE)
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = builder_build_test_hooks(outside = outside)
  )
  expect_identical(result$state, "failure")
  expect_false(result$publishable)
  expect_match(result$error, "outside the assigned stage")

  plan <- builder_build_test_plan()
  escaped_name <- paste0(basename(stage), "-escaped.crb")
  plan$items[[1L]]$filename <- file.path("..", escaped_name)
  escaped <- file.path(dirname(stage), escaped_name)
  on.exit(unlink(escaped), add = TRUE)
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = builder_build_test_hooks()
  )
  expect_identical(result$state, "failure")
  expect_match(result$error, "outside the assigned stage")
  expect_false(file.exists(escaped))
})

test_that("contract-v1 execution assembles App only after CRB verification", {
  stage <- tempfile("builder-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  plan <- builder_build_test_plan()
  plan$make_app <- TRUE
  plan$app_contract_version <- 1L
  plan$app_options$enabled <- TRUE
  calls <- character()
  hooks <- builder_build_test_hooks()
  hooks$verify <- function(path, item) {
    calls <<- c(calls, "verify_crb")
    list(valid = TRUE, path = path)
  }
  hooks$build_app <- function(request, stage) {
    calls <<- c(calls, "build_app")
    expect_true(all(file.exists(request$cerebro_data)))
    app_dir <- file.path(stage, "cerebro_app")
    dir.create(app_dir)
    app_dir
  }
  hooks$verify_app <- function(app_dir, request) {
    calls <<- c(calls, "verify_app")
    structure(
      list(valid = TRUE, app_dir = app_dir),
      class = c("builder_app_verification", "list")
    )
  }

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = hooks
  )

  expect_identical(calls, c("verify_crb", "build_app", "verify_app"))
  expect_identical(result$state, "success")
  expect_true(result$publishable)
  expect_named(result$built, "Dataset A")
  expect_identical(
    result$app_dir,
    file.path(normalizePath(stage, winslash = "/"), "cerebro_app")
  )
  expect_s3_class(result$app_verification, "builder_app_verification")
})

test_that("App execution rejects non-inert or non-exact verification evidence", {
  evidence <- list(
    structure(
      list(valid = TRUE, mutable = new.env(parent = emptyenv())),
      class = c("builder_app_verification", "list")
    ),
    structure(
      list(valid = TRUE, callback = function() TRUE),
      class = c("builder_app_verification", "list")
    ),
    structure(
      list(valid = TRUE),
      class = c("builder_app_verification", "spoofed", "list")
    )
  )

  for (value in evidence) {
    stage <- tempfile("builder-stage-")
    dir.create(stage)
    withr::defer(unlink(stage, recursive = TRUE, force = TRUE))
    plan <- builder_build_test_plan()
    plan$make_app <- TRUE
    plan$app_contract_version <- 1L
    plan$app_options$enabled <- TRUE
    hooks <- builder_build_test_hooks()
    hooks$build_app <- function(request, stage) {
      app_dir <- file.path(stage, "cerebro_app")
      dir.create(app_dir)
      app_dir
    }
    hooks$verify_app <- function(app_dir, request) value

    result <- builder_execute_plan(
      plan,
      stage,
      snapshots = list(`dataset-a` = list()),
      hooks = hooks
    )

    expect_identical(result$state, "failure")
    expect_match(result$error, "inert evidence")
    expect_null(result$app_verification)
  }
})

test_that("generated App execution remains closed without contract v1", {
  stage <- tempfile("builder-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  plan <- builder_build_test_plan()
  plan$make_app <- TRUE
  plan$app_options$enabled <- TRUE

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = builder_build_test_hooks()
  )

  expect_identical(result$state, "failure")
  expect_match(result$error, "contract")
  expect_null(result$app_dir)
})

test_that("CRB read-back matches exact frozen artifact identity", {
  crb <- tempfile(fileext = ".crb")
  on.exit(unlink(crb), add = TRUE)
  object <- new.env(parent = emptyenv())
  object$expression <- matrix(
    seq_len(4L),
    nrow = 2L,
    dimnames = list(c("Gene2", "Gene1"), c("cell-b", "cell-a"))
  )
  object$groups <- list(cluster = c("B", "A"))
  object$meta_data <- data.frame(
    cell_barcode = c("cell-b", "cell-a"),
    cluster = c("B", "A"),
    row.names = c("cell-b", "cell-a")
  )
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = c("cell-b", "cell-a")
    )
  )
  object$expression_backend <- list(type = "embedded", location = NULL)
  object$marker_genes <- list()
  object$most_expressed_genes <- list()
  object$enriched_pathways <- list()
  object$extra_material <- list()
  object$immune_repertoire <- list()
  object$trajectories <- list()
  image <- list(
    uri = "data:image/png;base64,AA==",
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  )
  object$spatial <- list(
    `slice-a` = list(
      coordinates = data.frame(x = 1, y = 2),
      expression = matrix(1),
      histology_image = image$uri,
      histology_image_bounds = image$bounds
    )
  )
  object$trekker <- NULL
  object$hla_typing <- NULL
  class(object) <- c("Cerebro_v1.3", "R6")
  saveRDS(object, crb)

  item <- builder_build_test_plan()$items[[1L]]
  item$artifact_identity$spatial_sections <- "slice-a"
  item$images <- list(`slice-a` = image)
  item$viewer_page_expectations$visible_conditional <- "spatial"
  observed <- builder_verify_crb(crb, item)
  expect_true(observed$valid)
  expect_identical(observed$cells, item$artifact_identity$cells)
  expect_identical(observed$features, item$artifact_identity$features)
  expect_identical(observed$groups, names(item$artifact_identity$group_levels))
  expect_identical(observed$projections, item$artifact_identity$projections)
  expect_identical(observed$metadata, item$artifact_identity$metadata)
  expect_identical(
    observed$spatial_sections,
    item$artifact_identity$spatial_sections
  )
  expect_identical(observed$image_sections, "slice-a")

  item$artifact_identity$cells <- rev(item$artifact_identity$cells)
  expect_error(builder_verify_crb(crb, item), "cell identity")
})

test_that("read-back verifies frozen H5 and BPCells sidecars", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("Matrix")
  root <- tempfile("builder-sidecars-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  base_object <- new.env(parent = emptyenv())
  base_object$expression <- NULL
  base_object$meta_data <- data.frame(
    cell_barcode = c("cell-b", "cell-a"),
    cluster = c("B", "A"),
    row.names = c("cell-b", "cell-a")
  )
  base_object$gene_data <- data.frame(
    row.names = c("Gene2", "Gene1")
  )
  base_object$groups <- list(cluster = c("B", "A"))
  base_object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = c("cell-b", "cell-a")
    )
  )
  for (field in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectories",
    "spatial"
  )) {
    base_object[[field]] <- list()
  }
  base_object$trekker <- NULL
  base_object$hla_typing <- NULL
  class(base_object) <- c("Cerebro_v1.3", "R6")

  for (mode in c("h5", "bpcells")) {
    item <- builder_build_test_plan()$items[[1L]]
    location <- paste0("dataset-a.", if (mode == "h5") "h5" else "bpcells")
    item$expression_backend <- mode
    item$sidecars <- location
    object <- unserialize(serialize(base_object, NULL))
    object$expression_backend <- list(type = mode, location = location)
    crb <- file.path(root, paste0(mode, ".crb"))
    sidecar <- file.path(root, location)
    if (mode == "h5") {
      matrix <- Matrix::Matrix(
        matrix(
          seq_len(4L),
          nrow = 2L,
          dimnames = list(
            item$artifact_identity$features,
            item$artifact_identity$cells
          )
        ),
        sparse = TRUE
      )
      HDF5Array::writeTENxMatrix(
        methods::as(Matrix::t(matrix), "CsparseMatrix"),
        sidecar,
        group = "expression"
      )
    } else {
      dir.create(sidecar)
    }
    saveRDS(object, crb)
    observed <- builder_verify_crb(crb, item)
    expect_true(observed$valid)
    expect_identical(observed$backend$type, mode)
  }
})

test_that("H5 sidecar identity is independent of CRB fallback fields", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("Matrix")
  root <- withr::local_tempdir()
  item <- builder_build_test_plan()$items[[1L]]
  item$expression_backend <- "h5"
  item$sidecars <- "dataset-a.h5"
  object <- new.env(parent = emptyenv())
  object$expression <- NULL
  object$meta_data <- data.frame(
    cell_barcode = item$artifact_identity$cells,
    cluster = c("B", "A"),
    row.names = item$artifact_identity$cells
  )
  object$gene_data <- data.frame(row.names = item$artifact_identity$features)
  object$groups <- list(cluster = c("B", "A"))
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = item$artifact_identity$cells
    )
  )
  for (field in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectories",
    "spatial"
  )) {
    object[[field]] <- list()
  }
  object$trekker <- NULL
  object$hla_typing <- NULL
  object$expression_backend <- list(type = "h5", location = item$sidecars)
  class(object) <- c("Cerebro_v1.3", "R6")
  crb <- file.path(root, "dataset-a.crb")
  saveRDS(object, crb)

  write_h5 <- function(cells, features, path = file.path(root, item$sidecars)) {
    if (file.exists(path)) {
      unlink(path)
    }
    matrix <- Matrix::Matrix(
      matrix(
        seq_len(length(cells) * length(features)),
        nrow = length(features),
        dimnames = list(features, cells)
      ),
      sparse = TRUE
    )
    HDF5Array::writeTENxMatrix(
      methods::as(Matrix::t(matrix), "CsparseMatrix"),
      path,
      group = "expression"
    )
  }

  write_h5(item$artifact_identity$cells, item$artifact_identity$features)
  expect_true(builder_verify_crb(crb, item)$valid)

  write_h5(rev(item$artifact_identity$cells), item$artifact_identity$features)
  expect_error(builder_verify_crb(crb, item), "H5 sidecar cell identity")

  write_h5(item$artifact_identity$cells, rev(item$artifact_identity$features))
  expect_error(builder_verify_crb(crb, item), "H5 sidecar feature identity")
})

test_that("H5 sidecars reject links and escapes before opening", {
  skip_if_not_installed("HDF5Array")
  skip_on_os("windows")
  root <- withr::local_tempdir()
  item <- builder_build_test_plan()$items[[1L]]
  item$expression_backend <- "h5"
  item$sidecars <- "dataset-a.h5"
  object <- new.env(parent = emptyenv())
  object$expression <- matrix(
    seq_len(4L),
    nrow = 2L,
    dimnames = list(
      item$artifact_identity$features,
      item$artifact_identity$cells
    )
  )
  object$meta_data <- data.frame(
    cell_barcode = item$artifact_identity$cells,
    cluster = c("B", "A"),
    row.names = item$artifact_identity$cells
  )
  object$gene_data <- data.frame(row.names = item$artifact_identity$features)
  object$groups <- list(cluster = c("B", "A"))
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = item$artifact_identity$cells
    )
  )
  for (field in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectories",
    "spatial"
  )) {
    object[[field]] <- list()
  }
  object$trekker <- NULL
  object$hla_typing <- NULL
  object$expression_backend <- list(type = "h5", location = item$sidecars)
  class(object) <- c("Cerebro_v1.3", "R6")
  crb <- file.path(root, "dataset-a.crb")
  saveRDS(object, crb)

  outside <- tempfile(fileext = ".h5")
  on.exit(unlink(outside), add = TRUE)
  writeBin(as.raw(c(1, 2, 3)), outside)
  expect_true(file.symlink(outside, file.path(root, item$sidecars)))
  expect_error(builder_verify_crb(crb, item), "sidecar does not match")

  unlink(file.path(root, item$sidecars))
  item$sidecars <- file.path("..", basename(outside))
  object$expression_backend$location <- item$sidecars
  saveRDS(object, crb)
  expect_error(builder_verify_crb(crb, item), "sidecar does not match")
})

test_that("read-back rejects a Viewer page-gate mismatch", {
  crb <- tempfile(fileext = ".crb")
  on.exit(unlink(crb), add = TRUE)
  object <- new.env(parent = emptyenv())
  object$expression <- matrix(
    seq_len(4L),
    nrow = 2L,
    dimnames = list(c("Gene2", "Gene1"), c("cell-b", "cell-a"))
  )
  object$meta_data <- data.frame(
    cell_barcode = c("cell-b", "cell-a"),
    cluster = c("B", "A"),
    row.names = c("cell-b", "cell-a")
  )
  object$groups <- list(cluster = c("B", "A"))
  object$projections <- list(
    umap = data.frame(
      x = c(1, 2),
      y = c(3, 4),
      row.names = c("cell-b", "cell-a")
    )
  )
  object$expression_backend <- list(type = "embedded", location = NULL)
  object$marker_genes <- list(
    wilcox = list(cluster = data.frame(gene = "Gene1"))
  )
  object$most_expressed_genes <- list()
  object$enriched_pathways <- list()
  object$extra_material <- list()
  object$immune_repertoire <- list()
  object$trajectories <- list()
  object$spatial <- list()
  object$trekker <- NULL
  object$hla_typing <- NULL
  class(object) <- c("Cerebro_v1.3", "R6")
  saveRDS(object, crb)

  expect_error(
    builder_verify_crb(crb, builder_build_test_plan()$items[[1L]]),
    "page contract"
  )
})

test_that("build preparation applies the frozen metadata policy", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  object$secret_note <- rep("private", ncol(object))
  item <- list(
    included_projections = "tsne",
    assay = "RNA",
    layer = "data",
    artifact_identity = list(
      group_levels = list(groups = sort(unique(object$groups)))
    ),
    metadata_policy = list(
      included = c(
        "cell_barcode",
        "groups",
        "nCount_RNA",
        "nFeature_RNA"
      )
    ),
    tables = list()
  )

  item$analyses <- character()
  prepared <- .builder_build_apply_metadata_policy(object, item)
  expect_setequal(
    colnames(prepared@meta.data),
    c("groups", "nCount_RNA", "nFeature_RNA")
  )
  expect_false("secret_note" %in% colnames(prepared@meta.data))
})

test_that("build preparation follows the frozen immune source", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  cells <- SeuratObject::Cells(object)
  repertoire <- function(cell, suffix) {
    data.frame(
      barcode = cell,
      CTgene = paste0("TRB_", suffix),
      CTnt = paste0("nt_", suffix),
      CTaa = paste0("aa_", suffix),
      CTstrict = paste0("strict_", suffix),
      stringsAsFactors = FALSE
    )
  }
  object@misc$immune_repertoire <- list(
    unified = repertoire(cells[[1L]], "unified")
  )
  object@misc$tcr_data <- list(
    legacy = repertoire(cells[[2L]], "legacy")
  )

  item <- list(
    manifest = list(
      immune_repertoire = list(
        disposition = "preserved",
        evidence = list(selected_sources = "unified_misc")
      )
    )
  )
  unified <- .builder_build_prepare_immune(object, item)
  expect_named(unified@misc$immune_repertoire, "unified")
  expect_null(unified@misc$tcr_data)
  expect_null(unified@misc$bcr_data)

  item$manifest$immune_repertoire$disposition <- "converted"
  item$manifest$immune_repertoire$evidence$selected_sources <- "legacy_tcr"
  legacy <- .builder_build_prepare_immune(object, item)
  expect_named(legacy@misc$immune_repertoire, "legacy")
  expect_identical(
    legacy@misc$immune_repertoire$legacy$barcode,
    cells[[2L]]
  )
  expect_null(legacy@misc$tcr_data)

  item$manifest$immune_repertoire$disposition <- "filtered"
  filtered <- .builder_build_prepare_immune(object, item)
  expect_null(filtered@misc$immune_repertoire)
  expect_null(filtered@misc$tcr_data)
  expect_null(filtered@misc$bcr_data)
})

test_that("a real example is exported and verified only inside its stage", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Seurat")
  object <- SeuratObject::pbmc_small
  cells <- SeuratObject::Cells(object)
  features <- rownames(object[["RNA"]])
  group_levels <- sort(unique(as.character(object$groups)))
  item <- list(
    id = "pbmc-small",
    name = "PBMC small",
    filename = "pbmc-small.crb",
    organism = "hg",
    assay = "RNA",
    layer = "data",
    included_groups = "groups",
    included_projections = "tsne",
    analyses = character(),
    analysis_dependency_graph = list(),
    artifact_identity = list(
      schema_version = 2L,
      cells = cells,
      features = features,
      group_levels = list(groups = group_levels),
      projections = "tsne",
      source_metadata = c(
        "cell_barcode",
        "groups",
        "nCount_RNA",
        "nFeature_RNA"
      ),
      metadata = c("cell_barcode", "groups", "nUMI", "nGene"),
      spatial_sections = character()
    ),
    tables = list(),
    images = list(),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    default_group = "groups",
    metadata_policy = list(
      included = c(
        "cell_barcode",
        "groups",
        "nCount_RNA",
        "nFeature_RNA"
      )
    ),
    expression_backend = "embedded",
    sidecars = character(),
    viewer_page_expectations = list(
      visible_conditional = character(),
      hidden_conditional = character()
    )
  )
  plan <- structure(
    list(items = list(item), make_app = FALSE),
    class = c("builder_build_plan", "list")
  )
  stage <- tempfile("builder-real-stage-")
  dir.create(stage, mode = "0700")
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  hooks <- list(
    open_snapshot = function(snapshot) unserialize(snapshot),
    prepare = .builder_build_prepare,
    run_analyses = function(object, item) {
      builder_run_analyses(object, item$analyses, item)
    },
    export = .builder_build_export,
    attach_extras = .builder_build_attach_extras,
    verify = builder_verify_crb
  )

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`pbmc-small` = serialize(object, NULL)),
    hooks = hooks
  )

  expect_identical(result$state, "success")
  expect_true(result$publishable)
  expect_length(result$built, 1L)
  expect_true(startsWith(result$built[[1L]], normalizePath(stage)))
  expect_true(result$verifications[["pbmc-small"]]$valid)
  expect_identical(
    result$verifications[["pbmc-small"]]$metadata,
    item$artifact_identity$metadata
  )
})
