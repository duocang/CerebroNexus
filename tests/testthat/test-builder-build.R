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
    "auth_material = auth_material",
    fixed = TRUE
  )
  expect_match(session, 'get(".builder_objects"', fixed = TRUE)
  expect_match(session, "objects = objects", fixed = TRUE)
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
        initial_page = "data_info",
        welcome_message = "Welcome to CerebroNexus!",
        point_size = list(overview_projection_point_size = 5),
        variable_to_compare = FALSE,
        host = "127.0.0.1",
        port = 8080L,
        max_request_size = 8000,
        display_mode = "normal",
        launch_browser = TRUE
      ),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
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

test_that("build execution reuses the worker's loaded object", {
  stage <- withr::local_tempdir()
  hooks <- builder_build_test_hooks()
  hooks$open_snapshot <- function(snapshot) {
    stop("snapshot should not be reopened")
  }
  loaded <- list(source = "worker-memory")

  result <- builder_execute_plan(
    builder_build_test_plan(),
    stage,
    snapshots = list(`dataset-a` = list(id = "snapshot-a")),
    hooks = hooks,
    objects = list(`dataset-a` = loaded)
  )

  expect_identical(result$state, "success")
  expect_true(result$publishable)
})

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
  hooks$build_app <- function(request, stage, auth_material = NULL) {
    calls <<- c(calls, "build_app")
    expect_true(all(file.exists(request$cerebro_data)))
    app_dir <- file.path(stage, "cerebro_app")
    dir.create(app_dir)
    app_dir
  }
  hooks$verify_app <- function(app_dir, request, auth_env_file = NULL) {
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
  expect_false(result$auth_enabled)
  expect_null(result$auth_env_file)
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
    hooks$build_app <- function(request, stage, auth_material = NULL) {
      app_dir <- file.path(stage, "cerebro_app")
      dir.create(app_dir)
      app_dir
    }
    hooks$verify_app <- function(app_dir, request, auth_env_file = NULL) value

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

test_that("failed login build propagates authentication cleanup failure", {
  stage <- withr::local_tempdir()
  plan <- builder_build_test_plan()
  plan$app_auth <- list(
    enabled = TRUE,
    account_count = 1L,
    timeout_minutes = 15L
  )
  source_dir <- file.path(stage, ".builder-auth-source")
  dir.create(source_dir)
  credentials <- file.path(source_dir, "credentials.sqlite")
  writeBin(as.raw(1:8), credentials)
  Sys.chmod(credentials, "0600", use_umask = FALSE)
  env_file <- .builder_auth_write_env(
    file.path(stage, "viewer-auth.env"),
    strrep("a", 64L)
  )
  material <- list(
    source_dir = source_dir,
    credentials = credentials,
    env_file = env_file,
    descriptor = list(
      credentials = credentials,
      passphrase_env = "CEREBRO_AUTH_PASSPHRASE",
      timeout_minutes = 15L
    )
  )
  original_cleanup <- .builder_auth_remove_partial_material
  assign(
    ".builder_auth_remove_partial_material",
    function(stage) {
      original_cleanup(stage, .unlink = function(...) 0L)
    },
    envir = environment(builder_execute_plan)
  )
  on.exit(
    assign(
      ".builder_auth_remove_partial_material",
      original_cleanup,
      envir = environment(builder_execute_plan)
    ),
    add = TRUE
  )

  expect_error(
    builder_execute_plan(
      plan,
      stage,
      snapshots = list(),
      hooks = builder_build_test_hooks()
    ),
    "authentication files could not be cleaned up"
  )
  expect_true(file.exists(credentials))
  expect_true(file.exists(env_file))
})

test_that("successful login execution removes source and retains only safe env path", {
  stage <- withr::local_tempdir()
  plan <- builder_build_test_plan()
  plan$make_app <- TRUE
  plan$app_contract_version <- 1L
  plan$app_options$enabled <- TRUE
  plan$app_auth <- list(
    enabled = TRUE,
    account_count = 1L,
    timeout_minutes = 15L
  )
  accounts <- builder_auth_validate_payload(
    TRUE,
    list(list(
      id = "auth-account-1",
      username = "execution-user-31a7",
      password = "execution-password-31a7"
    ))
  )$accounts
  material <- builder_auth_create_material(
    accounts,
    stage,
    .capability = function() list(available = TRUE, reason = NULL)
  )
  source_dir <- material$source_dir
  env_file <- material$env_file
  hooks <- builder_build_test_hooks()
  hooks$build_app <- function(request, stage, auth_material = NULL) {
    app_dir <- file.path(stage, "cerebro_app")
    dir.create(app_dir)
    app_dir
  }
  hooks$verify_app <- function(app_dir, request, auth_env_file = NULL) {
    expect_identical(auth_env_file, env_file)
    structure(
      list(valid = TRUE, app_dir = app_dir),
      class = c("builder_app_verification", "list")
    )
  }

  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = list(`dataset-a` = list()),
    hooks = hooks,
    auth_material = material
  )

  expect_identical(result$state, "success")
  expect_true(result$auth_enabled)
  expect_identical(result$auth_env_file, env_file)
  expect_false(dir.exists(source_dir))
  expect_true(file.exists(env_file))
  expect_false("auth_material" %in% names(result))
  expect_false(builder_auth_value_contains(result, "execution-user-31a7"))
  expect_false(builder_auth_value_contains(result, "execution-password-31a7"))
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
      histology_images = list(
        `Embedded tissue image` = list(
          histology_image = image$uri,
          histology_image_bounds = c(
            xmin = 0,
            xmax = 10,
            ymin = 0,
            ymax = 10
          ),
          histology_alignment = utils::modifyList(
            builder_alignment_payload(image),
            list(source = "Embedded tissue image")
          )
        )
      )
    )
  )
  object$trekker <- NULL
  object$hla_typing <- NULL
  class(object) <- c("Cerebro_v1.3", "R6")
  saveRDS(object, crb)

  item <- builder_build_test_plan()$items[[1L]]
  item$artifact_identity$spatial_sections <- "slice-a"
  expected_image <- image
  expected_image$bounds$xmax <- image$bounds$xmax + 2^-46
  expect_false(identical(expected_image$bounds$xmax, image$bounds$xmax))
  item$images <- list(`slice-a` = expected_image)
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

test_that("CRB read-back accepts sub-picounit transform serialization drift", {
  crb <- tempfile(fileext = ".crb")
  on.exit(unlink(crb), add = TRUE)
  source_coordinates <- data.frame(
    x = c(3243.684326171875, 3443.931640625),
    y = c(4736.96875, 5265.10546875),
    row.names = c("cell-b", "cell-a")
  )
  spec <- list(rotation_degrees = 78.2, scale = 1)
  transform <- .spx_coordinate_transform_normalize(spec, source_coordinates)
  coordinates <- .spx_apply_coordinate_transform(source_coordinates, spec)
  transform$source_coordinate_fingerprint <-
    .spx_coordinate_transform_fingerprint(source_coordinates)
  transform$transformed_coordinate_fingerprint <-
    .spx_coordinate_transform_fingerprint(coordinates)

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
  for (field in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectories"
  )) {
    object[[field]] <- list()
  }
  object$spatial <- list(
    `fov.2` = list(
      coordinates = coordinates,
      coordinate_transform = transform
    )
  )
  object$trekker <- NULL
  object$hla_typing <- NULL
  class(object) <- c("Cerebro_v1.3", "R6")
  saveRDS(object, crb)

  item <- builder_build_test_plan()$items[[1L]]
  item$artifact_identity$spatial_sections <- "fov.2"
  json_spec <- spec
  json_spec$rotation_degrees <- spec$rotation_degrees + 2^-46
  expect_false(identical(
    spec$rotation_degrees,
    json_spec$rotation_degrees
  ))
  item$spatial_coordinate_transforms <- list(`fov.2` = json_spec)
  item$viewer_page_expectations$visible_conditional <- "spatial"

  expect_true(builder_verify_crb(crb, item)$valid)
})

test_that("Spatial and Trekker alignments persist without upload paths", {
  crb_path <- tempfile(fileext = ".crb")
  on.exit(unlink(crb_path), add = TRUE)

  crb <- new.env(parent = emptyenv())
  crb$spatial <- list(
    `slice-a` = list(
      coordinates = data.frame(x = 1, y = 2),
      histology_images = list(
        Existing = list(
          histology_image = "data:image/png;base64,AA==",
          histology_image_bounds = list(xmin = 0, xmax = 1, ymin = 0, ymax = 2)
        )
      )
    )
  )
  crb$trekker <- NULL
  crb$availableSpatial <- function() names(crb$spatial)
  crb$getSpatialData <- function(name) crb$spatial[[name]]
  crb$addSpatialData <- function(name, value) {
    crb$spatial[[name]] <- value
    invisible(value)
  }
  crb$addTrekker <- function(value) {
    crb$trekker <- value
    invisible(value)
  }
  class(crb) <- c("Cerebro_v1.3", "R6")
  saveRDS(crb, crb_path)

  make_alignment <- function(section_id, section_kind) {
    builder_alignment_record(
      source = list(
        name = "/private/tmp/shiny-upload/tissue.png",
        type = "image/png"
      ),
      source_uri = "data:image/png;base64,AA==",
      uri = "data:image/png;base64,BB==",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 8),
      parameters = list(
        dx = 2,
        dy = -1,
        scale = 1.25,
        rotation = 90,
        flip_x = TRUE,
        flip_y = FALSE,
        image_opacity = 0.7,
        point_opacity = 0.8,
        point_size = 3
      ),
      section = list(id = section_id, kind = section_kind)
    )
  }
  spatial_alignment <- make_alignment("slice-a", "spatial")
  trekker_alignment <- make_alignment("trekker", "trekker")
  trekker <- list(x = 1, y = 2, clusters = 0L)

  result <- builder_attach_crb_extras(
    crb_path,
    images = list(`slice-a` = spatial_alignment),
    trekker = trekker,
    trekker_alignment = trekker_alignment
  )

  expect_null(result$error)
  observed <- readRDS(crb_path)
  expect_identical(
    observed$spatial[["slice-a"]]$histology_alignment,
    builder_alignment_payload(spatial_alignment)
  )
  expect_identical(
    observed$spatial[["slice-a"]]$histology_images[["tissue.png"]],
    builder_histology_image_payload(spatial_alignment)
  )
  expect_true(
    "Existing" %in%
      names(
        observed$spatial[["slice-a"]]$histology_images
      )
  )
  expect_identical(
    observed$trekker$histology_alignment,
    builder_alignment_payload(trekker_alignment)
  )
  expect_identical(
    observed$trekker$histology_image,
    trekker_alignment$uri
  )
  serialized <- paste(capture.output(str(observed)), collapse = "\n")
  expect_false(grepl("/private/tmp/shiny-upload", serialized, fixed = TRUE))
  expect_false(grepl("source_uri", serialized, fixed = TRUE))
})

test_that("external Spatial images materialize without entering CRB payloads", {
  root <- withr::local_tempdir()
  record <- builder_alignment_record(
    source = list(name = "H&E.png", type = "image/png"),
    source_uri = "data:image/png;base64,iVBORw0KGgo=",
    uri = "data:image/png;base64,iVBORw0KGgo=",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 8),
    parameters = list(
      dx = 2,
      dy = -1,
      scale = 1.25,
      rotation = 90,
      flip_x = TRUE,
      flip_y = FALSE,
      image_opacity = 0.8
    ),
    section = list(id = "slice-a", kind = "spatial")
  )
  item <- list(
    id = "dataset-a",
    name = "Dataset A",
    images = list(`slice-a` = list(`H&E` = record))
  )

  external <- .builder_build_materialize_spatial_images(item, root)
  descriptor <- external$images[["Dataset A"]][["slice-a"]][["H&E"]]
  setting <- external$settings[["Dataset A"]][["slice-a"]][["H&E"]]

  expect_true(file.exists(descriptor$path))
  expect_identical(
    unname(descriptor$bounds),
    c(0, 10, 0, 8)
  )
  expect_identical(setting$image_opacity, 0.8)
  expect_identical(setting$scale_x, 1.25)
  expect_identical(setting$scale_y, 1.25)
})

test_that("Builder image attachment is exact, collision-safe, and idempotent", {
  withr::local_options(warnPartialMatchDollar = TRUE)
  embedded <- list(
    histology_image = "data:image/png;base64,AA==",
    histology_image_bounds = c(xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  )
  spatial <- list(
    histology_images = list(`tissue.png` = embedded),
    ## An imported CRB may already own a canonical image and alignment. Without
    ## an explicit Builder marker it is user data, not ours to replace.
    histology_alignment = list(source = "tissue.png", image_opacity = 0.4)
  )
  make_record <- function(uri, dx) {
    builder_alignment_record(
      source = list(name = "tissue.png", type = "image/png"),
      source_uri = uri,
      uri = uri,
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 8),
      parameters = list(dx = dx, image_opacity = 0.7),
      section = list(id = "slice-a", kind = "spatial")
    )
  }

  first <- expect_no_warning(builder_attach_spatial_image(
    spatial,
    make_record("data:image/png;base64,BB==", 1)
  ))
  expect_identical(
    names(first$histology_images),
    c("tissue.png", "tissue.png.1")
  )
  expect_identical(first$histology_alignment$source, "tissue.png.1")
  expect_true(first$histology_alignment$builder_managed)
  expect_identical(first$histology_images[["tissue.png"]], embedded)

  second <- expect_no_warning(builder_attach_spatial_image(
    first,
    make_record("data:image/png;base64,CC==", 2)
  ))
  expect_identical(
    names(second$histology_images),
    c("tissue.png", "tissue.png.1")
  )
  expect_identical(second$histology_alignment$source, "tissue.png.1")
  expect_identical(
    second$histology_images[["tissue.png.1"]]$histology_image,
    "data:image/png;base64,CC=="
  )
  expect_identical(second$histology_images[["tissue.png"]], embedded)
})

test_that("one embedded FOV keeps every Builder image label and appearance", {
  spatial <- list(
    coordinates = data.frame(x = 1:2, y = 2:1, row.names = c("a", "b")),
    histology_images = list()
  )
  make_record <- function(name, opacity, point_size) {
    builder_alignment_record(
      source = list(name = paste0(name, ".png"), type = "image/png"),
      source_uri = paste0("data:image/png;base64,", name),
      uri = paste0("data:image/png;base64,", name),
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 6),
      parameters = list(image_opacity = opacity, point_size = point_size),
      section = list(id = "FOV_A", kind = "spatial")
    )
  }
  spatial <- builder_attach_spatial_image(
    spatial,
    make_record("axes", 0.6, 8),
    label = "Axes",
    replace_managed = TRUE
  )
  spatial <- builder_attach_spatial_image(
    spatial,
    make_record("layers", 0.4, 4),
    label = "Layers",
    replace_managed = FALSE
  )

  expect_identical(names(spatial$histology_images), c("Axes", "Layers"))
  expect_equal(
    spatial$histology_images$Axes$histology_alignment$image_opacity,
    0.6
  )
  expect_equal(
    spatial$histology_images$Layers$histology_alignment$point_size,
    4
  )
})

test_that("legacy singular Builder images migrate without leaving a duplicate", {
  old <- "data:image/png;base64,OLD="
  spatial <- list(
    histology_image = old,
    histology_image_bounds = c(xmin = 0, xmax = 4, ymin = 0, ymax = 4),
    histology_alignment = list(
      source = "tissue.png",
      dx = 0,
      dy = 0,
      scale = 1,
      rotation = 0,
      flip_x = FALSE,
      flip_y = FALSE,
      image_opacity = 0.8,
      point_opacity = 0.85,
      point_size = 5
    )
  )
  record <- builder_alignment_record(
    source = list(name = "tissue.png", type = "image/png"),
    source_uri = "data:image/png;base64,NEW=",
    uri = "data:image/png;base64,NEW=",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 8),
    section = list(id = "slice-a", kind = "spatial")
  )

  migrated <- builder_attach_spatial_image(spatial, record)
  expect_identical(names(migrated$histology_images), "tissue.png")
  expect_identical(
    migrated$histology_images[["tissue.png"]]$histology_image,
    "data:image/png;base64,NEW="
  )
  expect_true(migrated$histology_alignment$builder_managed)
  expect_null(migrated[["histology_image", exact = TRUE]])
  expect_null(migrated[["histology_image_bounds", exact = TRUE]])
})

test_that("unowned or malformed legacy singular images are preserved", {
  old <- "data:image/png;base64,OLD="
  bounds <- c(xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  record <- builder_alignment_record(
    source = list(name = "new.png", type = "image/png"),
    source_uri = "data:image/png;base64,NEW=",
    uri = "data:image/png;base64,NEW=",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 8),
    section = list(id = "slice-a", kind = "spatial")
  )
  cases <- list(
    plain = NULL,
    malformed = list(
      source = NULL,
      dx = 0,
      dy = 0,
      scale = 1,
      rotation = 0,
      flip_x = FALSE,
      flip_y = FALSE,
      image_opacity = 0.8,
      point_opacity = 0.85,
      point_size = 5
    )
  )

  for (case in names(cases)) {
    spatial <- list(
      histology_image = old,
      histology_image_bounds = bounds
    )
    if (!is.null(cases[[case]])) {
      spatial$histology_alignment <- cases[[case]]
    }
    attached <- builder_attach_spatial_image(spatial, record)
    expect_identical(
      names(attached$histology_images),
      c("Tissue background", "new.png"),
      info = case
    )
    expect_identical(
      attached$histology_images[["Tissue background"]]$histology_image,
      old,
      info = case
    )
  }
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
  object$orig.ident <- factor(rep("sample_a", ncol(object)))
  item <- list(
    included_projections = "tsne",
    assay = "RNA",
    layer = "data",
    artifact_identity = list(
      group_levels = list(groups = sort(unique(object$groups)))
    ),
    metadata_policy = list(
      retained = c(
        "cell_barcode",
        "groups",
        "nCount_RNA",
        "nFeature_RNA",
        "orig.ident"
      )
    ),
    tables = list()
  )

  item$analyses <- character()
  prepared <- .builder_build_apply_metadata_policy(object, item)
  expect_setequal(
    colnames(prepared@meta.data),
    c("groups", "nCount_RNA", "nFeature_RNA", "orig.ident")
  )
  expect_contains(colnames(prepared@meta.data), "orig.ident")
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

  motif_only_item <- list(
    manifest = list(
      immune_repertoire = list(
        disposition = NA_character_,
        evidence = list(selected_sources = character())
      ),
      hla_tcr_motifs = list(
        disposition = "converted",
        evidence = list(
          selected_sources = "legacy_tcr",
          selected_candidates = list(
            legacy_tcr = list(
              full_ir_ready = TRUE,
              hla_tcr_ready = TRUE
            )
          )
        )
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, motif_only_item),
    "cannot hide only one Viewer page",
    fixed = TRUE
  )

  incompatible_item <- list(
    included_groups = "groups",
    manifest = list(
      immune_repertoire = list(
        disposition = "converted",
        evidence = list(
          selected_sources = "metadata",
          selected_candidates = list(metadata = list(normalized = list()))
        )
      ),
      hla_tcr_motifs = list(
        disposition = "preserved",
        evidence = list(
          selected_sources = "unified_misc",
          selected_candidates = list(unified_misc = list())
        )
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, incompatible_item),
    "cannot be realized by one frozen immune payload",
    fixed = TRUE
  )

  partial_motif_item <- list(
    manifest = list(
      hla_tcr_motifs = list(
        disposition = "preserved",
        evidence = list(
          selected_sources = "unified_misc",
          selected_candidates = list(
            unified_misc = list(
              full_ir_ready = FALSE,
              hla_tcr_ready = TRUE
            )
          )
        )
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, partial_motif_item),
    "cannot be exported as one immune payload",
    fixed = TRUE
  )

  coupled_item <- list(
    manifest = list(
      immune_repertoire = list(
        disposition = "preserved",
        evidence = list(
          selected_sources = "unified_misc",
          selected_candidates = list(
            unified_misc = list(
              full_ir_ready = TRUE,
              hla_tcr_ready = TRUE
            )
          )
        )
      ),
      hla_tcr_motifs = list(
        disposition = "filtered",
        evidence = list(selected_sources = character())
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, coupled_item),
    "cannot hide only one Viewer page",
    fixed = TRUE
  )

  reverse_coupled_item <- list(
    manifest = list(
      immune_repertoire = list(
        disposition = "filtered",
        evidence = list(selected_sources = character())
      ),
      hla_tcr_motifs = list(
        disposition = "preserved",
        evidence = list(
          selected_sources = "unified_misc",
          selected_candidates = list(
            unified_misc = list(
              full_ir_ready = TRUE,
              hla_tcr_ready = TRUE
            )
          )
        )
      )
    )
  )
  expect_error(
    .builder_build_prepare_immune(object, reverse_coupled_item),
    "cannot hide only one Viewer page",
    fixed = TRUE
  )

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
    list(
      items = list(item),
      make_app = FALSE,
      app_options = list(enabled = FALSE),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    ),
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
