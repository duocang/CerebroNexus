projection_first_frame_indices <- function(metadata, percentage = 100) {
  scope <- new.env(parent = globalenv())
  scope$input <- list(test_percentage_cells_to_show = percentage)
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  scope$getGroups <- function() character()
  scope$viewerProjectionCellIndices("test", metadata)
}

test_that("a full projection preserves canonical row order", {
  metadata <- data.frame(
    cell_barcode = paste0("cell-", seq_len(8L)),
    state = factor(rep(c("A", "B"), 4L))
  )

  set.seed(42L)
  expect_identical(
    projection_first_frame_indices(metadata),
    seq_len(nrow(metadata))
  )
})

test_that("projection payload selects only required metadata columns", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  metadata <- data.frame(
    cell_barcode = c("c1", "c2"),
    major_type = factor(c("T", "B")),
    sample = factor(c("s1", "s2")),
    nUMI = 1:2,
    nGene = 2:3,
    unused = c("x", "y")
  )

  expect_identical(
    scope$viewerProjectionMetadataColumns(
      metadata,
      "major_type",
      hover_info = FALSE,
      groups = c("major_type", "sample")
    ),
    c("cell_barcode", "major_type")
  )
  expect_identical(
    scope$viewerProjectionMetadataColumns(
      metadata,
      "major_type",
      hover_info = TRUE,
      groups = c("major_type", "sample")
    ),
    c("cell_barcode", "major_type", "nUMI", "nGene", "sample")
  )
})

test_that("projection first frame excludes cell identities and hover metadata", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  metadata <- data.frame(
    cell_barcode = c("c1", "c2"),
    major_type = factor(c("T", "B")),
    nUMI = 1:2,
    nGene = 2:3
  )

  expect_identical(
    scope$viewerProjectionFirstFrameColumns(metadata, "major_type"),
    "major_type"
  )

  projection_source <- paste(
    readLines(
      viewer_test_path("overview", "obj_projection_data.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    projection_source,
    "viewerProjectionFirstFrameColumns",
    fixed = TRUE
  )
})

test_that("thin CRB first frames do not force cell-barcode hydration", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  object <- new.env(parent = emptyenv())
  metadata <- data.frame(group = factor(c("A", "B")))
  projection <- data.frame(x = 1:2, y = 3:4)
  attr(object, "cerebro_projection_first_frame") <- list(
    meta_data = metadata,
    projections = list(umap = projection)
  )
  scope$data_set <- function() object

  expect_identical(scope$viewerProjectionFirstFrameMetadata(), metadata)
  expect_identical(
    scope$viewerProjectionFirstFrameCoordinates("umap"),
    projection
  )

  update_source <- paste(
    readLines(
      viewer_test_path("overview", "func_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  deferred_at <- regexpr(
    "deferred_aux <- function",
    update_source,
    fixed = TRUE
  )
  metadata_at <- regexpr(
    "metadata <- getMetaData()",
    update_source,
    fixed = TRUE
  )
  expect_gt(metadata_at, deferred_at)
})

test_that("first-frame cache is optional outside a loaded dataset", {
  scope <- new.env(parent = globalenv())
  scope$data_set <- NULL
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)

  expect_null(scope$viewerProjectionFirstFrameCache())
})

test_that("projection auxiliary data is built only after it is requested", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  calls <- 0L
  pending <- list(
    id = "overview_projection",
    wire_token = 7L,
    build = function() {
      calls <<- calls + 1L
      list(
        selection_key = list(c("c2", "c4"), c("c1", "c3")),
        hover = list(hoverinfo = "skip")
      )
    }
  )

  expect_identical(calls, 0L)
  message <- scope$cerebroCellViewResolveDeferredAux(pending)
  expect_identical(calls, 1L)
  expect_null(message$build)
  expect_identical(message$id, "overview_projection")
  expect_identical(message$wire_token, 7L)
  expect_identical(
    message$selection_key,
    list(c("c2", "c4"), c("c1", "c3"))
  )
})

test_that("projection auxiliary rows preserve plotted group order", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  auxiliary <- scope$cerebroCellViewDeferredAux(
    selection_rows = list(c(2L, 4L), c(1L, 3L)),
    cell_barcodes = c("c1", "c2", "c3", "c4"),
    hover_columns = list(list(
      label = "Transcripts",
      format = "integer",
      values = c(10L, 20L, 30L, 40L)
    )),
    hover = TRUE
  )

  expect_identical(
    lapply(auxiliary$selection_key, as.character),
    list(c("c2", "c4"), c("c1", "c3"))
  )
  expect_identical(
    lapply(auxiliary$hover$columns[[1L]]$values, as.integer),
    list(c(20L, 40L), c(10L, 30L))
  )
})

test_that("trajectory first frame defers identities and hover", {
  source <- paste(
    readLines(
      viewer_test_path("trajectory", "projection_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "selection_keys = seq_len(nrow(cells_df))", fixed = TRUE)
  expect_match(source, "deferred_aux <- function()", fixed = TRUE)
  expect_match(source, "deferred_aux = deferred_aux", fixed = TRUE)
})

test_that("spatial first frame defers identities and hover", {
  source <- paste(
    readLines(
      viewer_test_path("spatial", "func_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    source,
    "selection_keys <- seq_len(nrow(metadata))",
    fixed = TRUE
  )
  expect_match(
    source,
    "build_deferred_aux <- function(selection_rows)",
    fixed = TRUE
  )
  expect_match(source, "deferred_aux = function()", fixed = TRUE)
})

test_that("gene-expression first frame defers identities and hover", {
  source <- paste(
    readLines(
      viewer_test_path("gene_expression", "func_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "selection_key = seq_len(nrow(metadata))", fixed = TRUE)
  expect_match(source, "deferred_aux <- function()", fixed = TRUE)
  expect_match(source, "deferred_aux = deferred_aux", fixed = TRUE)
})

test_that("projection rendering has one first-frame debounce entry point", {
  event_source <- paste(
    readLines(
      viewer_test_path("overview", "event_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    event_source,
    "data <- overview_projection_data_to_plot()",
    fixed = TRUE
  )
  expect_false(grepl(
    "overview_projection_rendered",
    event_source,
    fixed = TRUE
  ))
  expect_false(grepl(
    "overview_projection_data_to_plot_raw()",
    event_source,
    fixed = TRUE
  ))
})

test_that("projection can render before its dynamic controls bind", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  metadata <- data.frame(
    cell_barcode = c("c1", "c2"),
    sample = factor(c("a", "b")),
    cell_type = factor(c("T", "B"))
  )

  expect_identical(
    scope$viewerProjectionDefaults(
      metadata,
      projections = c("tsne", "umap"),
      parameters = list(main_group = "sample")
    ),
    list(projection = "tsne", color_variable = "sample")
  )
  expect_identical(
    scope$viewerProjectionDefaults(
      metadata,
      projections = "umap",
      parameters = list(main_group = "missing")
    )$color_variable,
    "cell_type"
  )

  parameter_source <- paste(
    readLines(
      viewer_test_path("overview", "obj_projection_parameters_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(parameter_source, "viewerProjectionDefaults", fixed = TRUE)
  expect_match(parameter_source, "current_scatter_defaults", fixed = TRUE)
  expect_false(grepl(
    '!is.null(input[["overview_projection_group_labels"]])',
    parameter_source,
    fixed = TRUE
  ))
  expect_match(parameter_source, "reset_axes = TRUE", fixed = TRUE)
})

test_that("a new projection mount redraws unchanged data", {
  server <- function(input, output, session) {
    renders <- 0L
    overview_projection_data_to_plot <- reactive({
      input[["late_bound_control"]]
      list(value = 1L)
    })
    overview_projection_update_plot <- function(data) {
      renders <<- renders + 1L
    }
    sys.source(
      viewer_test_path("overview", "event_projection_update_plot.R"),
      envir = environment()
    )
    session$userData$render_count <- function() renders
  }

  shiny::testServer(server, {
    session$setInputs(
      overview_projection_render_request = 1,
      late_bound_control = 1
    )
    expect_identical(session$userData$render_count(), 1L)

    session$setInputs(late_bound_control = 2)
    expect_identical(session$userData$render_count(), 1L)

    session$setInputs(overview_projection_render_request = 2)
    expect_identical(session$userData$render_count(), 2L)
  })
})

test_that("full ordered projection rows bypass row subsetting", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  table <- data.frame(
    x = 1:4,
    y = 5:8,
    unused = 9:12,
    row.names = paste0("cell-", 1:4)
  )

  expect_identical(
    scope$viewerProjectionSubsetRows(
      table,
      seq_len(nrow(table)),
      c("x", "y")
    ),
    table[, c("x", "y"), drop = FALSE]
  )
  expect_identical(
    scope$viewerProjectionSubsetRows(table, c(4L, 2L), c("x", "y")),
    table[c(4L, 2L), c("x", "y"), drop = FALSE]
  )
  expect_identical(
    scope$viewerProjectionSubsetRows(table, seq_len(nrow(table)), NULL),
    table
  )

  coordinate_source <- paste(
    readLines(
      viewer_test_path("overview", "obj_projection_coordinates.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    coordinate_source,
    "viewerProjectionSubsetRows",
    fixed = TRUE
  )
})

test_that("thin CRBs discard duplicate metadata row names", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  cells <- paste0("cell-", seq_len(4L))
  counts <- matrix(
    seq_len(12L),
    nrow = 3L,
    dimnames = list(paste0("gene-", seq_len(3L)), cells)
  )
  sparse <- methods::as(Matrix::Matrix(counts, sparse = TRUE), "CsparseMatrix")
  sidecar <- file.path(root, "expression.bpcells")
  BPCells::write_matrix_dir(methods::as(sparse, "IterableMatrix"), sidecar)

  object <- Cerebro$new()
  object$expression <- BPCells::open_matrix_dir(sidecar)
  object$setExpressionBackend("bpcells", basename(sidecar))
  object$meta_data <- data.frame(
    cell_barcode = cells,
    sample = factor(c("a", "a", "b", "b")),
    row.names = cells
  )
  object$projections <- list(
    umap = data.frame(
      UMAP_1 = seq_along(cells),
      UMAP_2 = rev(seq_along(cells)),
      row.names = cells
    )
  )
  crb <- file.path(root, "compact.crb")
  saveCerebro(object, crb)

  payload <- .readCerebroPayload(crb)
  expect_identical(.row_names_info(payload$meta_data, 1L), -4L)

  restored <- readCerebro(crb)
  expect_identical(.row_names_info(restored$meta_data, 1L), -4L)
  expect_identical(restored$meta_data$cell_barcode, cells)

  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  runtime_object <- runtime$read_cerebro_file(crb)
  runtime_object <- runtime$.attachExternalExpression(runtime_object, crb)
  expect_identical(.row_names_info(runtime_object$meta_data, 1L), -4L)
  expect_identical(runtime_object$meta_data$cell_barcode, cells)
  expect_true(rlang::env_binding_are_lazy(runtime_object, "expression"))
})
