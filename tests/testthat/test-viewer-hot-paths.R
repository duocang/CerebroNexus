## Per-frame and per-selection paths of the Viewer that run over every cell:
## they must give the same result as the plain character code they replace,
## without the copies and string conversions.

test_that("cell-view messages send bare AsIs arrays without copying them", {
  env <- viewer_runtime_env()
  x <- I(as.numeric(1:8))
  message <- env$cerebroCellViewMessage(
    "overview_projection",
    meta = list(color_type = "continuous"),
    data = list(x = x, color = c(a = 1, b = 2))
  )
  expect_identical(rlang::obj_address(message$data$x), rlang::obj_address(x))
  ## Named values are still unnamed and marked as arrays.
  expect_null(names(message$data$color))
  expect_s3_class(message$data$color, "AsIs")
})

test_that("factor group filters match the character lookup", {
  env <- viewer_runtime_env()
  values <- factor(
    c("A", NA, "B", "NA", "A", NA),
    levels = c("A", "B", "NA", "unused")
  )
  metadata <- data.frame(group = values, other = 1:6)
  for (selected in list("A", c("B", "NA"), c("A", NA), "unused", 1)) {
    expect_identical(
      env$cerebroGroupFilterMask(metadata, list(group = selected)),
      values %in% selected
    )
  }
  ## A factor with an NA level.
  metadata$group <- factor(c("A", NA, "B", NA, "A", "B"), exclude = NULL)
  expect_identical(
    env$cerebroGroupFilterMask(metadata, list(group = c("B", NA))),
    metadata$group %in% c("B", NA)
  )
})

test_that("factor hover columns encode like their labels", {
  env <- viewer_runtime_env()
  labels <- c("b", NA, "a", "NA", "b", "c")
  as_factor <- data.frame(
    group = factor(labels, levels = c("a", "b", "c", "NA", "unused"))
  )
  as_character <- data.frame(group = labels)
  expect_identical(
    env$cerebroProjectionHoverColumns(as_factor, groups = "group"),
    env$cerebroProjectionHoverColumns(as_character, groups = "group")
  )
  ## Registered levels, complete and incomplete.
  for (registered in list(c("c", "b", "a"), c("b", "a"))) {
    env$group_levels <- function(group) registered
    expect_identical(
      env$cerebroProjectionHoverColumns(as_factor, groups = "group"),
      env$cerebroProjectionHoverColumns(as_character, groups = "group")
    )
  }
})

test_that("per-group scatter payloads split a factor like its labels", {
  env <- viewer_runtime_env()
  payload <- function(color, n) {
    env$cerebroCellViewScatterPayload(
      coordinates = list(as.numeric(seq_len(n)), as.numeric(rev(seq_len(n)))),
      color = color,
      color_variable = "cluster",
      selection_keys = seq_len(n),
      point_size = 2,
      point_opacity = 1,
      color_assignments = c(
        B = "#222222",
        A = "#111111",
        unused = "#333333"
      ),
      hover_info = paste0("cell-", seq_len(n))
    )
  }
  for (n in c(9L, 4104L)) {
    labels <- rep_len(c("A", "B", NA), n)
    factor_payload <- payload(
      factor(labels, levels = c("A", "B", "unused")),
      n
    )
    expect_identical(factor_payload, payload(labels, n))
    expect_identical(
      unlist(factor_payload$meta$traces),
      c("B", "A", "(missing)")
    )
  }
  ## A level already named "(missing)" takes the missing values too.
  labels <- c("A", NA, "(missing)", "A")
  expect_identical(
    payload(factor(labels, levels = c("A", "(missing)")), 4L),
    payload(labels, 4L)
  )
})

test_that("auxiliary data after a core delivery builds no cell names", {
  env <- viewer_runtime_env()
  calls <- character()
  resolved <- env$cerebroCellViewResolveDeferredAux(
    list(build = function(keys = TRUE) list(keys = keys)),
    keys = FALSE
  )
  expect_false(resolved$keys)
  ## Builders without the argument are called as before.
  resolved <- env$cerebroCellViewResolveDeferredAux(
    list(build = function() list(keys = "all")),
    keys = FALSE
  )
  expect_identical(resolved$keys, "all")

  object <- new.env(parent = emptyenv())
  attr(object, "cerebro_projection_first_frame") <- list(
    meta_data = data.frame(
      nUMI = c(10L, 20L, 30L),
      cluster = factor(c("x", "y", "x"))
    )
  )
  env$data_set <- function() object
  env$getMetaData <- function() {
    calls <<- c(calls, "getMetaData")
    data.frame(
      cell_barcode = c("c1", "c2", "c3"),
      nUMI = c(10L, 20L, 30L),
      cluster = factor(c("x", "y", "x"))
    )
  }
  env$getGroups <- function() "cluster"
  env$cerebroCellViewScatterPayload <- function(...) {
    list(meta = list(), data = list(selection_key = 1:2), hover = list())
  }
  env$cerebroCellViewRender <- function(id, meta, data, hover, ...) {
    captured <- list(...)
    env$deferred_aux <- captured$deferred_aux
  }
  sys.source(
    viewer_test_path("overview", "func_projection_update_plot.R"),
    envir = env
  )
  env$overview_projection_update_plot(list(
    cells_df = data.frame(cluster = factor(c("x", "x"))),
    cell_indices = c(3L, 1L),
    coordinates = data.frame(x = 1:2, y = 3:4),
    reset_axes = FALSE,
    plot_parameters = list(
      color_variable = "cluster",
      hover_info = TRUE,
      draw_border = FALSE
    ),
    cell_count = 3L
  ))

  without_keys <- env$deferred_aux(keys = FALSE)
  expect_identical(calls, character())
  expect_null(without_keys$selection_key)
  expect_identical(
    as.integer(without_keys$hover$columns[[1L]]$values),
    c(30L, 10L)
  )
  with_keys <- env$deferred_aux()
  expect_identical(calls, "getMetaData")
  expect_identical(as.character(with_keys$selection_key), c("c3", "c1"))
  expect_identical(with_keys$hover, without_keys$hover)
})

## The browser's encoding (selectionRowBits() in cell_views.js): bit r of
## byte r %/% 8 for 0-based row r.
encode_row_bits <- function(rows) {
  bits <- integer((max(rows) - 1L) %/% 8L + 1L)
  for (row in rows - 1L) {
    byte <- row %/% 8L + 1L
    bits[[byte]] <- bitwOr(bits[[byte]], bitwShiftL(1L, row %% 8L))
  }
  jsonlite::base64_enc(as.raw(bits))
}

test_that("a selection reported by row decodes to its canonical rows", {
  env <- viewer_runtime_env()
  context <- list(dataset_key = "d", generation = 1L)
  env$viewerDatasetContextEqual <- function(a, b) identical(a, b)
  rows <- c(1L, 8L, 9L, 17L, 1000L)
  selection <- env$viewerPersistentSelection(
    list(
      rows_bits = encode_row_bits(rows),
      ids = list(),
      dataset_context = context
    ),
    context
  )
  expect_identical(selection, data.frame(row = rows))
  expect_identical(env$viewerSelectionRows(selection), rows)
  expect_null(env$viewerDecodeRowBits(""))
  expect_null(env$viewerDecodeRowBits(NULL))
  ## Rows outside the cells shown are not selected.
  expect_identical(
    env$selectedCellMask(
      character(4),
      NULL,
      selection,
      rows = c(9L, 2L, NA, 2000L)
    ),
    c(TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("summaries and tables of a selection by row read rows by position", {
  env <- viewer_runtime_env()
  metadata <- data.frame(
    cluster = factor(
      c("A", "B", "A", "C", "B"),
      levels = c("A", "B", "C", "D")
    ),
    nUMI = c(10, 20, 30, 40, 50)
  )
  env$viewerProjectionFirstFrameMetadata <- function() metadata
  env$getMetaData <- function() stop("must not hydrate the metadata")
  env$getGroups <- function() "cluster"
  env$getParameters <- function() list(main_group = "cluster")
  env$getObservationUnit <- function() list(plural = "cells")
  env$reactive_colors <- function() {
    list(cluster = c(A = "red", B = "blue", C = "green"))
  }
  env$viewerCellBarcodesAt <- function(rows) paste0("cell", rows)
  env$viewerDatasetContextEqual <- function(a, b) identical(a, b)
  selection <- data.frame(row = c(1L, 3L, 5L))
  by_name <- data.frame(selection_key = paste0("cell", c(1L, 3L, 5L)))
  named <- data.frame(cell_barcode = paste0("cell", 1:5), metadata)
  for (composition in c(FALSE, TRUE)) {
    expect_identical(
      as.character(env$cerebroSelectionSummary(
        selection,
        "UMAP",
        composition = composition
      )),
      as.character(env$cerebroSelectionSummary(
        by_name,
        "UMAP",
        metadata = named,
        composition = composition
      ))
    )
  }
  table <- env$viewerSelectedRowsMetadata(c(5L, 1L, 99L))
  expect_identical(table$cell_barcode, c("cell5", "cell1"))
  expect_identical(colnames(table), c("cell_barcode", "cluster", "nUMI"))
  ## Hidden legend groups drop their selected rows.
  visible <- env$viewerVisibleSelection(
    selection,
    list(groups = "B", dataset_context = 1),
    1,
    "cluster",
    function() stop("must not build the full metadata")
  )
  expect_identical(visible$row, c(1L, 3L))
  ## The plot compares the chosen column of the shown rows.
  compared <- env$prepareSelectedCellDistribution(
    NULL,
    metadata["nUMI"],
    selection,
    "nUMI",
    rows = 1:5
  )
  expect_identical(
    as.character(compared$group),
    c("selected", "not selected", "selected", "not selected", "selected")
  )
})

test_that("the server leaves a selection summary the browser draws", {
  env <- viewer_runtime_env()
  context <- list(dataset_key = "d", generation = 1L)
  env$viewer_loaded_dataset_context <- function() context
  env$viewerDatasetContextEqual <- function(a, b) identical(a, b)
  selection <- list(rows_bits = "AQ==", dataset_context = context)
  env$input <- list(overview_projection_persistent_selection = selection)
  expect_false(env$viewerClientSelection("overview_projection"))
  selection$client_summary <- TRUE
  env$input <- list(overview_projection_persistent_selection = selection)
  expect_true(env$viewerClientSelection("overview_projection"))
  ## Not for another data set's selection.
  env$input$overview_projection_persistent_selection$dataset_context <- list()
  expect_false(env$viewerClientSelection("overview_projection"))
})
