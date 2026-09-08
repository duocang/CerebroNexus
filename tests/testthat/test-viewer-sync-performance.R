sync_perf_root_candidates <- c(
  file.path(getwd(), "inst", "viewer"),
  file.path(getwd(), "..", "..", "inst", "viewer"),
  testthat::test_path("..", "..", "inst", "viewer")
)
sync_perf_viewer_root <- sync_perf_root_candidates[
  dir.exists(sync_perf_root_candidates)
][1L]
if (is.na(sync_perf_viewer_root)) {
  sync_perf_viewer_root <- system.file("viewer", package = "CerebroNexus")
}

read_sync_perf_viewer <- function(...) {
  paste(
    readLines(file.path(sync_perf_viewer_root, ...), warn = FALSE),
    collapse = "\n"
  )
}

test_that("the first reactive value bypasses debounce", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  expect_true(is.function(utility_env$debounceAfterFirst))

  compute_count <- 0L
  server <- function(input, output, session) {
    raw <- shiny::reactive({
      shiny::req(input$value)
      compute_count <<- compute_count + 1L
      input$value
    })
    ready <- utility_env$debounceAfterFirst(raw, 10000)
  }

  shiny::testServer(server, {
    session$setInputs(value = "first")
    expect_identical(ready(), "first")
    expect_identical(compute_count, 1L)

    session$setInputs(value = "second")
    expect_identical(ready(), "first")
    expect_identical(compute_count, 2L)
  })
})

test_that("event debounce coalesces expensive work after the first value", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  expect_true(is.function(utility_env$debounceEventAfterFirst))

  compute_count <- 0L
  server <- function(input, output, session) {
    event <- shiny::reactive(input$value)
    value <- shiny::reactive({
      compute_count <<- compute_count + 1L
      input$value
    })
    ready <- utility_env$debounceEventAfterFirst(event, value, 100)
  }

  shiny::testServer(server, {
    session$setInputs(value = "first")
    expect_identical(ready(), "first")
    expect_identical(compute_count, 1L)

    session$setInputs(value = "second")
    session$setInputs(value = "third")
    session$elapse(101)
    expect_identical(ready(), "third")
    expect_identical(compute_count, 2L)
  })
})

test_that("expression rows are fetched once and aligned by cell", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  expect_true(is.function(utility_env$viewerExpressionValues))

  calls <- 0L
  requested <- NULL
  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionMatrix <- function(cells, genes) {
    calls <<- calls + 1L
    requested <<- list(cells = cells, genes = genes)
    matrix(
      c(1, 2, 3, 4),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(c("A", "B"), c("c2", "c1"))
    )
  }

  values <- utility_env$viewerExpressionValues(
    data_set,
    cells = c("c1", "c2"),
    genes = c("B", "A", "B", "")
  )

  expect_identical(calls, 1L)
  expect_identical(requested$cells, c("c1", "c2"))
  expect_identical(requested$genes, c("B", "A"))
  expect_identical(values, list(B = c(4, 3), A = c(2, 1)))
})

test_that("single-gene expression uses row access with a matrix fallback", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )

  row_calls <- 0L
  matrix_calls <- 0L
  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionRow <- function(gene, cells) {
    row_calls <<- row_calls + 1L
    stats::setNames(c(2, 1), c("c2", "c1"))[cells]
  }
  data_set$getExpressionMatrix <- function(cells, genes) {
    matrix_calls <<- matrix_calls + 1L
    matrix(c(2, 1), nrow = 1L, dimnames = list(genes, cells))
  }

  values <- utility_env$viewerExpressionValues(data_set, c("c1", "c2"), "A")
  expect_identical(row_calls, 1L)
  expect_identical(matrix_calls, 0L)
  expect_identical(values, list(A = c(1, 2)))

  data_set$getExpressionRow <- NULL
  values <- utility_env$viewerExpressionValues(data_set, c("c1", "c2"), "A")
  expect_identical(matrix_calls, 1L)
  expect_identical(values, list(A = c(2, 1)))
})

test_that("already aligned expression cells skip the string match", {
  match_lengths <- integer()
  utility_env <- new.env(parent = globalenv())
  utility_env$match <- function(x, table, ...) {
    match_lengths <<- c(match_lengths, length(x))
    base::match(x, table, ...)
  }
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )

  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionMatrix <- function(cells, genes) {
    matrix(
      seq_len(length(cells) * length(genes)),
      nrow = length(genes),
      dimnames = list(genes, cells)
    )
  }
  utility_env$viewerExpressionValues(
    data_set,
    cells = c("c1", "c2", "c3"),
    genes = c("A", "B")
  )

  expect_false(3L %in% match_lengths)
})

test_that("Viewer hidden outputs activate on their first owning tab visit", {
  server <- read_sync_perf_viewer("shiny_server.R")
  group_filters <- read_sync_perf_viewer(
    "module",
    "group_filters",
    "group_filters_widget.R"
  )

  expect_match(server, "viewer_hidden_output_options", fixed = TRUE)
  expect_match(server, 'input[["sidebar"]]', fixed = TRUE)
  expect_match(server, "viewerOutputTab", fixed = TRUE)
  expect_no_match(server, "viewer_deferred_output_spacing_ms", fixed = TRUE)
  expect_no_match(server, "delay = 1", fixed = TRUE)
  expect_no_match(server, "cerebro_async", fixed = TRUE)
  expect_match(group_filters, 'get0("outputOptions"', fixed = TRUE)
})

test_that("hidden output IDs resolve to their owning sidebar tabs", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  expect_identical(
    utility_env$viewerOutputTab(c(
      "overview_projection_point_border_UI",
      "expression_projection_data_parameters_UI",
      "spatial_projection_main_parameters_UI",
      "coordviews_image_ui",
      "ir_display_panel",
      "trajectory_projection_group_labels_UI",
      "trekker_main_parameters_ui",
      "hla_more_parameters_ui",
      "unknown_output"
    )),
    c(
      "overview",
      "geneExpression",
      "spatial",
      "coordinated_views",
      "immune_repertoire",
      "trajectory",
      "trekker",
      "hla_tcr_motifs",
      NA_character_
    )
  )
})

test_that("visible projections keep debounce after their first render", {
  paths <- c(
    "overview/obj_projection_data_to_plot.R",
    "gene_expression/obj_projection_data_to_plot.R",
    "spatial/obj_projection_data_to_plot.R"
  )

  for (path in paths) {
    expect_match(
      read_sync_perf_viewer(path),
      "debounceEventAfterFirst",
      fixed = TRUE,
      info = path
    )
  }
})

test_that("RGB and linked expression use batched reads", {
  gene_expression <- read_sync_perf_viewer(
    "gene_expression",
    "obj_projection_expression_levels.R"
  )
  spatial <- read_sync_perf_viewer(
    "spatial",
    "obj_projection_data_to_plot.R"
  )
  linked <- read_sync_perf_viewer("coordinated_views", "server.R")

  expect_match(gene_expression, "viewerExpressionValues", fixed = TRUE)
  expect_match(spatial, "viewerExpressionValues", fixed = TRUE)
  expect_match(linked, "cv_gene_values_many", fixed = TRUE)
  expect_match(linked, "viewerExpressionValues", fixed = TRUE)
})

test_that("Spatial full extents are memoized by a session reactive", {
  spatial <- read_sync_perf_viewer(
    "spatial",
    "obj_projection_data_to_plot.R"
  )

  expect_match(
    spatial,
    "spatial_projection_full_extent <- cachePlot(",
    fixed = TRUE
  )
  expect_match(
    spatial,
    "extent <- spatial_projection_full_extent()",
    fixed = TRUE
  )
})

test_that("session cache reuses a Spatial-style A-B-A key", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  compute_count <- 0L
  server <- function(input, output, session) {
    extent <- utility_env$cachePlot(
      shiny::reactive({
        compute_count <<- compute_count + 1L
        input$key
      }),
      input$key
    )
  }

  shiny::testServer(server, {
    session$setInputs(key = "A")
    expect_identical(extent(), "A")
    session$setInputs(key = "B")
    expect_identical(extent(), "B")
    session$setInputs(key = "A")
    expect_identical(extent(), "A")
    expect_identical(compute_count, 2L)
  })
})

test_that("single-gene Viewer routes prefer row extraction", {
  paths <- c(
    "gene_expression/obj_projection_expression_levels.R",
    "spatial/obj_projection_data_to_plot.R",
    "spatial/out_morans_i.R",
    "trekker/server.R"
  )
  for (path in paths) {
    expect_match(
      read_sync_perf_viewer(path),
      "viewerExpressionRow",
      fixed = TRUE,
      info = path
    )
  }
})

test_that("Viewer source parsing is cached but evaluation remains per session", {
  source_cache <- file.path(sync_perf_viewer_root, "source_cache.R")
  expect_true(file.exists(source_cache))
  cache_env <- new.env(parent = globalenv())
  sys.source(source_cache, envir = cache_env)

  source_file <- tempfile(fileext = ".R")
  writeLines("value <- 1L", source_file)
  first <- new.env(parent = baseenv())
  second <- new.env(parent = baseenv())
  cache_env$viewerSource(source_file, first)
  cache_env$viewerSource(source_file, second)
  expect_identical(first$value, 1L)
  expect_identical(second$value, 1L)
  expect_identical(length(cache_env$.viewer_source_cache), 1L)

  writeLines("value <- 200L", source_file)
  third <- new.env(parent = baseenv())
  cache_env$viewerSource(source_file, third)
  expect_identical(third$value, 200L)
})
