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

test_that("Viewer startup defers hidden output warmup", {
  server <- read_sync_perf_viewer("shiny_server.R")
  group_filters <- read_sync_perf_viewer(
    "module",
    "group_filters",
    "group_filters_widget.R"
  )

  expect_match(server, "viewer_deferred_output_options", fixed = TRUE)
  expect_match(server, "viewer_deferred_output_spacing_ms <- 50L", fixed = TRUE)
  expect_match(server, "session$onFlushed", fixed = TRUE)
  expect_match(server, "delay = 1", fixed = TRUE)
  expect_no_match(server, "cerebro_async", fixed = TRUE)
  expect_match(group_filters, 'get0("outputOptions"', fixed = TRUE)
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
      "debounceAfterFirst",
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
    "spatial_projection_full_extent <- reactive({",
    fixed = TRUE
  )
  expect_match(
    spatial,
    "extent <- spatial_projection_full_extent()",
    fixed = TRUE
  )
})
