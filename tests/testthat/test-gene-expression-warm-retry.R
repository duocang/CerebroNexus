## While a thin CRB's cell names are checked in a daemon, the Gene expression
## backend warm-up looks again every 0.25 s with later(). A tab closed in that
## time must not be touched by the timer: reading its reactives raised outside
## any session and stopped the whole app, disconnecting every user.
warm_retry_server <- function(input, output, session) {
  scope <- environment()
  viewer_runtime_env(scope)
  expressionProjectionPaintedCurrent <- function() TRUE
  expression_selected_genes <- function() {
    list(genes_to_display_present = character())
  }
  data_set <- function() {
    structure(list(), cerebro_h5_cells_pending = function() TRUE)
  }
  viewer_server_load(
    viewer_server_statements(
      "gene_expression",
      "obj_projection_expression_levels.R"
    ),
    scope,
    names = c(
      "expression_projection_backend_warmed",
      "expression_projection_warm_retry"
    ),
    events = "expression_projection_warm_retry"
  )
}

test_that("the warm-up retry does nothing once its session has closed", {
  skip_if_not_installed("later")
  shiny::testServer(warm_retry_server, {
    session$flushReact() # the first run is ignoreInit
    session$setInputs(expression_projection_rendered_state = "painted")
    session$close()
    Sys.sleep(0.3)
    expect_no_error(later::run_now(1))
  })
})

test_that("an open session keeps looking while the cell names are checked", {
  skip_if_not_installed("later")
  shiny::testServer(warm_retry_server, {
    session$flushReact() # the first run is ignoreInit
    session$setInputs(expression_projection_rendered_state = "painted")
    expect_identical(expression_projection_warm_retry(), 0L)
    Sys.sleep(0.3)
    later::run_now(1)
    expect_identical(expression_projection_warm_retry(), 1L)
    expect_false(expression_projection_backend_warmed())
  })
})
