test_that("selected table does not require lazy appearance controls", {
  expr <- viewer_r_assignment(
    viewer_test_path("trajectory", "selected_cells_table.R"),
    "output.trajectory_details_selected_cells_table"
  )
  # Execute the actual output expression until data access, with no appearance
  # input initialized. A silent req failure would stop before this sentinel.
  scope <- new.env(parent = globalenv())
  scope$req <- shiny::req
  scope$output <- list()
  local_mocked_bindings(
    renderDataTable = function(expr, ...) {
      eval(substitute(expr), parent.frame())
    },
    .package = "DT"
  )
  scope$input <- list()
  scope$trajectory_selection_ok <- function() TRUE
  scope$trajectory_projection_selected_cells <- function() {
    stop("selection reached")
  }
  expect_error(eval(expr, scope), "selection reached")
  scope$trajectory_selection_ok <- function() FALSE
  expect_error(eval(expr, scope), class = "shiny.silent.error")
})
