test_that("Gene expression on a trajectory pairs each point with its own cell", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      metadata <- data.frame(
        cell_barcode = c("a", "b", "c", "d", "e"),
        group = c("x", "x", "y", "y", "x")
      )
      ## The trajectory leaves out cells a and c.
      meta <- data.frame(
        DR_1 = c(20, 40, 50),
        DR_2 = c(-20, -40, -50),
        state = c("s1", "s2", "s2"),
        row.names = c("b", "d", "e")
      )
      getMetaData <- function() metadata
      viewerProjectionFirstFrameMetadata <- function() metadata
      viewerPackCurrent <- function() NULL
      viewerPackTrajectoryIndex <- function(...) NULL
      current_scatter_percentage <- function() 100
      available_trajectories <- function() "m // t"
      availableProjections <- function() "umap"
      getMethodsForTrajectories <- function() "m"
      getNamesOfTrajectories <- function(method) "t"
      getTrajectory <- function(method, name) list(meta = meta, edges = NULL)
      expression_projection_selected_projection <- function() "m // t"
      expression_projection_parameters_plot <- function() {
        list(projection = "m // t")
      }
      ## Group filter "x" keeps cells a, b and e.
      viewerProjectionCellIndices <- function(prefix, metadata, percentage) {
        keep <- input$group %||% c("x", "y")
        which(metadata$group %in% keep)
      }
      for (file in c(
        "obj_projection_cells_to_show.R",
        "obj_projection_coordinates.R",
        "obj_projection_data.R"
      )) {
        sys.source(viewer_test_path("gene_expression", file), envir = scope)
      }
    },
    {
      session$setInputs(group = c("x", "y"))
      expect_identical(expression_projection_cells_to_show(), c(2L, 4L, 5L))
      expect_identical(expression_projection_coordinates()$DR_1, c(20, 40, 50))
      session$setInputs(group = "x")
      expect_identical(expression_projection_cells_to_show(), c(2L, 5L))
      expect_identical(expression_projection_coordinates()$DR_1, c(20, 50))
      expect_identical(
        expression_projection_trajectory()$meta$state,
        c("s1", "s2")
      )
    }
  )
})
