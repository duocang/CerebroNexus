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
      getGroups <- function() "group"
      getGroupLevels <- function(group) c("x", "y")
      viewer_loaded_dataset_context <- function() "context"
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
      viewerProjectionCellIndices <- function(
        prefix,
        metadata,
        percentage,
        filters
      ) {
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

test_that("a whole trajectory takes its geometry from the Viewer Pack", {
  runtime <- new.env(parent = globalenv())
  runtime$`%||%` <- function(x, y) if (is.null(x)) y else x
  runtime$expressionColorScale <- function(...) "scale"
  runtime$expressionReverseColorScale <- function(...) FALSE
  runtime$cerebroCellViewRender <- function(id, meta, data, ...) {
    runtime$captured <- data
  }
  runtime$cerebroCellViewRecolor <- runtime$cerebroCellViewRender
  runtime$viewerTrajectoryFrameAsset <- function(method, name) {
    list(
      cells = 3L,
      geometry_kind = "canonical_projection",
      projection = list(url = "projection/001.bin", cells = 5L),
      subset = list(kind = "exclude_uint32", url = "trajectory/001.subset.bin")
    )
  }
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = runtime
  )
  frame <- function(cells) {
    runtime$expression_projection_render_state$geometry <- NULL
    runtime$expression_projection_update_plot(list(
      coordinates = data.frame(
        DR_1 = c(20, 40, 50)[seq_along(cells)],
        DR_2 = 0
      ),
      reset_axes = FALSE,
      expression_levels = c(0, 1, 2)[seq_along(cells)],
      plot_parameters = list(
        draw_border = FALSE,
        keep_square = TRUE,
        plot_order = "Input order",
        point_size = 1,
        point_opacity = 1,
        x_range = c(0, 50),
        y_range = c(0, 1),
        is_trajectory = TRUE,
        hover_info = FALSE,
        projection = "m // t",
        n_dimensions = 2L
      ),
      color_settings = list(
        color_scale = "Viridis",
        color_range = c(0, 2),
        color_mode = "same",
        genes = "GeneA"
      ),
      metadata = NULL,
      trajectory = list(edges = data.frame()),
      display_mode = "single",
      cell_indices = cells,
      trajectory_rows = c(2L, 4L, 5L),
      projection_resource_failed = NULL,
      separate_panels = FALSE
    ))
    runtime$captured
  }
  whole <- frame(c(2L, 4L, 5L))
  expect_null(whole[["x"]])
  expect_identical(whole$projection_resource$url, "projection/001.bin")
  expect_identical(
    whole$projection_subset_resource$url,
    "trajectory/001.subset.bin"
  )
  filtered <- frame(c(2L, 5L))
  expect_identical(filtered[["x"]], c(20, 40))
  expect_null(filtered$projection_resource)
})
