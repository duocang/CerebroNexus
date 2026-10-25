test_that("Gene expression leaves % of cells to the browser with a Viewer Pack", {
  for (pack in list(list(canonical_order = TRUE), NULL)) {
    shiny::testServer(
      function(input, output, session) {
        viewerPackCurrent <- function() pack
        expression_projection_selected_projection <- function() "umap"
        available_trajectories <- function() character()
        viewerProjectionFirstFrameMetadata <- function() data.frame(x = 1:10)
        current_scatter_percentage <- reactiveVal(100)
        viewerProjectionCellIndices <- function(prefix, metadata, percentage) {
          seq_len(ceiling(nrow(metadata) * percentage / 100))
        }
        sys.source(
          viewer_test_path("gene_expression", "obj_projection_cells_to_show.R"),
          envir = environment()
        )
        runs <- 0L
        observe({
          expression_projection_cells_to_show()
          runs <<- runs + 1L
        })
        session$userData$state <- function() {
          list(
            runs = runs,
            cells = isolate(expression_projection_cells_to_show())
          )
        }
        session$userData$set <- current_scatter_percentage
      },
      {
        session$flushReact()
        session$userData$set(30)
        session$flushReact()
        state <- session$userData$state()
        if (is.null(pack)) {
          expect_identical(state$runs, 2L)
          expect_identical(state$cells, 1:3)
        } else {
          ## The slider edit neither resamples nor rebuilds the frame.
          expect_identical(state$runs, 1L)
          expect_identical(state$cells, 1:10)
        }
      }
    )
  }
})

test_that("a full Gene expression frame tells the browser to apply % of cells", {
  runtime <- new.env(parent = globalenv())
  runtime$`%||%` <- function(x, y) if (is.null(x)) y else x
  runtime$expressionColorScale <- function(...) "scale"
  runtime$expressionReverseColorScale <- function(...) FALSE
  runtime$current_scatter_percentage <- function() 40
  runtime$cerebroCellViewRender <- function(id, meta, data, ...) {
    runtime$captured <- data
  }
  runtime$cerebroCellViewRecolor <- runtime$cerebroCellViewRender
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = runtime
  )
  frame <- function(client) {
    runtime$captured <- NULL
    runtime$expression_projection_update_plot(list(
      coordinates = data.frame(x = 1:3, y = 3:1),
      reset_axes = FALSE,
      expression_levels = c(0, 1, 2),
      plot_parameters = list(
        draw_border = FALSE,
        keep_square = TRUE,
        plot_order = "Natural order",
        point_size = 1,
        point_opacity = 1,
        x_range = c(1, 3),
        y_range = c(1, 3),
        is_trajectory = FALSE,
        hover_info = FALSE,
        projection = "umap",
        n_dimensions = 2L
      ),
      color_settings = list(
        color_scale = "Viridis",
        color_range = c(0, 2),
        color_mode = "same",
        genes = "GeneA"
      ),
      metadata = NULL,
      trajectory = list(),
      display_mode = "single",
      cell_indices = 1:3,
      client_percentage = client,
      projection_resource_failed = NULL,
      separate_panels = FALSE
    ))
    runtime$captured
  }
  client <- frame(TRUE)
  expect_true(client$client_percentage)
  expect_identical(client$percentage_cells_to_show, 40)
  server <- frame(FALSE)
  expect_null(server$client_percentage)
  expect_null(server$percentage_cells_to_show)
})

test_that("the drawn Gene expression frame acknowledges the % slider's value", {
  server <- function(input, output, session) {
    context <- list(epoch = "test", dataset_key = "A", generation = 1L)
    viewer_loaded_dataset_context <- function() context
    viewerDatasetContextEqual <- function(a, b) identical(a, b)
    data_set <- function() "dataset"
    expression_projection_parameters_other <- reactiveValues(reset_axes = TRUE)
    frame <- list(dataset = "dataset", dataset_context = context, cells = 1:3)
    expression_projection_data_to_plot <- reactive(frame)
    expression_projection_data_to_plot_raw <- expression_projection_data_to_plot
    expressionProjectionProgressUpdate <- function(...) invisible(NULL)
    expressionProjectionProgressClose <- function(...) invisible(NULL)
    expression_projection_plotting_order <- function() FALSE
    expressionPlotOrderLabel <- function(highest_on_top) "Input order"
    cerebroFeedbackControls <- function(id, input) {
      isolate(list(
        expression_projection_percentage_cells_to_show = as.character(
          input[["expression_projection_percentage_cells_to_show"]]
        )
      ))
    }
    .cerebro_cell_view_render_serial <- 0L
    renders <- 0L
    expression_projection_update_plot <- function(data) {
      renders <<- renders + 1L
      .cerebro_cell_view_render_serial <<- .cerebro_cell_view_render_serial + 1L
    }
    acks <- list()
    cerebroSendViewFeedback <- function(id, token, context, controls) {
      acks[[length(acks) + 1L]] <<- controls
    }
    sys.source(
      viewer_test_path("gene_expression", "event_projection_update_plot.R"),
      envir = environment()
    )
    session$userData$state <- function() list(renders = renders, acks = acks)
  }
  shiny::testServer(server, {
    session$setInputs(
      expression_projection_percentage_cells_to_show = 100,
      expression_projection_render_request = list(
        nonce = 1,
        dataset_context = list(
          epoch = "test",
          dataset_key = "A",
          generation = 1L
        )
      )
    )
    expect_identical(session$userData$state()$renders, 1L)
    ## The browser applied the new share; its slider value arrives later.
    session$setInputs(expression_projection_percentage_cells_to_show = 50)
    state <- session$userData$state()
    expect_identical(state$renders, 1L)
    expect_identical(
      tail(state$acks, 1)[[1]]$expression_projection_percentage_cells_to_show,
      "50"
    )
  })
})
