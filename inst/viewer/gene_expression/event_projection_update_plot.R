##----------------------------------------------------------------------------##
## Update projection plot when expression_projection_data_to_plot() changes.
## Runs before the page's controls are built (priority 0), so the first frame
## is not queued behind them.
##----------------------------------------------------------------------------##
expression_projection_last_drawn <- NULL
expression_projection_last_feedback <- NULL
expression_projection_last_token <- NULL
observe({
  render_request <- input[["expression_projection_render_request"]]
  req(is.list(render_request), is.list(render_request$dataset_context))
  current_context <- viewer_loaded_dataset_context()
  req(viewerDatasetContextEqual(
    render_request$dataset_context,
    current_context
  ))
  ## The debounced value is not there yet for the first frame (or is from the
  ## previous dataset); take the current one rather than wait for the timer.
  data <- tryCatch(
    expression_projection_data_to_plot(),
    shiny.silent.error = function(error) NULL
  )
  if (!viewerDatasetContextEqual(data$dataset_context, current_context)) {
    data <- expression_projection_data_to_plot_raw()
  }
  req(
    data,
    identical(data$dataset, data_set()),
    viewerDatasetContextEqual(data$dataset_context, current_context)
  )
  ## "% of cells" reaches R first as the shared scatter edit (which may or may
  ## not draw a frame) and then as the slider's own debounced value. Re-run on
  ## the latter, so the drawn frame acknowledges it.
  ## The group filters the browser applies draw no frame either.
  input[["expression_projection_percentage_cells_to_show"]]
  expression_projection_group_filters()
  feedback <- cerebroFeedbackControls("expression_projection", input)
  ## The debounce timer later delivers the value drawn here. Controls that
  ## change without changing the frame (the drawer's controls binding, or an
  ## edit that draws the same cells) are acknowledged against the painted
  ## frame instead of resending it, once the current value agrees.
  if (identical(data, expression_projection_last_drawn)) {
    current <- tryCatch(
      expression_projection_data_to_plot_raw(),
      shiny.silent.error = function(error) NULL
    )
    if (
      !identical(feedback, expression_projection_last_feedback) &&
        !is.null(expression_projection_last_token) &&
        identical(current, data)
    ) {
      cerebroSendViewFeedback(
        "expression_projection",
        expression_projection_last_token,
        current_context,
        feedback
      )
      expression_projection_last_feedback <<- feedback
    }
    return()
  }
  expression_projection_last_drawn <<- data
  data$feedback <- feedback
  ## The order the browser was last told (a patch since the parameters).
  data$plot_parameters$plot_order <- expressionPlotOrderLabel(
    isolate(expression_projection_plotting_order())
  )
  expression_projection_parameters_other[['reset_axes']] <- FALSE
  ## Payload construction consults browser-published transport/cache state.
  ## Those reads are snapshots for this render, not plot dependencies: the
  ## browser republishes shared geometry after every paint, and subscribing to
  ## it here creates an endless render/publish feedback loop.
  expressionProjectionProgressUpdate(0.72, "Transferring expression colours...")
  token_before <- .cerebro_cell_view_render_serial
  tryCatch(
    isolate(expression_projection_update_plot(data)),
    error = function(error) {
      expressionProjectionProgressClose()
      stop(error)
    }
  )
  if (.cerebro_cell_view_render_serial > token_before) {
    expression_projection_last_token <<- .cerebro_cell_view_render_serial
    expression_projection_last_feedback <<- feedback
  }
  expressionProjectionProgressUpdate(0.9, "Drawing expression colours...")
}, priority = 100)

## "Highest expression on top" reorders the drawn points in the browser: the
## frame keeps its cells and colours.
observeEvent(
  expression_projection_plotting_order(),
  {
    req(!is.null(expression_projection_last_token))
    session$sendCustomMessage(
      "cell_view_appearance",
      list(
        id = "expression_projection",
        dataset_context = viewer_loaded_dataset_context(),
        dataset_fingerprint = viewerDatasetIdentity()$fingerprint,
        values = list(
          paint_order = if (isTRUE(expression_projection_plotting_order())) {
            "highest"
          } else {
            "natural"
          }
        )
      )
    )
  },
  ignoreInit = TRUE
)
