##----------------------------------------------------------------------------##
## Update projection plot when expression_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
observe({
  req(input[["expression_projection_render_request"]])
  data <- expression_projection_data_to_plot()
  req(data)
  expression_projection_parameters_other[['reset_axes']] <- FALSE
  ## Payload construction consults browser-published transport/cache state.
  ## Those reads are snapshots for this render, not plot dependencies: the
  ## browser republishes shared geometry after every paint, and subscribing to
  ## it here creates an endless render/publish feedback loop.
  isolate(expression_projection_update_plot(data))
})
