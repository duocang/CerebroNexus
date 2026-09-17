##----------------------------------------------------------------------------##
## Update projection plot when overview_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
overview_projection_last_data <- NULL
observe({
  req(input[["overview_projection_render_request"]])
  data <- overview_projection_data_to_plot()
  req(data)
  if (identical(data, overview_projection_last_data)) {
    return()
  }
  overview_projection_last_data <<- data
  overview_projection_update_plot(data)
})
