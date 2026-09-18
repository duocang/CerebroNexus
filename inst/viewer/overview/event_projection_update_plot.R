##----------------------------------------------------------------------------##
## Update projection plot when overview_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
overview_projection_last_render <- NULL
observe({
  req(identical(input[["sidebar"]], "overview"))
  req(input[["overview_projection_render_request"]])
  render_request <- input[["overview_projection_render_request"]]
  data <- overview_projection_data_to_plot()
  req(data)
  render <- list(request = render_request, data = data)
  if (identical(render, overview_projection_last_render)) {
    return()
  }
  overview_projection_last_render <<- render
  overview_projection_update_plot(data)
})
