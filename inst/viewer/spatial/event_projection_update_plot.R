##----------------------------------------------------------------------------##
## Update every visible Spatial panel from the shared payload batch.
##----------------------------------------------------------------------------##

observeEvent(spatial_projection_panel_payloads(), {
  payloads <- spatial_projection_panel_payloads()
  req(length(payloads) > 0L)

  withProgress(message = 'Updating spatial plots...', value = 0.5, {
    lapply(payloads, function(payload) {
      message(
        "[spatial] plot update triggered, panel = ",
        payload$panel$name,
        ", background_image = ",
        payload$data[['plot_parameters']][['background_image']]
      )
      spatial_projection_update_plot(
        payload$data,
        payload$panel$plot_id
      )
    })
  })
})
