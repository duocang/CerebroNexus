output[["spatial_details_selected_cells_plot"]] <- plotly::renderPlotly({
  req(
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    input[["spatial_selected_cells_plot_select_variable"]],
    spatial_projection_data_to_plot()
  )
  cells_df <- spatial_projection_interaction_data(
    spatial_projection_data_to_plot()
  )
  cerebroSelectedCellsPlot(
    cells_df,
    spatial_projection_selected_cells(),
    input[["spatial_selected_cells_plot_select_variable"]],
    fallback_key = "identifier"
  )
})
