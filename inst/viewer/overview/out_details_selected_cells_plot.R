output[["overview_details_selected_cells_plot"]] <- plotly::renderPlotly({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections(),
    input[["overview_selected_cells_plot_select_variable"]]
  )
  cells_df <- cbind(
    getProjection(input[["overview_projection_to_display"]]),
    getMetaData()
  )
  cerebroSelectedCellsPlot(
    cells_df,
    overview_projection_selected_cells(),
    input[["overview_selected_cells_plot_select_variable"]],
    fallback_key = "row"
  )
})
