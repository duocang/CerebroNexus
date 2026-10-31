spatial_selection_present <- viewerSelectionPresence("spatial_projection")

output[["spatial_selected_cells_plot_UI"]] <- renderUI({
  req(spatial_selection_present())
  cerebroSelectedCellsPlotUI("spatial", getVariableToCompareChoices())
})

cerebroRegisterInfo(
  input,
  "spatial_details_selected_cells_plot_info",
  cerebroSelectedCellsPlotInfo()
)

output[["spatial_selected_cells_table_UI"]] <- renderUI({
  req(spatial_selection_present())
  cerebroSelectedCellsTableUI("spatial")
})

cerebroRegisterInfo(
  input,
  "spatial_details_selected_cells_table_info",
  cerebroSelectedCellsTableInfo()
)
