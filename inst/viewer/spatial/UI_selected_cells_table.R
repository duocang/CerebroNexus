output[["spatial_selected_cells_table_UI"]] <- renderUI({
  req(spatial_projection_selected_cells())
  cerebroSelectedCellsTableUI("spatial")
})

cerebroRegisterInfo(
  input,
  "spatial_details_selected_cells_table_info",
  cerebroSelectedCellsTableInfo()
)
