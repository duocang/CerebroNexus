output[["overview_selected_cells_table_UI"]] <- renderUI({
  req(overview_projection_selected_cells())
  cerebroSelectedCellsTableUI("overview")
})

cerebroRegisterInfo(
  input,
  "overview_details_selected_cells_table_info",
  cerebroSelectedCellsTableInfo()
)
