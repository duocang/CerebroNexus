overview_selection_present <- viewerSelectionPresence("overview_projection")

output[["overview_selected_cells_plot_UI"]] <- renderUI({
  req(overview_selection_present())
  choices <- setdiff(
    colnames(viewerProjectionFirstFrameMetadata()),
    "cell_barcode"
  )
  cerebroSelectedCellsPlotUI("overview", choices)
})

cerebroRegisterInfo(
  input,
  "overview_details_selected_cells_plot_info",
  cerebroSelectedCellsPlotInfo()
)

output[["overview_selected_cells_table_UI"]] <- renderUI({
  req(overview_selection_present())
  cerebroSelectedCellsTableUI("overview")
})

cerebroRegisterInfo(
  input,
  "overview_details_selected_cells_table_info",
  cerebroSelectedCellsTableInfo()
)
