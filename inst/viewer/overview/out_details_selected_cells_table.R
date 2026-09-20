output[["overview_details_selected_cells_table"]] <- DT::renderDataTable({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections()
  )
  cerebroSelectedCellsTable(
    cbind(
      getProjection(input[["overview_projection_to_display"]]),
      getMetaData()
    ),
    viewerProjectionFirstFrameMetadata(),
    overview_projection_selected_cells(),
    fallback_key = "row",
    number_formatting = input[[
      "overview_details_selected_cells_table_number_formatting"
    ]],
    color_highlighting = input[[
      "overview_details_selected_cells_table_color_highlighting"
    ]],
    download_file_name = "overview_details_of_selected_cells"
  )
})
