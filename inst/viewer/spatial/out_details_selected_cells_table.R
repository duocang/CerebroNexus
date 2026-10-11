output[["spatial_details_selected_cells_table"]] <- DT::renderDataTable({
  req(
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    spatial_projection_data_to_plot()
  )
  meta_data <- getMetaData()
  req(!is.null(meta_data))
  cerebroSelectedCellsTable(
    spatial_projection_interaction_data(spatial_projection_data_to_plot()),
    meta_data,
    spatial_projection_selected_cells(),
    fallback_key = "identifier",
    number_formatting = input[[
      "spatial_details_selected_cells_table_number_formatting"
    ]],
    color_highlighting = input[[
      "spatial_details_selected_cells_table_color_highlighting"
    ]],
    download_file_name = "spatial_details_of_selected_cells"
  )
})
