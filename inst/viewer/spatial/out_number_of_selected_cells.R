##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["spatial_number_of_selected_cells"]] <- renderUI({
  selection <- spatial_projection_selected_cells()
  req(!is.null(selection), nrow(selection) > 0L)
  cerebroSelectionSummary(
    selection,
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_point_color"]]
  )
})

output[["spatial_projection_composition"]] <- renderUI({
  selection <- spatial_projection_selected_cells()
  req(!is.null(selection), nrow(selection) > 0L)
  cerebroSelectionSummary(
    selection,
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_point_color"]],
    composition = TRUE
  )
})
