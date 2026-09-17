##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["overview_number_of_selected_cells"]] <- renderUI({
  selection <- overview_projection_selected_cells()
  cerebroSelectionSummary(
    selection,
    input[["overview_projection_to_display"]],
    input[["overview_projection_point_color"]],
    metadata = if (is.null(selection)) {
      viewerProjectionFirstFrameMetadata()
    } else {
      getMetaData()
    }
  )
})

output[["overview_projection_composition"]] <- renderUI({
  selection <- overview_projection_selected_cells()
  cerebroSelectionSummary(
    selection,
    input[["overview_projection_to_display"]],
    input[["overview_projection_point_color"]],
    metadata = if (is.null(selection)) {
      viewerProjectionFirstFrameMetadata()
    } else {
      getMetaData()
    },
    composition = TRUE
  )
})
