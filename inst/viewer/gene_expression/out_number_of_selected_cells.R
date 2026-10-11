##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["expression_number_of_selected_cells"]] <- renderUI({
  selection <- expression_projection_selected_cells()
  cerebroSelectionSummary(
    selection,
    input[["expression_projection_to_display"]],
    # An empty selection needs only the validated row count, not cell IDs.
    metadata = if (is.null(selection)) {
      viewerProjectionFirstFrameMetadata()
    } else {
      getMetaData()
    }
  )
})

output[["expression_projection_composition"]] <- renderUI({
  cerebroSelectionSummary(
    expression_projection_selected_cells(),
    input[["expression_projection_to_display"]],
    composition = TRUE
  )
})
