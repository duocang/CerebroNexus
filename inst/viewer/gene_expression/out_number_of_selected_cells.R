##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["expression_number_of_selected_cells"]] <- renderUI({
  selected <- expression_projection_selected_cells()
  if (is.null(selected) || nrow(selected) == 0) {
    return(NULL)
  }
  selectionCountBadge(nrow(selected))
})
