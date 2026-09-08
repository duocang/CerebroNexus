##----------------------------------------------------------------------------##
## Hover info for cells in projection.
##----------------------------------------------------------------------------##
expression_projection_hover_info <- reactive({
  cells_to_show <- expression_projection_cells_to_show()
  req(cells_to_show)
  hover_info_projections(cells_to_show)
})
