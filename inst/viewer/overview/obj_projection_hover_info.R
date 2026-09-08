##----------------------------------------------------------------------------##
## Hover info of cells in projection.
##----------------------------------------------------------------------------##
overview_projection_hover_info <- reactive({
  cells_to_show <- overview_projection_cells_to_show()
  hover_info_projections(cells_to_show)
})
