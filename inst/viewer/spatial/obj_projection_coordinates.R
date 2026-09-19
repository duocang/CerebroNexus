##----------------------------------------------------------------------------##
## Coordinates of cells in projection.
##----------------------------------------------------------------------------##
spatial_projection_coordinates <- reactive({
  req(
    spatial_projection_parameters_plot(),
    spatial_projection_cells_to_show()
  )

  parameters <- spatial_projection_parameters_plot()
  cells_to_show <- spatial_projection_cells_to_show()
  req(parameters[["projection"]] %in% availableSpatial())

  spatial_data <- getSpatialData(parameters[["projection"]])
  canonical_index <- spatial_projection_cell_index()
  coordinates <- spatial_data$coordinates
  if (!spatial_coordinates_are_canonical_full_order(
    cells_to_show,
    canonical_index,
    nrow(coordinates)
  )) {
    coordinate_rows <- match(cells_to_show, canonical_index)
    req(!anyNA(coordinate_rows))
    coordinates <- coordinates[coordinate_rows, , drop = FALSE]
  }

  return(coordinates)
})
