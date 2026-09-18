##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  metadata <- getMetaData()
  parameters <- overview_projection_parameters_plot()
  columns <- unique(c(
    "cell_barcode",
    parameters[["color_variable"]],
    if (isTRUE(parameters[["hover_info"]])) c("nUMI", "nGene", getGroups())
  ))
  cells_df <- metadata[
    overview_projection_cells_to_show(),
    intersect(columns, colnames(metadata)),
    drop = FALSE
  ]
  return(cells_df)
})
