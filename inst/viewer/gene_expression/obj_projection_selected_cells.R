##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
expression_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(
    expression_projection_parameters_plot(),
    expression_projection_data()
  )
  ## The selection is held persistently on the JS side (shared
  ## cell_views.js) and pushed here as {x, y, ids} under
  ## <plot_id>_persistent_selection, so it survives plot-parameter changes
  ## (colour scale / range / point size). Plotly's volatile plotly_selected
  ## event is NOT used, because a re-render would wipe it. The identifier is
  ## built the same way the table keys cells (paste0 with '-'), so downstream
  ## filtering is unchanged.
  selection <- viewerPersistentSelection(
    input[["expression_projection_persistent_selection"]],
    viewer_loaded_dataset_context()
  )
  if (is.null(selection)) {
    return(NULL)
  }

  selection
})

##----------------------------------------------------------------------------##
## Rows of the frame (expression_projection_cells_to_show()) that the selection
## holds, in frame order. Identity strings are built only where a selection
## needs them, not for every cell on each selection change.
##----------------------------------------------------------------------------##
expression_projection_selected_rows <- reactive({
  selected_cells <- expression_projection_selected_cells()
  req(selected_cells)
  cells <- expression_projection_cells_to_show()
  selected <- viewerSelectionRows(selected_cells)
  if (!is.null(selected)) {
    return(which(viewerRowMask(selected, cells)))
  }
  stable <- if ("selection_key" %in% colnames(selected_cells)) {
    as.character(selected_cells[["selection_key"]])
  } else {
    character()
  }
  stable <- stable[!is.na(stable) & nzchar(stable)]
  if (length(stable)) {
    ## Stable selection is published only after deferred identity is
    ## available. First-frame metadata can contain compact row numbers, not
    ## real barcodes.
    barcodes <- getMetaData()[["cell_barcode"]]
    return(which(cells %in% which(barcodes %in% stable)))
  }
  if (!"identifier" %in% colnames(selected_cells)) {
    return(integer())
  }
  coordinates <- expression_projection_coordinates()
  which(
    paste0(coordinates[[1]], "-", coordinates[[2]]) %in%
      as.character(selected_cells[["identifier"]])
  )
})
