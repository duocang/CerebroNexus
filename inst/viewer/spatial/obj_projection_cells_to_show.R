##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
spatial_projection_cells_to_show <- reactive({
  req(input[["spatial_projection_percentage_cells_to_show"]])

  groups <- getGroups()

  # Collect all filters first
  group_filters <- list()
  for (i in groups) {
    filter_val <- input[[paste0("spatial_projection_group_filter_", i)]]
    group_filters[[i]] <- if (is.null(filter_val)) character() else filter_val
  }

  pct_cells <- input[["spatial_projection_percentage_cells_to_show"]]
  meta_data <- getMetaData()
  req(!is.null(meta_data))

  keep <- cerebroGroupFilterMask(meta_data, group_filters)

  indices <- which(keep)
  if (!length(indices)) {
    return(integer())
  }
  if (pct_cells < 100) {
    n <- ceiling(length(indices) * pct_cells / 100)
    indices <- indices[sample.int(length(indices), n)]
  }
  return(indices)
})
