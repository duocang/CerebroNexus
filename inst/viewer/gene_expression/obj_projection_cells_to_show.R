##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
expression_projection_cells_to_show <- reactive({
  req(input[["expression_projection_percentage_cells_to_show"]])
  groups <- getGroups()
  pct_cells <- input[["expression_projection_percentage_cells_to_show"]]
  group_filters <- list()
  ## store group filters
  for (i in groups) {
    filter_value <- input[[paste0(
      "expression_projection_group_filter_",
      i
    )]]
    group_filters[[i]] <- if (is.null(filter_value)) {
      character()
    } else {
      filter_value
    }
  }
  indices <- which(cerebroGroupFilterMask(getMetaData(), group_filters))
  if (!length(indices)) {
    return(integer())
  }
  if (pct_cells < 100) {
    n <- ceiling(length(indices) * pct_cells / 100)
    indices <- indices[sample.int(length(indices), n)]
  }
  return(indices)
})
