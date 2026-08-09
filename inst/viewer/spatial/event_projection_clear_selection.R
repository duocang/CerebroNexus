##----------------------------------------------------------------------------##
## Show scroll-down indicator when cells are selected.
##----------------------------------------------------------------------------##
observeEvent(spatial_projection_selected_cells(), {
  if (
    !is.null(spatial_projection_selected_cells()) &&
      nrow(spatial_projection_selected_cells()) > 0
  ) {
    shinyjs::js$showScrollDownIndicator("Charts generated below ↓")
  }
})
