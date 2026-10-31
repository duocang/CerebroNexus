##----------------------------------------------------------------------------##
## Number and composition of the selected cells.
##----------------------------------------------------------------------------##
viewerSelectionSummaryOutputs(
  "overview_projection",
  "overview_number_of_selected_cells",
  function() overview_projection_selected_cells(),
  source = function() input[["overview_projection_to_display"]],
  color = function() input[["overview_projection_point_color"]]
)
