##----------------------------------------------------------------------------##
## Number and composition of the selected cells.
##----------------------------------------------------------------------------##
viewerSelectionSummaryOutputs(
  "expression_projection",
  "expression_number_of_selected_cells",
  function() expression_projection_selected_cells(),
  source = function() input[["expression_projection_to_display"]]
)
