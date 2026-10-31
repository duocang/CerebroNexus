##----------------------------------------------------------------------------##
## UI elements to set additional plotting parameters, and their info box (see
## overview/scatter_parameters.R).
##----------------------------------------------------------------------------##
viewerScatterParametersUI(
  "expression_projection",
  info_title = "Additional parameters for gene (set) expression",
  data_control = function() {
    checkboxInput(
      "expression_projection_plotting_order",
      label = "Highest expression on top",
      value = expression_projection_default_plotting_order()
    )
  },
  info_item = "
      <li><b>Highest expression on top:</b> Off preserves the input cell order, like Seurat FeaturePlot(order = FALSE). On draws cells from low to high expression.</li>"
)
