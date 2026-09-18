##----------------------------------------------------------------------------##
## Collect color parameters for projection plot.
##----------------------------------------------------------------------------##
expression_projection_color_mode <- reactiveVal("shared")

observe({
  color_mode <- if (
    identical(input[["expression_projection_gene_color_mode"]], "different")
  ) {
    "different"
  } else {
    "shared"
  }
  if (!identical(color_mode, isolate(expression_projection_color_mode()))) {
    expression_projection_color_mode(color_mode)
  }
})

expression_projection_parameters_color <- reactive({
  ## require input UI elements
  req(expression_projection_expression_levels())
  request <- expression_projection_request()
  ## collect parameters
  parameters <- list(
    color_scale = "Cerebro orange",
    color_range = expressionValueRange(
      expression_projection_expression_levels()
    ),
    color_mode = expression_projection_color_mode(),
    genes = request$genes_present,
    rgb_genes = request$genes_data[["rgb_genes"]]
  )
  return(parameters)
})
