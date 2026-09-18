##----------------------------------------------------------------------------##
## Collect color parameters for projection plot.
##----------------------------------------------------------------------------##
expression_projection_parameters_color <- reactive({
  expression_levels <- expression_projection_expression_levels()
  genes <- expression_selected_genes()[["genes_to_display_present"]]
  no_gene_selected <- length(genes) == 0L
  req(no_gene_selected || length(expression_levels) > 0L)
  ## collect parameters
  parameters <- list(
    color_scale = "Cerebro orange",
    color_range = if (no_gene_selected) {
      c(0, 1)
    } else {
      expressionValueRange(expression_levels)
    },
    color_mode = if (
      identical(
        input[["expression_projection_gene_color_mode"]],
        "different"
      )
    ) {
      "different"
    } else {
      "shared"
    },
    genes = genes,
    rgb_genes = expression_selected_genes()[["rgb_genes"]]
  )
  return(parameters)
})
