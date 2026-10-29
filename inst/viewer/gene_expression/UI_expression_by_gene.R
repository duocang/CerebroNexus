##----------------------------------------------------------------------------##
## Expression by gene.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for plot.
##----------------------------------------------------------------------------##
output[["expression_by_gene_UI"]] <- renderUI({
  req(expression_summary_show_genes())
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by gene"),
        cerebroInfoButton("expression_by_gene_info")
      ),
      cerebroChartOutput("expression_by_gene")
    )
  )
})

##----------------------------------------------------------------------------##
## Bar plot.
##----------------------------------------------------------------------------##
output[["expression_by_gene"]] <- renderCerebroChart({
  if (!isTRUE(isolate(input[["expression_summary_viewport_request"]]))) {
    req(input[["expression_summary_viewport_request"]], cancelOutput = TRUE)
  }
  req(
    expression_projection_summary_ready(),
    expression_projection_parameters_color(),
    expression_summary_data()
  )
  expression_levels <- expressionProjectionGeneMeans(
    data_set(),
    expression_summary_data()$genes
  ) %>%
    dplyr::slice_max(expression, n = 50)
  color_settings <- expression_projection_parameters_color()
  color_scale <- expressionColorScale(color_settings[["color_scale"]])
  if (!is.list(color_scale)) {
    color_scale <- expressionColorScale("viridis")
  }
  ## one bar per gene, coloured by its mean on the expression colour scale
  cerebroCountBarData(
    expression_levels$gene,
    expression_levels$expression,
    cerebroScaleColors(
      expression_levels$expression,
      color_scale,
      reverse = expressionReverseColorScale(color_settings[["color_scale"]])
    ),
    y_title = "Expression level",
    format = "number",
    outline = "rgb(196,196,196)"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(input, "expression_by_gene_info", expression_by_gene_info)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_by_gene_info <- list(
  title = "Expression levels by gene",
  text = p(
    "Mean log-normalised expression across all cells for up to 50 selected genes. This comparison is shown only when at least two genes are selected."
  )
)
