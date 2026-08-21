##----------------------------------------------------------------------------##
## Only surface actionable input problems; selected genes are already visible
## in the selector and repeating them below the plot adds noise.
##----------------------------------------------------------------------------##
output[["expression_genes_displayed"]] <- renderUI({
  req(expression_selected_genes())
  missing <- expression_selected_genes()[["genes_to_display_missing"]]
  if (length(missing) == 0) {
    return(NULL)
  }
  tags$div(
    class = "cerebro-expression-notice",
    icon("triangle-exclamation"),
    tags$span(
      tags$strong("Not found in this dataset: "),
      paste(missing, collapse = ", ")
    )
  )
})
