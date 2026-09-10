## Compact feedback for custom gene names that are absent from the data set.
output[["expression_genes_displayed"]] <- renderUI({
  req(expression_selected_genes())
  missing <- expression_selected_genes()[["genes_to_display_missing"]]
  if (!length(missing)) {
    return(NULL)
  }
  div(
    class = "cerebro-gene-validation",
    icon("triangle-exclamation"),
    tags$span("Not found in this data set:"),
    tags$strong(paste(missing, collapse = ", "))
  )
})
