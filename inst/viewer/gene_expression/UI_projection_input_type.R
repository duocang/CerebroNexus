##----------------------------------------------------------------------------##
## UI elements to choose whether gene(s) or gene sets should be analyzed
##----------------------------------------------------------------------------##
output[["expression_projection_input_type_UI"]] <- renderUI({
  req(input[["expression_analysis_mode"]])
  if (input[["expression_analysis_mode"]] == "Gene(s)") {
    selectizeInput(
      'expression_genes_input',
      label = 'Gene(s)',
      choices = NULL,
      multiple = TRUE,
      width = "100%",
      options = list(
        create = TRUE,
        placeholder = "Search genes..."
      )
    )
  } else if (input[["expression_analysis_mode"]] == "Gene set") {
    selectizeInput(
      'expression_select_gene_set',
      label = 'Gene set',
      choices = data.table::as.data.table(
        data.frame("Gene sets" = c("-", msigdbr:::msigdbr_genesets$gs_name))
      ),
      multiple = FALSE,
      width = "100%"
    )
  }
})

## Keep large gene dictionaries out of the dynamic UI payload. Selectize asks
## the server for matching options as the user types instead of constructing
## thousands of browser-side option nodes when the page first opens.
observeEvent(
  list_of_genes(),
  {
    genes <- list_of_genes()
    session$onFlushed(
      function() {
        updateSelectizeInput(
          session,
          "expression_genes_input",
          choices = genes,
          server = TRUE
        )
      },
      once = TRUE
    )
  },
  ignoreInit = FALSE
)
