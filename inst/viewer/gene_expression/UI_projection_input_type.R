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

## Keep the dictionary server-side while handling the asynchronous binding of
## this renderUI-created selectize. Clicking still opens a browsable list;
## typing filters it without constructing every option node up front.
serverSideGeneSelector(
  session,
  "expression_genes_input",
  extra_triggers = function() input[["expression_analysis_mode"]],
  active = function() identical(input[["sidebar"]], "geneExpression")
)
