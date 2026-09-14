##----------------------------------------------------------------------------##
## Tab: Most expressed genes
##
## Select group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_select_group_UI"]] <- renderUI({
  if (
    !is.null(getGroupsWithMostExpressedGenes()) &&
      length(getGroupsWithMostExpressedGenes()) > 0
  ) {
    selectInput(
      "most_expressed_genes_selected_group",
      label = "Grouping variable",
      choices = getGroupsWithMostExpressedGenes(),
      width = "100%"
    )
  }
})
