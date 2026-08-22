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
    tagList(
      div(
        class = "cerebro-compact-select",
        tags$h3("Choose a grouping variable:"),
        selectInput(
          "most_expressed_genes_selected_group",
          label = NULL,
          choices = getGroupsWithMostExpressedGenes(),
          width = "100%"
        )
      )
    )
  }
})
