##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##
## Select method and table (group).
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of method and group, which are split
## because the group depends on which method is selected.
##----------------------------------------------------------------------------##
output[["enriched_pathways_select_method_and_table_UI"]] <- renderUI({
  if (
    !is.null(getMethodsForEnrichedPathways()) &&
      length(getMethodsForEnrichedPathways()) > 0
  ) {
    tagList(
      fluidRow(
        column(
          6,
          uiOutput("enriched_pathways_selected_method_UI")
        ),
        column(
          6,
          uiOutput("enriched_pathways_selected_table_UI")
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## UI element to select from which method the results should be shown.
##----------------------------------------------------------------------------##
output[["enriched_pathways_selected_method_UI"]] <- renderUI({
  selectInput(
    "enriched_pathways_selected_method",
    label = "Method",
    choices = getMethodsForEnrichedPathways(),
    width = "100%"
  )
})

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["enriched_pathways_selected_table_UI"]] <- renderUI({
  req(input[["enriched_pathways_selected_method"]])
  selectInput(
    "enriched_pathways_selected_table",
    label = "Grouping variable",
    choices = getGroupsWithEnrichedPathways(input[[
      "enriched_pathways_selected_method"
    ]]),
    width = "100%"
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["enriched_pathways_message_no_method_found"]] <- renderText({
  "Pathway-enrichment results are not included in this data set. Choose a data set that includes enrichment results."
})
