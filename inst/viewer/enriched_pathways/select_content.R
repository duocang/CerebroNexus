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
        class = "result-selectors",
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
  div(
    class = "result-selector-field",
    selectInput(
      "enriched_pathways_selected_method",
      label = "Choose a method:",
      choices = getMethodsForEnrichedPathways(),
      width = "100%"
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["enriched_pathways_selected_table_UI"]] <- renderUI({
  req(input[["enriched_pathways_selected_method"]])
  div(
    class = "result-selector-field",
    selectInput(
      "enriched_pathways_selected_table",
      label = "Choose a table:",
      choices = getGroupsWithEnrichedPathways(input[[
        "enriched_pathways_selected_method"
      ]]),
      width = "100%"
    )
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["enriched_pathways_message_no_method_found"]] <- renderText({
  "No data available."
})
