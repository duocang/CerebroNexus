##----------------------------------------------------------------------------##
## Select method and table (group).
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of method and group, which are split
## because the group depends on which method is selected.
##----------------------------------------------------------------------------##
output[["marker_genes_select_method_and_table_UI"]] <- renderUI({
  if (
    !is.null(getMethodsForMarkerGenes()) &&
      length(getMethodsForMarkerGenes()) > 0
  ) {
    tagList(
      fluidRow(
        column(
          6,
          uiOutput("marker_genes_selected_method_UI")
        ),
        column(
          6,
          uiOutput("marker_genes_selected_table_UI")
        )
      )
    )
  } else {
    fluidRow(
      cerebroBox(
        title = boxTitle("Marker genes"),
        textOutput("marker_genes_message_no_method_found")
      )
    )
  }
})

##----------------------------------------------------------------------------##
## UI element to select from which method the results should be shown.
##----------------------------------------------------------------------------##
output[["marker_genes_selected_method_UI"]] <- renderUI({
  selectInput(
    "marker_genes_selected_method",
    label = "Method",
    choices = getMethodsForMarkerGenes(),
    width = "100%"
  )
})

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["marker_genes_selected_table_UI"]] <- renderUI({
  req(input[["marker_genes_selected_method"]])
  selectInput(
    "marker_genes_selected_table",
    label = "Grouping variable",
    choices = getGroupsWithMarkerGenes(input[[
      "marker_genes_selected_method"
    ]]),
    width = "100%"
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["marker_genes_message_no_method_found"]] <- renderText({
  "Marker-gene results are not included in this data set. Choose a data set that includes marker-gene results."
})
