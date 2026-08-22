##----------------------------------------------------------------------------##
## Method and marker-table selectors.
##----------------------------------------------------------------------------##

output[["marker_genes_select_method_and_table_UI"]] <- renderUI({
  methods <- getMethodsForMarkerGenes()
  if (is.null(methods) || length(methods) == 0) {
    return(fluidRow(
      cerebroBox(
        title = boxTitle("Marker genes"),
        textOutput("marker_genes_message_no_method_found")
      )
    ))
  }

  selected_method <- methods[[1]]
  fluidRow(
    class = "result-selectors",
    column(
      6,
      div(
        class = "result-selector-field",
        selectInput(
          "marker_genes_selected_method",
          label = "Choose a method:",
          choices = methods,
          selected = selected_method,
          width = "100%"
        )
      )
    ),
    column(
      6,
      div(
        class = "result-selector-field",
        selectInput(
          "marker_genes_selected_table",
          label = "Choose a table:",
          choices = getGroupsWithMarkerGenes(selected_method),
          width = "100%"
        )
      )
    )
  )
})

observeEvent(input[["marker_genes_selected_method"]], {
  choices <- getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]])
  updateSelectInput(
    session,
    "marker_genes_selected_table",
    choices = choices,
    selected = choices[[1]]
  )
})

output[["marker_genes_message_no_method_found"]] <- renderText({
  "No data available."
})
