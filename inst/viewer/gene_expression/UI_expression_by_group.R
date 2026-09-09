##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##
## Expression by group
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with input selection (which group to show) and plot.
##----------------------------------------------------------------------------##
output[["expression_by_group_UI"]] <- renderUI({
  summary <- expression_summary_data()
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by group"),
        cerebroInfoButton("expression_by_group_info")
      ),
      tagList(
        selectInput(
          "expression_by_group_selected_group",
          label = "Select a group to show expression by:",
          choices = getGroups(),
          width = "100%"
        ),
        plotly::plotlyOutput(
          "expression_by_group",
          height = paste0(
            max(400, ceiling(length(summary$series) / 3) * 300),
            "px"
          )
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["expression_by_group"]] <- plotly::renderPlotly({
  req(
    expression_projection_data(),
    expression_summary_data(),
    input[["expression_by_group_selected_group"]]
  )
  cells_df <- expression_projection_data()
  selected_group <- input[["expression_by_group_selected_group"]]
  plotExpressionSummary(
    expression_summary_data()$series,
    cells_df[[selected_group]],
    reactive_colors()[[selected_group]]
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_by_group_info"]], {
  showModal(
    modalDialog(
      expression_by_group_info$text,
      title = expression_by_group_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_by_group_info <- list(
  title = "Expression levels by group",
  text = p(
    "Log-normalised expression by group. Mean expression mode averages the selected genes for each cell; Separate panels and RGB modes retain one panel per gene or populated channel."
  )
)
