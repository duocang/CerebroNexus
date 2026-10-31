##----------------------------------------------------------------------------##
## Expression in selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for plot.
##----------------------------------------------------------------------------##
expression_selection_present <- viewerSelectionPresence(
  "expression_projection"
)

output[["expression_in_selected_cells_UI"]] <- renderUI({
  req(
    expression_projection_summary_ready(),
    expression_selection_present()
  )
  summary <- expression_summary_data()
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels in selected cells"),
        cerebroInfoButton("expression_in_selected_cells_info")
      ),
      cerebroChartOutput(
        "expression_in_selected_cells",
        height = paste0(
          max(400, ceiling(length(summary$series) / 3) * 300),
          "px"
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##
output[["expression_in_selected_cells"]] <- renderCerebroChart({
  ## The browser draws it from the frame's expression values.
  if (viewerClientSelection("expression_projection", "expression")) {
    return(NULL)
  }
  req(
    expression_projection_summary_ready(),
    expression_projection_data(),
    expression_projection_coordinates(),
    expression_summary_data(),
    expression_projection_selected_cells()
  )
  series <- expression_summary_data()$series
  selected <- logical(length(expression_projection_cells_to_show()))
  selected[expression_projection_selected_rows()] <- TRUE
  ## Group filters applied in the browser leave every cell in the frame; the
  ## cells they hide are neither selected nor shown as "not selected".
  rows <- expression_projection_shown_rows()
  if (!is.null(rows)) {
    series <- lapply(series, function(entry) {
      entry$values <- entry$values[rows]
      entry
    })
    selected <- selected[rows]
  }
  groups <- structure(
    2L - selected,
    levels = c("selected", "not selected"),
    class = "factor"
  )
  plotExpressionSummary(
    series,
    groups,
    setNames(c("#e74c3c", "#7f8c8d"), levels(groups))
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "expression_in_selected_cells_info",
  expression_in_selected_cells_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_in_selected_cells_info <- list(
  title = "Expression levels in selected cells",
  text = p(
    "This plot compares selected and non-selected cells. Mean expression mode averages the selected genes for each cell; Separate panels and RGB modes retain one panel per gene or populated channel."
  )
)
