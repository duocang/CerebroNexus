source(
  file.path(
    Cerebro.options[["cerebro_root"]],
    "viewer/load_data/overview_ui.R"
  ),
  local = TRUE
)

output$load_data_overview <- renderUI({
  viewerOverviewUI(current_dataset_info())
})

## The colour of every category as the other pages draw it, so a bar can be
## matched to its group at a glance. It needs the loaded data set and the
## Colour management choices; until then (or for a column that is not a
## colourable group) the bars keep their accent colour.
viewerOverviewColors <- function(column) {
  tryCatch(reactive_group_colors(column), error = function(e) NULL)
}

output$load_data_sample_plot <- renderUI({
  sample <- current_dataset_info()$overview$sample
  if (is.null(sample)) {
    return(NULL)
  }
  viewerOverviewBars(sample, viewerOverviewColors(sample$column))
})

output$load_data_annotation_plot <- renderUI({
  annotations <- current_dataset_info()$overview$annotations
  if (!length(annotations)) {
    return(NULL)
  }
  selected <- input$load_data_annotation
  if (is.null(selected) || !selected %in% names(annotations)) {
    selected <- names(annotations)[1L]
  }
  viewerOverviewBars(annotations[[selected]], viewerOverviewColors(selected))
})

for (destination in c(
  "overview",
  "geneExpression",
  "groups",
  "spatial",
  "trajectory",
  "immune_repertoire",
  "markerGenes",
  "enrichedPathways",
  "extra_material"
)) {
  local({
    target <- destination
    observeEvent(
      input[[paste0("dataset_open_", target)]],
      {
        updateTabItems(session, "sidebar", selected = target)
      },
      ignoreInit = TRUE
    )
  })
}
