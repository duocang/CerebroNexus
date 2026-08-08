##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_main_parameters_UI"]] <- renderUI({
  projections <- availableProjections()
  viewer_defaults <- configuredViewerContent(
    Cerebro.options[["viewer_content"]],
    available_crb_files$selected,
    available_crb_files$files
  )
  default_projection <- viewer_defaults$default_projection
  if (
    !is.character(default_projection) ||
      length(default_projection) != 1L ||
      is.na(default_projection) ||
      !default_projection %in% projections
  ) {
    default_projection <- if (length(projections)) projections[[1L]] else NULL
  }
  tagList(
    selectInput(
      "overview_projection_to_display",
      label = "Projection",
      choices = projections,
      selected = default_projection
    ),
    selectInput(
      "overview_projection_point_color",
      label = "Color cells by",
      choices = colnames(getMetaData())[
        !colnames(getMetaData()) %in% c("cell_barcode")
      ]
    )
  )
})

## Keep dataset-dependent choices current even when another tab is active.
## Otherwise Shiny suspends this hidden output and a dataset switch can leave
## stale projection and metadata defaults in the overview controls.
outputOptions(
  output,
  "overview_projection_main_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      overview_projection_main_parameters_info[["text"]],
      title = overview_projection_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
overview_projection_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)
