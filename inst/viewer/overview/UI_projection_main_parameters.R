##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_main_parameters_UI"]] <- renderUI({
  metadata <- viewerProjectionFirstFrameMetadata()
  color_choices <- setdiff(colnames(metadata), "cell_barcode")
  parameters <- tryCatch(getParameters(), error = function(e) list())
  defaults <- viewerProjectionDefaults(
    metadata,
    availableProjections(),
    parameters
  )
  tagList(
    selectInput(
      "overview_projection_to_display",
      label = "Projection",
      choices = availableProjections(),
      selected = defaults$projection
    ),
    selectInput(
      "overview_projection_point_color",
      label = "Colour by",
      choices = color_choices,
      selected = defaults$color_variable
    )
  )
})

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
      <li><b>Colour by:</b> Select which variable, categorical or continuous, from the meta data should be used to colour the cells.</li>
    </ul>
    "
  )
)
