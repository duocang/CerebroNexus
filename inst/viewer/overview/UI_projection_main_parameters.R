##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_main_parameters_UI"]] <- renderUI({
  colour_groups <- viewerColourGroupChoices()
  tagList(
    selectInput(
      "overview_projection_to_display",
      label = "Projection",
      choices = availableProjections()
    ),
    selectInput(
      "overview_projection_point_color",
      label = "Colour by",
      choices = colour_groups$choices,
      selected = colour_groups$selected
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
      <li><b>Colour by:</b> Select one of the data set's registered grouping variables.</li>
    </ul>
    "
  )
)
