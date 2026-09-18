##----------------------------------------------------------------------------##
## UI elements to choose which projection/trajectory to show.
##----------------------------------------------------------------------------##
output[["expression_projection_select_projection_UI"]] <- renderUI({
  available_projections <- availableProjections()
  available_trajectories <- available_trajectories()
  shared <- isolate(input[["coordviews_shared_base"]])
  selected_projection <- as.character(shared$projection %||% "")
  if (!selected_projection %in% available_projections) {
    selected_projection <- if (length(available_projections)) {
      available_projections[[1L]]
    } else if (length(available_trajectories)) {
      available_trajectories[[1L]]
    } else {
      NULL
    }
  }
  selectInput(
    "expression_projection_to_display",
    label = "Projection",
    choices = list(
      "Projections" = as.list(available_projections),
      "Trajectories" = as.list(available_trajectories)
    ),
    selected = selected_projection
  )
})
