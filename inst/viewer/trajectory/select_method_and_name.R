##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Select method and name.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of method and name, which are split
## because the names of available trajectories depends on which method is
## selected. If no method is available, show message that data is missing.
##----------------------------------------------------------------------------##

output[["trajectory_select_method_and_name_UI"]] <- renderUI({
  available_methods <- viewerSupportedTrajectoryMethods(
    getMethodsForTrajectories()
  )

  if (length(available_methods) == 0) {
    textOutput("trajectory_missing")
  } else if (length(available_methods) > 0) {
    tagList(
      uiOutput("trajectory_selected_method_UI"),
      uiOutput("trajectory_selected_name_UI")
    )
  }
})

##----------------------------------------------------------------------------##
## UI element to select from which method the results should be shown.
##----------------------------------------------------------------------------##

output[["trajectory_selected_method_UI"]] <- renderUI({
  available_methods <- viewerSupportedTrajectoryMethods(
    getMethodsForTrajectories()
  )
  method_labels <- c(
    monocle2 = "Monocle 2",
    marker_guided = "Marker-guided",
    illustrative = "Illustrative"
  )

  selectInput(
    "trajectory_selected_method",
    label = "Choose a method",
    choices = stats::setNames(
      available_methods,
      unname(method_labels[available_methods])
    ),
    width = "100%"
  )
})

##----------------------------------------------------------------------------##
## UI element to select which trajectory (name) should be shown.
##----------------------------------------------------------------------------##

output[["trajectory_selected_name_UI"]] <- renderUI({
  ## Guard against a stale method from a previous dataset: only ask for names
  ## once the selected method is actually available in the current dataset,
  ## otherwise getNamesOfTrajectories() throws "Method `X` is not available."
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_method"]] %in% getMethodsForTrajectories()
  )
  selectInput(
    "trajectory_selected_name",
    label = "Choose a trajectory",
    choices = getNamesOfTrajectories(input[[
      "trajectory_selected_method"
    ]]),
    width = "100%"
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["trajectory_missing"]] <- renderText({
  "No trajectories available to display."
})
