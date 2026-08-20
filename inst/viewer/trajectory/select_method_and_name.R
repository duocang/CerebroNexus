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
  ## currently, only trajectories from monocle2 are supported
  available_methods <- getMethodsForTrajectories()
  available_methods <- available_methods[available_methods %in% c('monocle2')]

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
  ## currently, only trajectories from monocle2 are supported
  available_methods <- getMethodsForTrajectories()
  available_methods <- available_methods[available_methods %in% c('monocle2')]
  viewer_defaults <- configuredViewerContent(
    Cerebro.options[["viewer_content"]],
    available_crb_files$selected,
    available_crb_files$files
  )
  default_method <- viewer_defaults$default_trajectory$method
  if (
    !is.character(default_method) ||
      length(default_method) != 1L ||
      is.na(default_method) ||
      !default_method %in% available_methods
  ) {
    default_method <- if (length(available_methods)) {
      available_methods[[1L]]
    } else {
      NULL
    }
  }

  selectInput(
    "trajectory_selected_method",
    label = "Choose a method",
    choices = available_methods,
    selected = default_method,
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
  available_names <- getNamesOfTrajectories(input[[
    "trajectory_selected_method"
  ]])
  viewer_defaults <- configuredViewerContent(
    Cerebro.options[["viewer_content"]],
    available_crb_files$selected,
    available_crb_files$files
  )
  default_trajectory <- viewer_defaults$default_trajectory
  configured_name <- if (
    is.list(default_trajectory) &&
      is.character(default_trajectory$name) &&
      length(default_trajectory$name) == 1L &&
      !is.na(default_trajectory$name)
  ) {
    default_trajectory$name
  } else {
    NULL
  }
  default_name <- if (
    is.list(default_trajectory) &&
      identical(
        default_trajectory$method,
        input[[
          "trajectory_selected_method"
        ]]
      ) &&
      !is.null(configured_name) &&
      configured_name %in% available_names
  ) {
    configured_name
  } else if (length(available_names)) {
    available_names[[1L]]
  } else {
    NULL
  }
  selectInput(
    "trajectory_selected_name",
    label = "Choose a trajectory",
    choices = available_names,
    selected = default_name,
    width = "100%"
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["trajectory_missing"]] <- renderText({
  "No trajectories available to display."
})
