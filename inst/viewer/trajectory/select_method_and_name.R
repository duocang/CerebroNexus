##----------------------------------------------------------------------------##
## Trajectory method and name selectors.
##----------------------------------------------------------------------------##

trajectory_viewer_defaults <- reactive({
  configuredViewerContent(
    Cerebro.options[["viewer_content"]],
    available_crb_files$selected,
    available_crb_files$files
  )$default_trajectory
})

output[["trajectory_select_method_and_name_UI"]] <- renderUI({
  methods <- intersect(getMethodsForTrajectories(), "monocle2")
  if (length(methods) == 0) {
    return(textOutput("trajectory_missing"))
  }

  defaults <- trajectory_viewer_defaults()
  method <- if (
    is.list(defaults) &&
      length(defaults$method) == 1 &&
      defaults$method %in% methods
  ) {
    defaults$method
  } else {
    methods[[1]]
  }
  names <- getNamesOfTrajectories(method)
  name <- if (
    is.list(defaults) &&
      identical(defaults$method, method) &&
      length(defaults$name) == 1 &&
      defaults$name %in% names
  ) {
    defaults$name
  } else {
    names[[1]]
  }

  tagList(
    selectInput(
      "trajectory_selected_method",
      label = "Choose a method",
      choices = methods,
      selected = method,
      width = "100%"
    ),
    selectInput(
      "trajectory_selected_name",
      label = "Choose a trajectory",
      choices = names,
      selected = name,
      width = "100%"
    )
  )
})

observeEvent(
  input[["trajectory_selected_method"]],
  {
    method <- input[["trajectory_selected_method"]]
    req(method %in% getMethodsForTrajectories())
    names <- getNamesOfTrajectories(method)
    updateSelectInput(
      session,
      "trajectory_selected_name",
      choices = names,
      selected = names[[1]]
    )
  },
  ignoreInit = TRUE
)

output[["trajectory_missing"]] <- renderText({
  "No trajectories available to display."
})
