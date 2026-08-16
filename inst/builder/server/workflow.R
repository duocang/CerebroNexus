output$workflow_progress <- renderUI({
  if (!is.null(import_focus_id())) {
    return(NULL)
  }
  builder_workflow_progress_ui(
    workflow()$stage,
    available = builder_workflow_stage_availability(
      workflow(),
      datasets_ready = length(store()$datasets %||% list()) > 0L
    ),
    confirmed = is.list(workflow()$confirmation),
    locked = builder_build_controls_locked(build_flow())
  )
})

navigate_workflow_stage <- function(stage) {
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return(invisible(FALSE))
  }
  state <- isolate(workflow())
  available <- builder_workflow_stage_availability(
    state,
    datasets_ready = length(isolate(store()$datasets %||% list())) > 0L
  )
  if (!isTRUE(available[[stage]])) {
    return(invisible(FALSE))
  }
  workflow_manual_navigation(identical(stage, "upload"))
  workflow(builder_reduce_workflow(
    state,
    list(type = "navigate", stage = stage, datasets_ready = TRUE)
  ))
  if (!identical(stage, "build")) {
    build_flow(list(stage = "idle", plan = NULL))
    session$sendCustomMessage("builder_build_dialog", list(action = "close"))
  }
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = stage))
    },
    once = TRUE
  )
  invisible(TRUE)
}

observeEvent(input$workflow_stage_upload, {
  navigate_workflow_stage("upload")
})
observeEvent(input$workflow_stage_configure, {
  navigate_workflow_stage("configure")
})
observeEvent(input$workflow_stage_review, {
  navigate_workflow_stage("review")
})
observeEvent(input$workflow_stage_build, {
  navigate_workflow_stage("build")
})

output$workbench <- renderUI({
  loading_id <- import_focus_id()
  loading_entry <- if (is.null(loading_id)) {
    NULL
  } else {
    builder_import_find(imports(), loading_id)
  }
  if (!is.null(loading_entry)) {
    return(tagAppendAttributes(
      builder_loading_workbench_ui(loading_entry),
      class = "builder-stage-upload",
      `data-workflow-stage` = "upload"
    ))
  }

  stage <- workflow()$stage
  switch(
    stage,
    upload = tagAppendAttributes(
      builder_empty_workbench_ui(),
      class = "builder-stage-upload",
      `data-workflow-stage` = "upload"
    ),
    configure = render_configure_workbench(),
    review = render_review_workbench(),
    build = render_build_workbench(),
    stop("Unsupported Builder workflow stage.", call. = FALSE)
  )
})

observeEvent(input$continue_to_review, {
  plan <- isolate(frozen_review_plan())
  req(builder_review_can_build(plan))
  workflow(builder_reduce_workflow(
    isolate(workflow()),
    list(type = "open_review", plan = plan)
  ))
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = "review"))
    },
    once = TRUE
  )
})

render_build_workbench <- function() {
  state <- workflow()
  plan <- state$review_plan
  if (
    !identical(state$stage, "build") ||
      !builder_review_can_build(plan) ||
      !builder_workflow_confirmation_matches(state, plan)
  ) {
    return(NULL)
  }
  builder_build_workbench_ui(builder_review_model(plan))
}

output$build_stage_controls <- renderUI({
  req(identical(workflow()$stage, "build"))
  builder_build_stage_controls_ui(
    selected_output() %||% character(),
    controls_disabled = builder_build_controls_locked(build_flow())
  )
})

observeEvent(input$back_to_review, {
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return()
  }
  state <- isolate(workflow())
  if (!identical(state$stage, "build")) {
    return()
  }
  workflow(builder_reduce_workflow(
    state,
    list(type = "back_to_review")
  ))
  build_flow(list(stage = "idle", plan = NULL))
  session$sendCustomMessage(
    "builder_build_dialog",
    list(action = "close")
  )
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = "review"))
    },
    once = TRUE
  )
})

observe({
  state <- workflow()
  if (
    (is.null(state$review_plan) || is.null(state$confirmation)) &&
      !is.null(isolate(selected_output()))
  ) {
    selected_output(NULL)
  }
})
