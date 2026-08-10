output$workflow_progress <- renderUI({
  if (!is.null(active_import_id())) {
    return(NULL)
  }
  builder_workflow_progress_ui(workflow()$stage)
})

output$workbench <- renderUI({
  loading_id <- active_import_id()
  loading_entry <- if (is.null(loading_id)) {
    NULL
  } else {
    builder_import_find(imports(), loading_id)
  }
  if (!is.null(loading_entry)) {
    return(tagAppendAttributes(
      builder_loading_workbench_ui(loading_entry),
      class = "builder-stage-upload"
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
