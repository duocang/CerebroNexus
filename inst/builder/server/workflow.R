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
    return(builder_loading_workbench_ui(loading_entry))
  }

  stage <- workflow()$stage
  switch(
    stage,
    upload = builder_empty_workbench_ui(),
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
      session$sendCustomMessage("builder_focus_review", list())
    },
    once = TRUE
  )
})
