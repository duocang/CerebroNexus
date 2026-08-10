builder_workflow_progress_ui <- function(stage) {
  stages <- c("upload", "configure", "review", "build")
  if (
    !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !stage %in% stages
  ) {
    stop("A valid Builder workflow stage is required.", call. = FALSE)
  }
  labels <- c("Upload", "Configure", "Review", "Build")
  tags$nav(
    class = "builder-workflow-progress",
    `aria-label` = "Builder progress",
    tags$ol(lapply(seq_along(stages), function(index) {
      current <- identical(stage, stages[[index]])
      tags$li(
        class = if (current) "is-current" else NULL,
        `aria-current` = if (current) "step" else NULL,
        labels[[index]]
      )
    }))
  )
}

builder_configure_actions_ui <- function(message, can_continue, app_control) {
  stopifnot(
    is.character(message),
    length(message) == 1L,
    !is.na(message),
    is.logical(can_continue),
    length(can_continue) == 1L,
    !is.na(can_continue)
  )
  div(
    class = "builder-stage-actions builder-configure-actions",
    p(class = "builder-configure-readiness", message),
    app_control,
    actionButton(
      "continue_to_review",
      "Continue",
      class = "btn btn-action",
      disabled = !can_continue
    )
  )
}

builder_build_workbench_ui <- function(model, output_path, status = NULL) {
  stopifnot(
    is.list(model),
    is.character(output_path),
    length(output_path) <= 1L,
    !length(output_path) || !is.na(output_path)
  )
  output_label <- if (isTRUE(model$output$private_app)) {
    "Viewer App"
  } else {
    "CRB files"
  }
  selected_label <- if (builder_has_text(output_path)) {
    output_path
  } else {
    "No output folder selected"
  }
  div(
    class = "builder-stage builder-stage-build builder-card builder-section",
    `data-workflow-stage` = "build",
    h2("Build your Viewer"),
    p(
      class = "stage-intro",
      "Build the frozen plan you reviewed and confirmed."
    ),
    tags$section(
      class = "builder-build-summary",
      h3("Reviewed output"),
      p(
        strong(paste0(model$output$crb_count, " dataset")),
        if (identical(model$output$crb_count, 1L)) "" else "s",
        " · ",
        output_label
      )
    ),
    p(class = "builder-selected-output", selected_label),
    div(
      class = "builder-stage-actions builder-build-actions",
      actionButton(
        "back_to_review",
        "Back to review",
        class = "btn"
      ),
      actionButton(
        "choose_output_folder",
        "Choose folder…",
        class = "btn"
      ),
      actionButton(
        "build",
        "Build",
        class = "btn btn-action",
        disabled = !builder_has_text(output_path)
      )
    ),
    status
  )
}
