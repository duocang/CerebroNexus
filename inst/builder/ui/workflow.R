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
