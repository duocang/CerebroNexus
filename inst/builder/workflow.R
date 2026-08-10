.builder_workflow_stages <- c("upload", "configure", "review", "build")

.builder_workflow_copy <- function(value) {
  unserialize(serialize(value, NULL, version = 3L))
}

.builder_workflow_plan_valid <- function(plan) {
  is.list(plan) &&
    identical(class(plan), c("builder_build_plan", "list")) &&
    identical(plan$readiness, "ready")
}

.builder_workflow_state_valid <- function(state) {
  if (
    !is.list(state) ||
      !identical(class(state), c("builder_workflow_state", "list")) ||
      !all(
        c("stage", "review_plan", "confirmation", "revision") %in%
          names(state)
      ) ||
      !is.character(state$stage) ||
      length(state$stage) != 1L ||
      is.na(state$stage) ||
      !state$stage %in% .builder_workflow_stages ||
      !is.integer(state$revision) ||
      length(state$revision) != 1L ||
      is.na(state$revision) ||
      state$revision < 0L ||
      state$revision == .Machine$integer.max ||
      (!is.null(state$review_plan) &&
        !.builder_workflow_plan_valid(state$review_plan)) ||
      (!is.null(state$confirmation) && !is.list(state$confirmation))
  ) {
    return(FALSE)
  }

  TRUE
}

.builder_workflow_stop_invalid <- function() {
  stop("A valid Builder workflow event is required.", call. = FALSE)
}

builder_review_plan_identity <- function(plan) {
  if (!.builder_workflow_plan_valid(plan)) {
    stop("A ready frozen BuildPlan is required.", call. = FALSE)
  }

  fields <- c(
    "revision",
    "dataset_order",
    "make_app",
    "app_contract_version",
    "items",
    "manifest",
    "app_options",
    "app_auth",
    "acknowledgements"
  )
  .builder_workflow_copy(plan[fields])
}

builder_workflow_state <- function() {
  structure(
    list(
      stage = "upload",
      review_plan = NULL,
      confirmation = NULL,
      revision = 0L
    ),
    class = c("builder_workflow_state", "list")
  )
}

builder_workflow_confirmation_matches <- function(state, plan) {
  if (
    !.builder_workflow_state_valid(state) ||
      !is.list(state$confirmation) ||
      !.builder_workflow_plan_valid(plan)
  ) {
    return(FALSE)
  }

  identity <- tryCatch(
    builder_review_plan_identity(plan),
    error = function(error) NULL
  )
  !is.null(identity) && identical(state$confirmation$identity, identity)
}

builder_reduce_workflow <- function(state, event) {
  if (
    !.builder_workflow_state_valid(state) ||
      !is.list(event) ||
      is.object(event) ||
      !is.character(event$type) ||
      length(event$type) != 1L ||
      is.na(event$type) ||
      !nzchar(event$type)
  ) {
    .builder_workflow_stop_invalid()
  }

  next_state <- .builder_workflow_copy(state)
  type <- event$type

  if (identical(type, "empty")) {
    next_state$stage <- "upload"
    next_state$review_plan <- NULL
    next_state$confirmation <- NULL
  } else if (identical(type, "datasets_ready")) {
    next_state$stage <- "configure"
  } else if (identical(type, "open_review")) {
    builder_review_plan_identity(event$plan)
    next_state$review_plan <- .builder_workflow_copy(event$plan)
    next_state$stage <- "review"
  } else if (identical(type, "confirm_review")) {
    if (!identical(next_state$stage, "review")) {
      stop("Review must be open before confirmation.", call. = FALSE)
    }
    identity <- builder_review_plan_identity(event$plan)
    reviewed_identity <- builder_review_plan_identity(
      next_state$review_plan
    )
    if (!identical(identity, reviewed_identity)) {
      stop(
        "The reviewed BuildPlan changed before confirmation.",
        call. = FALSE
      )
    }
    next_state$review_plan <- .builder_workflow_copy(event$plan)
    next_state$confirmation <- list(
      identity = identity,
      plan_revision = event$plan$revision
    )
    next_state$stage <- "build"
  } else if (identical(type, "back_to_review")) {
    if (is.null(next_state$review_plan)) {
      stop("No reviewed BuildPlan is available.", call. = FALSE)
    }
    next_state$stage <- "review"
  } else if (identical(type, "invalidate")) {
    stage <- event$stage
    if (is.null(stage)) {
      stage <- "configure"
    }
    if (
      !is.character(stage) ||
        length(stage) != 1L ||
        is.na(stage) ||
        !stage %in% .builder_workflow_stages
    ) {
      .builder_workflow_stop_invalid()
    }
    next_state$stage <- stage
    next_state$review_plan <- NULL
    next_state$confirmation <- NULL
  } else {
    stop(
      "The Builder workflow event is not supported.",
      call. = FALSE
    )
  }

  next_state$revision <- next_state$revision + 1L
  class(next_state) <- c("builder_workflow_state", "list")
  next_state
}
