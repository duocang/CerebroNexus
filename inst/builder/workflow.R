.builder_workflow_stages <- c("upload", "configure", "review", "build")
.builder_workflow_fields <- c(
  "stage",
  "review_plan",
  "confirmation",
  "revision"
)
.builder_review_identity_fields <- c(
  "revision",
  "dataset_order",
  "items",
  "manifest",
  "acknowledgements"
)

.builder_workflow_copy <- function(value) {
  unserialize(serialize(value, NULL, version = 3L))
}

.builder_workflow_plan_valid <- function(plan) {
  is.list(plan) &&
    inherits(plan, "builder_build_plan") &&
    identical(plan$readiness, "ready")
}

.builder_workflow_plan_identity <- function(plan) {
  .builder_workflow_copy(plan[.builder_review_identity_fields])
}

.builder_workflow_confirmation_valid <- function(confirmation, review_plan) {
  if (
    !is.list(confirmation) ||
      is.object(confirmation) ||
      !identical(names(confirmation), c("identity", "plan_revision")) ||
      !.builder_workflow_plan_valid(review_plan)
  ) {
    return(FALSE)
  }

  identity <- tryCatch(
    .builder_workflow_plan_identity(review_plan),
    error = function(error) error
  )
  !inherits(identity, "condition") &&
    identical(confirmation$identity, identity) &&
    identical(confirmation$plan_revision, review_plan$revision)
}

.builder_workflow_state_valid <- function(state) {
  if (
    !is.list(state) ||
      !identical(class(state), c("builder_workflow_state", "list")) ||
      !identical(names(state), .builder_workflow_fields) ||
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
      (state$stage %in%
        c("review", "build") &&
        !.builder_workflow_plan_valid(state$review_plan)) ||
      (!is.null(state$confirmation) &&
        !.builder_workflow_confirmation_valid(
          state$confirmation,
          state$review_plan
        )) ||
      (identical(state$stage, "build") &&
        is.null(state$confirmation))
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

  .builder_workflow_plan_identity(plan)
}

builder_final_build_identity <- function(plan) {
  if (!.builder_workflow_plan_valid(plan)) {
    stop("A ready frozen BuildPlan is required.", call. = FALSE)
  }
  list(
    review = builder_review_plan_identity(plan),
    output = .builder_workflow_copy(plan[c(
      "out_dir",
      "overwrite",
      "targets",
      "make_app",
      "app_contract_version",
      "app_options",
      "app_auth"
    )])
  )
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

builder_workflow_stage_availability <- function(state, datasets_ready) {
  if (
    !.builder_workflow_state_valid(state) ||
      !is.logical(datasets_ready) ||
      length(datasets_ready) != 1L ||
      is.na(datasets_ready)
  ) {
    stop(
      "Valid Builder workflow availability inputs are required.",
      call. = FALSE
    )
  }
  c(
    upload = TRUE,
    configure = isTRUE(datasets_ready),
    review = .builder_workflow_plan_valid(state$review_plan),
    build = .builder_workflow_confirmation_valid(
      state$confirmation,
      state$review_plan
    )
  )
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
    next_state[c("review_plan", "confirmation")] <- list(NULL, NULL)
  } else if (identical(type, "datasets_ready")) {
    next_state$stage <- "configure"
    next_state[c("review_plan", "confirmation")] <- list(NULL, NULL)
  } else if (identical(type, "open_review")) {
    identity <- builder_review_plan_identity(event$plan)
    if (
      !is.list(next_state$confirmation) ||
        !identical(next_state$confirmation$identity, identity)
    ) {
      next_state["confirmation"] <- list(NULL)
    }
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
  } else if (identical(type, "back_to_settings")) {
    if (is.null(next_state$review_plan)) {
      stop("No reviewed BuildPlan is available.", call. = FALSE)
    }
    next_state$stage <- "configure"
  } else if (identical(type, "navigate")) {
    target <- event$stage
    if (
      !is.character(target) ||
        length(target) != 1L ||
        is.na(target) ||
        !target %in% .builder_workflow_stages
    ) {
      .builder_workflow_stop_invalid()
    }
    datasets_ready <- event$datasets_ready
    if (is.null(datasets_ready)) {
      datasets_ready <- !identical(next_state$stage, "upload") ||
        !is.null(next_state$review_plan)
    }
    availability <- builder_workflow_stage_availability(
      next_state,
      datasets_ready = datasets_ready
    )
    if (!isTRUE(availability[[target]])) {
      stop("The requested Builder stage is not available.", call. = FALSE)
    }
    next_state$stage <- target
  } else if (identical(type, "invalidate")) {
    stage <- event$stage
    if (is.null(stage)) {
      stage <- "configure"
    }
    if (
      !is.character(stage) ||
        length(stage) != 1L ||
        is.na(stage) ||
        !stage %in% c("upload", "configure")
    ) {
      stop(
        "Invalidation must return to upload or configure.",
        call. = FALSE
      )
    }
    next_state$stage <- stage
    next_state[c("review_plan", "confirmation")] <- list(NULL, NULL)
  } else {
    stop(
      "The Builder workflow event is not supported.",
      call. = FALSE
    )
  }

  next_state$revision <- next_state$revision + 1L
  class(next_state) <- c("builder_workflow_state", "list")
  if (!.builder_workflow_state_valid(next_state)) {
    .builder_workflow_stop_invalid()
  }
  next_state
}
