## Builder state: build.

builder_build_state <- function() {
  structure(
    list(
      status = "idle",
      id = NULL,
      plan_revision = NULL,
      result = NULL,
      error = NULL,
      revision = 0L
    ),
    class = c("builder_build_state", "list")
  )
}

#' Translate a staged worker result into one explicit state transition.
builder_build_action <- function(result, id) {
  if (!is.list(result) || !.builder_state_text(result$state)) {
    .builder_state_abort(
      "invalid_build_result",
      "The worker returned an invalid build result."
    )
  }
  if (identical(result$state, "success")) {
    if (!isTRUE(result$publishable) && !isTRUE(result$published)) {
      .builder_state_abort(
        "invalid_build_result",
        "A successful build result must be publishable or already published."
      )
    }
    return(list(type = "succeed", id = id, result = result))
  }
  if (identical(result$state, "needs_decision")) {
    return(list(type = "needs_decision", id = id, result = result))
  }
  if (identical(result$state, "failure")) {
    error <- result$error
    if (!.builder_state_text(error)) {
      error <- "The staged build failed."
    }
    return(list(type = "fail", id = id, error = error))
  }
  .builder_state_abort(
    "invalid_build_result",
    "The worker returned an unknown build state."
  )
}

#' Apply a typed event to the pure single-flight build state.
builder_reduce_build <- function(state, action) {
  if (!inherits(state, "builder_build_state") || !is.list(state)) {
    .builder_state_abort(
      "invalid_build_state",
      "Expected a Builder build state."
    )
  }
  if (!is.list(action) || !.builder_state_text(action$type)) {
    .builder_state_abort(
      "invalid_build_action",
      "Build actions require a type."
    )
  }
  revision <- .builder_state_revision(state$revision) + 1L

  require_current_build <- function() {
    if (
      !.builder_state_fact_text(action$id) ||
        !identical(action$id, state$id)
    ) {
      .builder_state_abort(
        "stale_build_event",
        "The build event does not match the active build."
      )
    }
  }

  if (identical(action$type, "start")) {
    if (state$status %in% c("running", "cancelling")) {
      .builder_state_abort(
        "build_in_flight",
        "A Builder build is already in flight."
      )
    }
    if (!.builder_state_fact_text(action$id)) {
      .builder_state_abort("invalid_build_id", "A build id is required.")
    }
    state$status <- "running"
    state$id <- action$id
    state$plan_revision <- .builder_state_revision(action$revision)
    state$result <- NULL
    state$error <- NULL
  } else if (identical(action$type, "succeed")) {
    if (!state$status %in% c("running", "cancelling")) {
      .builder_state_abort("invalid_build_transition", "No build is running.")
    }
    require_current_build()
    state$status <- "success"
    state$result <- action$result
  } else if (identical(action$type, "fail")) {
    if (!state$status %in% c("running", "cancelling")) {
      .builder_state_abort("invalid_build_transition", "No build is running.")
    }
    require_current_build()
    state$status <- "failed"
    state$error <- action$error
  } else if (identical(action$type, "needs_decision")) {
    if (!state$status %in% c("running", "cancelling")) {
      .builder_state_abort("invalid_build_transition", "No build is running.")
    }
    require_current_build()
    state$status <- "needs_decision"
    state$result <- action$result
    state$error <- NULL
  } else if (identical(action$type, "cancel")) {
    if (!identical(state$status, "running")) {
      .builder_state_abort(
        "invalid_build_transition",
        "No build can be cancelled."
      )
    }
    require_current_build()
    state$status <- "cancelling"
  } else if (identical(action$type, "cancelled")) {
    if (!identical(state$status, "cancelling")) {
      .builder_state_abort(
        "invalid_build_transition",
        "Build is not cancelling."
      )
    }
    require_current_build()
    state$status <- "cancelled"
  } else if (identical(action$type, "reset")) {
    if (state$status %in% c("running", "cancelling")) {
      .builder_state_abort(
        "build_in_flight",
        "A running build cannot be reset."
      )
    }
    if (!identical(state$status, "idle")) {
      require_current_build()
    }
    state <- builder_build_state()
  } else {
    .builder_state_abort(
      "unknown_build_action",
      "Build action type is not supported."
    )
  }
  state$revision <- revision
  structure(state, class = c("builder_build_state", "list"))
}
