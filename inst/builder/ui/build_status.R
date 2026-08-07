## Typed top-level Builder result models and actions.

.builder_result <- function(state, message = NULL, fields = list()) {
  if (
    !is.character(state) ||
      length(state) != 1L ||
      !state %in%
        c(
          "success",
          "needs_decision",
          "failure",
          "recovery_required"
        ) ||
      !is.list(fields)
  ) {
    stop("A supported typed build result is required.", call. = FALSE)
  }
  value <- c(list(state = state, message = message), fields)
  structure(
    value,
    class = c(paste0("builder_result_", state), "builder_result", "list")
  )
}

builder_result_success <- function(
  published = FALSE,
  built = character(),
  warnings = character(),
  app_dir = NULL,
  app_verified = FALSE,
  report_path = NULL,
  ...
) {
  .builder_result(
    "success",
    fields = c(
      list(
        published = isTRUE(published),
        built = built,
        warnings = as.character(warnings),
        app_dir = app_dir,
        app_verified = isTRUE(app_verified),
        report_path = report_path
      ),
      list(...)
    )
  )
}

builder_result_needs_decision <- function(message, ...) {
  .builder_result("needs_decision", message, list(...))
}

builder_result_failure <- function(message, ...) {
  .builder_result("failure", message, c(list(error = message), list(...)))
}

builder_result_recovery_required <- function(message, ...) {
  .builder_result(
    "recovery_required",
    message,
    c(list(error = message), list(...))
  )
}

builder_release_error_result <- function(
  message,
  target,
  .recovery = builder_coordinator_recovery
) {
  recovery <- if (builder_stage_has_text(target %||% "")) {
    try(.recovery(target), silent = TRUE)
  } else {
    NULL
  }
  if (
    is.list(recovery) &&
      identical(recovery$state, "recovery_required")
  ) {
    return(builder_result_recovery_required(
      recovery$message %||% message,
      recovery = recovery
    ))
  }
  builder_result_failure(message)
}

builder_as_result <- function(result) {
  if (inherits(result, "builder_result")) {
    return(result)
  }
  if (
    !is.list(result) ||
      !is.character(result$state) ||
      length(result$state) != 1L
  ) {
    stop("A typed build result is required.", call. = FALSE)
  }
  state <- result$state
  message <- result$error %||% result$message %||% NULL
  result$state <- NULL
  result$message <- NULL
  .builder_result(state, message, result)
}

builder_build_status_model <- function(result) {
  if (!inherits(result, "builder_result")) {
    stop("A typed build result is required.", call. = FALSE)
  }
  type <- result$state
  if (
    !type %in% c("success", "needs_decision", "failure", "recovery_required")
  ) {
    stop("The build result state is not supported.", call. = FALSE)
  }
  warnings <- as.character(result$warnings %||% character())
  model <- list(
    type = type,
    variant = if (identical(type, "success") && length(warnings)) {
      "warnings"
    } else {
      "default"
    },
    message = result$error %||% result$message %||% NULL,
    warnings = warnings,
    built = as.character(result$built %||% character()),
    app_dir = if (
      identical(type, "success") &&
        isTRUE(result$published) &&
        isTRUE(result$app_verified) &&
        builder_stage_has_text(result$app_dir %||% "")
    ) {
      result$app_dir
    } else {
      NULL
    },
    release_dir = if (length(result$built %||% character())) {
      dirname(result$built[[1L]])
    } else if (builder_stage_has_text(result$app_dir %||% "")) {
      dirname(result$app_dir)
    } else {
      NULL
    },
    report_path = result$report_path %||% NULL
  )
  structure(model, class = c("builder_build_status", "list"))
}

builder_open_final_app <- function(
  result,
  .open = function(path) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop("The callr package is required to open the App.", call. = FALSE)
    }
    callr::r_bg(
      function(path) shiny::runApp(path, launch.browser = TRUE),
      args = list(path = path),
      supervise = FALSE
    )
    TRUE
  }
) {
  result <- builder_as_result(result)
  if (
    !identical(result$state, "success") ||
      !isTRUE(result$published) ||
      !isTRUE(result$app_verified) ||
      !builder_stage_has_text(result$app_dir %||% "")
  ) {
    stop("A verified final App directory is required.", call. = FALSE)
  }
  isTRUE(.open(result$app_dir))
}

builder_reveal_release <- function(result, .reveal = NULL) {
  result <- builder_as_result(result)
  model <- builder_build_status_model(result)
  if (!builder_stage_has_text(model$release_dir %||% "")) {
    stop("A final release directory is required.", call. = FALSE)
  }
  if (is.null(.reveal)) {
    .reveal <- function(path) {
      if (identical(Sys.info()[["sysname"]], "Darwin")) {
        identical(system2("open", path, stdout = FALSE, stderr = FALSE), 0L)
      } else if (.Platform$OS.type == "windows") {
        shell.exec(path)
        TRUE
      } else {
        identical(system2("xdg-open", path, stdout = FALSE, stderr = FALSE), 0L)
      }
    }
  }
  isTRUE(.reveal(model$release_dir))
}

builder_copy_result_path <- function(result, kind, .copy) {
  result <- builder_as_result(result)
  model <- builder_build_status_model(result)
  value <- switch(
    kind,
    release = model$release_dir,
    report = model$report_path,
    app = model$app_dir,
    NULL
  )
  if (!builder_stage_has_text(value %||% "")) {
    stop("The requested final result path is unavailable.", call. = FALSE)
  }
  isTRUE(.copy(value))
}

builder_build_status_ui <- function(model) {
  if (inherits(model, "builder_result")) {
    model <- builder_build_status_model(model)
  }
  stopifnot(inherits(model, "builder_build_status"))
  title <- switch(
    model$type,
    success = "Build complete",
    needs_decision = "Build needs a decision",
    failure = "Build failed",
    recovery_required = "Release recovery required"
  )
  div(
    class = paste("card result-card", model$type, model$variant),
    h2(title),
    if (!is.null(model$message)) p(model$message),
    builder_stage_text_items(model$warnings),
    if (length(model$built)) builder_stage_text_items(basename(model$built)),
    if (!is.null(model$app_dir)) {
      actionButton(
        "open_app",
        "Open App",
        class = "btn btn-primary",
        `data-path` = model$app_dir
      )
    },
    if (!is.null(model$release_dir)) {
      actionButton(
        "reveal_folder",
        "Reveal Folder",
        class = "btn btn-quiet",
        `data-path` = model$release_dir
      )
    },
    if (!is.null(model$release_dir)) {
      actionButton(
        "copy_path",
        "Copy Path",
        class = "btn btn-quiet",
        `data-path` = model$release_dir
      )
    },
    if (!is.null(model$report_path)) {
      actionButton(
        "copy_report",
        "Copy Report",
        class = "btn btn-quiet",
        `data-report` = model$report_path
      )
    }
  )
}
