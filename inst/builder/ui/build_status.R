## Typed top-level Builder result models and actions.

builder_mutations_locked <- function(flow, protocol) {
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (!is.list(flow) || !scalar_text(flow$stage)) {
    return(TRUE)
  }
  if (!is.null(protocol)) {
    if (!is.list(protocol) || !scalar_text(protocol$build_status)) {
      return(TRUE)
    }
    if (protocol$build_status %in% c("queued", "running", "cancelling")) {
      return(TRUE)
    }
  }
  flow$stage %in%
    c(
      "queued",
      "building",
      "choosing",
      "choosing_folder",
      "conflict"
    )
}

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
  if (identical(state, "success")) {
    auth_enabled <- value$auth_enabled
    auth_env_file <- value$auth_env_file
    valid_enabled <- is.logical(auth_enabled) &&
      length(auth_enabled) == 1L &&
      !is.na(auth_enabled)
    valid_env <- is.null(auth_env_file) ||
      (is.character(auth_env_file) &&
        length(auth_env_file) == 1L &&
        !is.na(auth_env_file) &&
        nzchar(auth_env_file))
    if (
      !valid_enabled ||
        !valid_env ||
        (isTRUE(auth_enabled) && is.null(auth_env_file)) ||
        (!isTRUE(auth_enabled) && !is.null(auth_env_file))
    ) {
      stop("The result authentication fields are invalid.", call. = FALSE)
    }
  } else if (any(c("auth_enabled", "auth_env_file") %in% names(value))) {
    stop(
      "Only successful results may carry authentication fields.",
      call. = FALSE
    )
  }
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
  auth_enabled = FALSE,
  auth_env_file = NULL,
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
        report_path = report_path,
        auth_enabled = auth_enabled,
        auth_env_file = auth_env_file
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
  result <- builder_as_result(result)
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
    release_dir = if (builder_stage_has_text(result$release$target %||% "")) {
      result$release$target
    } else if (builder_stage_has_text(result$app_dir %||% "")) {
      dirname(result$app_dir)
    } else if (length(result$built %||% character())) {
      dirname(result$built[[1L]])
    } else {
      NULL
    },
    auth_enabled = identical(type, "success") && isTRUE(result$auth_enabled),
    report_path = result$report_path %||% NULL,
    retry_closure = result$retry_closure %||% character(),
    failed_dataset_id = result$failed_dataset_id %||% NULL,
    restartable_worker = isTRUE(result$restartable_worker),
    recovery = result$recovery %||% NULL
  )
  structure(model, class = c("builder_build_status", "list"))
}

.builder_open_app_child <- function(
  path,
  env_file,
  env_name,
  .run_app = shiny::runApp
) {
  if (
    !identical(env_name, "CEREBRO_AUTH_PASSPHRASE") || !is.function(.run_app)
  ) {
    stop("The authentication environment is invalid.", call. = FALSE)
  }
  path_is_link <- function(candidate) {
    link <- tryCatch(
      Sys.readlink(candidate),
      error = function(error) NA_character_
    )
    is.character(link) && length(link) == 1L && !is.na(link) && nzchar(link)
  }
  value <- NULL
  previous <- Sys.getenv(env_name, unset = NA_character_)
  installed <- FALSE
  on.exit(
    {
      if (installed) {
        if (is.na(previous)) {
          Sys.unsetenv(env_name)
        } else {
          do.call(Sys.setenv, stats::setNames(list(previous), env_name))
        }
      }
      value <- NULL
    },
    add = TRUE
  )
  if (!is.null(env_file)) {
    valid <- tryCatch(
      {
        app_dir <- normalizePath(path, winslash = "/", mustWork = TRUE)
        secret <- normalizePath(env_file, winslash = "/", mustWork = TRUE)
        env_info <- fs::file_info(env_file, fail = TRUE, follow = FALSE)
        if (
          !identical(secret, file.path(dirname(app_dir), "viewer-auth.env")) ||
            path_is_link(env_file) ||
            !identical(as.character(env_info$type), "file") ||
            !identical(as.double(env_info$hard_links), 1) ||
            (.Platform$OS.type != "windows" &&
              !identical(as.integer(file.info(env_file)$mode), 384L))
        ) {
          stop("invalid")
        }
        lines <- readLines(env_file, warn = FALSE, encoding = "UTF-8")
        pattern <- paste0("^", env_name, "=([0-9a-f]{64})$")
        if (length(lines) != 1L || !grepl(pattern, lines, perl = TRUE)) {
          stop("invalid")
        }
        value <- sub(paste0("^", env_name, "="), "", lines)
        database <- file.path(
          app_dir,
          "private-data",
          "auth",
          "credentials.sqlite"
        )
        if (
          path_is_link(database) ||
            !file.exists(database) ||
            dir.exists(database) ||
            !isTRUE(CerebroNexus:::.viewerAuthValidateDatabase(database, value))
        ) {
          stop("invalid")
        }
        TRUE
      },
      error = function(error) FALSE,
      warning = function(warning) FALSE
    )
    if (!isTRUE(valid)) {
      value <- NULL
      stop("The authentication environment is invalid.", call. = FALSE)
    }
    do.call(Sys.setenv, stats::setNames(list(value), env_name))
    installed <- TRUE
  }
  .run_app(path, launch.browser = TRUE)
}

builder_open_final_app <- function(
  result,
  .verify_auth = function(app_dir, env_file) {
    builder_auth_verify_database_pair(
      file.path(app_dir, "private-data", "auth", "credentials.sqlite"),
      env_file
    )
  },
  .child = .builder_open_app_child,
  .run_app = NULL,
  .on_open = NULL,
  .open = function(path, env_file) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop("The callr package is required to open the App.", call. = FALSE)
    }
    args <- list(
      path = path,
      env_file = env_file,
      env_name = "CEREBRO_AUTH_PASSPHRASE"
    )
    if (!is.null(.run_app)) {
      args$.run_app <- .run_app
    }
    process <- callr::r_bg(
      .child,
      args = args,
      supervise = FALSE
    )
    if (is.function(.on_open)) {
      .on_open(process)
    }
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
  app_dir <- normalizePath(result$app_dir, winslash = "/", mustWork = TRUE)
  env_file <- NULL
  if (isTRUE(result$auth_enabled)) {
    valid <- tryCatch(
      {
        env_file <- normalizePath(
          result$auth_env_file,
          winslash = "/",
          mustWork = TRUE
        )
        identical(env_file, file.path(dirname(app_dir), "viewer-auth.env")) &&
          isTRUE(.verify_auth(app_dir, env_file))
      },
      error = function(error) FALSE
    )
    if (!isTRUE(valid)) {
      stop(
        paste(
          "Authentication files are incomplete; rebuild the App",
          "or restore its matching viewer-auth.env."
        ),
        call. = FALSE
      )
    }
  }
  isTRUE(.open(app_dir, env_file))
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

builder_build_pipeline_ui <- function(state) {
  states <- c(
    queued = "Queued",
    building = "Building",
    complete = "Complete",
    failure = "Failed"
  )
  if (
    !is.character(state) ||
      length(state) != 1L ||
      !state %in% names(states)
  ) {
    stop("A server-known build pipeline state is required.", call. = FALSE)
  }
  div(
    class = "builder-build-pipeline pipeline",
    `data-pipeline-state` = state,
    `aria-label` = paste("Build status:", states[[state]]),
    lapply(names(states), function(step) {
      div(
        class = "pipeline-step",
        `data-step` = step,
        span(class = "pipeline-light", `aria-hidden` = "true"),
        span(states[[step]])
      )
    })
  )
}

builder_build_stage_status_model <- function(
  flow,
  protocol,
  note,
  result,
  output_selected
) {
  flow_valid <-
    is.list(flow) &&
    is.character(flow$stage) &&
    length(flow$stage) == 1L &&
    !is.na(flow$stage)
  flow_stage <- if (flow_valid) {
    flow$stage
  } else {
    "idle"
  }
  build_status <- if (
    is.list(protocol) &&
      is.character(protocol$build_status) &&
      length(protocol$build_status) == 1L &&
      !is.na(protocol$build_status)
  ) {
    protocol$build_status
  } else {
    "idle"
  }
  state <- if (!is.null(result)) {
    "result"
  } else if (identical(build_status, "queued")) {
    "queued"
  } else if (build_status %in% c("running", "cancelling")) {
    "building"
  } else if (identical(flow_stage, "choosing_folder")) {
    "choosing_folder"
  } else {
    "ready"
  }
  message <- if (
    is.character(note) &&
      length(note) == 1L &&
      !is.na(note)
  ) {
    note
  } else {
    NULL
  }
  protocol_ready <- isTRUE(tryCatch(
    builder_protocol_is_quiescent(protocol),
    error = function(error) FALSE
  ))
  list(
    state = state,
    message = message,
    pipeline_state = switch(
      state,
      queued = "queued",
      building = "building",
      NULL
    ),
    can_build = identical(state, "ready") &&
      flow_valid &&
      identical(flow_stage, "idle") &&
      protocol_ready &&
      isTRUE(output_selected),
    result_model = if (is.null(result)) {
      NULL
    } else {
      builder_build_status_model(result)
    }
  )
}

builder_build_stage_status_ui <- function(model) {
  if (
    !is.list(model) ||
      !is.character(model$state) ||
      length(model$state) != 1L ||
      is.na(model$state)
  ) {
    stop("A Build-stage status model is required.", call. = FALSE)
  }
  content <- switch(
    model$state,
    ready = tagList(
      if (
        !isTRUE(model$can_build) &&
          builder_stage_has_text(model$message %||% "")
      ) {
        p(class = "builder-build-readiness", model$message)
      },
      actionButton(
        "build",
        "Build Viewer",
        class = "btn btn-action",
        disabled = !isTRUE(model$can_build)
      )
    ),
    choosing_folder = div(
      class = "builder-build-waiting",
      span(class = "spinner"),
      span("Choosing output folder…")
    ),
    queued = tagList(
      builder_build_pipeline_ui("queued"),
      p(model$message %||% "Build queued…")
    ),
    building = tagList(
      builder_build_pipeline_ui("building"),
      p(model$message %||% "Building Viewer…")
    ),
    result = builder_build_status_ui(model$result_model),
    stop("The Build-stage status is unsupported.", call. = FALSE)
  )
  div(
    class = paste(
      "builder-build-stage-status-content",
      paste0("is-", model$state)
    ),
    content
  )
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
  icon <- switch(
    model$type,
    success = "check",
    needs_decision = "question",
    failure = "error",
    recovery_required = "recovery"
  )
  pipeline_state <- switch(
    model$type,
    success = "complete",
    failure = "failure",
    recovery_required = "failure",
    NULL
  )
  div(
    class = paste(
      "card result-card",
      model$type,
      model$variant,
      paste0(
        "is-",
        if (identical(model$type, "recovery_required")) {
          "recovery"
        } else {
          model$type
        }
      )
    ),
    h2(`data-icon` = icon, title),
    if (!is.null(pipeline_state)) builder_build_pipeline_ui(pipeline_state),
    if (!is.null(model$message)) p(model$message),
    if (identical(model$type, "success")) {
      p(
        class = "builder-auth-status",
        paste(
          "Login:",
          if (isTRUE(model$auth_enabled)) "Required" else "Not required"
        )
      )
    },
    if (identical(model$type, "needs_decision")) {
      div(
        class = "builder-recovery-action",
        p(
          "Retry the failed optional work, or remove it, review, and build again."
        ),
        actionButton(
          "retry_failed_analysis",
          "Retry optional work",
          class = "btn btn-primary"
        ),
        if (builder_stage_has_text(model$failed_dataset_id %||% "")) {
          actionButton(
            "remove_failed_analysis",
            "Remove and review",
            class = "btn btn-quiet"
          )
        }
      )
    },
    if (identical(model$type, "failure") && isTRUE(model$restartable_worker)) {
      div(
        class = "builder-recovery-action",
        p(
          "Your settings are safe. Restart the worker from its saved snapshots, then retry."
        ),
        actionButton(
          "restart_worker",
          "Restart worker",
          class = "btn btn-primary"
        )
      )
    },
    if (identical(model$type, "recovery_required")) {
      div(
        class = "builder-recovery-action",
        h3("Manual recovery steps"),
        p(
          "Keep the preserved backup, close other processes using the output, and restore the backup named above before building again."
        )
      )
    },
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
