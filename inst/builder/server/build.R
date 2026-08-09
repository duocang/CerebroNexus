## Builder server: build.

## -- build ---------------------------------------------------------------
## The whole export runs in the worker: analyses, matrix write, bundle. This
## process only sends a plan and waits for the report, so the page keeps
## answering while a marker-gene run takes its minutes.
auth_accounts_state <- auth_accounts

enqueue_build_plan <- function(
  plan,
  auth_accounts
) {
  rs <- worker()
  req(rs)
  current_protocol <- isolate(protocol())
  req(builder_protocol_is_quiescent(current_protocol))
  plan <- unserialize(serialize(plan, NULL, version = 3L))
  parsed_auth <- builder_auth_validate_payload(
    isTRUE(plan$app_auth$enabled),
    auth_accounts
  )
  if (
    !isTRUE(parsed_auth$ok) ||
      length(parsed_auth$accounts) != plan$app_auth$account_count
  ) {
    return(invisible(FALSE))
  }
  result(NULL)
  queued <- enqueue(list(
    kind = "build",
    plan = plan,
    auth_accounts = parsed_auth$accounts,
    note = paste0(
      "Building ",
      length(plan$items),
      " dataset",
      if (length(plan$items) == 1L) "" else "s",
      "…"
    )
  ))
  build_flow(list(
    stage = if (isTRUE(queued)) "building" else "idle",
    plan = NULL
  ))
  if (isTRUE(queued)) {
    auth_accounts_state(builder_auth_empty_accounts())
    auth_validation(list(
      ok = FALSE,
      error = "Set up login accounts again before the next build."
    ))
    session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
  }
  invisible(isTRUE(queued))
}

prepare_selected_output <- function(path, overwrite = FALSE) {
  isolate({
    plan <- freeze_plan_for_output(path, overwrite = overwrite)
    if (
      !inherits(plan, "builder_build_plan") ||
        !identical(plan$readiness, "ready")
    ) {
      build_flow(list(stage = "idle", plan = NULL))
      showNotification(
        plan$error %||% "The selected folder cannot be used.",
        type = "error",
        duration = 6
      )
      return(invisible(FALSE))
    }
    if (length(plan$existing_targets) && !isTRUE(overwrite)) {
      build_flow(list(stage = "conflict", plan = plan))
      session$sendCustomMessage(
        "builder_build_dialog",
        list(
          type = "conflict",
          title = "Files already exist",
          files = basename(plan$existing_targets)
        )
      )
      return(invisible(FALSE))
    }
    enqueue_build_plan(plan, auth_accounts = isolate(auth_accounts()))
  })
}

choose_build_folder <- function() {
  build_flow(list(stage = "choosing_folder", plan = NULL))
  session$onFlushed(
    function() {
      choice <- builder_choose_output_directory()
      if (identical(choice$status, "cancelled")) {
        build_flow(list(stage = "idle", plan = NULL))
        return()
      }
      if (!identical(choice$status, "selected")) {
        build_flow(list(stage = "idle", plan = NULL))
        showNotification(
          choice$error %||% "The folder picker could not be opened.",
          type = "error",
          duration = 6
        )
        return()
      }
      prepare_selected_output(choice$path)
    },
    once = TRUE
  )
}

observeEvent(input$build, {
  req(identical(isolate(build_flow())$stage, "idle"))
  plan <- isolate(frozen_review_plan())
  datasets <- isolate(sets())
  review_statuses <- lapply(
    datasets,
    builder_dataset_review_status,
    active = FALSE
  )
  attention <- which(vapply(
    review_statuses,
    function(status) identical(status$id, "needs-attention"),
    logical(1)
  ))
  unreviewed <- which(
    !vapply(datasets, builder_dataset_is_reviewed, logical(1))
  )
  if (length(attention)) {
    target <- datasets[[attention[[1L]]]]$id
    build_flow(list(
      stage = "attention_required",
      plan = NULL,
      target = target
    ))
    session$sendCustomMessage(
      "builder_build_dialog",
      list(
        type = "needs_attention",
        title = "Some datasets still need attention",
        names = vapply(
          datasets[attention],
          function(entry) {
            paste0(
              entry$settings$name,
              " — Resolve the highlighted settings."
            )
          },
          character(1)
        )
      )
    )
    return()
  }
  if (length(unreviewed)) {
    target <- datasets[[unreviewed[[1L]]]]$id
    build_flow(list(stage = "review_required", plan = NULL, target = target))
    session$sendCustomMessage(
      "builder_build_dialog",
      list(
        type = "unreviewed",
        title = "Some datasets have not been reviewed",
        names = vapply(
          datasets[unreviewed],
          function(entry) entry$settings$name,
          character(1)
        )
      )
    )
    return()
  }
  if (!builder_review_can_build(plan)) {
    showNotification(
      plan$error %||% "Resolve the highlighted settings before building.",
      type = "warning",
      duration = 6
    )
    return()
  }
  if (length(datasets) >= 2L) {
    build_flow(list(stage = "confirming", plan = NULL))
    session$sendCustomMessage(
      "builder_build_dialog",
      list(
        type = "datasets",
        title = "Ready to build all datasets?",
        count = length(datasets),
        names = vapply(
          datasets,
          function(entry) entry$settings$name %||% "Dataset",
          character(1)
        )
      )
    )
    return()
  }
  choose_build_folder()
})

observeEvent(input$builder_build_dialog, {
  action <- input$builder_build_dialog$action %||% "cancel"
  flow <- isolate(build_flow())
  if (identical(action, "continue") && identical(flow$stage, "confirming")) {
    choose_build_folder()
  } else if (
    identical(action, "replace") &&
      identical(flow$stage, "conflict") &&
      inherits(flow$plan, "builder_build_plan")
  ) {
    prepare_selected_output(flow$plan$out_dir, overwrite = TRUE)
  } else if (
    identical(action, "choose_another") &&
      identical(flow$stage, "conflict")
  ) {
    choose_build_folder()
  } else if (
    identical(action, "review_now") &&
      identical(flow$stage, "review_required")
  ) {
    current(flow$target)
    build_flow(list(stage = "idle", plan = NULL))
    focus_dataset_settings()
  } else if (
    identical(action, "fix_issues") &&
      identical(flow$stage, "attention_required")
  ) {
    current(flow$target)
    build_flow(list(stage = "idle", plan = NULL))
    focus_dataset_settings()
  } else {
    build_flow(list(stage = "idle", plan = NULL))
  }
})

validate_rail_removal <- function(next_state, id) {
  builder_validate_next_plan(
    next_state,
    out_dir = file.path(tempdir(), "cerebro-builder-output-preview"),
    make_app = isTRUE(isolate(input$make_app)),
    overwrite = FALSE
  )
}

remove_dataset <- function(
  previous_state,
  updated,
  id,
  validation
) {
  ids <- vapply(previous_state$datasets, `[[`, character(1), "id")
  entry <- previous_state$datasets[[match(id, ids)]]
  previous_removed <- previous_state$last_removed
  if (is.list(previous_removed)) {
    identity <- .builder_worker_identity(previous_removed$entry$snapshot)
    pending_drops <- isolate(pending_snapshot_drops())
    pending_drops[[previous_removed$id]] <- identity
    pending_snapshot_drops(pending_drops)
    queued <- enqueue(list(
      kind = "drop",
      id = previous_removed$id,
      dataset_revision = previous_removed$entry$revision %||% 0L,
      snapshot_identity = identity,
      note = "Releasing memory…"
    ))
    if (!isTRUE(queued)) {
      pending_drops[[previous_removed$id]] <- NULL
      pending_snapshot_drops(pending_drops)
    }
  }
  result(NULL)
  showNotification(
    tagList(
      paste0("Removed ", entry$settings$name, ". "),
      actionLink("undo_remove", "Undo")
    ),
    type = "message",
    duration = 10
  )
}

# observeEvent(input$drop_ds, ...) is owned by builder_dataset_rail_server().
rail_controller <- builder_dataset_rail_server(
  input = input,
  session = session,
  store = store,
  validate_remove = validate_rail_removal,
  on_select = function(id) {
    active_import_id(NULL)
    result(NULL)
  },
  on_remove = remove_dataset,
  on_undo = function() result(NULL),
  on_validation = function(validation) {
    if (isTRUE(validation$ok)) {
      add_error(NULL)
    } else if (!identical(validation$code, "confirmation_required")) {
      add_error(validation$message)
    }
  }
)

output$busy <- renderUI({
  note <- busy_note()
  if (is.null(note)) {
    return(NULL)
  }
  current_protocol <- protocol()
  build_phase <- current_protocol$build_status %||% "idle"
  pipeline <- if (identical(build_phase, "queued")) {
    builder_build_pipeline_ui("queued")
  } else if (identical(build_phase, "running")) {
    builder_build_pipeline_ui("building")
  } else {
    NULL
  }
  div(
    class = paste("busy", if (is.null(pipeline)) NULL else "is-building"),
    if (is.null(pipeline)) span(class = "spinner"),
    pipeline,
    span(note)
  )
})
