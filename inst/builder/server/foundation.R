## Builder server: foundation.

## Each entry: id, path, format, object, profile, settings.
## `settings` is what the user chose; it is written back whenever an input
## changes so switching between data sets does not lose it.
store <- reactiveVal(builder_state())
imports <- reactiveVal(builder_import_queue(max_active = 1L))
workflow <- reactiveVal(builder_workflow_state())
workflow_manual_navigation <- reactiveVal(FALSE)
selected_output <- reactiveVal(NULL)
active_import_id <- reactiveVal(NULL)
import_focus_id <- reactive(builder_import_focus_id(imports()))
client_import_server_ids <- reactiveVal(character())
released_client_imports <- reactiveVal(character())
released_client_import_records <- reactiveVal(list())
pending_client_import_dispatch <- reactiveVal(NULL)
pending_client_upload <- reactiveVal(NULL)
pending_client_upload_sequence <- reactiveVal(0L)
session_active <- TRUE
external_import_active <- reactiveVal(NULL)
client_import_history_limit <- 200L

client_import_id_for <- function(server_id) {
  ids <- isolate(client_import_server_ids())
  value <- if (builder_has_text(server_id) && server_id %in% names(ids)) {
    unname(ids[[server_id]])
  } else {
    character()
  }
  if (length(value) == 1L && !is.na(value) && nzchar(value)) value else NULL
}

replay_existing_client_import <- function(client_id) {
  if (!builder_has_text(client_id)) {
    return(invisible(FALSE))
  }
  record <- isolate(released_client_import_records())[[client_id]]
  if (is.list(record)) {
    session$sendCustomMessage(
      "builder_client_import_release",
      list(
        client_id = client_id,
        server_id = record$server_id,
        outcome = record$state,
        message = record$message
      )
    )
    return(invisible(TRUE))
  }
  ids <- isolate(client_import_server_ids())
  server_ids <- names(ids)[unname(ids) == client_id]
  if (!length(server_ids)) {
    return(invisible(FALSE))
  }
  session$sendCustomMessage(
    "builder_client_import_accepted",
    list(
      client_id = client_id,
      server_id = server_ids[[1L]],
      name = NULL,
      kind = NULL
    )
  )
  invisible(TRUE)
}

bind_client_import <- function(client_id, server_id, name, kind) {
  if (
    !builder_has_text(client_id) ||
      !builder_has_text(server_id) ||
      !builder_has_text(name) ||
      !kind %in% c("file", "example")
  ) {
    return(invisible(FALSE))
  }
  ids <- isolate(client_import_server_ids())
  existing_server_ids <- names(ids)[unname(ids) == client_id]
  if (length(existing_server_ids) && !server_id %in% existing_server_ids) {
    session$sendCustomMessage(
      "builder_client_import_accepted",
      list(
        client_id = client_id,
        server_id = existing_server_ids[[1L]],
        name = name,
        kind = kind
      )
    )
    return(invisible(FALSE))
  }
  ids[[server_id]] <- client_id
  client_import_server_ids(ids)
  session$sendCustomMessage(
    "builder_client_import_accepted",
    list(
      client_id = client_id,
      server_id = server_id,
      name = name,
      kind = kind
    )
  )
  invisible(TRUE)
}

release_client_import <- function(
  client_id,
  server_id = NULL,
  outcome,
  message = NULL
) {
  if (
    !builder_has_text(client_id) ||
      !builder_has_text(outcome) ||
      !outcome %in% c("ready", "error", "cancelled", "rejected")
  ) {
    return(invisible(FALSE))
  }
  released <- isolate(released_client_imports())
  if (client_id %in% released) {
    return(invisible(FALSE))
  }
  released <- c(released, client_id)
  records <- isolate(released_client_import_records())
  records[[client_id]] <- list(
    client_id = client_id,
    server_id = server_id,
    state = outcome,
    message = message
  )
  if (length(records) > client_import_history_limit) {
    keep <- tail(names(records), client_import_history_limit)
    records <- records[keep]
    released <- released[released %in% keep]
  }
  released_client_imports(released)
  released_client_import_records(records)
  ids <- isolate(client_import_server_ids())
  client_import_server_ids(ids[unname(ids) != client_id])
  session$sendCustomMessage(
    "builder_client_import_release",
    list(
      client_id = client_id,
      server_id = server_id,
      outcome = outcome,
      message = message
    )
  )
  invisible(TRUE)
}
example_directory_sent <- reactiveVal(NULL)
current_id <- reactiveVal(NULL)
update_current_id <- function(value) {
  if (!identical(value, isolate(current_id()))) {
    current_id(value)
  }
  invisible(value)
}
observe({
  update_current_id(store()$current_dataset)
})
observe({
  loaded <- store()$datasets %||% list()
  pending <- imports()$entries %||% list()
  state <- isolate(workflow())
  if (!length(loaded) && !length(pending)) {
    workflow_manual_navigation(FALSE)
    if (!is.null(isolate(selected_output()))) {
      selected_output(NULL)
    }
    if (!identical(state$stage, "upload") || !is.null(state$review_plan)) {
      workflow(builder_reduce_workflow(state, list(type = "empty")))
    }
  } else if (
    length(loaded) &&
      !length(pending) &&
      identical(state$stage, "upload") &&
      !isTRUE(isolate(workflow_manual_navigation()))
  ) {
    workflow(builder_reduce_workflow(state, list(type = "datasets_ready")))
  }
})
app_store_compat_entries <- function(state, datasets, mark = FALSE) {
  ids <- vapply(
    datasets,
    function(entry) {
      stopifnot(
        is.list(entry),
        builder_has_text(entry$id),
        is.list(entry$settings)
      )
      entry$id
    },
    character(1)
  )
  stopifnot(!anyDuplicated(ids))
  state$datasets <- datasets
  if (is.null(state$current_dataset) || !state$current_dataset %in% ids) {
    state["current_dataset"] <- list(if (length(ids)) ids[[1L]] else NULL)
  }
  state$revision <- as.integer(state$revision %||% 0L) + 1L
  if (isTRUE(mark)) {
    state$.state_only_fixture <- TRUE
  }
  structure(state, class = c("builder_state", "list"))
}
use_state_only_fixture <- function(datasets = list()) {
  fixture <- app_store_compat_entries(builder_state(), datasets, mark = TRUE)
  store(fixture)
  invisible(fixture)
}
sets <- function(value) {
  if (missing(value)) {
    return(store()$datasets)
  }
  current_state <- isolate(store())
  updated <- try(
    builder_reduce_state(
      current_state,
      list(type = "replace_all", datasets = value)
    ),
    silent = TRUE
  )
  if (inherits(updated, "try-error")) {
    if (!isTRUE(current_state$.state_only_fixture)) {
      stop(attr(updated, "condition"))
    }
    ## Explicit state-only fixtures may carry the legacy minimal records used
    ## by UI tests. The fixture mark must exist before this setter is called.
    updated <- app_store_compat_entries(
      current_state,
      value
    )
  }
  store(updated)
  invisible(value)
}
current <- function(value) {
  if (missing(value)) {
    return(current_id())
  }
  if (is.null(value)) {
    return(invisible(NULL))
  }
  updated <- builder_reduce_state(
    isolate(store()),
    list(type = "select", id = value)
  )
  store(updated)
  active_import_id(NULL)
  update_current_id(updated$current_dataset)
  invisible(value)
}
dataset_check_marks <- reactiveVal(character())
checked_dataset_ids <- reactive({
  entries <- sets()
  marks <- dataset_check_marks()
  ids <- vapply(entries, `[[`, character(1), "id")
  identities <- vapply(
    entries,
    function(entry) .builder_worker_identity(entry$snapshot),
    character(1)
  )
  matched <- ids %in% names(marks)
  matched[matched] <- unname(marks[ids[matched]]) == identities[matched]
  ids[matched]
})
all_datasets_checked <- reactive({
  ids <- vapply(sets(), `[[`, character(1), "id")
  length(ids) > 0L && all(ids %in% checked_dataset_ids())
})
observe({
  ids <- vapply(sets(), `[[`, character(1), "id")
  marks <- isolate(dataset_check_marks())
  kept <- marks[names(marks) %in% ids]
  if (!identical(kept, marks)) dataset_check_marks(kept)
})
result <- reactiveVal(NULL)
build_flow <- reactiveVal(list(stage = "idle", plan = NULL))
builder_build_controls_locked <- function(flow) {
  !is.list(flow) || !identical(flow$stage, "idle")
}
review_options <- reactiveVal(builder_review_options())
review_validation <- reactiveVal(list(ok = TRUE, error = NULL))
build_mode <- reactiveVal(FALSE)
build_initial_dataset <- reactiveVal(NULL)
auth_enabled <- reactiveVal(FALSE)
auth_accounts <- reactiveVal(builder_auth_empty_accounts())
auth_validation <- reactiveVal(list(ok = TRUE, error = NULL))
enhance_contract <- reactiveVal(list(
  id = NULL,
  organism = NULL,
  analysis_dependencies = character(),
  marker_import_ids = character()
))
review_page_contract <- reactiveVal(list(
  dataset = NULL,
  choices = c("Data info" = "data_info")
))
seq_id <- reactiveVal(0L)
add_error <- reactiveVal(NULL)
preview_frame <- reactiveVal(NULL)
projection_previews <- reactiveVal(list(dataset = NULL, frames = list()))
trajectory_previews <- reactiveVal(list(dataset = NULL, frames = list()))
projection_preview_contract <- reactiveVal(NULL)
trajectory_preview_contract <- reactiveVal(NULL)
spatial_coords <- reactiveVal(NULL)
alignment_preview <- reactiveVal(NULL)
marker_import_drafts <- reactiveVal(list())

marker_import_draft_of <- function(id) {
  marker_import_drafts()[[id]]
}

replace_marker_import_draft <- function(id, draft) {
  drafts <- isolate(marker_import_drafts())
  if (is.null(draft)) {
    drafts[[id]] <- NULL
  } else {
    drafts[[id]] <- draft
  }
  marker_import_drafts(drafts)
  invisible(draft)
}

entry_of <- function(id) {
  all <- sets()
  hit <- Filter(function(e) identical(e$id, id), all)
  if (length(hit)) hit[[1]] else NULL
}

observe({
  state <- store()
  id <- state$current_dataset
  entries <- state$datasets %||% list()
  index <- which(vapply(
    entries,
    function(entry) identical(entry$id, id),
    logical(1)
  ))
  entry <- if (length(index) == 1L) entries[[index]] else NULL
  next_contract <- list(
    id = id,
    organism = entry$settings$organism %||% NULL,
    analysis_dependencies = intersect(
      unname(entry$settings$analyses %||% character()),
      "marker_genes"
    ),
    marker_import_ids = names(entry$settings$marker_imports %||% list()) %||%
      character()
  )
  if (!identical(next_contract, isolate(enhance_contract()))) {
    enhance_contract(next_contract)
  }
})

observe({
  entries <- store()$datasets %||% list()
  entry <- if (length(entries)) entries[[1L]] else NULL
  state <- if (is.null(entry)) {
    NULL
  } else {
    try(builder_dataset_state(entry), silent = TRUE)
  }
  choices <- builder_review_initial_page_choices(
    if (is.null(state) || inherits(state, "try-error")) {
      list()
    } else {
      state$page_expectations %||% list()
    }
  )
  next_contract <- list(
    dataset = entry$id %||% NULL,
    choices = choices
  )
  if (!identical(next_contract, isolate(review_page_contract()))) {
    review_page_contract(next_contract)
  }
})

## Names end up as the app's dataset switcher labels, so duplicates are not
## cosmetic: they block the build. Loading the same example twice is an
## obvious thing to do, so suffix rather than refuse.
unique_name <- function(label) {
  taken <- vapply(sets(), function(e) e$settings$name, character(1))
  if (!label %in% taken) {
    return(label)
  }
  n <- 2L
  while (paste0(label, " ", n) %in% taken) {
    n <- n + 1L
  }
  paste0(label, " ", n)
}

replace_entry <- function(updated) {
  all <- isolate(sets())
  index <- which(vapply(
    all,
    function(entry) identical(entry$id, updated$id),
    logical(1)
  ))
  if (length(index) != 1L) {
    return(invisible(FALSE))
  }
  existing <- all[[index]]
  if (identical(existing$settings, updated$settings)) {
    return(invisible(FALSE))
  }
  existing$settings <- updated$settings
  existing$revision <- as.integer(existing$revision %||% 0L) + 1L
  current_state <- isolate(store())
  updated_state <- try(
    builder_reduce_state(
      current_state,
      list(type = "replace", id = existing$id, entry = existing)
    ),
    silent = TRUE
  )
  if (inherits(updated_state, "try-error")) {
    if (!isTRUE(current_state$.state_only_fixture)) {
      stop(attr(updated_state, "condition"))
    }
    all[[index]] <- existing
    updated_state <- app_store_compat_entries(current_state, all)
  }
  store(updated_state)
  current_protocol <- protocol()
  if (!is.null(current_protocol)) {
    protocol(builder_protocol_dataset(
      current_protocol,
      existing$id,
      existing$revision,
      .builder_worker_identity(existing$snapshot)
    ))
  }
  invisible(TRUE)
}

output$format_line <- renderText(builder_format_summary())
output$ds_count <- renderText({
  n <- length(store()$datasets) + length(imports()$entries)
  if (n == 0) "" else paste0(n)
})
output$rail_undo <- renderUI({
  if (!isTRUE(store()$can_undo_remove)) {
    return(NULL)
  }
  actionLink("undo_remove", "Undo remove")
})

## -- the worker process ---------------------------------------------------
## Objects live there, never here. The protocol owns queue order, request
## identity and acknowledgement barriers; the poller only applies a result
## after that identity has been validated.
worker <- reactiveVal(NULL)
worker_available <- reactiveVal(FALSE)
protocol <- reactiveVal(NULL)
build_state <- reactiveVal(builder_build_state())
active_release <- reactiveVal(NULL)
request_sequence <- reactiveVal(0L)
pending_snapshot_drops <- reactiveVal(list())
pending_sources <- reactiveVal(character())
pending_uploads <- reactiveVal(list())
cancelled_loads <- reactiveVal(character())
import_of <- function(id) {
  builder_import_find(imports(), id)
}
set_import_state <- function(id, state, generation, error = NULL) {
  updated <- try(
    builder_import_transition(
      isolate(imports()),
      id,
      state,
      generation,
      error = error
    ),
    silent = TRUE
  )
  if (inherits(updated, "try-error")) {
    return(invisible(FALSE))
  }
  imports(updated)
  if (state %in% c("ready", "error", "cancelled")) {
    entry <- builder_import_find(updated, id)
    client_id <- client_import_id_for(id)
    release_message <- if (identical(state, "error")) entry$error else NULL
    session$onFlushed(
      function() {
        release_client_import(
          client_id,
          server_id = id,
          outcome = state,
          message = release_message
        )
      },
      once = TRUE
    )
    if (identical(isolate(external_import_active()), id)) {
      external_import_active(NULL)
      session$sendCustomMessage(
        "builder_import_scheduler_state",
        list(active = FALSE, server_id = id)
      )
    }
  }
  invisible(TRUE)
}
forget_import <- function(id) {
  current_imports <- isolate(imports())
  if (is.null(builder_import_find(current_imports, id))) {
    return(invisible(FALSE))
  }
  imports(builder_import_remove(current_imports, id))
  if (identical(isolate(active_import_id()), id)) {
    active_import_id(NULL)
  }
  invisible(TRUE)
}
release_pending_source <- function(payload, drop_import = TRUE) {
  if (!is.list(payload) || !identical(payload$kind, "load")) {
    return(invisible(FALSE))
  }
  value <- if (identical(payload$source, "file")) {
    payload$path
  } else {
    payload$example
  }
  key <- builder_source_key(payload$source, value)
  pending_sources(builder_source_release(pending_sources(), key))
  id <- as.character(payload$id %||% character())
  if (length(id) == 1L && !is.na(id) && nzchar(id)) {
    if (id %in% isolate(cancelled_loads())) {
      release_client_import(
        client_import_id_for(id),
        server_id = id,
        outcome = "cancelled"
      )
    }
    if (isTRUE(drop_import)) {
      forget_import(id)
    }
    builder_import_progress_remove(payload$progress_path %||% "")
    uploads <- pending_uploads()
    uploads[[id]] <- NULL
    pending_uploads(uploads)
    cancelled_loads(setdiff(cancelled_loads(), id))
  }
  invisible(TRUE)
}
enqueue <- function(req) {
  current_protocol <- protocol()
  if (is.null(current_protocol) || !isTRUE(worker_available())) {
    add_error("The background worker is not ready yet.")
    return(invisible(FALSE))
  }
  dataset <- req$id %||% "session"
  entry <- entry_of(dataset)
  revision <- if (!is.null(req$dataset_revision)) {
    as.integer(req$dataset_revision)
  } else if (is.null(entry)) {
    0L
  } else {
    as.integer(entry$revision %||% 0L)
  }
  snapshot_identity <- req$snapshot_identity %||%
    if (is.null(entry)) {
      NULL
    } else {
      .builder_worker_identity(entry$snapshot)
    }
  if (!is.null(entry)) {
    current_protocol <- builder_protocol_dataset(
      current_protocol,
      dataset,
      revision,
      snapshot_identity
    )
  }
  replaceable <- req$kind %in%
    c(
      "preview",
      "projection_previews",
      "trajectory_previews",
      "coords",
      "spatial_preview"
    )
  if (replaceable) {
    request_sequence(request_sequence() + 1L)
    if (builder_preview_revision_independent(req$kind)) {
      req$revision_independent <- TRUE
    }
    request <- builder_query(
      kind = req$kind,
      dataset = dataset,
      generation = request_sequence(),
      slot = req$replaces %||% req$kind,
      payload = req,
      revision = revision,
      snapshot_identity = snapshot_identity
    )
  } else {
    if (identical(req$kind, "build") && is.null(req$id)) {
      request_sequence(request_sequence() + 1L)
      req$id <- paste0("build-", request_sequence())
    }
    request <- builder_command(
      kind = req$kind,
      dataset = dataset,
      payload = req,
      revision = revision,
      snapshot_identity = snapshot_identity
    )
  }
  queued <- try(builder_enqueue(current_protocol, request), silent = TRUE)
  if (inherits(queued, "try-error")) {
    add_error(conditionMessage(attr(queued, "condition")))
    return(invisible(FALSE))
  }
  protocol(queued)
  invisible(TRUE)
}
busy_note <- reactiveVal(NULL)

update_build_state <- function(action) {
  updated <- try(builder_reduce_build(build_state(), action), silent = TRUE)
  if (inherits(updated, "try-error")) {
    add_error(conditionMessage(attr(updated, "condition")))
    return(invisible(FALSE))
  }
  build_state(updated)
  invisible(TRUE)
}

abort_release_result <- function(release, reason) {
  if (is.null(release)) {
    return(builder_result_failure(reason))
  }
  try(builder_coordinator_abort(release$handle), silent = TRUE)
  builder_release_error_result(reason, release$handle$target)
}

settle_failed_builds <- function(recovery, reason) {
  failed <- Filter(
    function(request) identical(request$kind, "build"),
    recovery$failed %||% list()
  )
  for (request in failed) {
    release <- isolate(active_release())
    release_result <- builder_result_failure(reason)
    if (!is.null(release) && identical(release$id, request$build_id)) {
      release_result <- abort_release_result(release, reason)
      active_release(NULL)
    }
    state <- build_state()
    build_id <- request$build_id
    if (is.null(build_id) || !nzchar(build_id)) {
      next
    }
    if (
      !state$status %in% c("running", "cancelling") ||
        !identical(state$id, build_id)
    ) {
      started <- try(
        builder_reduce_build(
          state,
          list(type = "start", id = build_id, revision = 0L)
        ),
        silent = TRUE
      )
      if (inherits(started, "try-error")) {
        next
      }
      build_state(started)
      state <- started
    }
    action <- if (identical(state$status, "cancelling")) {
      list(type = "cancelled", id = build_id)
    } else {
      list(type = "fail", id = build_id, error = reason)
    }
    update_build_state(action)
    result(release_result)
  }
}

apply_protocol_recovery <- function(
  current_protocol,
  recovered_worker,
  reason,
  retry_persistent,
  error = NULL
) {
  recovered <- try(
    builder_protocol_recover(
      current_protocol,
      epoch = if (isTRUE(retry_persistent)) {
        recovered_worker$epoch
      } else {
        .builder_worker_epoch()
      },
      reason = reason,
      retry_persistent = retry_persistent
    ),
    silent = TRUE
  )
  busy_note(NULL)
  if (inherits(recovered, "try-error")) {
    protocol(NULL)
    worker_available(FALSE)
    add_error(paste0(
      "The worker protocol could not be recovered. Restart this Builder session. ",
      conditionMessage(attr(recovered, "condition"))
    ))
    return(invisible(FALSE))
  }
  worker(recovered_worker)
  worker_available(isTRUE(retry_persistent))
  protocol(recovered$protocol)
  settle_failed_builds(recovered, reason)
  failed_requests <- recovered$failed %||% list()
  invisible(lapply(failed_requests, function(request) {
    payload <- request$payload
    if (identical(payload$kind, "load")) {
      entry <- isolate(import_of(payload$id))
      if (!is.null(entry)) {
        set_import_state(
          payload$id,
          "error",
          payload$import_generation %||% entry$generation,
          reason
        )
        release_pending_source(payload, drop_import = FALSE)
        return(invisible(TRUE))
      }
    }
    release_pending_source(payload)
  }))
  retried_builds <- Filter(
    function(request) identical(request$kind, "build"),
    recovered$retried %||% list()
  )
  if (length(retried_builds)) {
    release <- isolate(active_release())
    if (!is.null(release)) {
      try(builder_coordinator_abort(release$handle), silent = TRUE)
      active_release(NULL)
    }
  }

  if (!is.null(error)) {
    has_failed_load <- any(vapply(
      failed_requests,
      function(request) identical(request$payload$kind, "load"),
      logical(1)
    ))
    public_error <- if (has_failed_load) {
      builder_import_public_error(error)
    } else {
      error
    }
    add_error(paste0(
      public_error,
      " No queued action was left pending; restart this Builder session."
    ))
    return(invisible(FALSE))
  }
  retried <- length(recovered$retried %||% list())
  failed <- length(recovered$failed %||% list())
  discarded <- length(recovered$discarded %||% list())
  detail <- c(
    if (retried) paste(retried, "persistent action(s) will resume"),
    if (failed) paste(failed, "action(s), including any Build, were stopped"),
    if (discarded) {
      paste(discarded, "obsolete preview request(s) were discarded")
    }
  )
  add_error(paste0(
    "The background worker restarted from immutable snapshots.",
    if (length(detail)) {
      paste0(" ", paste(detail, collapse = "; "), ".")
    } else {
      ""
    }
  ))
  invisible(TRUE)
}

restart_worker_protocol <- function(
  current_worker,
  current_protocol,
  reason
) {
  restarted <- try(builder_worker_restart(current_worker), silent = TRUE)
  typed_failure <- inherits(restarted, "builder_worker_restart_failed") ||
    (is.list(restarted) && identical(restarted$event, "restart_failed"))
  if (inherits(restarted, "try-error") || typed_failure) {
    failed_worker <- if (
      is.list(restarted) &&
        inherits(restarted$worker, "builder_worker")
    ) {
      restarted$worker
    } else {
      current_worker
    }
    restart_error <- if (inherits(restarted, "try-error")) {
      conditionMessage(attr(restarted, "condition"))
    } else {
      restarted$error %||% "The background worker could not restart."
    }
    return(apply_protocol_recovery(
      current_protocol,
      failed_worker,
      reason,
      retry_persistent = FALSE,
      error = restart_error
    ))
  }
  apply_protocol_recovery(
    current_protocol,
    restarted,
    reason,
    retry_persistent = TRUE
  )
}

observe({
  if (!is.null(worker())) {
    return()
  }
  started <- builder_session_start(getwd())
  if (!is.null(started$error)) {
    add_error(started$error)
    return()
  }
  worker(started$worker)
  worker_available(TRUE)
  protocol(builder_request_protocol(started$worker$epoch))
})

session$onSessionEnded(function() {
  session_active <<- FALSE
  current_worker <- isolate(worker())
  if (!is.null(current_worker)) {
    stopped <- try(
      builder_worker_stop(current_worker, grace_ms = 5000L),
      silent = TRUE
    )
    if (inherits(stopped, "try-error") || !isTRUE(stopped$stopped)) {
      return()
    }
    if (!isTRUE(stopped$worker$cleanup_safe)) {
      return()
    }
    current_worker <- stopped$worker
    released_all <- TRUE
    released_identities <- character()
    for (snapshot in current_worker$snapshot_registry) {
      identity <- .builder_worker_identity(snapshot)
      if (identity %in% released_identities) {
        next
      }
      released <- try(.builder_snapshot_release(snapshot), silent = TRUE)
      released_all <- released_all && isTRUE(released)
      if (isTRUE(released)) {
        released_identities <- c(released_identities, identity)
      }
    }
    builder_import_progress_cleanup(current_worker$snapshot_root)
    remaining <- list.files(
      current_worker$snapshot_root,
      all.files = TRUE,
      no.. = TRUE
    )
    if (
      isTRUE(current_worker$owns_root) &&
        released_all &&
        !length(remaining)
    ) {
      unlink(current_worker$snapshot_root, recursive = TRUE, force = TRUE)
    }
  }
  release <- isolate(active_release())
  if (!is.null(release)) {
    try(builder_coordinator_abort(release$handle), silent = TRUE)
    active_release(NULL)
  }
})
