## Builder server: imports.

dataset_mutations_locked <- function(notify = TRUE) {
  if (exists("builder_operation_allowed", mode = "function", inherits = TRUE)) {
    return(
      !isTRUE(builder_operation_allowed(
        "mutate_datasets",
        notify = notify
      ))
    )
  }
  locked <- builder_mutations_locked(
    isolate(build_flow()),
    isolate(protocol())
  )
  if (isTRUE(locked) && isTRUE(notify)) {
    showNotification(
      "Wait for the active build to finish before changing datasets.",
      type = "warning",
      duration = 6
    )
  }
  isTRUE(locked)
}

## -- native file picker and examples --------------------------------------
## An example already on the list is not an offer any more. Which ones are
## taken is derived state, so it is pushed rather than re-rendered.
observe({
  directory <- builder_example_directory_state(sets(), imports())
  if (identical(directory, isolate(example_directory_sent()))) {
    return()
  }
  example_directory_sent(directory)
  session$sendCustomMessage(
    "builder_used_examples",
    directory
  )
})

start_load <- function(
  kind,
  arg,
  label,
  file_meta = NULL,
  client_id = NULL,
  dataset_id = NULL,
  source_origin = NULL,
  example_id = NULL
) {
  if (isTRUE(replay_existing_client_import(client_id))) {
    return(invisible(FALSE))
  }
  restoring_source <- !is.null(dataset_id) &&
    exists("builder_project_pending_entries", inherits = TRUE) &&
    dataset_id %in% names(isolate(builder_project_pending_entries()))
  if (!isTRUE(restoring_source) && dataset_mutations_locked()) {
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "Dataset changes are locked while the active build finishes."
    )
    return(invisible(FALSE))
  }
  rs <- worker()
  if (is.null(rs)) {
    add_error("The background worker is not ready yet.")
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "The background worker is not ready yet."
    )
    return(invisible(FALSE))
  }
  reservation_entries <- sets()
  if (isTRUE(restoring_source)) {
    reservation_entries <- Filter(
      function(entry) !identical(entry$id, dataset_id),
      reservation_entries
    )
  }
  reservation <- builder_source_reserve(
    reservation_entries,
    pending_sources(),
    kind,
    arg
  )
  if (!isTRUE(reservation$ok)) {
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "This dataset has already been added or is already waiting."
    )
    return(invisible(FALSE))
  }
  pending_sources(reservation$pending)
  add_error(NULL)
  existing_ids <- c(
    vapply(sets(), `[[`, character(1), "id"),
    vapply(imports()$entries %||% list(), `[[`, character(1), "id")
  )
  allocation <- builder_project_allocate_dataset_id(
    seq_id(),
    existing_ids,
    restored_id = dataset_id
  )
  seq_id(allocation$sequence)
  id <- allocation$id
  filename <- NULL
  file_type <- NULL
  file_size <- NA_real_
  retained_source <- NULL
  if (identical(kind, "file") && is.list(file_meta)) {
    uploads <- pending_uploads()
    filename <- builder_safe_file_name(file_meta$name, paste0(label, ".rds"))
    file_type <- builder_file_type_label(filename, file_meta$type)
    file_size <- suppressWarnings(as.numeric(file_meta$size %||% NA_real_))
    uploads[[id]] <- list(
      id = id,
      filename = filename,
      type = file_type,
      size = file_size,
      visible = TRUE
    )
    pending_uploads(uploads)
  } else if (identical(kind, "example")) {
    example_source <- try(
      builder_project_example_source(arg, builder_example_catalog()),
      silent = TRUE
    )
    if (!inherits(example_source, "try-error")) {
      filename <- example_source$filename
      file_size <- suppressWarnings(as.numeric(
        file.info(example_source$path)$size[[1L]]
      ))
      retained_source <- example_source$path
    }
  }
  generation <- 1L
  progress_path <- NULL
  snapshot_root <- rs$snapshot_root %||% NULL
  if (
    is.character(snapshot_root) &&
      length(snapshot_root) == 1L &&
      !is.na(snapshot_root) &&
      dir.exists(snapshot_root)
  ) {
    candidate <- try(
      builder_import_progress_path(snapshot_root, id, generation),
      silent = TRUE
    )
    if (!inherits(candidate, "try-error")) {
      progress_path <- candidate
    }
  }
  retain_example <- identical(kind, "example") &&
    builder_has_text(retained_source) &&
    builder_has_text(snapshot_root) &&
    dir.exists(snapshot_root)
  if (identical(kind, "file") || retain_example) {
    retained <- try(
      builder_project_retain_session_source(
        retained_source %||% arg,
        filename,
        snapshot_root,
        id
      ),
      silent = TRUE
    )
    if (inherits(retained, "try-error")) {
      pending_sources(builder_source_release(
        pending_sources(),
        reservation$key
      ))
      uploads <- pending_uploads()
      uploads[[id]] <- NULL
      pending_uploads(uploads)
      release_client_import(
        client_id,
        outcome = "error",
        message = "The uploaded source could not be retained for this session."
      )
      add_error("The uploaded source could not be retained for this session.")
      return(invisible(FALSE))
    }
    retained_source <- retained
    if (identical(kind, "file")) {
      arg <- retained
    }
  }
  source_descriptor <- list(
    kind = kind,
    origin = source_origin %||%
      if (identical(kind, "example")) {
        "example"
      } else {
        "upload"
      },
    staged_path = retained_source,
    example = example_id %||% if (identical(kind, "example")) arg else NULL,
    reservation_key = reservation$key,
    fingerprint = if (identical(kind, "example")) {
      paste0("example:", arg, ":builder-profile-v1")
    } else {
      info <- suppressWarnings(file.info(arg))
      paste(
        suppressWarnings(as.numeric(info$size[[1L]] %||% file_size)),
        suppressWarnings(as.numeric(info$mtime[[1L]] %||% NA_real_)),
        sep = ":"
      )
    }
  )
  pending_entry <- builder_import_entry(
    id = id,
    label = label,
    source = source_descriptor,
    filename = filename,
    file_type = file_type,
    size = file_size,
    generation = generation
  )
  imports(builder_import_add(isolate(imports()), pending_entry))
  next_focus <- builder_import_auto_focus(
    isolate(current()),
    isolate(active_import_id()),
    id
  )
  if (!is.null(next_focus)) {
    active_import_id(next_focus)
  }
  if (builder_has_text(client_id)) {
    bind_client_import(client_id, id, filename %||% label, kind)
  }
  queued <- enqueue(list(
    kind = "load",
    source = kind,
    id = id,
    path = if (identical(kind, "file")) arg else NA_character_,
    retained_path = retained_source,
    example = example_id %||% if (identical(kind, "example")) arg else NULL,
    source_origin = source_descriptor$origin,
    label = label,
    filename = filename,
    reservation_key = reservation$key,
    import_generation = generation,
    progress_path = progress_path,
    note = paste0("Loading ", label, "…")
  ))
  if (!isTRUE(queued)) {
    pending_sources(builder_source_release(
      pending_sources(),
      reservation$key
    ))
    uploads <- pending_uploads()
    uploads[[id]] <- NULL
    pending_uploads(uploads)
    forget_import(id)
    builder_import_progress_remove(progress_path %||% "")
    release_client_import(
      client_id,
      server_id = NULL,
      outcome = "error",
      message = "The background worker is not ready yet."
    )
  }
  invisible(isTRUE(queued))
}

process_client_import_upload <- function(uploads, dispatch) {
  pending_client_id <- if (is.list(dispatch)) dispatch$client_id else NULL
  if (
    !is.data.frame(uploads) ||
      nrow(uploads) != 1L ||
      !all(c("name", "datapath") %in% names(uploads))
  ) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "Expected exactly one uploaded file."
    )
    return()
  }
  path <- as.character(uploads$datapath[[1L]])
  label <- as.character(uploads$name[[1L]])
  size <- if ("size" %in% names(uploads)) {
    suppressWarnings(as.numeric(uploads$size[[1L]]))
  } else {
    NA_real_
  }
  type <- if ("type" %in% names(uploads)) {
    as.character(uploads$type[[1L]])
  } else {
    ""
  }
  metadata_matches <- is.list(dispatch) &&
    builder_has_text(pending_client_id) &&
    identical(dispatch$name, label) &&
    (is.na(dispatch$size) ||
      is.na(size) ||
      identical(as.numeric(dispatch$size), as.numeric(size)))
  if (
    !metadata_matches || !builder_has_text(path) || !builder_has_text(label)
  ) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "The uploaded file did not match its dispatch metadata."
    )
    return()
  }
  supported_extensions <- unique(tolower(unlist(lapply(
    builder_formats,
    `[[`,
    "extensions"
  ))))
  if (!tolower(tools::file_ext(label)) %in% supported_extensions) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "This file format is not supported."
    )
    return()
  }
  start_load(
    "file",
    path,
    tools::file_path_sans_ext(basename(label)),
    file_meta = list(name = label, type = type, size = size),
    client_id = pending_client_id
  )
  invisible(TRUE)
}

consume_client_import_upload <- function() {
  dispatch <- isolate(pending_client_import_dispatch())
  upload <- isolate(pending_client_upload())
  if (is.null(dispatch) || is.null(upload)) {
    return(invisible(FALSE))
  }
  pending_client_import_dispatch(NULL)
  pending_client_upload(NULL)
  process_client_import_upload(upload$files, dispatch)
}

expire_pending_client_upload <- function(token) {
  if (builder_session_closed()) {
    return(invisible(FALSE))
  }
  upload <- isolate(pending_client_upload())
  dispatch <- isolate(pending_client_import_dispatch())
  if (
    is.null(upload) ||
      !identical(upload$token, token) ||
      !is.null(dispatch)
  ) {
    return(invisible(FALSE))
  }
  pending_client_upload(NULL)
  invisible(TRUE)
}

observeEvent(input$builder_client_import_dispatch, {
  event <- input$builder_client_import_dispatch
  if (!is.list(event) || is.object(event)) {
    pending_client_import_dispatch(NULL)
    return()
  }
  pending_client_import_dispatch(list(
    client_id = event$client_id %||% NULL,
    name = event$name %||% NULL,
    size = suppressWarnings(as.numeric(event$size %||% NA_real_)[1L]),
    token = event$nonce %||% NULL
  ))
  consumed <- consume_client_import_upload()
  if (!isTRUE(consumed)) {
    current <- isolate(pending_client_import_dispatch())
    if (is.list(current) && identical(current$token, event$nonce %||% NULL)) {
      session$sendCustomMessage(
        "builder_client_import_dispatch_ready",
        list(client_id = current$client_id)
      )
    }
  }
  token <- event$nonce %||% NULL
  later::later(
    function() {
      if (builder_session_closed()) {
        return(invisible(FALSE))
      }
      dispatch <- isolate(pending_client_import_dispatch())
      if (is.null(dispatch) || !identical(dispatch$token, token)) {
        return()
      }
      pending_client_import_dispatch(NULL)
      release_client_import(
        dispatch$client_id,
        outcome = "rejected",
        message = "The file upload was not received in time."
      )
    },
    delay = 30
  )
})

observeEvent(input$dataset_files, {
  token <- isolate(pending_client_upload_sequence()) + 1L
  pending_client_upload_sequence(token)
  pending_client_upload(list(
    files = input$dataset_files,
    received = Sys.time(),
    token = token
  ))
  consume_client_import_upload()
  later::later(
    function() expire_pending_client_upload(token),
    delay = 30
  )
})

observeEvent(input$builder_import_example, {
  event <- input$builder_import_example
  client_id <- if (is.list(event) && !is.object(event)) {
    event$client_id
  } else {
    NULL
  }
  example_id <- if (is.list(event) && !is.object(event)) event$example else NULL
  ex <- Filter(
    function(e) identical(e$id, example_id),
    builder_examples()
  )
  if (!builder_has_text(client_id) || !length(ex)) {
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "The selected example is unavailable."
    )
    return()
  }
  start_load(
    "example",
    ex[[1L]]$id,
    ex[[1L]]$label,
    client_id = client_id
  )
})

observeEvent(input$builder_import_sync_request, {
  ids <- isolate(client_import_server_ids())
  queue <- isolate(imports())
  active <- lapply(names(ids), function(server_id) {
    entry <- builder_import_find(queue, server_id)
    if (is.null(entry)) {
      return(NULL)
    }
    list(
      client_id = unname(ids[[server_id]]),
      server_id = server_id,
      state = entry$load_state,
      message = entry$error
    )
  })
  active <- Filter(Negate(is.null), active)
  terminal <- unname(isolate(released_client_import_records()))
  dispatch <- isolate(pending_client_import_dispatch())
  if (is.list(dispatch) && builder_has_text(dispatch$client_id)) {
    active <- c(
      active,
      list(list(
        client_id = dispatch$client_id,
        server_id = NULL,
        state = "awaiting_upload",
        message = NULL
      ))
    )
  }
  session$sendCustomMessage(
    "builder_import_sync",
    list(
      imports = unname(c(active, terminal)),
      server_busy = builder_has_text(isolate(external_import_active()))
    )
  )
})

remove_pending_import <- function(id) {
  entry <- isolate(import_of(id))
  if (
    !is.character(id) ||
      length(id) != 1L ||
      is.na(id) ||
      !nzchar(id) ||
      is.null(entry)
  ) {
    return(invisible(FALSE))
  }
  current_protocol <- isolate(protocol())
  if (is.null(current_protocol)) {
    return(invisible(FALSE))
  }
  belongs <- function(request) {
    !is.null(request) &&
      identical(request$dataset, id) &&
      identical(request$kind, "load")
  }
  running <- belongs(current_protocol$pending) ||
    any(vapply(current_protocol$awaiting_ack, belongs, logical(1)))
  if (running) {
    uploads <- isolate(pending_uploads())
    if (!is.null(uploads[[id]])) {
      uploads[[id]]$visible <- FALSE
      pending_uploads(uploads)
    }
    cancelled_loads(unique(c(cancelled_loads(), id)))
    forget_import(id)
    return(invisible(TRUE))
  }
  if (identical(entry$load_state, "error")) {
    release_pending_source(list(
      kind = "load",
      source = entry$source$kind,
      id = entry$id,
      path = entry$source$staged_path,
      example = entry$source$example
    ))
    return(invisible(TRUE))
  }
  forgotten <- try(
    builder_protocol_forget_dataset(
      current_protocol,
      id,
      reason = "upload_cancelled"
    ),
    silent = TRUE
  )
  if (inherits(forgotten, "try-error")) {
    add_error("This file could not be removed while it was loading.")
    return(invisible(FALSE))
  }
  removed <- c(forgotten$failed, forgotten$discarded)
  if (!length(removed)) {
    return(invisible(FALSE))
  }
  protocol(forgotten$protocol)
  invisible(lapply(removed, function(request) {
    release_client_import(
      client_import_id_for(request$payload$id),
      server_id = request$payload$id,
      outcome = "cancelled"
    )
    release_pending_source(request$payload)
  }))
  invisible(TRUE)
}

observeEvent(input$cancel_pending_upload, {
  event <- input$cancel_pending_upload
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  remove_pending_import(id)
})

observeEvent(input$remove_import, {
  event <- input$remove_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  remove_pending_import(id)
})

observeEvent(input$pick_import, {
  event <- input$pick_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    event
  }
  if (
    is.character(id) &&
      length(id) == 1L &&
      !is.na(id) &&
      !is.null(isolate(import_of(id)))
  ) {
    active_import_id(id)
  }
})

observeEvent(input$retry_import, {
  event <- input$retry_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  entry <- if (is.character(id) && length(id) == 1L) {
    isolate(import_of(id))
  } else {
    NULL
  }
  if (is.null(entry) || !identical(entry$load_state, "error")) {
    return()
  }
  next_queue <- builder_import_retry(isolate(imports()), id)
  entry <- builder_import_find(next_queue, id)
  current_worker <- isolate(worker())
  progress_path <- NULL
  if (
    is.list(current_worker) &&
      is.character(current_worker$snapshot_root) &&
      length(current_worker$snapshot_root) == 1L &&
      dir.exists(current_worker$snapshot_root)
  ) {
    candidate <- try(
      builder_import_progress_path(
        current_worker$snapshot_root,
        id,
        entry$generation
      ),
      silent = TRUE
    )
    if (!inherits(candidate, "try-error")) {
      progress_path <- candidate
    }
  }
  imports(next_queue)
  active_import_id(id)
  external_import_active(id)
  session$sendCustomMessage(
    "builder_import_scheduler_state",
    list(active = TRUE, server_id = id)
  )
  cancelled_loads(setdiff(cancelled_loads(), id))
  queued <- enqueue(list(
    kind = "load",
    source = entry$source$kind,
    id = entry$id,
    path = entry$source$staged_path,
    retained_path = entry$source$staged_path,
    example = entry$source$example,
    source_origin = entry$source$origin,
    label = entry$label,
    filename = entry$filename,
    import_generation = entry$generation,
    progress_path = progress_path,
    note = paste0("Loading ", entry$label, "…")
  ))
  if (!isTRUE(queued)) {
    external_import_active(NULL)
    session$sendCustomMessage(
      "builder_import_scheduler_state",
      list(active = FALSE, server_id = id)
    )
    set_import_state(
      id,
      "error",
      entry$generation,
      "The background worker is not ready yet."
    )
  }
})

## -- dispatcher: send the next request when the worker is free ----------
observe({
  current_protocol <- protocol()
  if (is.null(current_protocol)) {
    return()
  }
  dispatched <- builder_protocol_dispatch(current_protocol)
  if (is.null(dispatched$request)) {
    return()
  }
  request <- dispatched$request
  nxt <- request$payload
  coordinator <- NULL
  auth_material <- NULL
  build_auth_accounts <- NULL
  handed_off <- FALSE
  if (identical(nxt$kind, "build")) {
    build_auth_accounts <- nxt$auth_accounts
    nxt$auth_accounts <- NULL
    request$payload$auth_accounts <- NULL
    dispatched$request <- builder_request_redact_auth(dispatched$request)
    dispatched$protocol <- builder_protocol_redact_auth(dispatched$protocol)
    current_protocol <- builder_protocol_redact_auth(current_protocol)
    on.exit(
      {
        build_auth_accounts <- NULL
        auth_material <- NULL
        current_protocol <- NULL
        dispatched$request <- NULL
        nxt$auth_accounts <- NULL
        request$payload$auth_accounts <- NULL
        latest_protocol <- isolate(protocol())
        if (!is.null(latest_protocol)) {
          protocol(builder_protocol_redact_auth(latest_protocol))
        }
        if (!is.null(coordinator) && !handed_off) {
          try(builder_coordinator_abort(coordinator), silent = TRUE)
        }
      },
      add = TRUE
    )
    protocol(dispatched$protocol)
  } else {
    protocol(dispatched$protocol)
  }
  current_worker <- worker()
  req(current_worker)
  if (identical(nxt$kind, "load")) {
    set_import_state(
      nxt$id,
      "reading",
      nxt$import_generation %||% 1L
    )
  }
  if (identical(nxt$kind, "build")) {
    # Legacy prohibition: never use `plan <- builder_make_plan` here.
    plan <- nxt$plan
    plan_error <- plan$error
    if (
      is.null(plan_error) &&
        length(plan$existing_targets) &&
        !isTRUE(plan$overwrite)
    ) {
      plan_error <- paste0(
        "These outputs already exist: ",
        paste(basename(plan$existing_targets), collapse = ", "),
        ". Choose another folder or replace the matching files."
      )
    }
    if (!is.null(plan_error)) {
      completed <- builder_protocol_complete(
        dispatched$protocol,
        builder_worker_response(
          request,
          list(error = plan_error)
        )
      )
      result(builder_result_failure(plan_error))
      protocol(builder_protocol_acknowledge(
        completed$protocol,
        request$request_id
      ))
      busy_note(NULL)
      return()
    }
    coordinator <- try(
      builder_coordinator_prepare(plan, request$build_id),
      silent = TRUE
    )
    if (inherits(coordinator, "try-error")) {
      plan_error <- conditionMessage(attr(coordinator, "condition"))
      completed <- builder_protocol_complete(
        dispatched$protocol,
        builder_worker_response(request, list(error = plan_error))
      )
      result(builder_release_error_result(
        plan_error,
        plan$output_release$directory
      ))
      protocol(builder_protocol_acknowledge(
        completed$protocol,
        request$request_id
      ))
      busy_note(NULL)
      return()
    }
    if (isTRUE(plan$app_auth$enabled)) {
      auth_material <- try(
        builder_auth_create_material(build_auth_accounts, coordinator$stage),
        silent = TRUE
      )
      build_auth_accounts <- NULL
      if (inherits(auth_material, "try-error")) {
        plan_error <- conditionMessage(attr(auth_material, "condition"))
        completed <- builder_protocol_complete(
          dispatched$protocol,
          builder_worker_response(request, list(error = plan_error))
        )
        protocol(builder_protocol_acknowledge(
          completed$protocol,
          request$request_id
        ))
        try(builder_coordinator_abort(coordinator), silent = TRUE)
        result(builder_release_error_result(
          plan_error,
          plan$output_release$directory
        ))
        busy_note(NULL)
        return()
      }
    }
    build_auth_accounts <- NULL
    nxt$plan <- plan
  }
  started_call <- try(
    switch(
      nxt$kind,
      load = if (identical(nxt$source, "file")) {
        builder_session_load(
          current_worker,
          nxt$id,
          nxt$path,
          request,
          progress_path = nxt$progress_path,
          import_generation = nxt$import_generation %||% 1L
        )
      } else {
        builder_session_example(
          current_worker,
          nxt$id,
          nxt$example,
          request,
          progress_path = nxt$progress_path,
          import_generation = nxt$import_generation %||% 1L
        )
      },
      preview = builder_session_preview(
        current_worker,
        nxt$id,
        nxt$reduction,
        nxt$group,
        BUILDER_PREVIEW_MAX,
        request
      ),
      projection_previews = builder_session_projection_previews(
        current_worker,
        nxt$id,
        nxt$projections,
        nxt$group,
        nxt$max_cells %||% 600L,
        request
      ),
      trajectory_previews = builder_session_trajectory_previews(
        current_worker,
        nxt$id,
        nxt$trajectories,
        nxt$max_cells %||% 600L,
        request
      ),
      coords = builder_session_coords(
        current_worker,
        nxt$id,
        nxt$image,
        request
      ),
      spatial_preview = builder_session_spatial_preview(
        current_worker,
        nxt$id,
        nxt$default_projection,
        nxt$group,
        nxt$section,
        nxt$assay,
        nxt$layer,
        nxt$coordinate_transforms,
        4000L,
        request
      ),
      align_all = builder_session_section_bounds(
        current_worker,
        nxt$id,
        nxt$sections,
        nxt$mode,
        nxt$extent_width,
        nxt$extent_height,
        nxt$um_per_px,
        nxt$dx,
        nxt$dy,
        nxt$scale,
        request
      ),
      build = builder_session_build(
        current_worker,
        nxt$plan,
        request,
        coordinator = coordinator,
        auth_material = auth_material
      ),
      drop = builder_session_drop(current_worker, nxt$id, request)
    ),
    silent = TRUE
  )
  if (inherits(started_call, "try-error")) {
    if (identical(nxt$kind, "build")) {
      release <- isolate(active_release())
      if (!is.null(release)) {
        try(builder_coordinator_abort(release$handle), silent = TRUE)
        active_release(NULL)
      }
    }
    dispatch_error <- conditionMessage(attr(started_call, "condition"))
    if (identical(nxt$kind, "load")) {
      dispatch_error <- builder_import_public_error(
        dispatch_error,
        nxt$path %||% character()
      )
    }
    restart_worker_protocol(
      current_worker,
      dispatched$protocol,
      dispatch_error
    )
    return()
  }
  if (identical(nxt$kind, "build")) {
    handed_off <- TRUE
    auth_material <- NULL
    active_release(list(
      id = request$build_id,
      handle = coordinator,
      plan = plan
    ))
    update_build_state(list(
      type = "start",
      id = request$build_id,
      revision = plan$revision
    ))
  }
  busy_note(nxt$note)
})

## -- one poller drains whatever the worker was asked to do ---------------
observe({
  current_protocol <- protocol()
  if (is.null(current_protocol) || is.null(current_protocol$pending)) {
    return()
  }
  current_worker <- worker()
  req(current_worker)
  invalidateLater(100, session)
  got <- try(builder_session_poll(current_worker), silent = TRUE)
  if (inherits(got, "try-error")) {
    poll_error <- conditionMessage(attr(got, "condition"))
    pending_payload <- current_protocol$pending$payload
    if (identical(pending_payload$kind, "load")) {
      poll_error <- builder_import_public_error(
        poll_error,
        pending_payload$path %||% character()
      )
    }
    restart_worker_protocol(
      current_worker,
      current_protocol,
      poll_error
    )
    return()
  }
  worker(got$worker)
  if (identical(got$event, "restarted")) {
    apply_protocol_recovery(
      current_protocol,
      got$worker,
      "The background worker stopped before returning its result.",
      retry_persistent = TRUE
    )
    return()
  }
  if (identical(got$event, "restart_failed")) {
    apply_protocol_recovery(
      current_protocol,
      got$worker,
      "The background worker stopped and could not be restored.",
      retry_persistent = FALSE,
      error = got$error %||% "The background worker could not restart."
    )
    return()
  }
  request <- current_protocol$pending
  p <- request$payload
  if (is.null(got$result)) {
    if (identical(p$kind, "load") && !is.null(p$progress_path)) {
      progress <- builder_import_progress_read(
        p$progress_path,
        p$import_generation %||% 1L
      )
      if (!is.null(progress)) {
        set_import_state(
          p$id,
          progress$stage,
          progress$generation
        )
      }
    }
    return()
  }
  if (!is.null(got$result$error)) {
    worker_error <- if (identical(p$kind, "load")) {
      builder_import_public_error(
        got$result$error,
        p$path %||% character()
      )
    } else {
      got$result$error
    }
    if (identical(request$kind, "build")) {
      release <- isolate(active_release())
      release_result <- builder_result_failure(worker_error)
      if (!is.null(release)) {
        release_result <- abort_release_result(release, worker_error)
        active_release(NULL)
      }
      result(release_result)
    } else {
      add_error(worker_error)
    }
    restart_worker_protocol(
      got$worker,
      current_protocol,
      worker_error
    )
    return()
  }
  completed <- builder_protocol_complete(
    current_protocol,
    got$result$value
  )
  if (!is.null(completed$protocol$pending)) {
    restart_worker_protocol(
      got$worker,
      current_protocol,
      "A worker response did not match the pending request."
    )
    return()
  }
  protocol(completed$protocol)
  busy_note(NULL)
  if (!isTRUE(completed$accepted)) {
    if (identical(p$kind, "load")) {
      release_client_import(
        client_import_id_for(p$id),
        server_id = p$id,
        outcome = "error",
        message = "A stale dataset import result was rejected."
      )
    }
    release_pending_source(p)
    if (isTRUE(request$persistent)) {
      protocol(builder_protocol_acknowledge(
        protocol(),
        request$request_id
      ))
      if (identical(request$kind, "drop")) {
        restart_worker_protocol(
          got$worker,
          protocol(),
          "A stale dataset release result was rejected."
        )
        return()
      }
      add_error(
        "A stale persistent worker result was discarded. Retry the action."
      )
    }
    return()
  }
  value <- completed$value
  if (!is.null(completed$error)) {
    cancelled <- identical(p$kind, "load") && p$id %in% cancelled_loads()
    if (identical(request$kind, "build")) {
      result(builder_result_failure(completed$error))
      update_build_state(list(
        type = "fail",
        id = request$build_id,
        error = completed$error
      ))
    } else if (identical(p$kind, "load") && !cancelled) {
      set_import_state(
        p$id,
        "error",
        p$import_generation %||% 1L,
        completed$error
      )
      builder_import_progress_remove(p$progress_path %||% "")
      add_error(NULL)
    } else if (!cancelled) {
      add_error(completed$error)
    }
    if (cancelled) {
      release_pending_source(p)
    }
    if (isTRUE(request$persistent)) {
      protocol(builder_protocol_acknowledge(
        protocol(),
        request$request_id
      ))
    }
    return()
  }
  if (identical(request$kind, "build") && isTRUE(request$persistent)) {
    on.exit(
      {
        current <- isolate(protocol())
        if (!is.null(current)) {
          acknowledged <- try(
            builder_app_acknowledge_build(current, request$request_id),
            silent = TRUE
          )
          if (inherits(acknowledged, "try-error")) {
            protocol(NULL)
            worker_available(FALSE)
            add_error(paste0(
              "The completed Build could not be acknowledged. ",
              "Restart this Builder session."
            ))
          } else {
            protocol(acknowledged)
          }
        }
      },
      add = TRUE
    )
  }

  if (identical(p$kind, "load")) {
    cancelled <- p$id %in% cancelled_loads()
    if (!is.null(value$error)) {
      if (!cancelled) {
        set_import_state(
          p$id,
          "error",
          p$import_generation %||% 1L,
          value$error
        )
        builder_import_progress_remove(p$progress_path %||% "")
        add_error(NULL)
      } else {
        release_pending_source(p)
      }
      protocol(builder_protocol_acknowledge(protocol(), request$request_id))
      return()
    }
    pending_entry <- isolate(import_of(p$id))
    if (
      is.null(pending_entry) ||
        !identical(
          pending_entry$generation,
          as.integer(p$import_generation %||% 1L)
        )
    ) {
      cancelled <- TRUE
    }
    if (cancelled) {
      updated_worker <- try(
        builder_worker_register_snapshot(got$worker, p$id, value$snapshot),
        silent = TRUE
      )
      if (inherits(updated_worker, "try-error")) {
        restart_worker_protocol(
          got$worker,
          protocol(),
          conditionMessage(attr(updated_worker, "condition"))
        )
        return()
      }
      worker(updated_worker)
      identity <- .builder_worker_identity(value$snapshot)
      accepted <- builder_protocol_dataset(protocol(), p$id, 1L, identity)
      acknowledged <- try(
        builder_protocol_acknowledge(accepted, request$request_id),
        silent = TRUE
      )
      if (inherits(acknowledged, "try-error")) {
        protocol(NULL)
        worker_available(FALSE)
        add_error(paste0(
          "The cancelled upload could not be released safely. ",
          "Restart this Builder session."
        ))
        return()
      }
      protocol(acknowledged)
      pending_drops <- pending_snapshot_drops()
      pending_drops[[p$id]] <- identity
      pending_snapshot_drops(pending_drops)
      release_pending_source(p)
      queued <- enqueue(list(
        kind = "drop",
        id = p$id,
        dataset_revision = 1L,
        snapshot_identity = identity,
        note = "Releasing cancelled upload…"
      ))
      if (!isTRUE(queued)) {
        pending_drops[[p$id]] <- NULL
        pending_snapshot_drops(pending_drops)
        add_error(
          "The cancelled upload will be released when this session closes."
        )
      }
      return()
    }
    profile <- value$profile
    set_import_state(
      p$id,
      "preparing",
      p$import_generation %||% 1L
    )
    recommendations <- list(
      metadata = builder_recommend_metadata(value$dataset_profile)
    )
    settings <- builder_default_settings(
      profile,
      unique_name(p$label),
      dataset_profile = value$dataset_profile
    )
    settings$recommendations <- recommendations
    settings$metadata_policy <- builder_metadata_policy_set_retained(
      recommendations$metadata,
      recommendations$metadata$retained
    )
    entry <- list(
      id = p$id,
      source_id = p$id,
      output_id = p$id,
      selector_value = p$id,
      path = p$retained_path %||% p$path,
      filename = p$filename %||% basename(p$retained_path %||% p$path),
      source_origin = p$source_origin %||%
        if (identical(p$source, "example")) "example" else "upload",
      ## Which built-in example produced this, so removing it puts the
      ## example back on offer. NULL for anything read from a file.
      example = p$example,
      format = value$format,
      profile = profile,
      dataset_profile = value$dataset_profile,
      snapshot = value$snapshot,
      revision = 1L,
      ## Level names per grouping variable, in the order the exporter will
      ## produce them -- the keys a configured palette has to match.
      levels = value$levels %||% list(),
      settings = settings
    )
    prepared <- try(
      builder_prepare_loaded_entry_attachment(entry),
      silent = TRUE
    )
    if (inherits(prepared, "try-error")) {
      message <- conditionMessage(attr(prepared, "condition"))
      released <- try(.builder_snapshot_release(value$snapshot), silent = TRUE)
      acknowledged <- try(
        builder_protocol_acknowledge(protocol(), request$request_id),
        silent = TRUE
      )
      if (inherits(acknowledged, "try-error")) {
        protocol(NULL)
        worker_available(FALSE)
      } else {
        protocol(acknowledged)
      }
      set_import_state(
        p$id,
        "error",
        p$import_generation %||% 1L,
        error = message
      )
      add_error(
        if (isTRUE(released)) {
          message
        } else {
          paste0(
            message,
            " The temporary snapshot will be cleaned up when Builder stops."
          )
        }
      )
      builder_import_progress_remove(p$progress_path %||% "")
      release_pending_source(p)
      return()
    }
    entry <- prepared$entry
    next_state <- prepared$state
    updated_worker <- try(
      builder_worker_register_snapshot(
        got$worker,
        p$id,
        value$snapshot
      ),
      silent = TRUE
    )
    if (inherits(updated_worker, "try-error")) {
      restart_worker_protocol(
        got$worker,
        protocol(),
        conditionMessage(attr(updated_worker, "condition"))
      )
      return()
    }
    worker(updated_worker)
    store(next_state)
    if (exists("builder_project_mark_restored_entry", mode = "function")) {
      builder_project_mark_restored_entry(entry)
    }
    protocol(builder_protocol_dataset(
      isolate(protocol()),
      p$id,
      entry$revision,
      .builder_worker_identity(entry$snapshot)
    ))
    watched <- identical(isolate(active_import_id()), p$id)
    next_current <- builder_import_ready_target(
      watched = watched,
      current_id = isolate(current()),
      loaded_id = p$id
    )
    if (!identical(next_current, isolate(current()))) {
      current(next_current)
    }
    session$sendCustomMessage(
      "builder_import_status",
      list(text = paste0(entry$settings$name, " is ready."))
    )
    set_import_state(
      p$id,
      "ready",
      p$import_generation %||% 1L
    )
    release_pending_source(p)
    result(NULL)
  } else if (identical(p$kind, "preview")) {
    if (identical(current(), p$id)) {
      preview_frame(value)
    }
  } else if (identical(p$kind, "projection_previews")) {
    projection_previews(builder_preview_cache_store(
      isolate(projection_previews()),
      p$id,
      value
    ))
  } else if (identical(p$kind, "trajectory_previews")) {
    trajectory_previews(builder_preview_cache_store(
      isolate(trajectory_previews()),
      p$id,
      value
    ))
  } else if (identical(p$kind, "coords")) {
    if (
      identical(current(), p$id) &&
        identical(active_slice(), p$image)
    ) {
      spatial_coords(value)
    }
  } else if (identical(p$kind, "spatial_preview")) {
    if (
      identical(current(), p$id) &&
        identical(active_slice(), p$section)
    ) {
      alignment_preview(value)
      if (isTRUE(value$available)) {
        spatial_coords(list(
          x = value$spatial$x,
          y = value$spatial$y,
          sx = value$spatial$x,
          sy = value$spatial$y
        ))
      }
    }
  } else if (identical(p$kind, "align_all")) {
    apply_section_bounds(p$id, value, p$picture)
  } else if (identical(p$kind, "build")) {
    release <- isolate(active_release())
    if (
      is.null(release) ||
        !identical(release$id, request$build_id)
    ) {
      release <- NULL
    }
    value <- builder_app_settle_release(release, value)
    active_release(NULL)
    result(value)
    update_build_state(builder_app_build_action(value, request$build_id))
  } else if (identical(p$kind, "drop")) {
    active_state <- store()
    retained <- active_state$datasets
    if (is.list(active_state$last_removed)) {
      retained <- c(retained, list(active_state$last_removed$entry))
    }
    pending_drops <- pending_snapshot_drops()
    # The transition excludes other_drop_ids with a shared pending identity.
    released <- try(
      builder_snapshot_release_transition(
        worker = got$worker,
        id = p$id,
        identity = request$snapshot_identity,
        retained = retained,
        pending = pending_drops,
        release = function(worker, id, identity) {
          builder_worker_release_snapshot(
            worker,
            id,
            expected_identity = identity
          )
        },
        unregister = function(worker, id) {
          worker$snapshot_registry[[id]] <- NULL
          worker
        },
        identity_of = .builder_worker_identity
      ),
      silent = TRUE
    )
    if (inherits(released, "try-error")) {
      restart_worker_protocol(
        got$worker,
        protocol(),
        conditionMessage(attr(released, "condition"))
      )
      return()
    }
    worker(released$worker)
    pending_snapshot_drops(released$pending)
    all <- Filter(function(e) !identical(e$id, p$id), sets())
    sets(all)
    if (identical(current(), p$id)) {
      current(if (length(all)) all[[1]]$id else NULL)
      result(NULL)
    }
    acknowledged <- try(
      builder_protocol_acknowledge(protocol(), request$request_id),
      silent = TRUE
    )
    if (inherits(acknowledged, "try-error")) {
      protocol(NULL)
      worker_available(FALSE)
      add_error(paste0(
        "The dataset was removed, but its protocol acknowledgement failed. ",
        "Restart this Builder session."
      ))
      return()
    }
    forgotten <- try(
      builder_protocol_forget_dataset(acknowledged, p$id),
      silent = TRUE
    )
    if (inherits(forgotten, "try-error")) {
      protocol(NULL)
      worker_available(FALSE)
      add_error(paste0(
        "The dataset was removed, but its queued actions could not be ",
        "cleared. Restart this Builder session."
      ))
      return()
    }
    protocol(forgotten$protocol)
    cancelled <- length(forgotten$failed) + length(forgotten$discarded)
    if (cancelled) {
      showNotification(
        paste0(
          "Dataset removed; ",
          cancelled,
          " obsolete queued action",
          if (cancelled == 1L) " was" else "s were",
          " cancelled."
        ),
        type = "message",
        duration = 5
      )
    }
    return()
  }
  if (isTRUE(request$persistent) && !identical(request$kind, "build")) {
    protocol(builder_protocol_acknowledge(protocol(), request$request_id))
  }
})

update_enhance_histology_choices <- function(entry) {
  collection <- builder_image_collection_normalize(
    entry$settings$images %||% list()
  )
  choices <- lapply(collection, names)
  invisible(choices)
}

commit_enhance_images <- function(entry, images, internal = FALSE) {
  entry$settings$images <- builder_image_collection_normalize(images)
  changed <- replace_entry(entry, internal = internal)
  if (!isTRUE(changed)) {
    return(invisible(FALSE))
  }
  update_enhance_histology_choices(entry)
  invisible(entry)
}

## Take the per-section extents the worker computed and pair each with the
## one shared picture.
apply_section_bounds <- function(id, per_section, a) {
  if (is.null(a) || !length(per_section)) {
    return()
  }
  e <- isolate(entry_of(id))
  if (is.null(e)) {
    return()
  }
  paired <- builder_pair_sections(a, per_section)
  imgs <- utils::modifyList(e$settings$images %||% list(), paired)
  short <- names(Filter(function(x) x$outside > 0, paired))
  commit_enhance_images(e, imgs)

  ## Saying "done" when four of five slides have every cell off the image is
  ## how the earlier version of this hid its own bug.
  if (length(short)) {
    showNotification(
      paste0(
        "Applied to ",
        length(per_section),
        " sections, but cells still fall outside the image in: ",
        paste(short, collapse = ", "),
        ". Bounding-box mode usually works best for multi-section objects."
      ),
      type = "warning",
      duration = 10
    )
  } else {
    showNotification(
      paste0(
        "The image was fitted to the coordinates of all ",
        length(per_section),
        " sections."
      ),
      type = "message",
      duration = 5
    )
  }
}

output$add_error <- renderUI({
  msg <- add_error()
  if (is.null(msg)) {
    return(NULL)
  }
  div(class = "notice bad", msg)
})
