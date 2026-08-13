## Builder server: imports.

dataset_mutations_locked <- function(notify = TRUE) {
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

start_load <- function(kind, arg, label, file_meta = NULL) {
  if (dataset_mutations_locked()) {
    return(invisible(FALSE))
  }
  rs <- worker()
  if (is.null(rs)) {
    add_error("The background worker is not ready yet.")
    return()
  }
  reservation <- builder_source_reserve(sets(), pending_sources(), kind, arg)
  if (!isTRUE(reservation$ok)) {
    return(invisible(FALSE))
  }
  pending_sources(reservation$pending)
  add_error(NULL)
  seq_id(seq_id() + 1L)
  id <- paste0("ds", seq_id())
  filename <- NULL
  file_type <- NULL
  file_size <- NA_real_
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
  source_descriptor <- list(
    kind = kind,
    staged_path = if (identical(kind, "file")) arg else NULL,
    example = if (identical(kind, "example")) arg else NULL,
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
  active_import_id(id)
  queued <- enqueue(list(
    kind = "load",
    source = kind,
    id = id,
    path = if (identical(kind, "file")) arg else NA_character_,
    example = if (identical(kind, "example")) arg else NULL,
    label = label,
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
  }
  invisible(isTRUE(queued))
}

observeEvent(input$dataset_files, {
  uploads <- input$dataset_files
  if (
    !is.data.frame(uploads) ||
      !all(c("name", "datapath") %in% names(uploads)) ||
      !nrow(uploads)
  ) {
    return()
  }
  paths <- as.character(uploads$datapath)
  labels <- as.character(uploads$name)
  sizes <- if ("size" %in% names(uploads)) {
    suppressWarnings(as.numeric(uploads$size))
  } else {
    rep(NA_real_, nrow(uploads))
  }
  types <- if ("type" %in% names(uploads)) {
    as.character(uploads$type)
  } else {
    rep("", nrow(uploads))
  }
  valid <- !is.na(paths) & nzchar(paths) & !is.na(labels) & nzchar(labels)
  paths <- paths[valid]
  labels <- labels[valid]
  sizes <- sizes[valid]
  types <- types[valid]
  duplicate <- duplicated(paths) |
    paths %in%
      vapply(
        sets(),
        function(entry) entry$path,
        character(1)
      )
  for (i in which(!duplicate)) {
    start_load(
      "file",
      paths[[i]],
      tools::file_path_sans_ext(basename(labels[[i]])),
      file_meta = list(
        name = labels[[i]],
        type = types[[i]],
        size = sizes[[i]]
      )
    )
  }
  if (any(duplicate)) {
    add_error(paste0(
      sum(duplicate),
      if (sum(duplicate) == 1L) " file has" else " files have",
      " already been added."
    ))
  }
})

observeEvent(input$use_example, {
  used <- as.character(unlist(Filter(
    Negate(is.null),
    lapply(sets(), function(entry) entry$example)
  )))
  if (input$use_example %in% used) {
    return()
  }
  ex <- Filter(
    function(e) identical(e$id, input$use_example),
    builder_examples()
  )
  if (!length(ex)) {
    return()
  }
  start_load("example", ex[[1]]$id, ex[[1]]$label)
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
  cancelled_loads(setdiff(cancelled_loads(), id))
  queued <- enqueue(list(
    kind = "load",
    source = entry$source$kind,
    id = entry$id,
    path = entry$source$staged_path,
    example = entry$source$example,
    label = entry$label,
    import_generation = entry$generation,
    progress_path = progress_path,
    note = paste0("Loading ", entry$label, "…")
  ))
  if (!isTRUE(queued)) {
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
      path = p$path,
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
    next_state <- builder_reduce_state(
      isolate(store()),
      list(type = "add", entry = entry)
    )
    store(next_state)
    protocol(builder_protocol_dataset(
      protocol(),
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
    if (identical(current(), p$id)) {
      projection_previews(list(dataset = p$id, frames = value %||% list()))
    }
  } else if (identical(p$kind, "trajectory_previews")) {
    if (identical(current(), p$id)) {
      trajectory_previews(list(dataset = p$id, frames = value %||% list()))
    }
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

commit_enhance_images <- function(entry, images) {
  entry$settings$images <- builder_image_collection_normalize(images)
  replace_entry(entry)
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
