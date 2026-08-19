## Builder server: durable projects.

builder_project <- reactiveVal(NULL)
builder_project_saving <- reactiveVal(FALSE)
builder_project_first_save_offered <- reactiveVal(FALSE)
builder_project_first_save_scheduled <- reactiveVal(FALSE)
builder_project_restore <- reactiveVal(NULL)
builder_project_pending_entries <- reactiveVal(list())
builder_project_artifacts <- reactiveVal(list())
builder_project_build_plan <- reactiveVal(NULL)
builder_project_checkpoint <- reactiveVal(FALSE)
builder_project_auto_save_signature <- reactiveVal(NULL)
builder_project_operation_phase <- reactiveVal("idle")
builder_project_last_save_error <- reactiveVal(NULL)
builder_project_pending_folder <- reactiveVal(NULL)
builder_project_source_sync <- reactiveVal(list(
  status = "idle",
  completed = 0L,
  total = 0L,
  failed = 0L
))
builder_project_source_process <- reactiveVal(NULL)
builder_project_source_generation <- reactiveVal(0)
builder_project_source_run <- reactiveVal(NULL)
builder_project_source_queue <- reactiveVal(list())
builder_project_source_active_ids <- reactiveVal(character())
builder_project_source_progress <- reactiveVal(NULL)
builder_client_import_state <- reactiveVal(list(nonce = 0, pending = 0L))
builder_connection_state <- reactiveVal("connected")
builder_project_restore_progress <- reactiveVal(list(
  mode = "idle",
  total = 0L,
  remaining = 0L
))

builder_project_record_map <- function(manifest) {
  records <- manifest$datasets %||% list()
  ids <- vapply(records, function(record) as.character(record$id), character(1))
  stats::setNames(records, ids)
}

register_loaded_entry_finalizer(function(entry) {
  pending <- isolate(builder_project_pending_entries())
  record <- pending[[entry$id]] %||% NULL
  project <- isolate(builder_project())
  if (is.null(record) || is.null(project)) {
    return(entry)
  }
  builder_project_hydrate_loaded_entry(entry, record, project$root)
})

builder_project_mark_restored_entry <- function(entry) {
  if (!is.list(entry) || !builder_has_text(entry$id)) {
    return(invisible(FALSE))
  }
  record <- isolate(builder_project_pending_entries())[[entry$id]] %||% NULL
  project <- isolate(builder_project())
  if (is.null(record) || is.null(project)) {
    return(invisible(FALSE))
  }
  status <- builder_project_dataset_status(record, project$root)
  mark <- builder_project_restored_check_identity(
    record,
    entry,
    status,
    project$root
  )
  if (is.null(mark)) {
    return(invisible(FALSE))
  }
  marks <- isolate(dataset_check_marks())
  marks[[entry$id]] <- mark
  dataset_check_marks(marks)
  invisible(TRUE)
}

builder_project_dirty <- reactive({
  project <- builder_project()
  if (is.null(project)) {
    return(FALSE)
  }
  builder_project_live_dirty(
    sets(),
    checked_dataset_ids(),
    project$manifest,
    identity_cache = builder_configuration_identity_cache
  )
})

builder_project_phase <- reactive({
  operation <- builder_project_operation_phase()
  if (!identical(operation, "idle")) {
    return(operation)
  }
  if (is.null(builder_project())) {
    return("none")
  }
  if (isTRUE(builder_project_dirty())) "dirty" else "clean"
})

builder_spatial_drafts_dirty <- reactive({
  drafts <- alignment_server$coordinate_drafts()
  length(drafts %||% list()) > 0L
})

builder_activity <- reactive({
  project <- builder_project()
  entries <- sets()
  client_state <- builder_client_import_state()
  builder_activity_state(
    connection = builder_connection_state(),
    client_imports = client_state$pending %||% 0L,
    server_imports = length(imports()$entries %||% list()) > 0L,
    project_phase = builder_project_phase(),
    spatial_dirty = builder_spatial_drafts_dirty(),
    source_syncing = identical(builder_project_source_sync()$status, "syncing"),
    build_locked = builder_mutations_locked(build_flow(), protocol()),
    has_project = !is.null(project),
    has_datasets = length(entries) > 0L
  )
})

builder_build_overlay <- reactive({
  builder_build_operation_overlay_model(
    flow = build_flow(),
    protocol = protocol(),
    note = busy_note(),
    result = result()
  )
})

builder_capabilities <- reactive({
  builder_activity_capabilities(builder_activity())
})

builder_operation_allowed <- function(operation, notify = TRUE) {
  activity <- isolate(builder_activity())
  allowed <- isTRUE(builder_activity_capabilities(activity)[[operation]])
  if (!allowed && isTRUE(notify)) {
    showNotification(
      builder_activity_reason(activity, operation),
      type = "warning",
      duration = 6
    )
  }
  allowed
}

observeEvent(
  input$builder_client_import_state,
  {
    state <- input$builder_client_import_state
    if (
      !is.list(state) ||
        is.object(state) ||
        !all(c("nonce", "pending") %in% names(state)) ||
        !is.numeric(state$nonce) ||
        length(state$nonce) != 1L ||
        is.na(state$nonce) ||
        !is.finite(state$nonce) ||
        !is.numeric(state$pending) ||
        length(state$pending) != 1L ||
        is.na(state$pending) ||
        !is.finite(state$pending) ||
        state$pending < 0
    ) {
      return()
    }
    previous <- isolate(builder_client_import_state())
    if (state$nonce < (previous$nonce %||% 0)) {
      return()
    }
    builder_client_import_state(list(
      nonce = as.double(state$nonce),
      pending = as.integer(state$pending)
    ))
  },
  ignoreInit = TRUE
)

observeEvent(
  input$builder_client_connection,
  {
    state <- input$builder_client_connection
    if (is.list(state) && identical(state$status %||% NULL, "connected")) {
      builder_connection_state("connected")
      activity_message <- builder_activity_message(
        isolate(builder_activity()),
        build_overlay = isolate(builder_build_overlay())
      )
      session$onFlushed(
        function() {
          session$sendCustomMessage(
            "builder_activity_state",
            activity_message
          )
        },
        once = TRUE
      )
    }
  },
  ignoreInit = TRUE
)

builder_activity_message <- function(
  activity,
  restore_progress = list(),
  build_overlay = list(active = FALSE)
) {
  capabilities <- builder_activity_capabilities(activity)
  phase <- activity$project_phase
  build_active <- is.list(build_overlay) && isTRUE(build_overlay$active)
  title <- if (build_active) {
    build_overlay$title
  } else {
    switch(
      phase,
      opening = "Opening project",
      restoring = "Restoring datasets",
      saving = "Saving project",
      registering = "Preparing reusable CRBs",
      "Working on your Builder project"
    )
  }
  message <- if (build_active) {
    build_overlay$message
  } else {
    switch(
      phase,
      saving = "Saving the Builder project. Keep this page open.",
      opening = "Reading the project file and checking saved datasets…",
      restoring = "Loading the selected source datasets…",
      registering = "Adding reusable CRBs to the project. Keep this page open.",
      conflict = "This project changed in another Builder window.",
      NULL
    )
  }
  total <- as.integer(restore_progress$total %||% 0L)
  remaining <- as.integer(restore_progress$remaining %||% 0L)
  detail <- if (build_active) {
    build_overlay$detail
  } else if (identical(phase, "opening")) {
    "Keep this page open."
  } else if (identical(phase, "restoring") && total > 0L) {
    paste0(total - remaining, " of ", total, " datasets restored")
  } else {
    NULL
  }
  list(
    phase = phase,
    capabilities = capabilities,
    busy_title = title,
    busy_message = message,
    busy_detail = detail,
    has_project = isTRUE(activity$has_project),
    warn_before_unload = isTRUE(capabilities$warn_before_unload) || build_active,
    page_inert = isTRUE(capabilities$page_inert) || build_active
  )
}

observe({
  session$sendCustomMessage(
    "builder_activity_state",
    builder_activity_message(
      builder_activity(),
      builder_project_restore_progress(),
      builder_build_overlay()
    )
  )
})

observe({
  progress <- builder_project_source_sync()
  session$sendCustomMessage(
    "builder_project_source_progress",
    list(
      status = progress$status %||% "idle",
      completed = as.integer(progress$completed %||% 0L),
      total = as.integer(progress$total %||% 0L),
      failed = as.integer(progress$failed %||% 0L)
    )
  )
})

builder_project_capture_build_plan <- function(plan) {
  if (is.null(isolate(builder_project()))) {
    builder_project_build_plan(NULL)
    return(invisible(FALSE))
  }
  builder_project_build_plan(unserialize(serialize(plan, NULL, version = 3L)))
  invisible(plan)
}

builder_project_source_runtime_file <- function() {
  candidates <- c(
    file.path(getwd(), "project.R"),
    file.path(getwd(), "inst", "builder", "project.R")
  )
  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    stop("The Builder Project runtime could not be found.", call. = FALSE)
  }
  normalizePath(found[[1L]], winslash = "/", mustWork = TRUE)
}

builder_project_poll_source_sync <- NULL

invalidate_builder_project_source_sync <- function() {
  builder_project_source_generation(
    as.double(isolate(builder_project_source_generation())) + 1
  )
  builder_project_source_queue(list())
  builder_project_source_active_ids(character())
  builder_project_source_sync(list(
    status = "idle",
    completed = 0L,
    total = 0L,
    failed = 0L
  ))
  invisible(TRUE)
}

builder_project_start_source_sync <- function() {
  process <- isolate(builder_project_source_process())
  if (!is.null(process)) {
    return(invisible(FALSE))
  }
  jobs <- isolate(builder_project_source_queue())
  if (!length(jobs)) {
    return(invisible(FALSE))
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    builder_project_source_sync(list(
      status = "failed",
      completed = 0L,
      total = length(jobs),
      failed = length(jobs)
    ))
    showNotification(
      "Source files could not be saved in the background because callr is unavailable.",
      type = "error",
      duration = 8
    )
    return(invisible(FALSE))
  }
  project <- isolate(builder_project())
  if (is.null(project)) {
    return(invisible(FALSE))
  }
  active_ids <- vapply(jobs, `[[`, character(1), "id")
  source_run <- tryCatch(
    builder_project_source_context(
      project,
      isolate(builder_project_source_generation()),
      active_ids = active_ids
    ),
    error = identity
  )
  if (inherits(source_run, "condition")) {
    builder_project_source_sync(list(
      status = "failed",
      completed = 0L,
      total = length(jobs),
      failed = length(jobs)
    ))
    showNotification(conditionMessage(source_run), type = "error", duration = 8)
    return(invisible(FALSE))
  }
  builder_project_source_queue(list())
  builder_project_source_active_ids(active_ids)
  progress_path <- tempfile(
    pattern = ".builder-source-sync-",
    tmpdir = project$root,
    fileext = ".rds"
  )
  builder_project_source_progress(progress_path)
  process <- tryCatch(
    callr::r_bg(
      function(jobs, progress_path, runtime_file) {
        runtime <- new.env(parent = globalenv())
        runtime$`%||%` <- function(left, right) {
          if (is.null(left)) right else left
        }
        sys.source(runtime_file, envir = runtime)
        runtime$builder_project_copy_source_jobs(jobs, progress_path)
      },
      args = list(
        jobs = unname(jobs),
        progress_path = progress_path,
        runtime_file = builder_project_source_runtime_file()
      ),
      supervise = TRUE,
      stdout = "|",
      stderr = "|"
    ),
    error = identity
  )
  if (inherits(process, "error")) {
    builder_project_source_active_ids(character())
    builder_project_source_progress(NULL)
    builder_project_source_run(NULL)
    builder_project_source_sync(list(
      status = "failed",
      completed = 0L,
      total = length(jobs),
      failed = length(jobs)
    ))
    showNotification(conditionMessage(process), type = "error", duration = 8)
    return(invisible(FALSE))
  }
  builder_project_source_process(process)
  builder_project_source_run(source_run)
  builder_project_source_sync(list(
    status = "syncing",
    completed = 0L,
    total = length(jobs),
    failed = 0L
  ))
  later::later(builder_project_poll_source_sync, delay = 0.2)
  invisible(TRUE)
}

builder_project_poll_source_sync <- function() {
  process <- isolate(builder_project_source_process())
  if (is.null(process)) {
    return(invisible(FALSE))
  }
  source_run <- isolate(builder_project_source_run())
  project <- isolate(builder_project())
  run_is_current <- builder_project_source_context_matches(
    source_run,
    project,
    isolate(builder_project_source_generation())
  )
  progress_path <- isolate(builder_project_source_progress())
  if (
    isTRUE(run_is_current) &&
      .builder_project_text(progress_path) &&
      file.exists(progress_path)
  ) {
    progress <- tryCatch(readRDS(progress_path), error = function(error) NULL)
    if (is.list(progress)) {
      builder_project_source_sync(list(
        status = "syncing",
        completed = as.integer(progress$completed %||% 0L),
        total = as.integer(progress$total %||% 0L),
        failed = as.integer(progress$failed %||% 0L)
      ))
    }
  }
  if (isTRUE(process$is_alive())) {
    later::later(builder_project_poll_source_sync, delay = 0.2)
    return(invisible(TRUE))
  }
  results <- tryCatch(process$get_result(), error = identity)
  active_ids <- source_run$active_ids %||%
    isolate(builder_project_source_active_ids())
  if (inherits(results, "error")) {
    results <- lapply(active_ids, function(id) {
      list(
        id = id,
        status = "failed",
        error = conditionMessage(results)
      )
    })
  }
  written <- NULL
  if (isTRUE(run_is_current)) {
    updated <- tryCatch(
      builder_project_apply_source_results(
        project$manifest,
        results,
        project$root
      ),
      error = identity
    )
    if (!inherits(updated, "error")) {
      written <- tryCatch(
        builder_project_write(
          updated,
          project$root,
          expected_revision = project$manifest$project$revision
        ),
        error = identity
      )
    } else {
      written <- updated
    }
  }
  unlink(progress_path %||% "", force = TRUE)
  builder_project_source_process(NULL)
  builder_project_source_run(NULL)
  builder_project_source_progress(NULL)
  builder_project_source_active_ids(character())
  if (!isTRUE(run_is_current)) {
    builder_project_source_sync(list(
      status = "idle",
      completed = 0L,
      total = 0L,
      failed = 0L
    ))
    if (length(isolate(builder_project_source_queue()))) {
      builder_project_start_source_sync()
    }
    return(invisible(FALSE))
  }
  failed <- sum(vapply(
    results,
    function(result) identical(result$status, "failed"),
    logical(1)
  ))
  if (inherits(written, "error") || is.null(written)) {
    failed <- max(1L, failed)
    showNotification(
      if (inherits(written, "error")) {
        conditionMessage(written)
      } else {
        "The Project source results could not be saved."
      },
      type = "error",
      duration = 8
    )
  } else {
    project$manifest <- written$manifest
    project$path <- written$path
    builder_project(project)
    current_worker <- isolate(worker())
    live_entries <- isolate(sets())
    committed <- tryCatch(
      builder_project_commit_source_entries(
        live_entries,
        written$manifest,
        results,
        project$root,
        session_root = current_worker$snapshot_root %||% NULL
      ),
      error = identity
    )
    if (
      !inherits(committed, "condition") &&
        !identical(committed$entries, live_entries)
    ) {
      sets(committed$entries)
    }
  }
  builder_project_source_sync(list(
    status = if (failed) "failed" else "ready",
    completed = length(results),
    total = length(results),
    failed = as.integer(failed)
  ))
  if (length(isolate(builder_project_source_queue()))) {
    builder_project_start_source_sync()
  }
  invisible(failed == 0L)
}

request_builder_project_source_sync <- function(jobs) {
  jobs <- Filter(Negate(is.null), jobs %||% list())
  if (!length(jobs)) {
    return(invisible(FALSE))
  }
  queued <- isolate(builder_project_source_queue())
  for (job in jobs) {
    queued[[job$id]] <- job
  }
  builder_project_source_queue(queued)
  builder_project_start_source_sync()
}

builder_project_build_manifest <- function(entries, project) {
  manifest <- project$manifest
  previous <- builder_project_record_map(manifest)
  checked <- isolate(checked_dataset_ids())
  artifacts <- isolate(builder_project_artifacts())
  staged_entries <- vector("list", length(entries))
  records <- vector("list", length(entries))
  jobs <- vector("list", length(entries))
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    prior <- previous[[entry$id]] %||% NULL
    if (identical(entry$load_state %||% "loaded", "artifact_ready")) {
      staged <- list(
        entry = entry,
        source = prior$source %||% list(kind = "missing", path = NULL),
        job = NULL
      )
    } else {
      staged <- builder_project_prepare_source(entry, project$root, prior)
    }
    configuration_entry <- builder_project_configuration_entry(staged$entry)
    configuration_entry <- builder_project_stage_spatial_assets(
      configuration_entry,
      project$root
    )
    current_digest <- builder_project_configuration_digest(configuration_entry)
    staged$entry <- builder_project_adopt_spatial_assets(
      staged$entry,
      configuration_entry
    )
    artifact <- artifacts[[entry$id]] %||%
      entry$project_artifact %||%
      prior$artifact %||%
      NULL
    if (is.list(artifact)) {
      if (
        !identical(entry$load_state %||% "loaded", "artifact_ready") &&
          !identical(
            artifact$built_from_configuration %||% "",
            current_digest
          )
      ) {
        artifact$status <- "stale"
      }
      if (
        is.list(artifact$plan_item) &&
          !.builder_project_text(artifact$plan_payload %||% "")
      ) {
        artifact$plan_payload <- jsonlite::serializeJSON(
          artifact$plan_item,
          digits = NA,
          pretty = FALSE
        )
      }
      artifact$plan_item <- NULL
      artifact$resolved_path <- NULL
      if (length(artifact$members %||% list())) {
        artifact$members <- lapply(artifact$members, function(member) {
          member$resolved_path <- NULL
          member
        })
      }
    }
    staged_entries[[index]] <- staged$entry
    jobs[[index]] <- staged$job
    records[[index]] <- builder_project_dataset_record(
      staged$entry,
      staged$source,
      checked = entry$id %in% checked,
      artifact = artifact,
      order = index,
      payload_entry = configuration_entry,
      root = project$root,
      configuration_digest = current_digest
    )
  }
  retained_ids <- vapply(records, `[[`, character(1), "id")
  inactive <- previous[setdiff(names(previous), retained_ids)]
  if (length(inactive)) {
    inactive <- lapply(inactive, function(record) {
      record$release <- list(included = FALSE)
      record
    })
    records <- c(records, unname(inactive))
  }
  manifest$datasets <- records
  manifest$configuration <- builder_project_configuration(
    review_options = isolate(review_options()),
    build_mode = isTRUE(isolate(build_mode())),
    auth_enabled = isTRUE(isolate(auth_enabled())),
    initial_dataset = isolate(build_initial_dataset())
  )
  manifest$last_ui <- list(
    stage = isolate(selected_workflow_stage()),
    selected_dataset = isolate(current()),
    spatial = builder_project_last_ui_spatial(
      isolate(alignment_server$project_selection()),
      selected_dataset = isolate(current())
    )
  )
  list(
    manifest = manifest,
    entries = staged_entries,
    jobs = Filter(Negate(is.null), jobs)
  )
}

restore_builder_project_preferences <- function(manifest) {
  configuration <- manifest$configuration %||% builder_project_configuration()
  saved_options <- configuration$review_options %||% NULL
  if (is.list(saved_options)) {
    restored <- try(
      do.call(builder_review_options, saved_options),
      silent = TRUE
    )
    if (!inherits(restored, "try-error")) {
      review_options(restored)
      review_validation(list(ok = TRUE, error = NULL))
    }
  }
  make_app <- isTRUE(configuration$build_mode)
  build_mode(make_app)
  build_initial_dataset(configuration$initial_dataset %||% NULL)
  auth_accounts(builder_auth_empty_accounts())
  login_requested <- make_app && isTRUE(configuration$auth_enabled)
  auth_enabled(login_requested)
  auth_validation(
    if (login_requested) {
      list(
        ok = FALSE,
        error = "Add login accounts again before building this App."
      )
    } else {
      list(ok = TRUE, error = NULL)
    }
  )
  invisible(TRUE)
}

restore_builder_project_last_ui <- function(manifest) {
  ids <- vapply(isolate(sets()), `[[`, character(1), "id")
  target <- builder_project_last_ui_target(
    manifest$last_ui %||% list(),
    available_ids = ids,
    checked_ids = isolate(checked_dataset_ids())
  )
  if (!is.null(target$selected_dataset)) {
    current(target$selected_dataset)
  }
  alignment_server$restore_project_selection(target$spatial)
  saved_initial <- isolate(build_initial_dataset())
  if (!is.null(saved_initial) && !saved_initial %in% ids) {
    build_initial_dataset(NULL)
  }
  if (identical(target$stage, "review")) {
    navigate_workflow_stage("review")
  } else {
    navigate_workflow_stage("configure")
  }
  invisible(target)
}

save_builder_project_state <- function(
  show_actions = FALSE,
  materialize = TRUE,
  notify = TRUE,
  manage_lifecycle = TRUE,
  last_ui = NULL
) {
  project <- isolate(builder_project())
  if (is.null(project)) {
    shiny::showModal(builder_project_first_save_dialog())
    return(invisible(FALSE))
  }
  if (isTRUE(manage_lifecycle) && isTRUE(isolate(builder_project_saving()))) {
    return(invisible(FALSE))
  }
  if (isTRUE(manage_lifecycle)) {
    builder_project_saving(TRUE)
    builder_project_operation_phase("saving")
    on.exit(builder_project_saving(FALSE), add = TRUE)
  }
  save_failed <- function(message) {
    builder_project_last_save_error(message)
    if (isTRUE(manage_lifecycle)) {
      builder_project_operation_phase(
        if (grepl("updated by another Builder window", message, fixed = TRUE)) {
          "conflict"
        } else {
          "save_failed"
        }
      )
    }
    invisible(FALSE)
  }
  entries <- isolate(sets())
  if (isTRUE(materialize) && length(entries)) {
    materialized <- alignment_server$materialize_coordinate_drafts(
      notify = FALSE
    )
    if (!isTRUE(materialized$ok)) {
      if (isTRUE(notify)) {
        showNotification(
          materialized$error %||%
            "Current spatial settings could not be saved.",
          type = "error",
          duration = 7
        )
      }
      return(save_failed(
        materialized$error %||% "Current spatial settings could not be saved."
      ))
    }
    entries <- materialized$all_entries
  }
  built <- tryCatch(
    builder_project_build_manifest(entries, project),
    error = function(error) error
  )
  if (inherits(built, "condition")) {
    if (isTRUE(notify)) {
      showNotification(conditionMessage(built), type = "error", duration = 7)
    }
    return(save_failed(conditionMessage(built)))
  }
  if (is.list(last_ui)) {
    built$manifest$last_ui <- last_ui
  }
  entries_changed <- !identical(entries, built$entries)
  written <- tryCatch(
    builder_project_write(
      built$manifest,
      project$root,
      expected_revision = project$manifest$project$revision
    ),
    error = function(error) error
  )
  if (inherits(written, "condition")) {
    if (isTRUE(notify)) {
      showNotification(conditionMessage(written), type = "error", duration = 8)
    }
    return(save_failed(conditionMessage(written)))
  }
  if (entries_changed) {
    sets(built$entries)
  }
  project$manifest <- written$manifest
  project$path <- written$path
  project$name <- written$manifest$project$name
  builder_project(project)
  request_builder_project_source_sync(built$jobs)
  builder_project_last_save_error(NULL)
  if (isTRUE(manage_lifecycle)) {
    builder_project_operation_phase("idle")
  }
  if (isTRUE(show_actions)) {
    records <- builder_project_record_map(written$manifest)
    checked <- sum(vapply(
      records,
      function(record) isTRUE(record$configuration$checked),
      logical(1)
    ))
    checked_entries <- Filter(
      function(entry) entry$id %in% isolate(checked_dataset_ids()),
      built$entries
    )
    required_entries <- builder_project_entries_requiring_crb(
      checked_entries,
      isolate(builder_project_artifacts()),
      project$root
    )
    reusable <- length(checked_entries) - length(required_entries)
    source_sync <- isolate(builder_project_source_sync())
    session$sendCustomMessage(
      "builder_project_save_result",
      list(
        checked = as.integer(checked),
        reusable = as.integer(reusable),
        source_syncing = length(built$jobs) > 0L ||
          identical(
            source_sync$status,
            "syncing"
          ),
        completed = as.integer(source_sync$completed %||% 0L),
        total = as.integer(max(
          source_sync$total %||% 0L,
          length(built$jobs)
        ))
      )
    )
  } else if (isTRUE(notify)) {
    showNotification(
      if (length(built$jobs)) {
        "Project settings saved. Source files are saving in the background."
      } else {
        "Project saved."
      },
      type = "message",
      duration = 4
    )
  }
  invisible(TRUE)
}

request_builder_project_save <- function(
  show_actions = FALSE,
  materialize = TRUE,
  notify = TRUE,
  after = NULL
) {
  if (isTRUE(isolate(builder_project_saving()))) {
    return(invisible(FALSE))
  }
  builder_project_saving(TRUE)
  builder_project_operation_phase("saving")
  session$onFlushed(
    function() {
      shiny::isolate({
        ok <- tryCatch(
          save_builder_project_state(
            show_actions = show_actions,
            materialize = materialize,
            notify = notify,
            manage_lifecycle = FALSE
          ),
          error = function(error) {
            builder_project_last_save_error(conditionMessage(error))
            if (isTRUE(notify)) {
              showNotification(
                conditionMessage(error),
                type = "error",
                duration = 8
              )
            }
            FALSE
          }
        )
        builder_project_saving(FALSE)
        if (isTRUE(ok)) {
          builder_project_operation_phase("idle")
        } else {
          message <- builder_project_last_save_error() %||% ""
          builder_project_operation_phase(
            if (
              grepl("updated by another Builder window", message, fixed = TRUE)
            ) {
              "conflict"
            } else {
              "save_failed"
            }
          )
        }
        if (is.function(after)) {
          after(isTRUE(ok))
        }
      })
    },
    once = TRUE
  )
  invisible(TRUE)
}

output$project_status <- renderUI({
  project <- builder_project()
  builder_project_status_ui(
    if (is.null(project)) NULL else list(name = project$name),
    phase = builder_project_phase(),
    source_sync = builder_project_source_sync()
  )
})

observe({
  shiny::updateActionButton(
    session,
    "save_builder_project",
    label = if (is.null(builder_project())) {
      "Create project…"
    } else {
      "Save project"
    }
  )
})

observe({
  ready <- builder_project_first_save_offer_ready(
    sets(),
    builder_project(),
    builder_project_first_save_offered(),
    builder_activity(),
    protocol()
  )
  if (!ready || isTRUE(builder_project_first_save_scheduled())) {
    return()
  }
  builder_project_first_save_scheduled(TRUE)
  session$onFlushed(
    function() {
      shiny::isolate({
        builder_project_first_save_scheduled(FALSE)
        if (builder_session_closed()) {
          return()
        }
        ready_after_flush <- builder_project_first_save_offer_ready(
          sets(),
          builder_project(),
          builder_project_first_save_offered(),
          builder_activity(),
          protocol()
        )
        if (!ready_after_flush) {
          return()
        }
        builder_project_first_save_offered(TRUE)
        shiny::showModal(builder_project_first_save_dialog())
      })
    },
    once = TRUE
  )
})

observe({
  project <- builder_project()
  entries <- sets()
  if (
    is.null(project) || !length(entries) || isTRUE(builder_project_saving())
  ) {
    return()
  }
  if (!isTRUE(builder_capabilities()$save_project)) {
    return()
  }
  current_ids <- vapply(entries, `[[`, character(1), "id")
  saved_ids <- names(builder_project_record_map(project$manifest))
  missing_ids <- sort(setdiff(current_ids, saved_ids))
  if (!length(missing_ids)) {
    builder_project_auto_save_signature(NULL)
    return()
  }
  signature <- paste(missing_ids, collapse = "\r")
  if (identical(signature, isolate(builder_project_auto_save_signature()))) {
    return()
  }
  builder_project_auto_save_signature(signature)
  request_builder_project_save(
    show_actions = FALSE,
    materialize = FALSE,
    notify = FALSE,
    after = function(ok) {
      if (!isTRUE(ok)) {
        showNotification(
          "The new dataset was not saved to the project. Use Save project to retry.",
          type = "error",
          duration = 8
        )
      }
    }
  )
})

show_builder_project_folder_error <- function(message, type = "error") {
  showNotification(message, type = type, duration = 7)
  invisible(FALSE)
}

builder_project_folder_available <- function(folder) {
  if (inherits(folder, "condition")) {
    return(show_builder_project_folder_error(conditionMessage(folder)))
  }
  if (identical(folder$kind, "project")) {
    return(show_builder_project_folder_error(
      "This folder already contains a Builder project. Use Open project instead.",
      type = "warning"
    ))
  }
  conflicts <- folder$managed_conflicts %||% character()
  if (length(conflicts)) {
    return(show_builder_project_folder_error(paste0(
      "This folder already contains Builder-managed names: ",
      paste(conflicts, collapse = ", "),
      ". Choose another folder."
    )))
  }
  TRUE
}

create_builder_project_in_folder <- function(path) {
  folder <- tryCatch(builder_project_folder_state(path), error = identity)
  if (!isTRUE(builder_project_folder_available(folder))) {
    return(invisible(FALSE))
  }
  manifest_path <- builder_project_manifest_path(folder$root)
  manifest <- builder_project_new_manifest(folder$root)
  builder_project_pending_folder(NULL)
  builder_project(list(
    root = folder$root,
    path = manifest_path,
    name = manifest$project$name,
    manifest = manifest
  ))
  shiny::removeModal()
  request_builder_project_save(show_actions = FALSE, materialize = TRUE)
  invisible(TRUE)
}

request_builder_project_folder <- function() {
  builder_project_pending_folder(NULL)
  if (!builder_operation_allowed("create_project")) {
    return(invisible(FALSE))
  }
  choice <- builder_choose_project_directory()
  if (!builder_operation_allowed("create_project")) {
    return(invisible(FALSE))
  }
  if (identical(choice$status, "cancelled")) {
    return(invisible(FALSE))
  }
  if (!identical(choice$status, "selected")) {
    showNotification(
      choice$error %||% "The project folder could not be opened.",
      type = "error",
      duration = 7
    )
    return(invisible(FALSE))
  }
  folder <- tryCatch(
    builder_project_folder_state(choice$path),
    error = identity
  )
  if (!isTRUE(builder_project_folder_available(folder))) {
    return(invisible(FALSE))
  }
  if (identical(folder$kind, "nonempty")) {
    builder_project_pending_folder(choice$path)
    shiny::showModal(builder_project_nonempty_folder_dialog(choice$path))
    return(invisible(TRUE))
  }
  create_builder_project_in_folder(choice$path)
}

observeEvent(input$choose_builder_project_folder, {
  request_builder_project_folder()
})

observeEvent(input$confirm_builder_project_folder, {
  if (!builder_operation_allowed("create_project")) {
    return()
  }
  path <- isolate(builder_project_pending_folder())
  if (is.null(path)) {
    return()
  }
  folder <- tryCatch(builder_project_folder_state(path), error = identity)
  builder_project_pending_folder(NULL)
  if (!isTRUE(builder_project_folder_available(folder))) {
    shiny::removeModal()
    return()
  }
  create_builder_project_in_folder(path)
})

observeEvent(input$choose_another_builder_project_folder, {
  builder_project_pending_folder(NULL)
  shiny::removeModal()
  request_builder_project_folder()
})

observeEvent(input$cancel_builder_project_folder, {
  builder_project_pending_folder(NULL)
  shiny::removeModal()
})

session$onSessionEnded(function() {
  builder_project_pending_folder(NULL)
})

observeEvent(input$save_builder_project, {
  if (is.null(isolate(builder_project()))) {
    if (!builder_operation_allowed("create_project")) {
      return()
    }
    shiny::showModal(builder_project_first_save_dialog())
  } else {
    if (!builder_operation_allowed("save_project")) {
      return()
    }
    request_builder_project_save(show_actions = TRUE, materialize = TRUE)
  }
})

observeEvent(input$open_builder_project, {
  if (!builder_operation_allowed("open_project")) {
    return()
  }
  choice <- builder_choose_project_manifest()
  if (!builder_operation_allowed("open_project")) {
    return()
  }
  if (identical(choice$status, "cancelled")) {
    return()
  }
  if (!identical(choice$status, "selected")) {
    showNotification(
      choice$error %||% "The project file could not be opened.",
      type = "error",
      duration = 7
    )
    return()
  }
  builder_project_restore_progress(list(
    mode = "opening",
    total = 0L,
    remaining = 0L
  ))
  builder_project_operation_phase("opening")
  selected_path <- choice$path
  session$onFlushed(
    function() {
      shiny::isolate({
        manifest <- tryCatch(
          builder_project_read(selected_path),
          error = identity
        )
        if (inherits(manifest, "error")) {
          builder_project_restore_progress(list(
            mode = "idle",
            total = 0L,
            remaining = 0L
          ))
          builder_project_operation_phase("idle")
          showNotification(
            conditionMessage(manifest),
            type = "error",
            duration = 8
          )
          return()
        }
        if (identical(manifest$pending_build$status %||% NULL, "running")) {
          manifest$pending_build$status <- "interrupted"
        }
        root <- dirname(selected_path)
        statuses <- builder_project_status_snapshot(manifest, root)
        manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
          record$runtime_restore_status <- statuses[[record$id]] %||% NULL
          record
        })
        builder_project_restore(list(
          manifest = manifest,
          root = root,
          path = selected_path
        ))
        builder_project_restore_progress(list(
          mode = "idle",
          total = 0L,
          remaining = 0L
        ))
        builder_project_operation_phase("idle")
        shiny::showModal(builder_project_restore_dialog(manifest, root))
      })
    },
    once = TRUE
  )
})

observeEvent(input$confirm_builder_project_open, {
  pending <- isolate(builder_project_restore())
  operation <- isolate(builder_project_operation_phase())
  if (is.null(pending) || !operation %in% c("idle", "conflict")) {
    return()
  }
  manifest <- pending$manifest
  root <- pending$root
  records <- builder_project_record_map(manifest)
  reusable_entries <- list()
  pending_entries <- list()
  artifacts <- list()
  for (record in records) {
    action <- input[[paste0("project_restore_", record$id)]] %||% "skip"
    status <- builder_project_dataset_status(record, root)
    if (identical(action, "reuse") && status$artifact_ready) {
      entry <- builder_project_restore_entry(
        record,
        root,
        hydrate_spatial_assets = FALSE,
        status = status
      )
      entry <- builder_project_artifact_entry(
        entry,
        record$artifact,
        root,
        status = status,
        record = record
      )
      reusable_entries[[length(reusable_entries) + 1L]] <- entry
      artifacts[[record$id]] <- record$artifact
    } else if (identical(action, "resume") && status$restorable) {
      record$runtime_restore_status <- status
      pending_entries[[record$id]] <- record
    }
  }
  reusable_entries <- reusable_entries[order(vapply(
    reusable_entries,
    function(entry) records[[entry$id]]$order %||% 0L,
    numeric(1)
  ))]
  invalidate_builder_project_source_sync()
  store(builder_state(
    datasets = reusable_entries,
    current_dataset = if (length(reusable_entries)) {
      reusable_entries[[1L]]$id
    } else {
      NULL
    }
  ))
  projection_previews(list())
  trajectory_previews(list())
  spatial_previews(list())
  builder_project_configuration_cache_clear(
    builder_configuration_identity_cache
  )
  marks <- character()
  if (length(reusable_entries)) {
    for (entry in reusable_entries) {
      record <- records[[entry$id]]
      status <- builder_project_dataset_status(record, root)
      restored_mark <- builder_project_restored_check_identity(
        record,
        entry,
        status,
        root
      )
      if (!is.null(restored_mark)) {
        marks[[entry$id]] <- restored_mark
      }
    }
  }
  dataset_check_marks(marks)
  builder_project_pending_entries(pending_entries)
  builder_project_artifacts(artifacts)
  builder_project(list(
    root = root,
    path = pending$path,
    name = manifest$project$name,
    manifest = manifest
  ))
  restore_builder_project_preferences(manifest)
  builder_project_restore(NULL)
  shiny::removeModal()
  restore_total <- length(pending_entries)
  if (!restore_total) {
    builder_project_restore_progress(list(
      mode = "idle",
      total = 0L,
      remaining = 0L
    ))
    builder_project_operation_phase("idle")
    restore_builder_project_last_ui(manifest)
    return()
  }
  builder_project_restore_progress(list(
    mode = "restoring",
    total = restore_total,
    remaining = restore_total
  ))
  ## Keep the project locked while the client receives the restore UI update,
  ## but do not advertise the sources as restoring until start_load() has
  ## registered their import records. Otherwise the abandoned-entry observer
  ## can run in this flush, see neither a live dataset nor an import, and drop
  ## the saved settings before the onFlushed callback gets to enqueue them.
  builder_project_operation_phase("opening")
  entries_to_restore <- pending_entries
  session$onFlushed(
    function() {
      shiny::isolate({
        for (record in entries_to_restore) {
          source <- record$source %||% list()
          if (identical(source$kind, "example")) {
            start_load(
              "example",
              source$example,
              record$name,
              dataset_id = record$id
            )
          } else {
            path <- builder_project_resolve_path(
              source$path,
              root,
              source$kind %||% "managed"
            )
            info <- file.info(path)
            start_load(
              "file",
              path,
              record$name,
              file_meta = list(
                name = source$filename %||% basename(path),
                type = "application/octet-stream",
                size = as.double(info$size[[1L]])
              ),
              dataset_id = record$id,
              source_origin = source$origin %||% "upload",
              example_id = source$example %||% NULL
            )
          }
        }
        builder_project_operation_phase("restoring")
      })
    },
    once = TRUE
  )
})

observe({
  pending <- builder_project_pending_entries()
  if (!length(pending)) {
    return()
  }
  state <- store()
  entries <- state$datasets %||% list()
  root <- isolate(builder_project())$root
  batch <- builder_project_hydrate_pending_entries(entries, pending, root)
  restored_ids <- names(batch$restored)
  failed_ids <- names(batch$failures)
  if (!length(restored_ids) && !length(failed_ids)) {
    return()
  }
  entries <- batch$entries
  pending <- batch$pending
  marks <- isolate(dataset_check_marks())
  for (id in restored_ids) {
    restored <- batch$restored[[id]]$entry
    record <- batch$restored[[id]]$record
    project <- isolate(builder_project())
    status <- builder_project_dataset_status(record, project$root)
    restored_mark <- builder_project_restored_check_identity(
      record,
      restored,
      status,
      project$root
    )
    if (!is.null(restored_mark)) {
      marks[[id]] <- restored_mark
    }
  }
  if (length(failed_ids)) {
    marks <- marks[setdiff(names(marks), failed_ids)]
    pending_drops <- isolate(pending_snapshot_drops())
    for (id in failed_ids) {
      failed <- batch$failures[[id]]
      identity <- try(
        .builder_worker_identity(failed$entry$snapshot),
        silent = TRUE
      )
      if (!inherits(identity, "try-error")) {
        pending_drops[[id]] <- identity
        queued <- enqueue(list(
          kind = "drop",
          id = id,
          dataset_revision = failed$entry$revision %||% 0L,
          snapshot_identity = identity,
          note = "Releasing a failed project restore…"
        ))
        if (!isTRUE(queued)) {
          current_worker <- isolate(worker())
          current_protocol <- isolate(protocol())
          forgotten <- try(
            builder_protocol_forget_dataset(current_protocol, id),
            silent = TRUE
          )
          released_worker <- if (!inherits(forgotten, "try-error")) {
            try(
              builder_worker_release_snapshot(
                current_worker,
                id,
                expected_identity = identity
              ),
              silent = TRUE
            )
          } else {
            forgotten
          }
          if (
            !inherits(released_worker, "try-error") &&
              !inherits(forgotten, "try-error")
          ) {
            worker(released_worker)
            protocol(forgotten$protocol)
            pending_drops[[id]] <- NULL
          } else {
            recovered <- restart_worker_protocol(
              current_worker,
              current_protocol,
              paste0(
                "The failed project restore for ",
                id,
                " could not enqueue snapshot cleanup."
              )
            )
            if (isTRUE(recovered)) {
              enqueue(list(
                kind = "drop",
                id = id,
                dataset_revision = failed$entry$revision %||% 0L,
                snapshot_identity = identity,
                note = "Retrying failed project restore cleanup…"
              ))
            }
          }
        }
      }
    }
    pending_snapshot_drops(pending_drops)
    add_error(paste(
      vapply(
        batch$failures,
        `[[`,
        character(1),
        "message"
      ),
      collapse = "\n"
    ))
  }
  sets(entries)
  alignment_server$restore_project_settings(restored_ids)
  dataset_check_marks(marks)
  builder_project_pending_entries(pending)
  progress <- isolate(builder_project_restore_progress())
  builder_project_restore_progress(list(
    mode = "restoring",
    total = as.integer(progress$total %||% length(pending)),
    remaining = length(pending)
  ))
  restored_last_ui <- isolate(builder_project())$manifest$last_ui
  saved <- save_builder_project_state(
    show_actions = FALSE,
    materialize = FALSE,
    notify = FALSE,
    manage_lifecycle = FALSE,
    last_ui = restored_last_ui
  )
  if (!length(pending)) {
    builder_project_restore_progress(list(
      mode = "idle",
      total = 0L,
      remaining = 0L
    ))
    builder_project_operation_phase(
      if (isTRUE(saved)) "idle" else "save_failed"
    )
    if (isTRUE(saved)) {
      restore_builder_project_last_ui(
        list(last_ui = restored_last_ui)
      )
    }
  }
})

observe({
  if (!identical(builder_project_operation_phase(), "restoring")) {
    return()
  }
  pending <- builder_project_pending_entries()
  if (!length(pending)) {
    builder_project_operation_phase("idle")
    return()
  }
  live_ids <- vapply(sets(), `[[`, character(1), "id")
  import_ids <- vapply(
    imports()$entries %||% list(),
    `[[`,
    character(1),
    "id"
  )
  abandoned <- builder_project_abandoned_entries(
    pending,
    live_ids,
    import_ids,
    builder_project_operation_phase()
  )
  if (!length(abandoned)) {
    return()
  }
  pending[abandoned] <- NULL
  builder_project_pending_entries(pending)
  progress <- isolate(builder_project_restore_progress())
  builder_project_restore_progress(list(
    mode = if (length(pending)) "restoring" else "idle",
    total = as.integer(progress$total %||% length(pending)),
    remaining = length(pending)
  ))
  if (!length(pending)) {
    builder_project_operation_phase("idle")
  }
})

observeEvent(input$project_resume_current_source, {
  if (!builder_operation_allowed("edit_dataset")) {
    return()
  }
  id <- isolate(current())
  project <- isolate(builder_project())
  if (is.null(project) || is.null(id)) {
    return()
  }
  records <- builder_project_record_map(project$manifest)
  record <- records[[id]] %||% NULL
  status <- if (is.null(record)) {
    NULL
  } else {
    builder_project_dataset_status(record, project$root)
  }
  if (is.null(record) || !isTRUE(status$restorable)) {
    showNotification(
      "The source file must be re-linked before editing.",
      type = "warning"
    )
    return()
  }
  pending <- isolate(builder_project_pending_entries())
  previous_pending <- pending[[id]] %||% NULL
  pending[[id]] <- record
  builder_project_pending_entries(pending)
  source <- record$source
  started <- if (identical(source$kind, "example")) {
    start_load("example", source$example, record$name, dataset_id = id)
  } else {
    path <- builder_project_resolve_path(source$path, project$root, source$kind)
    info <- file.info(path)
    start_load(
      "file",
      path,
      record$name,
      file_meta = list(
        name = source$filename %||% basename(path),
        type = "application/octet-stream",
        size = as.double(info$size[[1L]])
      ),
      dataset_id = id,
      source_origin = source$origin %||% "upload",
      example_id = source$example %||% NULL
    )
  }
  if (!isTRUE(started)) {
    pending <- isolate(builder_project_pending_entries())
    pending[[id]] <- previous_pending
    builder_project_pending_entries(pending)
  }
})

enqueue_builder_project_checkpoint <- function(plan) {
  current_protocol <- isolate(protocol())
  previous_project <- isolate(builder_project())
  if (
    is.null(current_protocol) ||
      !isTRUE(tryCatch(
        builder_protocol_is_quiescent(current_protocol),
        error = function(error) FALSE
      ))
  ) {
    if (!is.null(previous_project)) {
      builder_project_cleanup_checkpoint(plan$out_dir, previous_project$root)
    }
    showNotification(
      "Wait for the current background action to finish.",
      type = "warning"
    )
    return(invisible(FALSE))
  }
  builder_project_checkpoint(TRUE)
  project <- previous_project
  if (!is.null(project)) {
    project$manifest$pending_build <- list(
      id = paste0(
        "checkpoint-",
        format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
      ),
      status = "running",
      dataset_ids = as.character(vapply(plan$items, `[[`, character(1), "id")),
      started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    )
    builder_project(project)
    saved <- save_builder_project_state(
      show_actions = FALSE,
      materialize = FALSE,
      notify = FALSE
    )
    if (!isTRUE(saved)) {
      builder_project_checkpoint(FALSE)
      builder_project(previous_project)
      builder_project_cleanup_checkpoint(plan$out_dir, previous_project$root)
      showNotification(
        builder_project_last_save_error() %||%
          "The checkpoint could not be saved. The build was not started.",
        type = "error",
        duration = 8
      )
      return(invisible(FALSE))
    }
  }
  result(NULL)
  queued <- enqueue(list(
    kind = "build",
    plan = unserialize(serialize(plan, NULL, version = 3L)),
    auth_accounts = builder_auth_empty_accounts(),
    note = paste0(
      "Preparing ",
      length(plan$items),
      " checked CRB",
      if (length(plan$items) == 1L) "" else "s",
      "…"
    )
  ))
  if (!isTRUE(queued)) {
    builder_project_checkpoint(FALSE)
    project <- isolate(builder_project())
    failed_saved <- FALSE
    if (!is.null(project)) {
      project$manifest <- builder_project_finish_pending_build(
        project$manifest,
        status = "failed",
        error = "The checkpoint build could not be queued."
      )
      builder_project(project)
      failed_saved <- save_builder_project_state(
        show_actions = FALSE,
        materialize = FALSE,
        notify = FALSE
      )
    }
    if (isTRUE(failed_saved) && !is.null(previous_project)) {
      builder_project_cleanup_checkpoint(plan$out_dir, previous_project$root)
    }
    showNotification(
      "The checkpoint build could not be queued.",
      type = "error",
      duration = 7
    )
    return(invisible(FALSE))
  }
  builder_project_capture_build_plan(plan)
  build_flow(list(stage = "building", plan = NULL))
  shiny::removeModal()
  showNotification(
    "The project is safe. Checked CRBs are being prepared.",
    type = "message",
    duration = 5
  )
  invisible(TRUE)
}

prepare_builder_project_crbs <- function() {
  project <- isolate(builder_project())
  checked <- isolate(checked_dataset_ids())
  entries <- Filter(function(entry) entry$id %in% checked, isolate(sets()))
  if (is.null(project) || !length(entries)) {
    return(invisible(FALSE))
  }
  entries <- builder_project_entries_requiring_crb(
    entries,
    isolate(builder_project_artifacts()),
    project$root
  )
  if (!length(entries)) {
    session$sendCustomMessage(
      "builder_project_crb_progress",
      list(status = "ready", completed = 0L, total = 0L)
    )
    return(invisible(TRUE))
  }
  budget <- builder_project_checkpoint_budget(entries, project$root)
  if (!isTRUE(budget$ok)) {
    showNotification(
      budget$error %||% "The project volume does not have enough free space.",
      type = "error",
      duration = 8
    )
    return(invisible(FALSE))
  }
  checkpoint_parent <- file.path(project$root, "checkpoints")
  if (
    .builder_project_path_has_link_within(checkpoint_parent, project$root)
  ) {
    showNotification(
      "The checkpoint folder is not a safe managed project path.",
      type = "error",
      duration = 8
    )
    return(invisible(FALSE))
  }
  output <- file.path(
    project$root,
    "checkpoints",
    format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
  )
  if (!dir.create(output, recursive = TRUE, showWarnings = FALSE)) {
    showNotification(
      "The checkpoint folder could not be created.",
      type = "error",
      duration = 8
    )
    return(invisible(FALSE))
  }
  checkpoint_entries <- builder_project_checkpoint_entries(entries)
  plan <- freeze_plan_for_output(
    output,
    overwrite = TRUE,
    output_options = builder_build_options(make_app = FALSE),
    entries_override = checkpoint_entries
  )
  if (
    !inherits(plan, "builder_build_plan") ||
      !identical(plan$readiness, "ready")
  ) {
    error <- plan$error %||% "Checked CRBs could not be prepared."
    session$sendCustomMessage(
      "builder_project_crb_progress",
      list(
        status = "failed",
        completed = 0L,
        total = length(entries),
        error = error
      )
    )
    showNotification(error, type = "error", duration = 8)
    builder_project_cleanup_checkpoint(output, project$root)
    return(invisible(FALSE))
  }
  session$sendCustomMessage(
    "builder_project_crb_progress",
    list(status = "building", completed = 0L, total = length(plan$items))
  )
  queued <- enqueue_builder_project_checkpoint(plan)
  if (!isTRUE(queued)) {
    session$sendCustomMessage(
      "builder_project_crb_progress",
      list(
        status = "failed",
        completed = 0L,
        total = length(plan$items),
        error = "Reusable CRBs could not be queued."
      )
    )
  }
  invisible(isTRUE(queued))
}

observeEvent(input$prepare_builder_project_crbs, {
  if (!builder_operation_allowed("prepare_crbs")) {
    return()
  }
  if (isTRUE(isolate(builder_project_dirty()))) {
    request_builder_project_save(
      show_actions = FALSE,
      materialize = TRUE,
      notify = TRUE,
      after = function(ok) {
        if (isTRUE(ok)) prepare_builder_project_crbs()
      }
    )
  } else {
    prepare_builder_project_crbs()
  }
})

observe({
  value <- result()
  plan <- isolate(builder_project_build_plan())
  project <- isolate(builder_project())
  if (is.null(plan) || is.null(project) || !is.list(value)) {
    return()
  }
  terminal <- any(vapply(
    c("success", "failure", "needs_decision"),
    identical,
    logical(1),
    y = value$state
  )) ||
    isTRUE(value$published)
  if (!isTRUE(terminal)) {
    return()
  }
  total_crbs <- length(plan$items)
  completed_crbs <- length(value$built %||% character())
  session$sendCustomMessage(
    "builder_project_crb_progress",
    list(
      status = "registering",
      completed = completed_crbs,
      total = total_crbs
    )
  )
  builder_project_operation_phase("registering")
  builder_project_build_plan(NULL)
  builder_project_checkpoint(FALSE)
  session$onFlushed(
    function() {
      project_before_registration <- project
      on.exit(
        {
          if (
            identical(isolate(builder_project_operation_phase()), "registering")
          ) {
            builder_project_operation_phase("save_failed")
          }
        },
        add = TRUE
      )
      terminal_status <- if (
        identical(value$state, "success") &&
          length(value$built %||% character())
      ) {
        "completed"
      } else {
        "failed"
      }
      if (!is.null(project$manifest$pending_build)) {
        project$manifest <- builder_project_finish_pending_build(
          project$manifest,
          status = terminal_status,
          error = if (identical(terminal_status, "failed")) {
            value$error %||% "The checkpoint build failed."
          } else {
            NULL
          }
        )
        builder_project(project)
      }
      if (
        !identical(value$state, "success") ||
          !length(value$built %||% character())
      ) {
        saved <- save_builder_project_state(
          show_actions = FALSE,
          materialize = FALSE,
          notify = FALSE,
          manage_lifecycle = FALSE
        )
        builder_project_operation_phase(
          if (isTRUE(saved)) "idle" else "save_failed"
        )
        if (!isTRUE(saved)) {
          builder_project(project_before_registration)
        }
        builder_project_cleanup_terminal_checkpoint(
          saved = saved,
          status = terminal_status,
          path = plan$out_dir,
          root = project$root
        )
        session$sendCustomMessage(
          "builder_project_crb_progress",
          list(
            status = "failed",
            completed = completed_crbs,
            total = total_crbs,
            error = value$error %||% "Reusable CRBs could not be prepared."
          )
        )
        return()
      }
      previous_artifacts <- isolate(builder_project_artifacts())
      artifacts <- previous_artifacts
      entries <- isolate(sets())
      entry_ids <- vapply(entries, `[[`, character(1), "id")
      registration_error <- NULL
      for (item in plan$items) {
        built <- value$built[[item$name]] %||% NULL
        if (!.builder_project_text(built) || !file.exists(built)) {
          registration_error <- paste0(
            "The reusable CRB for ",
            item$name,
            " is missing."
          )
          break
        }
        bundle <- tryCatch(
          builder_project_store_artifact_bundle(
            built,
            sidecars = item$sidecars %||% character(),
            dataset_id = item$id,
            root = project$root,
            promote = TRUE
          ),
          error = function(error) error
        )
        if (inherits(bundle, "condition")) {
          registration_error <- conditionMessage(bundle)
          break
        }
        stored_item <- item
        stored_item$reused_artifact <- NULL
        revision <- if (item$id %in% entry_ids) {
          entries[[match(item$id, entry_ids)]]$revision %||% 0L
        } else {
          0L
        }
        artifacts[[item$id]] <- list(
          status = "ready",
          reusable = identical(
            item$spatial_image_storage %||% "embedded",
            "embedded"
          ) &&
            !length(setdiff(
              item$private_assets %||% character(),
              c(item$filename, item$sidecars %||% character())
            )),
          path = bundle$path,
          fingerprint = bundle$fingerprint,
          built_from_revision = as.integer(revision),
          built_from_configuration = if (item$id %in% entry_ids) {
            builder_project_configuration_digest(entries[[match(
              item$id,
              entry_ids
            )]])
          } else {
            NULL
          },
          built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
          plan_payload = jsonlite::serializeJSON(
            stored_item,
            digits = NA,
            pretty = FALSE
          ),
          members = bundle$members
        )
      }
      if (.builder_project_text(registration_error)) {
        project$manifest <- builder_project_finish_pending_build(
          project$manifest,
          status = "failed",
          error = registration_error
        )
        builder_project(project)
        saved <- save_builder_project_state(
          show_actions = FALSE,
          materialize = FALSE,
          notify = FALSE,
          manage_lifecycle = FALSE
        )
        builder_project_operation_phase(
          if (isTRUE(saved)) "idle" else "save_failed"
        )
        if (!isTRUE(saved)) {
          builder_project(project_before_registration)
        }
        builder_project_cleanup_terminal_checkpoint(
          saved = saved,
          status = "failed",
          path = plan$out_dir,
          root = project$root
        )
        session$sendCustomMessage(
          "builder_project_crb_progress",
          list(
            status = "failed",
            completed = 0L,
            total = total_crbs,
            error = registration_error
          )
        )
        showNotification(registration_error, type = "error", duration = 8)
        return()
      }
      builder_project_artifacts(artifacts)
      saved <- save_builder_project_state(
        show_actions = FALSE,
        materialize = FALSE,
        notify = FALSE,
        manage_lifecycle = FALSE
      )
      builder_project_operation_phase(
        if (isTRUE(saved)) "idle" else "save_failed"
      )
      if (!isTRUE(saved)) {
        builder_project_artifacts(previous_artifacts)
        builder_project(project_before_registration)
      }
      if (isTRUE(saved)) {
        builder_project_cleanup_checkpoint(plan$out_dir, project$root)
        session$sendCustomMessage(
          "builder_project_crb_progress",
          list(status = "ready", completed = total_crbs, total = total_crbs)
        )
        showNotification(
          "Reusable CRBs were added to the project.",
          type = "message",
          duration = 5
        )
      } else {
        session$sendCustomMessage(
          "builder_project_crb_progress",
          list(
            status = "failed",
            completed = completed_crbs,
            total = total_crbs,
            error = "Reusable CRBs were created but could not be saved to the project."
          )
        )
      }
    },
    once = TRUE
  )
})
