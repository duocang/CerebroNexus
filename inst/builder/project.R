## Builder projects keep durable user choices separate from session-only
## workers. Schema v3 is an index: exact configuration lives in small typed
## sidecars and source-derived profiles live in replaceable binary caches.

.builder_project_schema_version <- 3L
.builder_project_supported_schema_versions <- c(1L, 2L, 3L)
.builder_project_config_schema_version <- 1L
.builder_project_profile_cache_schema_version <- 1L
.builder_project_configuration_contract_version <- 1L

.builder_project_phases <- c(
  "none",
  "clean",
  "dirty",
  "saving",
  "opening",
  "restoring",
  "registering",
  "save_failed",
  "conflict"
)

builder_activity_state <- function(
  connection = "connected",
  client_imports = 0L,
  server_imports = FALSE,
  project_phase = "none",
  spatial_dirty = FALSE,
  source_syncing = FALSE,
  build_locked = FALSE,
  has_project = FALSE,
  has_datasets = FALSE
) {
  if (
    !identical(connection, "connected") &&
      !identical(connection, "disconnected")
  ) {
    stop("A valid Builder connection state is required.", call. = FALSE)
  }
  client_imports <- suppressWarnings(as.integer(client_imports))
  if (
    length(client_imports) != 1L || is.na(client_imports) || client_imports < 0L
  ) {
    stop("A valid client import count is required.", call. = FALSE)
  }
  if (
    !is.logical(server_imports) ||
      length(server_imports) != 1L ||
      is.na(server_imports) ||
      !is.character(project_phase) ||
      length(project_phase) != 1L ||
      is.na(project_phase) ||
      !project_phase %in% .builder_project_phases ||
      !is.logical(spatial_dirty) ||
      length(spatial_dirty) != 1L ||
      is.na(spatial_dirty) ||
      !is.logical(source_syncing) ||
      length(source_syncing) != 1L ||
      is.na(source_syncing) ||
      !is.logical(build_locked) ||
      length(build_locked) != 1L ||
      is.na(build_locked) ||
      !is.logical(has_project) ||
      length(has_project) != 1L ||
      is.na(has_project) ||
      !is.logical(has_datasets) ||
      length(has_datasets) != 1L ||
      is.na(has_datasets)
  ) {
    stop("A valid Builder activity state is required.", call. = FALSE)
  }
  structure(
    list(
      connection = connection,
      client_imports = client_imports,
      server_imports = server_imports,
      project_phase = project_phase,
      spatial_dirty = spatial_dirty,
      source_syncing = source_syncing,
      build_locked = build_locked,
      has_project = has_project,
      has_datasets = has_datasets
    ),
    class = c("builder_activity_state", "list")
  )
}

builder_activity_capabilities <- function(activity) {
  if (!inherits(activity, "builder_activity_state")) {
    stop("A Builder activity state is required.", call. = FALSE)
  }
  connected <- identical(activity$connection, "connected")
  importing <- activity$client_imports > 0L || isTRUE(activity$server_imports)
  project_busy <- activity$project_phase %in%
    c("saving", "opening", "restoring", "registering", "conflict")
  mutable <- connected && !isTRUE(activity$build_locked) && !project_busy
  stable <- mutable && !importing
  open_safe <- connected &&
    !isTRUE(activity$build_locked) &&
    !importing &&
    !isTRUE(activity$spatial_dirty) &&
    ((identical(activity$project_phase, "none") &&
      !isTRUE(activity$has_project) &&
      !isTRUE(activity$has_datasets)) ||
      identical(activity$project_phase, "conflict"))
  list(
    select_dataset = connected &&
      !activity$project_phase %in%
        c("saving", "registering"),
    add_dataset = mutable,
    edit_dataset = mutable,
    mutate_datasets = mutable,
    check_dataset = stable,
    create_project = mutable &&
      !isTRUE(activity$has_project) &&
      isTRUE(activity$has_datasets),
    save_project = stable &&
      isTRUE(activity$has_project) &&
      isTRUE(activity$has_datasets) &&
      activity$project_phase %in%
        c("clean", "dirty", "save_failed"),
    open_project = open_safe,
    prepare_crbs = stable && isTRUE(activity$has_project),
    navigate_workflow = stable,
    build = stable,
    page_inert = connected &&
      activity$project_phase %in%
        c("saving", "opening", "restoring", "registering"),
    warn_before_unload = importing ||
      isTRUE(activity$build_locked) ||
      isTRUE(activity$source_syncing) ||
      isTRUE(activity$spatial_dirty) ||
      activity$project_phase %in%
        c(
          "dirty",
          "saving",
          "opening",
          "restoring",
          "registering",
          "save_failed",
          "conflict"
        )
  )
}

builder_imports_idle <- function(activity, protocol) {
  if (!inherits(activity, "builder_activity_state")) {
    stop("A Builder activity state is required.", call. = FALSE)
  }
  if (!is.list(protocol)) {
    return(FALSE)
  }
  requests <- c(
    if (is.null(protocol$pending)) list() else list(protocol$pending),
    protocol$queue %||% list(),
    unname(protocol$awaiting_ack %||% list())
  )
  has_pending_load <- any(vapply(
    requests,
    function(request) is.list(request) && identical(request$kind, "load"),
    logical(1)
  ))
  activity$client_imports == 0L &&
    !isTRUE(activity$server_imports) &&
    !has_pending_load
}

builder_project_first_save_offer_ready <- function(
  entries,
  project,
  offered,
  activity,
  protocol
) {
  length(entries) > 0L &&
    is.null(project) &&
    !isTRUE(offered) &&
    builder_imports_idle(activity, protocol)
}

builder_activity_reason <- function(activity, operation) {
  capabilities <- builder_activity_capabilities(activity)
  if (isTRUE(capabilities[[operation]])) {
    return(NULL)
  }
  if (identical(activity$connection, "disconnected")) {
    return("Reconnect to Builder before continuing.")
  }
  if (isTRUE(activity$build_locked)) {
    return("Wait for the active build to finish.")
  }
  if (identical(activity$project_phase, "conflict")) {
    return("Reopen the project before saving more changes.")
  }
  if (activity$project_phase %in% c("saving", "registering")) {
    return("Wait for the project operation to finish.")
  }
  if (activity$project_phase %in% c("opening", "restoring")) {
    return("Wait for the selected project datasets to finish loading.")
  }
  if (activity$client_imports > 0L || isTRUE(activity$server_imports)) {
    return("Wait for all dataset imports to finish.")
  }
  switch(
    operation,
    create_project = "Add a dataset before creating a Builder project.",
    save_project = "Add a dataset before saving the project.",
    open_project = "Open a saved project from an empty Builder session.",
    prepare_crbs = "Save a Builder project before preparing reusable CRBs.",
    "This action is not available right now."
  )
}

builder_project_abandoned_entries <- function(
  pending,
  live_ids = character(),
  import_ids = character(),
  operation = "idle"
) {
  if (!identical(operation, "restoring") || !length(pending)) {
    return(character())
  }
  setdiff(names(pending), c(live_ids, import_ids))
}

builder_project_live_dirty <- function(entries, checked_ids, manifest) {
  if (!is.list(manifest) || !is.list(manifest$datasets) || !length(entries)) {
    return(FALSE)
  }
  record_ids <- vapply(
    manifest$datasets,
    function(record) as.character(record$id),
    character(1)
  )
  records <- stats::setNames(manifest$datasets, record_ids)
  ids <- vapply(entries, `[[`, character(1), "id")
  if (any(!ids %in% names(records))) {
    return(TRUE)
  }
  saved_order <- names(sort(vapply(
    records[ids],
    function(record) as.integer(record$order %||% 0L),
    integer(1)
  )))
  if (!identical(ids, saved_order)) {
    return(TRUE)
  }
  any(vapply(
    seq_along(entries),
    function(index) {
      entry <- entries[[index]]
      record <- records[[ids[[index]]]]
      !identical(
        builder_project_configuration_digest(entry),
        as.character(record$configuration$digest %||% "")
      ) ||
        !identical(
          ids[[index]] %in% checked_ids,
          isTRUE(record$configuration$checked)
        )
    },
    logical(1)
  ))
}
.builder_project_manifest_name <- "builder-project.json"

.builder_project_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_project_integer <- function(value, default = 0L) {
  parsed <- suppressWarnings(as.integer(value %||% default))
  if (length(parsed) != 1L || is.na(parsed) || parsed < 0L) {
    return(as.integer(default))
  }
  parsed
}

.builder_project_identifier <- function(value) {
  .builder_project_text(value) &&
    nchar(value, type = "bytes") <= 128L &&
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", value)
}

builder_project_allocate_dataset_id <- function(
  sequence,
  existing_ids = character(),
  restored_id = NULL
) {
  sequence <- .builder_project_integer(sequence)
  existing_ids <- as.character(existing_ids)
  if (!is.null(restored_id)) {
    if (!.builder_project_identifier(restored_id)) {
      stop("A restored dataset requires a safe stable id.", call. = FALSE)
    }
    if (restored_id %in% existing_ids) {
      stop("The restored dataset id is already in use.", call. = FALSE)
    }
    matched <- regmatches(
      restored_id,
      regexec("^ds([0-9]+)$", restored_id)
    )[[1L]]
    if (length(matched) == 2L) {
      restored_sequence <- suppressWarnings(as.integer(matched[[2L]]))
      if (!is.na(restored_sequence)) {
        sequence <- max(sequence, restored_sequence)
      }
    }
    return(list(id = restored_id, sequence = as.integer(sequence)))
  }
  repeat {
    if (sequence >= .Machine$integer.max) {
      stop("No dataset id remains available in this session.", call. = FALSE)
    }
    sequence <- sequence + 1L
    id <- paste0("ds", sequence)
    if (!id %in% existing_ids) {
      return(list(id = id, sequence = as.integer(sequence)))
    }
  }
}

builder_project_reused_plan_matches <- function(saved, expected) {
  if (!is.list(saved) || !is.list(expected)) {
    return(FALSE)
  }
  fields <- c(
    "id",
    "name",
    "filename",
    "sidecars",
    "viewer_bundle_assets",
    "private_assets",
    "viewer_bundle_asset_claims",
    "private_asset_claims"
  )
  all(vapply(
    fields,
    function(field) identical(saved[[field]], expected[[field]]),
    logical(1)
  ))
}

builder_project_finish_pending_build <- function(
  manifest,
  status = c("completed", "failed", "interrupted"),
  error = NULL,
  finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
) {
  status <- match.arg(status)
  if (!is.list(manifest) || !is.list(manifest$pending_build)) {
    return(manifest)
  }
  manifest$pending_build$status <- status
  manifest$pending_build$finished_at <- as.character(finished_at)
  manifest$pending_build$error <- if (.builder_project_text(error)) {
    as.character(error)
  } else {
    NULL
  }
  manifest
}

builder_project_id <- function() {
  paste0(
    "project-",
    format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"),
    "-",
    sprintf("%08x", sample.int(.Machine$integer.max, 1L))
  )
}

builder_project_manifest_path <- function(root) {
  file.path(root, .builder_project_manifest_name)
}

builder_project_normalize_root <- function(root, must_work = TRUE) {
  if (!.builder_project_text(root)) {
    stop("Choose a project folder.", call. = FALSE)
  }
  root <- normalizePath(
    path.expand(root),
    winslash = "/",
    mustWork = must_work
  )
  if (!dir.exists(root)) {
    stop("The project folder is not available.", call. = FALSE)
  }
  root
}

builder_project_relative_path <- function(path, root) {
  root <- builder_project_normalize_root(root)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (!startsWith(path, prefix)) {
    stop("A managed project file escaped the project folder.", call. = FALSE)
  }
  substring(path, nchar(prefix) + 1L)
}

builder_project_resolve_path <- function(path, root, kind = "managed") {
  if (!.builder_project_text(path)) {
    return(NULL)
  }
  if (identical(kind, "external")) {
    return(normalizePath(path.expand(path), winslash = "/", mustWork = FALSE))
  }
  root <- builder_project_normalize_root(root)
  candidate <- normalizePath(
    file.path(root, path),
    winslash = "/",
    mustWork = FALSE
  )
  prefix <- paste0(root, "/")
  if (!startsWith(candidate, prefix)) {
    stop("A managed project path is unsafe.", call. = FALSE)
  }
  candidate
}

builder_project_file_fingerprint <- function(path, content = FALSE) {
  if (!.builder_project_text(path) || !file.exists(path) || dir.exists(path)) {
    return(NULL)
  }
  info <- file.info(path)
  list(
    bytes = as.double(info$size[[1L]]),
    modified_at = format(
      info$mtime[[1L]],
      "%Y-%m-%dT%H:%M:%OS3Z",
      tz = "UTC"
    ),
    md5 = if (isTRUE(content)) unname(tools::md5sum(path)) else NULL
  )
}

builder_project_fingerprint_matches <- function(recorded, current) {
  if (is.null(recorded) || is.null(current)) {
    return(FALSE)
  }
  same_basic <- identical(
    suppressWarnings(as.double(recorded$bytes)),
    suppressWarnings(as.double(current$bytes))
  ) &&
    identical(
      as.character(recorded$modified_at %||% ""),
      as.character(current$modified_at %||% "")
    )
  if (!same_basic) {
    return(FALSE)
  }
  recorded_md5 <- recorded$md5 %||% NULL
  current_md5 <- current$md5 %||% NULL
  is.null(recorded_md5) ||
    is.null(current_md5) ||
    identical(as.character(recorded_md5), as.character(current_md5))
}

builder_project_content_fingerprint_matches <- function(recorded, current) {
  if (!is.list(recorded) || !is.list(current)) {
    return(FALSE)
  }
  recorded_md5 <- recorded$md5 %||% NULL
  current_md5 <- current$md5 %||% NULL
  if (
    .builder_project_text(recorded_md5) && .builder_project_text(current_md5)
  ) {
    return(identical(as.character(recorded_md5), as.character(current_md5)))
  }
  builder_project_fingerprint_matches(recorded, current)
}

builder_project_stage_source <- function(entry, root) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  source <- entry$path %||% NULL
  if (!.builder_project_text(source) || !file.exists(source)) {
    if (!is.null(entry$example)) {
      return(list(
        entry = entry,
        source = list(
          kind = "example",
          origin = "example",
          example = as.character(entry$example),
          filename = NULL,
          path = NULL,
          fingerprint = paste0("example:", entry$example)
        )
      ))
    }
    return(list(
      entry = entry,
      source = list(
        kind = "missing",
        origin = entry$source_origin %||% "upload",
        filename = if (.builder_project_text(source)) {
          basename(source)
        } else {
          NULL
        },
        path = NULL,
        fingerprint = NULL
      )
    ))
  }
  root <- builder_project_normalize_root(root)
  target_dir <- file.path(root, "sources", entry$id)
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
    stop("The dataset source folder could not be created.", call. = FALSE)
  }
  target <- file.path(target_dir, basename(source))
  same <- identical(
    normalizePath(source, winslash = "/", mustWork = TRUE),
    normalizePath(target, winslash = "/", mustWork = FALSE)
  )
  if (
    !same &&
      !file.copy(
        source,
        target,
        overwrite = TRUE,
        copy.mode = TRUE,
        copy.date = TRUE
      )
  ) {
    stop(
      "The uploaded dataset could not be retained in the project.",
      call. = FALSE
    )
  }
  entry$path <- normalizePath(target, winslash = "/", mustWork = TRUE)
  list(
    entry = entry,
    source = list(
      kind = "managed",
      origin = entry$source_origin %||%
        if (!is.null(entry$example)) {
          "example"
        } else {
          "upload"
        },
      example = entry$example %||% NULL,
      filename = basename(target),
      path = builder_project_relative_path(target, root),
      fingerprint = builder_project_file_fingerprint(target)
    )
  )
}

builder_project_example_source <- function(example_id, catalog) {
  if (!.builder_project_text(example_id) || !is.list(catalog)) {
    stop("A valid Builder example is required.", call. = FALSE)
  }
  record <- catalog[[example_id]] %||% NULL
  source <- if (is.list(record)) record$serialized_path %||% NULL else NULL
  if (!.builder_project_text(source) || !file.exists(source)) {
    stop("The Builder example source file was not found.", call. = FALSE)
  }
  list(
    origin = "example",
    example = example_id,
    path = normalizePath(source, winslash = "/", mustWork = TRUE),
    filename = basename(source)
  )
}

builder_project_source_job <- function(entry, root) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  source <- entry$path %||% NULL
  filename <- entry$filename %||%
    if (.builder_project_text(source)) {
      basename(source)
    } else {
      NULL
    }
  if (!.builder_project_text(filename)) {
    filename <- paste0(entry$id, ".rds")
  }
  filename <- basename(filename)
  target <- file.path(root, "sources", entry$id, filename)
  list(
    id = entry$id,
    source = source,
    target = target,
    part = paste0(target, ".part"),
    filename = filename,
    origin = entry$source_origin %||%
      if (!is.null(entry$example)) {
        "example"
      } else {
        "upload"
      },
    example = entry$example %||% NULL
  )
}

builder_project_prepare_source <- function(entry, root, prior = NULL) {
  job <- builder_project_source_job(entry, root)
  relative <- paste("sources", entry$id, job$filename, sep = "/")
  prior_source <- if (is.list(prior)) prior$source %||% list() else list()
  target_ready <- file.exists(job$target) && !dir.exists(job$target)
  if (target_ready) {
    return(list(
      entry = entry,
      source = list(
        kind = "managed",
        origin = job$origin,
        example = job$example,
        filename = job$filename,
        path = relative,
        status = "ready",
        fingerprint = builder_project_file_fingerprint(
          job$target,
          content = TRUE
        )
      ),
      job = NULL
    ))
  }
  if (!.builder_project_text(job$source) || !file.exists(job$source)) {
    source <- prior_source
    source$status <- "failed"
    source$origin <- source$origin %||% job$origin
    source$example <- source$example %||% job$example
    source$filename <- source$filename %||% job$filename
    source$path <- source$path %||% relative
    source$error <- "The dataset source is no longer available."
    return(list(entry = entry, source = source, job = NULL))
  }
  list(
    entry = entry,
    source = list(
      kind = "managed",
      origin = job$origin,
      example = job$example,
      filename = job$filename,
      path = relative,
      status = "pending",
      fingerprint = NULL
    ),
    job = job
  )
}

builder_project_copy_source_job <- function(job) {
  fail <- function(message) {
    if (is.character(job$part) && length(job$part) == 1L) {
      unlink(job$part, force = TRUE)
    }
    list(id = job$id, status = "failed", error = message)
  }
  if (
    !is.list(job) ||
      !.builder_project_identifier(job$id) ||
      !.builder_project_text(job$source) ||
      !file.exists(job$source)
  ) {
    return(fail("The dataset source is no longer available."))
  }
  target_dir <- dirname(job$target)
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
    return(fail("The Project source directory could not be created."))
  }
  source_md5 <- unname(as.character(tools::md5sum(job$source)))
  if (
    file.exists(job$target) &&
      identical(
        unname(as.character(tools::md5sum(job$target))),
        source_md5
      )
  ) {
    return(list(
      id = job$id,
      status = "ready",
      path = job$target,
      fingerprint = builder_project_file_fingerprint(job$target, content = TRUE)
    ))
  }
  unlink(job$part, force = TRUE)
  if (!file.copy(job$source, job$part, overwrite = TRUE, copy.mode = TRUE)) {
    return(fail("The dataset source could not be copied."))
  }
  source_size <- suppressWarnings(as.numeric(file.info(job$source)$size[[1L]]))
  part_size <- suppressWarnings(as.numeric(file.info(job$part)$size[[1L]]))
  part_md5 <- unname(as.character(tools::md5sum(job$part)))
  if (!identical(source_size, part_size) || !identical(source_md5, part_md5)) {
    return(fail("The copied dataset source did not pass verification."))
  }
  backup <- paste0(job$target, ".previous")
  unlink(backup, force = TRUE)
  had_target <- file.exists(job$target)
  if (had_target && !file.rename(job$target, backup)) {
    return(fail("The existing Project source could not be replaced safely."))
  }
  if (!file.rename(job$part, job$target)) {
    if (had_target && file.exists(backup)) {
      file.rename(backup, job$target)
    }
    return(fail("The verified Project source could not be committed."))
  }
  unlink(backup, force = TRUE)
  list(
    id = job$id,
    status = "ready",
    path = job$target,
    fingerprint = builder_project_file_fingerprint(job$target, content = TRUE)
  )
}

builder_project_copy_source_jobs <- function(jobs, progress_path = NULL) {
  results <- vector("list", length(jobs))
  for (index in seq_along(jobs)) {
    results[[index]] <- builder_project_copy_source_job(jobs[[index]])
    if (.builder_project_text(progress_path)) {
      temporary <- paste0(progress_path, ".tmp")
      saveRDS(
        list(completed = index, total = length(jobs), results = results),
        temporary,
        version = 3L
      )
      file.rename(temporary, progress_path)
    }
  }
  results
}

builder_project_apply_source_results <- function(manifest, results, root) {
  if (!is.list(manifest) || !is.list(manifest$datasets)) {
    stop("A Builder project manifest is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  result_map <- stats::setNames(
    results,
    vapply(results, function(result) as.character(result$id), character(1))
  )
  manifest$datasets <- lapply(manifest$datasets, function(record) {
    result <- result_map[[record$id]] %||% NULL
    if (is.null(result)) {
      return(record)
    }
    source <- record$source %||% list(kind = "managed")
    source$status <- result$status %||% "failed"
    source$error <- result$error %||% NULL
    if (identical(source$status, "ready")) {
      path <- normalizePath(result$path, winslash = "/", mustWork = TRUE)
      prefix <- paste0(root, "/")
      if (!startsWith(path, prefix)) {
        stop("A synchronized source escaped the Project folder.", call. = FALSE)
      }
      source$path <- substring(path, nchar(prefix) + 1L)
      source$fingerprint <- result$fingerprint
    }
    record$source <- source
    record
  })
  manifest
}

builder_project_retain_session_source <- function(
  source,
  filename,
  root,
  dataset_id
) {
  if (
    !.builder_project_text(source) ||
      !file.exists(source) ||
      !.builder_project_text(root) ||
      !dir.exists(root) ||
      !.builder_project_identifier(dataset_id)
  ) {
    stop("A valid uploaded dataset source is required.", call. = FALSE)
  }
  retained_name <- if (.builder_project_text(filename)) {
    basename(filename)
  } else {
    basename(source)
  }
  if (!nzchar(retained_name) || retained_name %in% c(".", "..")) {
    retained_name <- paste0(dataset_id, ".rds")
  }
  target_dir <- file.path(root, "session-sources", dataset_id)
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
    stop("The session source folder could not be created.", call. = FALSE)
  }
  target <- file.path(target_dir, retained_name)
  same <- identical(
    normalizePath(source, winslash = "/", mustWork = TRUE),
    normalizePath(target, winslash = "/", mustWork = FALSE)
  )
  if (!same && !file.copy(source, target, overwrite = TRUE, copy.mode = TRUE)) {
    stop("The uploaded dataset source could not be retained.", call. = FALSE)
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

builder_project_safe_entry <- function(entry) {
  safe <- unserialize(serialize(entry, NULL, version = 3L))
  safe$snapshot <- NULL
  safe$project_artifact <- NULL
  safe$project_hydration <- NULL
  safe$load_state <- "reload_required"
  safe
}

builder_project_configuration_entry <- function(entry) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id %||% NULL)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  list(
    id = as.character(entry$id),
    revision = as.integer(entry$revision %||% 0L),
    settings = entry$settings %||% list(),
    acknowledgements = entry$acknowledgements %||% character(),
    spatial_drafts = entry$spatial_drafts %||% list()
  )
}

builder_project_dataset_config_path <- function(dataset_id, root) {
  if (!.builder_project_identifier(dataset_id)) {
    stop("A safe dataset id is required.", call. = FALSE)
  }
  builder_project_resolve_path(
    paste("datasets", dataset_id, "config.json", sep = "/"),
    root,
    "managed"
  )
}

builder_project_profile_cache_path <- function(dataset_id, root) {
  if (!.builder_project_identifier(dataset_id)) {
    stop("A safe dataset id is required.", call. = FALSE)
  }
  builder_project_resolve_path(
    paste("cache", dataset_id, "profile.rds", sep = "/"),
    root,
    "managed"
  )
}

.builder_project_commit_sidecar <- function(temporary, target) {
  target_dir <- dirname(target)
  if (
    !dir.exists(target_dir) &&
      !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop("A Project sidecar directory could not be created.", call. = FALSE)
  }
  backup <- paste0(target, ".previous")
  unlink(backup, force = TRUE)
  had_target <- file.exists(target)
  if (had_target && !file.rename(target, backup)) {
    stop("An existing Project sidecar could not be replaced.", call. = FALSE)
  }
  if (!file.rename(temporary, target)) {
    if (had_target && file.exists(backup)) {
      file.rename(backup, target)
    }
    stop("A Project sidecar could not be committed.", call. = FALSE)
  }
  unlink(backup, force = TRUE)
  invisible(target)
}

builder_project_write_dataset_config <- function(entry, root) {
  root <- builder_project_normalize_root(root)
  config <- builder_project_configuration_entry(entry)
  target <- builder_project_dataset_config_path(config$id, root)
  target_dir <- dirname(target)
  if (
    !dir.exists(target_dir) &&
      !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop(
      "The dataset configuration directory could not be created.",
      call. = FALSE
    )
  }
  temporary <- tempfile(
    paste0(config$id, "-config-"),
    tmpdir = target_dir,
    fileext = ".part"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeLines(
    jsonlite::serializeJSON(config, digits = NA, pretty = TRUE),
    temporary,
    useBytes = TRUE
  )
  .builder_project_commit_sidecar(temporary, target)
  list(
    schema_version = .builder_project_config_schema_version,
    path = builder_project_relative_path(target, root),
    revision = config$revision,
    fingerprint = builder_project_file_fingerprint(target, content = TRUE)
  )
}

builder_project_read_dataset_config <- function(record, root) {
  configuration <- record$configuration %||% list()
  path <- configuration$path %||% NULL
  if (.builder_project_text(path)) {
    resolved <- builder_project_resolve_path(path, root, "managed")
    if (!file.exists(resolved) || dir.exists(resolved)) {
      stop("A saved dataset configuration is missing.", call. = FALSE)
    }
    current <- builder_project_file_fingerprint(resolved, content = TRUE)
    if (
      is.list(configuration$fingerprint) &&
        !builder_project_content_fingerprint_matches(
          configuration$fingerprint,
          current
        )
    ) {
      stop(
        "A saved dataset configuration failed its integrity check.",
        call. = FALSE
      )
    }
    payload <- paste(readLines(resolved, warn = FALSE), collapse = "\n")
    config <- jsonlite::unserializeJSON(payload)
    if (
      !is.list(config) ||
        !identical(as.character(config$id %||% ""), as.character(record$id))
    ) {
      stop("A saved dataset configuration is invalid.", call. = FALSE)
    }
    return(config)
  }
  payload <- configuration$legacy_payload %||% configuration$payload %||% NULL
  if (!.builder_project_text(payload)) {
    stop("A saved dataset configuration is missing.", call. = FALSE)
  }
  jsonlite::unserializeJSON(payload)
}

builder_project_profile_cache_value <- function(entry) {
  list(
    schema_version = .builder_project_profile_cache_schema_version,
    id = as.character(entry$id),
    profile = entry$profile %||% list(),
    dataset_profile = entry$dataset_profile %||% list(),
    levels = entry$levels %||% list()
  )
}

builder_project_write_profile_cache <- function(
  entry,
  root,
  source_fingerprint = NULL,
  prior = NULL
) {
  root <- builder_project_normalize_root(root)
  target <- builder_project_profile_cache_path(entry$id, root)
  prior_cache <- if (is.list(prior)) prior$cache %||% list() else list()
  current <- builder_project_file_fingerprint(target, content = TRUE)
  if (
    file.exists(target) &&
      is.list(prior_cache$fingerprint) &&
      builder_project_content_fingerprint_matches(
        prior_cache$fingerprint,
        current
      ) &&
      identical(prior_cache$source_fingerprint %||% NULL, source_fingerprint)
  ) {
    return(prior_cache)
  }
  target_dir <- dirname(target)
  if (
    !dir.exists(target_dir) &&
      !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop(
      "The dataset profile cache directory could not be created.",
      call. = FALSE
    )
  }
  temporary <- tempfile(
    paste0(entry$id, "-profile-"),
    tmpdir = target_dir,
    fileext = ".part"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(
    builder_project_profile_cache_value(entry),
    temporary,
    version = 3L,
    compress = FALSE
  )
  .builder_project_commit_sidecar(temporary, target)
  list(
    schema_version = .builder_project_profile_cache_schema_version,
    path = builder_project_relative_path(target, root),
    source_fingerprint = source_fingerprint,
    fingerprint = builder_project_file_fingerprint(target, content = TRUE)
  )
}

builder_project_read_profile_cache <- function(record, root) {
  cache <- record$cache %||% list()
  if (!.builder_project_text(cache$path %||% NULL)) {
    return(NULL)
  }
  path <- builder_project_resolve_path(cache$path, root, "managed")
  current <- builder_project_file_fingerprint(path, content = TRUE)
  if (
    !file.exists(path) ||
      !is.list(cache$fingerprint) ||
      !builder_project_content_fingerprint_matches(cache$fingerprint, current)
  ) {
    return(NULL)
  }
  value <- tryCatch(readRDS(path), error = function(error) NULL)
  if (
    !is.list(value) ||
      !identical(
        .builder_project_integer(value$schema_version),
        .builder_project_profile_cache_schema_version
      ) ||
      !identical(as.character(value$id %||% ""), as.character(record$id))
  ) {
    return(NULL)
  }
  value
}

builder_project_profile_cache_available <- function(record, root) {
  cache <- record$cache %||% list()
  if (!.builder_project_text(cache$path %||% NULL)) {
    return(FALSE)
  }
  path <- tryCatch(
    builder_project_resolve_path(cache$path, root, "managed"),
    error = function(error) NULL
  )
  if (!.builder_project_text(path) || !file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  builder_project_content_fingerprint_matches(
    cache$fingerprint %||% NULL,
    builder_project_file_fingerprint(path, content = TRUE)
  )
}

builder_project_hydrate_profile_cache <- function(entry, record, root) {
  cached <- builder_project_read_profile_cache(record, root)
  if (is.null(cached)) {
    return(entry)
  }
  entry$profile <- cached$profile %||% entry$profile %||% list()
  entry$dataset_profile <- cached$dataset_profile %||%
    entry$dataset_profile %||%
    list()
  entry$levels <- cached$levels %||% entry$levels %||% list()
  entry
}

.builder_project_asset_segment <- function(value) {
  value <- as.character(value %||% "")[[1L]]
  readable <- gsub("[^A-Za-z0-9._-]+", "-", value)
  readable <- gsub("(^-+|-+$)", "", readable)
  if (!nzchar(readable)) {
    readable <- "item"
  }
  readable <- substr(readable, 1L, 60L)
  digest <- unclass(as.character(openssl::md5(charToRaw(enc2utf8(value)))))
  paste0(readable, "-", substr(digest, 1L, 10L))
}

.builder_project_decode_image_uri <- function(uri) {
  if (!.builder_project_text(uri)) {
    stop("A Spatial image payload is missing.", call. = FALSE)
  }
  matched <- regexec(
    "^data:([^;,]+);base64,(.*)$",
    uri,
    perl = TRUE
  )
  parts <- regmatches(uri, matched)[[1L]]
  if (length(parts) != 3L) {
    stop("A Spatial image payload is not a supported data URI.", call. = FALSE)
  }
  bytes <- tryCatch(
    base64enc::base64decode(parts[[3L]]),
    error = function(error) NULL
  )
  if (is.null(bytes) || !length(bytes)) {
    stop("A Spatial image payload could not be decoded.", call. = FALSE)
  }
  list(mime = parts[[2L]], bytes = bytes)
}

.builder_project_map_spatial_images <- function(entry, transform) {
  images <- entry$settings$images %||% list()
  if (!is.list(images) || !length(images)) {
    return(entry)
  }
  for (section in names(images)) {
    collection <- images[[section]]
    legacy_record <- is.list(collection) &&
      any(
        c("source_uri", "uri", "project_asset") %in% names(collection)
      )
    if (legacy_record) {
      label <- collection$source$name %||% "image"
      images[[section]] <- transform(collection, section, as.character(label))
      next
    }
    if (!is.list(collection) || !length(collection)) {
      next
    }
    for (label in names(collection)) {
      images[[section]][[label]] <- transform(
        collection[[label]],
        section,
        label
      )
    }
  }
  entry$settings$images <- images
  entry
}

builder_project_stage_spatial_assets <- function(entry, root) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  staged <- unserialize(serialize(entry, NULL, version = 3L))
  .builder_project_map_spatial_images(staged, function(record, section, label) {
    if (!is.list(record)) {
      return(record)
    }
    field_order <- names(record)
    payload <- record$source_uri %||% record$uri %||% NULL
    asset <- record$project_asset %||% NULL
    if (is.list(asset) && !.builder_project_text(payload)) {
      fail <- function(reason) {
        stop(
          paste0(
            "Spatial image asset for dataset “",
            entry$id,
            "”, FOV “",
            section,
            "”, image “",
            label,
            "” ",
            reason,
            "."
          ),
          call. = FALSE
        )
      }
      path <- tryCatch(
        builder_project_resolve_path(asset$path %||% "", root, "managed"),
        error = function(error) NULL
      )
      if (
        !.builder_project_text(path) || !file.exists(path) || dir.exists(path)
      ) {
        fail("is missing")
      }
      current <- builder_project_file_fingerprint(path, content = TRUE)
      if (!builder_project_fingerprint_matches(asset$fingerprint, current)) {
        fail("failed its integrity check")
      }
      return(record)
    }
    parsed <- .builder_project_decode_image_uri(
      payload
    )
    extension <- switch(
      tolower(parsed$mime),
      "image/jpeg" = "jpg",
      "image/webp" = "webp",
      "png"
    )
    asset_id <- unclass(as.character(openssl::md5(parsed$bytes)))
    target_dir <- file.path(
      root,
      "spatial-assets",
      entry$id,
      .builder_project_asset_segment(section)
    )
    if (
      !dir.exists(target_dir) &&
        !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    ) {
      stop("The Spatial asset directory could not be created.", call. = FALSE)
    }
    target <- file.path(target_dir, paste0(asset_id, ".", extension))
    if (
      !file.exists(target) ||
        !identical(
          unname(as.character(tools::md5sum(target))),
          asset_id
        )
    ) {
      part <- tempfile(
        pattern = paste0(asset_id, "-"),
        tmpdir = target_dir,
        fileext = ".part"
      )
      on.exit(unlink(part, force = TRUE), add = TRUE)
      writeBin(parsed$bytes, part)
      if (
        !identical(unname(as.character(tools::md5sum(part))), asset_id) ||
          !file.rename(part, target)
      ) {
        stop(
          "A Spatial image asset could not be committed safely.",
          call. = FALSE
        )
      }
    }
    record$project_asset <- list(
      schema_version = 1L,
      path = builder_project_relative_path(target, root),
      mime = parsed$mime,
      fingerprint = builder_project_file_fingerprint(target, content = TRUE),
      field_order = field_order
    )
    record$source_uri <- NULL
    record$uri <- NULL
    record
  })
}

builder_project_restore_spatial_assets <- function(entry, root) {
  dataset_id <- as.character(entry$id %||% "dataset")
  .builder_project_map_spatial_images(entry, function(record, section, label) {
    asset <- if (is.list(record)) record$project_asset %||% NULL else NULL
    if (is.null(asset)) {
      return(record)
    }
    fail <- function(reason) {
      stop(
        paste0(
          "Spatial image asset for dataset “",
          dataset_id,
          "”, FOV “",
          section,
          "”, image “",
          label,
          "” ",
          reason,
          "."
        ),
        call. = FALSE
      )
    }
    path <- tryCatch(
      builder_project_resolve_path(asset$path %||% "", root, "managed"),
      error = function(error) NULL
    )
    if (
      !.builder_project_text(path) || !file.exists(path) || dir.exists(path)
    ) {
      fail("is missing")
    }
    current <- builder_project_file_fingerprint(path, content = TRUE)
    if (!builder_project_fingerprint_matches(asset$fingerprint, current)) {
      fail("failed its integrity check")
    }
    mime <- as.character(asset$mime %||% "image/png")[[1L]]
    uri <- paste0(
      "data:",
      mime,
      ";base64,",
      base64enc::base64encode(path)
    )
    record$source_uri <- uri
    record$uri <- uri
    field_order <- as.character(asset$field_order %||% character())
    if (length(field_order)) {
      record <- record[c(
        intersect(field_order, names(record)),
        setdiff(names(record), field_order)
      )]
    }
    record
  })
}

builder_project_checkpoint_entries <- function(entries) {
  checkpoint <- unserialize(serialize(entries, NULL, version = 3L))
  lapply(checkpoint, function(entry) {
    if (length(entry$settings$images %||% list())) {
      entry$settings$spatial_image_storage <- "embedded"
    }
    entry
  })
}

builder_project_configuration_digest <- function(entry) {
  digest_entry <- list(
    settings = entry$settings %||% list(),
    acknowledgements = entry$acknowledgements %||% character(),
    spatial_drafts = entry$spatial_drafts %||% list()
  )
  digest_entry <- .builder_project_map_spatial_images(
    digest_entry,
    function(record, section, label) {
      if (is.list(record)) {
        source_md5 <- record$project_asset$fingerprint$md5 %||% NULL
        if (!.builder_project_text(source_md5)) {
          source_md5 <- tryCatch(
            {
              parsed <- .builder_project_decode_image_uri(
                record$source_uri %||% record$uri %||% NULL
              )
              unclass(as.character(openssl::md5(parsed$bytes)))
            },
            error = function(error) NULL
          )
        }
        # Digest-only content identity; never serialized as project UI state.
        record$source_content_md5 <- source_md5
        record$source_uri <- NULL
        record$uri <- NULL
        record$project_asset <- NULL
      }
      record
    }
  )
  # `levels` is source-derived and immutable after import. Source fingerprinting,
  # rather than the editable-configuration digest, invalidates it on reload.
  value <- list(
    contract_version = .builder_project_configuration_contract_version,
    settings = digest_entry$settings %||% list(),
    acknowledgements = digest_entry$acknowledgements %||% character(),
    spatial_drafts = digest_entry$spatial_drafts %||% list()
  )
  unclass(as.character(openssl::md5(
    serialize(value, NULL, version = 3L)
  )))
}

builder_project_dataset_record <- function(
  entry,
  source,
  checked = FALSE,
  artifact = NULL,
  order = 1L,
  payload_entry = entry,
  root = NULL,
  prior = NULL
) {
  profile <- entry$profile %||% list()
  spatial <- entry$dataset_profile$content$spatial %||%
    entry$profile$viewer_content$spatial %||%
    list()
  sections <- spatial$sections %||% spatial$fovs %||% character()
  digest <- builder_project_configuration_digest(entry)
  sidecar <- if (.builder_project_text(root %||% NULL)) {
    builder_project_write_dataset_config(payload_entry, root)
  } else {
    NULL
  }
  configuration <- if (is.null(sidecar)) {
    list(
      revision = as.integer(entry$revision %||% 0L),
      digest = digest,
      checked = isTRUE(checked),
      payload = jsonlite::serializeJSON(
        builder_project_safe_entry(payload_entry),
        digits = NA,
        pretty = FALSE
      )
    )
  } else {
    utils::modifyList(
      sidecar,
      list(
        digest = digest,
        checked = isTRUE(checked),
        checked_digest = if (isTRUE(checked)) digest else NULL,
        contract_version = .builder_project_configuration_contract_version
      )
    )
  }
  cache <- if (is.null(sidecar)) {
    NULL
  } else {
    builder_project_write_profile_cache(
      entry,
      root,
      source_fingerprint = source$fingerprint %||% NULL,
      prior = prior
    )
  }
  list(
    id = entry$id,
    name = entry$settings$name %||% entry$id,
    order = as.integer(order),
    format = entry$format %||% NULL,
    source = source,
    inspection = list(
      cells = as.integer(profile$n_cells %||% 0L),
      genes = as.integer(profile$n_genes %||% 0L),
      assays = as.character(names(profile$assay_profiles %||% list())),
      projections = as.character(entry$settings$reductions %||% character()),
      fovs = as.character(names(sections) %||% sections %||% character())
    ),
    configuration = configuration,
    cache = cache,
    artifact = artifact,
    release = list(included = TRUE)
  )
}

builder_project_restore_entry <- function(
  record,
  root,
  hydrate_spatial_assets = TRUE,
  hydrate_profile_cache = FALSE
) {
  entry <- builder_project_read_dataset_config(record, root)
  entry$id <- as.character(record$id)
  entry$source_id <- entry$id
  entry$output_id <- entry$id
  entry$selector_value <- entry$id
  entry$revision <- as.integer(
    entry$revision %||% record$configuration$revision %||% 0L
  )
  entry$settings <- entry$settings %||% list(name = record$name %||% entry$id)
  inspection <- record$inspection %||% list()
  entry$profile <- entry$profile %||%
    list(
      n_cells = as.integer(inspection$cells %||% 0L),
      n_genes = as.integer(inspection$genes %||% 0L),
      assays = as.character(inspection$assays %||% character()),
      reductions = as.character(inspection$projections %||% character())
    )
  entry$dataset_profile <- entry$dataset_profile %||% list()
  entry$levels <- entry$levels %||% list()
  entry$format <- entry$format %||% record$format %||% NULL
  if (isTRUE(hydrate_profile_cache)) {
    entry <- builder_project_hydrate_profile_cache(entry, record, root)
  }
  entry$load_state <- "reload_required"
  if (isTRUE(hydrate_spatial_assets)) {
    entry <- builder_project_restore_spatial_assets(entry, root)
  }
  source <- record$source %||% list()
  entry$source_origin <- source$origin %||%
    if (identical(source$kind, "example")) "example" else "upload"
  if (identical(source$kind, "example")) {
    entry$example <- source$example
    entry$path <- NULL
  } else {
    entry$example <- if (identical(entry$source_origin, "example")) {
      source$example %||% NULL
    } else {
      NULL
    }
    entry$path <- builder_project_resolve_path(
      source$path %||% "",
      root,
      source$kind %||% "managed"
    )
  }
  entry
}

builder_project_hydrate_loaded_entry <- function(loaded, record, root) {
  if (!is.list(loaded) || !.builder_project_identifier(loaded$id)) {
    stop("A loaded dataset entry is required.", call. = FALSE)
  }
  if (!is.list(record) || !identical(as.character(record$id), loaded$id)) {
    stop(
      "The saved dataset configuration does not match the loaded entry.",
      call. = FALSE
    )
  }
  saved <- builder_project_restore_entry(record, root)
  hydrated <- loaded
  hydrated$settings <- saved$settings
  hydrated$acknowledgements <- saved$acknowledgements %||% NULL
  hydrated$spatial_drafts <- saved$spatial_drafts %||% NULL
  hydrated$revision <- max(
    as.integer(loaded$revision %||% 0L),
    as.integer(saved$revision %||% 0L)
  ) +
    1L
  hydrated$project_hydration <- list(
    schema_version = 1L,
    configuration_digest = as.character(
      record$configuration$digest %||% ""
    )
  )
  hydrated
}

builder_project_entry_hydrated_from <- function(entry, record) {
  is.list(entry) &&
    is.list(record) &&
    is.list(entry$project_hydration) &&
    .builder_project_text(record$configuration$digest %||% NULL) &&
    identical(
      as.character(entry$project_hydration$configuration_digest %||% ""),
      as.character(record$configuration$digest)
    )
}

builder_project_invalidate_entry_hydration <- function(entry) {
  if (is.list(entry)) {
    entry$project_hydration <- NULL
  }
  entry
}

builder_project_hydrate_pending_entries <- function(entries, pending, root) {
  if (!is.list(entries) || !is.list(pending)) {
    stop("Project restore state is invalid.", call. = FALSE)
  }
  keep <- rep(TRUE, length(entries))
  restored <- list()
  failures <- list()
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    id <- as.character(entry$id %||% "")
    record <- pending[[id]] %||% NULL
    if (
      is.null(record) ||
        !identical(entry$load_state %||% "loaded", "loaded")
    ) {
      next
    }
    hydrated <- tryCatch(
      if (builder_project_entry_hydrated_from(entry, record)) {
        entry
      } else {
        builder_project_hydrate_loaded_entry(entry, record, root)
      },
      error = identity
    )
    pending[[id]] <- NULL
    if (inherits(hydrated, "condition")) {
      keep[[index]] <- FALSE
      failures[[id]] <- list(
        entry = entry,
        record = record,
        message = paste0(
          "The saved project configuration for ",
          id,
          " could not be restored: ",
          conditionMessage(hydrated)
        )
      )
      next
    }
    entries[[index]] <- hydrated
    restored[[id]] <- list(entry = hydrated, record = record)
  }
  list(
    entries = entries[keep],
    pending = pending,
    restored = restored,
    failures = failures
  )
}

builder_project_artifact_available <- function(artifact, root) {
  if (
    !is.list(artifact) ||
      !identical(artifact$status, "ready") ||
      isFALSE(artifact$reusable %||% TRUE)
  ) {
    return(FALSE)
  }
  path <- tryCatch(
    builder_project_resolve_path(artifact$path %||% "", root, "managed"),
    error = function(error) NULL
  )
  .builder_project_text(path) && file.exists(path) && !dir.exists(path)
}

builder_project_entries_requiring_crb <- function(entries, artifacts, root) {
  Filter(
    function(entry) {
      artifact <- artifacts[[entry$id]] %||% NULL
      !is.list(artifact) ||
        !builder_project_artifact_available(artifact, root) ||
        !identical(
          as.character(artifact$built_from_configuration %||% ""),
          builder_project_configuration_digest(entry)
        )
    },
    entries
  )
}

builder_project_spatial_assets_status <- function(record, root) {
  entry <- tryCatch(
    builder_project_read_dataset_config(record, root),
    error = identity
  )
  if (inherits(entry, "condition")) {
    return(list(ready = FALSE, error = conditionMessage(entry)))
  }
  entry$id <- as.character(record$id %||% entry$id %||% "dataset")
  restored <- tryCatch(
    builder_project_restore_spatial_assets(entry, root),
    error = identity
  )
  if (inherits(restored, "condition")) {
    return(list(ready = FALSE, error = conditionMessage(restored)))
  }
  list(ready = TRUE, error = NULL)
}

builder_project_record_configuration_confirmed <- function(record) {
  is.list(record) &&
    is.list(record$configuration) &&
    isTRUE(record$configuration$checked) &&
    identical(
      as.character(
        record$configuration$checked_digest %||%
          record$configuration$digest %||%
          ""
      ),
      as.character(record$configuration$digest %||% "")
    )
}

builder_project_dataset_status <- function(record, root) {
  artifact_ready <- builder_project_artifact_available(record$artifact, root)
  profile_cache_ready <- builder_project_profile_cache_available(record, root)
  spatial_assets <- builder_project_spatial_assets_status(record, root)
  source <- record$source %||% list()
  source_path <- if (identical(source$kind, "example")) {
    source$example %||% NULL
  } else {
    tryCatch(
      builder_project_resolve_path(
        source$path %||% "",
        root,
        source$kind %||% "managed"
      ),
      error = function(error) NULL
    )
  }
  source_ready <- identical(source$kind, "example") ||
    (.builder_project_text(source_path) && file.exists(source_path))
  current_fingerprint <- if (
    source_ready && !identical(source$kind, "example")
  ) {
    builder_project_file_fingerprint(source_path)
  } else {
    NULL
  }
  recorded_fingerprint <- source$fingerprint %||% NULL
  source_matches <- identical(source$kind, "example") ||
    is.null(recorded_fingerprint) ||
    builder_project_fingerprint_matches(
      recorded_fingerprint,
      current_fingerprint
    )
  checked <- isTRUE(spatial_assets$ready) &&
    builder_project_record_configuration_confirmed(record) &&
    source_matches
  list(
    source_ready = source_ready,
    source_matches = source_matches,
    spatial_assets_ready = isTRUE(spatial_assets$ready),
    spatial_assets_error = spatial_assets$error,
    restorable = source_ready && isTRUE(spatial_assets$ready),
    artifact_ready = artifact_ready,
    profile_cache_ready = profile_cache_ready,
    checked = checked,
    label = if (!isTRUE(spatial_assets$ready)) {
      "Needs check · spatial image missing"
    } else if (checked && artifact_ready) {
      "Checked · CRB ready"
    } else if (checked) {
      "Checked · source reload required"
    } else if (source_ready && !source_matches) {
      "Needs check · source changed"
    } else if (source_ready) {
      "Needs check · load source"
    } else {
      "Needs check · source missing"
    }
  )
}

builder_project_new_manifest <- function(root, name = basename(root)) {
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  list(
    schema_version = .builder_project_schema_version,
    project = list(
      id = builder_project_id(),
      name = as.character(name),
      revision = 0L,
      created_at = now,
      updated_at = now,
      builder_version = as.character(
        tryCatch(
          utils::packageVersion("CerebroNexus"),
          error = function(error) "development"
        )
      )
    ),
    datasets = list(),
    configuration = builder_project_configuration(),
    pending_build = NULL,
    releases = list(),
    last_ui = list(stage = "upload", selected_dataset = NULL)
  )
}

builder_project_configuration <- function(
  review_options = NULL,
  build_mode = FALSE,
  auth_enabled = FALSE,
  initial_dataset = NULL
) {
  scalar_flag <- function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }
  if (!scalar_flag(build_mode) || !scalar_flag(auth_enabled)) {
    stop("Builder project preferences are invalid.", call. = FALSE)
  }
  if (!is.null(initial_dataset)) {
    initial_dataset <- as.character(initial_dataset)
    if (
      length(initial_dataset) != 1L ||
        is.na(initial_dataset) ||
        !.builder_project_identifier(initial_dataset)
    ) {
      stop("The Builder starting dataset preference is invalid.", call. = FALSE)
    }
  }
  allowed <- c(
    "welcome_message",
    "initial_page",
    "point_size",
    "variable_to_compare",
    "host",
    "port",
    "max_request_size",
    "display_mode",
    "launch_browser",
    "show_upload_ui"
  )
  saved_options <- if (is.null(review_options)) {
    NULL
  } else {
    if (!is.list(review_options)) {
      stop("Builder Review preferences are invalid.", call. = FALSE)
    }
    candidate <- unclass(review_options)[
      intersect(allowed, names(review_options))
    ]
    required <- setdiff(allowed, names(candidate))
    if (length(required)) {
      stop("Builder Review preferences are incomplete.", call. = FALSE)
    }
    valid_text <- function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    }
    valid_number <- function(value, lower, upper = Inf, whole = FALSE) {
      is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value) &&
        value >= lower &&
        value <= upper &&
        (!whole || value == floor(value))
    }
    valid_flag <- function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value)
    }
    if (
      !valid_text(candidate$welcome_message) ||
        !valid_text(candidate$initial_page) ||
        !valid_number(candidate$point_size, 0, 20) ||
        !valid_flag(candidate$variable_to_compare) ||
        !valid_text(candidate$host) ||
        !valid_number(candidate$port, 1, 65535, whole = TRUE) ||
        !valid_number(candidate$max_request_size, .Machine$double.eps) ||
        !valid_text(candidate$display_mode) ||
        !candidate$display_mode %in% c("auto", "normal", "showcase") ||
        !valid_flag(candidate$launch_browser) ||
        !valid_flag(candidate$show_upload_ui)
    ) {
      stop("Builder Review preferences are invalid.", call. = FALSE)
    }
    candidate$point_size <- as.double(candidate$point_size)
    candidate$port <- as.integer(candidate$port)
    candidate$max_request_size <- as.double(candidate$max_request_size)
    candidate
  }
  list(
    review_options = saved_options,
    build_mode = isTRUE(build_mode),
    auth_enabled = isTRUE(auth_enabled),
    initial_dataset = initial_dataset
  )
}

builder_project_migrate_manifest <- function(manifest, root = NULL) {
  version <- .builder_project_integer(manifest$schema_version)
  if (!version %in% .builder_project_supported_schema_versions) {
    stop("This is not a supported Builder project.", call. = FALSE)
  }
  if (version < .builder_project_schema_version) {
    manifest$migrated_from_schema <- version
    if (identical(version, 1L)) {
      manifest$configuration <- builder_project_configuration()
    }
    can_write_sidecars <- .builder_project_text(root %||% NULL) &&
      dir.exists(root)
    manifest$datasets <- lapply(manifest$datasets, function(record) {
      payload <- record$configuration$payload %||% NULL
      if (!.builder_project_text(payload)) {
        return(record)
      }
      entry <- tryCatch(jsonlite::unserializeJSON(payload), error = identity)
      if (inherits(entry, "condition")) {
        return(record)
      }
      entry$id <- as.character(record$id %||% entry$id %||% "")
      old_digest <- as.character(record$configuration$digest %||% "")
      new_digest <- tryCatch(
        builder_project_configuration_digest(entry),
        error = function(error) NULL
      )
      if (!.builder_project_text(new_digest)) {
        return(record)
      }
      if (can_write_sidecars) {
        config_entry <- builder_project_configuration_entry(entry)
        config_entry <- builder_project_stage_spatial_assets(
          config_entry,
          root
        )
        descriptor <- builder_project_write_dataset_config(config_entry, root)
        record$configuration <- utils::modifyList(
          descriptor,
          list(
            digest = new_digest,
            checked = isTRUE(record$configuration$checked),
            checked_digest = if (isTRUE(record$configuration$checked)) {
              new_digest
            } else {
              NULL
            },
            contract_version = .builder_project_configuration_contract_version
          )
        )
        record$cache <- builder_project_write_profile_cache(
          entry,
          root,
          source_fingerprint = record$source$fingerprint %||% NULL,
          prior = record
        )
      } else {
        record$configuration$legacy_payload <- payload
        record$configuration$payload <- NULL
        record$configuration$digest <- new_digest
        record$configuration$checked_digest <- if (
          isTRUE(record$configuration$checked)
        ) {
          new_digest
        } else {
          NULL
        }
      }
      if (
        is.list(record$artifact) &&
          identical(
            as.character(
              record$artifact$built_from_configuration %||% ""
            ),
            old_digest
          )
      ) {
        record$artifact$built_from_configuration <- new_digest
      }
      record
    })
    manifest$schema_version <- .builder_project_schema_version
  }
  configuration <- manifest$configuration %||% list()
  manifest$configuration <- builder_project_configuration(
    review_options = configuration$review_options %||% NULL,
    build_mode = configuration$build_mode %||% FALSE,
    auth_enabled = configuration$auth_enabled %||% FALSE,
    initial_dataset = configuration$initial_dataset %||% NULL
  )
  last_ui <- manifest$last_ui %||% list()
  stage <- as.character(last_ui$stage %||% "configure")
  if (
    length(stage) != 1L ||
      is.na(stage) ||
      !stage %in% c("upload", "configure", "review", "build")
  ) {
    stage <- "configure"
  }
  selected <- last_ui$selected_dataset %||% NULL
  if (!is.null(selected)) {
    selected <- as.character(selected)
    if (length(selected) != 1L || is.na(selected) || !nzchar(selected)) {
      selected <- NULL
    }
  }
  manifest$last_ui <- list(
    stage = stage,
    selected_dataset = selected,
    spatial = builder_project_last_ui_spatial(
      last_ui$spatial %||% NULL,
      selected_dataset = selected
    )
  )
  manifest
}

builder_project_last_ui_spatial <- function(
  spatial,
  selected_dataset = NULL
) {
  if (is.null(spatial)) {
    return(NULL)
  }
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (
    !is.list(spatial) ||
      !scalar_text(spatial$dataset) ||
      !scalar_text(spatial$section) ||
      (!is.null(spatial$image) && !scalar_text(spatial$image)) ||
      (!is.null(selected_dataset) &&
        !identical(spatial$dataset, selected_dataset))
  ) {
    return(NULL)
  }
  list(
    dataset = spatial$dataset,
    section = spatial$section,
    image = spatial$image %||% NULL
  )
}

builder_project_last_ui_target <- function(
  last_ui,
  available_ids,
  checked_ids = character()
) {
  available_ids <- unique(as.character(available_ids %||% character()))
  checked_ids <- unique(as.character(checked_ids %||% character()))
  saved_dataset <- if (is.list(last_ui)) last_ui$selected_dataset else NULL
  selected_dataset <- if (
    length(available_ids) &&
      is.character(saved_dataset) &&
      length(saved_dataset) == 1L &&
      !is.na(saved_dataset) &&
      saved_dataset %in% available_ids
  ) {
    saved_dataset
  } else if (length(available_ids)) {
    available_ids[[1L]]
  } else {
    NULL
  }
  saved_stage <- if (is.list(last_ui)) {
    as.character(last_ui$stage %||% "configure")
  } else {
    "configure"
  }
  all_checked <- length(available_ids) > 0L &&
    all(available_ids %in% checked_ids)
  stage <- if (
    length(saved_stage) == 1L &&
      !is.na(saved_stage) &&
      saved_stage %in% c("review", "build") &&
      all_checked
  ) {
    ## A build-stage confirmation is intentionally session-only. Reopen its
    ## frozen plan in Review rather than pretending the old build is confirmed.
    "review"
  } else {
    "configure"
  }
  list(
    selected_dataset = selected_dataset,
    stage = stage,
    spatial = builder_project_last_ui_spatial(
      if (is.list(last_ui)) last_ui$spatial %||% NULL else NULL,
      selected_dataset = selected_dataset
    )
  )
}

builder_project_read <- function(path) {
  if (!.builder_project_text(path) || !file.exists(path) || dir.exists(path)) {
    stop("Choose a Builder project JSON file.", call. = FALSE)
  }
  manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (
    !is.list(manifest) ||
      !.builder_project_integer(manifest$schema_version) %in%
        .builder_project_supported_schema_versions ||
      !is.list(manifest$project) ||
      !.builder_project_identifier(manifest$project$id) ||
      !is.list(manifest$datasets)
  ) {
    stop("This is not a supported Builder project.", call. = FALSE)
  }
  dataset_ids <- vapply(
    manifest$datasets,
    function(record) {
      if (!is.list(record) || !.builder_project_identifier(record$id)) {
        return(NA_character_)
      }
      as.character(record$id)
    },
    character(1)
  )
  if (anyNA(dataset_ids) || anyDuplicated(dataset_ids)) {
    stop(
      "The Builder project contains unsafe or duplicate dataset ids.",
      call. = FALSE
    )
  }
  manifest$project$revision <- .builder_project_integer(
    manifest$project$revision
  )
  builder_project_migrate_manifest(manifest, root = dirname(path))
}

builder_project_write <- function(manifest, root, expected_revision = NULL) {
  root <- builder_project_normalize_root(root)
  target <- builder_project_manifest_path(root)
  disk <- if (file.exists(target)) builder_project_read(target) else NULL
  disk_revision <- if (is.null(disk)) 0L else disk$project$revision
  if (
    !is.null(expected_revision) &&
      !identical(as.integer(expected_revision), as.integer(disk_revision))
  ) {
    stop(
      "This project was updated by another Builder window. Reopen it before saving.",
      call. = FALSE
    )
  }
  if (!is.list(manifest$project)) {
    stop("The Builder project header is missing.", call. = FALSE)
  }
  inline_payloads <- vapply(
    manifest$datasets %||% list(),
    function(record) {
      configuration <- record$configuration %||% list()
      .builder_project_text(configuration$payload %||% NULL) ||
        .builder_project_text(configuration$legacy_payload %||% NULL)
    },
    logical(1)
  )
  if (any(inline_payloads)) {
    stop(
      "Schema v3 cannot write inline dataset payloads.",
      call. = FALSE
    )
  }
  manifest$schema_version <- .builder_project_schema_version
  manifest$migrated_from_schema <- NULL
  manifest$configuration <- builder_project_configuration(
    review_options = manifest$configuration$review_options %||% NULL,
    build_mode = manifest$configuration$build_mode %||% FALSE,
    auth_enabled = manifest$configuration$auth_enabled %||% FALSE,
    initial_dataset = manifest$configuration$initial_dataset %||% NULL
  )
  manifest$project$revision <- as.integer(disk_revision) + 1L
  manifest$project$updated_at <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%OS3Z",
    tz = "UTC"
  )
  temporary <- tempfile("builder-project-", tmpdir = root, fileext = ".json")
  backup <- paste0(target, ".bak")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  jsonlite::write_json(
    manifest,
    temporary,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = TRUE,
    digits = NA
  )
  manifest_bytes <- suppressWarnings(as.double(file.info(temporary)$size[[1L]]))
  if (!is.finite(manifest_bytes) || manifest_bytes > 10 * 1024^2) {
    stop(
      "The Project manifest exceeded the schema v3 size limit.",
      call. = FALSE
    )
  }
  if (file.exists(target)) {
    if (file.exists(backup)) {
      unlink(backup)
    }
    if (!file.rename(target, backup)) {
      stop(
        "The previous project manifest could not be backed up.",
        call. = FALSE
      )
    }
  }
  if (!file.rename(temporary, target)) {
    if (file.exists(backup)) {
      file.rename(backup, target)
    }
    stop("The project manifest could not be committed.", call. = FALSE)
  }
  list(manifest = manifest, path = target)
}

builder_project_artifact_entry <- function(entry, artifact, root) {
  if (!builder_project_artifact_available(artifact, root)) {
    stop("The saved CRB is no longer available.", call. = FALSE)
  }
  entry$load_state <- "artifact_ready"
  entry$snapshot <- NULL
  if (.builder_project_text(artifact$plan_payload %||% "")) {
    artifact$plan_item <- jsonlite::unserializeJSON(artifact$plan_payload)
  }
  artifact$resolved_path <- builder_project_resolve_path(
    artifact$path,
    root,
    "managed"
  )
  if (length(artifact$members %||% list())) {
    artifact$members <- lapply(artifact$members, function(member) {
      member$resolved_path <- builder_project_resolve_path(
        member$path,
        root,
        "managed"
      )
      member
    })
  }
  entry$project_artifact <- artifact
  entry
}

builder_project_check_identity <- function(entry) {
  if (
    is.list(entry) &&
      identical(entry$load_state %||% "loaded", "artifact_ready") &&
      is.list(entry$project_artifact)
  ) {
    value <- entry$project_artifact$fingerprint$md5 %||%
      entry$project_artifact$path %||%
      entry$id
    return(paste0("artifact:", value))
  }
  paste0(
    "configuration:",
    builder_project_configuration_digest(entry)
  )
}

builder_project_effective_check_identity <- function(
  entry,
  coordinate_drafts = list()
) {
  records <- if (
    is.list(entry) &&
      .builder_project_text(entry$id %||% "") &&
      is.list(coordinate_drafts)
  ) {
    coordinate_drafts[[entry$id]] %||% list()
  } else {
    list()
  }
  if (length(records)) {
    snapshot_identity <- tryCatch(
      .builder_worker_identity(entry$snapshot),
      error = function(error) NULL
    )
    applied <- if (is.null(snapshot_identity)) {
      NULL
    } else {
      tryCatch(
        builder_coordinate_drafts_apply_entry(
          entry,
          records,
          snapshot_identity = snapshot_identity
        ),
        error = function(error) NULL
      )
    }
    if (!is.list(applied) || !is.list(applied$entry)) {
      return(NA_character_)
    }
    entry <- applied$entry
  }
  builder_project_check_identity(entry)
}

builder_project_checked_ids <- function(
  entries,
  marks,
  coordinate_drafts = list()
) {
  ids <- vapply(entries, `[[`, character(1), "id")
  identities <- vapply(
    entries,
    builder_project_effective_check_identity,
    character(1),
    coordinate_drafts = coordinate_drafts
  )
  matched <- !is.na(identities) & ids %in% names(marks)
  matched[matched] <- unname(marks[ids[matched]]) == identities[matched]
  ids[matched]
}

builder_project_restored_check_identity <- function(record, entry, status) {
  if (
    !is.list(record) ||
      !is.list(entry) ||
      !is.list(status) ||
      !isTRUE(status$checked) ||
      !isTRUE(status$source_matches)
  ) {
    return(NULL)
  }
  builder_project_check_identity(entry)
}
