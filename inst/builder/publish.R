##----------------------------------------------------------------------------##
## Parent-owned, recoverable publication of one Builder release.
##
## A coordinator holds one owner-identified directory lock from stage
## registration through publication. Every destructive transition is preceded
## or followed by an atomic journal record so a later parent can recover after
## process death without guessing ownership from elapsed time.
##----------------------------------------------------------------------------##

.builder_release_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.builder_release_exists <- function(path) {
  file.exists(path) || dir.exists(path)
}

.builder_release_link <- function(path) {
  linked <- tryCatch(fs::is_link(path), error = function(error) NA)
  length(linked) == 1L && (is.na(linked) || isTRUE(unname(linked)))
}

.builder_release_token <- function(prefix = "owner") {
  paste(
    prefix,
    Sys.getpid(),
    format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"),
    sprintf("%08x", sample.int(.Machine$integer.max, 1L)),
    sep = "-"
  )
}

.builder_release_host <- function() {
  host <- unname(Sys.info()[["nodename"]])
  if (!.builder_release_text(host)) "unknown-host" else host
}

.builder_release_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

.builder_release_mode_owner_only <- function(path) {
  if (identical(.Platform$OS.type, "windows")) {
    return(TRUE)
  }
  mode <- file.info(path)$mode
  length(mode) == 1L && !is.na(mode) && bitwAnd(as.integer(mode), 63L) == 0L
}

.builder_release_path <- function(target) {
  if (!.builder_release_text(target)) {
    stop("A release target is required.", call. = FALSE)
  }
  if (.builder_release_exists(target) && .builder_release_link(target)) {
    stop("The release target cannot be a symbolic link.", call. = FALSE)
  }
  canonical <- .canonicalTargetPath(target)
  leaf <- basename(canonical)
  if (
    !nzchar(leaf) ||
      leaf %in% c(".", "..", "/") ||
      .windowsPathSegmentInvalid(leaf)
  ) {
    stop("The release target has an unsafe name.", call. = FALSE)
  }
  parent <- dirname(canonical)
  if (!dir.exists(parent) || .builder_release_link(parent)) {
    stop(
      "The release parent must be an existing real directory.",
      call. = FALSE
    )
  }
  canonical
}

builder_release_control_path <- function(target) {
  target <- .builder_release_path(target)
  .canonicalTargetPath(file.path(
    dirname(target),
    paste0(".", basename(target), ".cerebro-control")
  ))
}

.builder_release_relative <- function(path, root) {
  path <- .canonicalTargetPath(path)
  root <- .canonicalTargetPath(root)
  if (!.pathWithin(path, root)) {
    stop("A release entry escaped its owned root.", call. = FALSE)
  }
  if (identical(path, root)) "" else substring(path, nchar(root) + 2L)
}

builder_release_identity <- function(target) {
  target <- .builder_release_path(target)
  if (!.builder_release_exists(target)) {
    return(list(schema_version = 1L, exists = FALSE, entries = list()))
  }
  if (!dir.exists(target) || .builder_release_link(target)) {
    stop("A Builder release must be a real directory.", call. = FALSE)
  }
  entries <- list.files(
    target,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  if (!length(entries)) {
    return(list(schema_version = 1L, exists = TRUE, entries = list()))
  }
  linked <- vapply(entries, .builder_release_link, logical(1))
  if (any(linked)) {
    stop(
      "A release identity cannot include a symbolic link: ",
      basename(entries[[which(linked)[[1L]]]]),
      call. = FALSE
    )
  }
  relative <- vapply(entries, .builder_release_relative, "", root = target)
  order_index <- order(relative, method = "radix")
  entries <- entries[order_index]
  relative <- relative[order_index]
  records <- lapply(seq_along(entries), function(index) {
    path <- entries[[index]]
    directory <- dir.exists(path)
    if (!directory && !file.exists(path)) {
      stop("The release changed while its identity was read.", call. = FALSE)
    }
    list(
      path = gsub("\\", "/", relative[[index]], fixed = TRUE),
      type = if (directory) "directory" else "file",
      size = if (directory) 0 else as.double(file.info(path)$size),
      md5 = if (directory) NA_character_ else unname(tools::md5sum(path))
    )
  })
  list(schema_version = 1L, exists = TRUE, entries = records)
}

.builder_release_identity_valid <- function(identity) {
  is.list(identity) &&
    identical(identity$schema_version, 1L) &&
    is.logical(identity$exists) &&
    length(identity$exists) == 1L &&
    !is.na(identity$exists) &&
    is.list(identity$entries)
}

.builder_release_atomic_rds <- function(value, path, token) {
  temporary <- file.path(
    dirname(path),
    paste0(".", basename(path), ".", token, ".tmp")
  )
  if (.builder_release_exists(temporary)) {
    stop("A journal temporary path is already occupied.", call. = FALSE)
  }
  saved <- FALSE
  on.exit(
    {
      if (!saved && file.exists(temporary)) unlink(temporary, force = TRUE)
    },
    add = TRUE
  )
  saveRDS(value, temporary, version = 3)
  Sys.chmod(temporary, mode = "0600")
  if (!file.rename(temporary, path)) {
    stop(
      "The publication journal could not be replaced atomically.",
      call. = FALSE
    )
  }
  saved <- TRUE
  invisible(value)
}

.builder_release_read_rds <- function(path, subject) {
  if (!file.exists(path) || dir.exists(path) || .builder_release_link(path)) {
    stop(subject, " is missing or unsafe.", call. = FALSE)
  }
  tryCatch(readRDS(path), error = function(error) {
    stop(subject, " cannot be read safely.", call. = FALSE)
  })
}

.builder_release_allowed_control <- c(
  "journal.rds",
  "lock",
  "stages",
  "backup",
  "diagnostics"
)

.builder_release_assert_control <- function(control) {
  if (!dir.exists(control) || .builder_release_link(control)) {
    stop("The release control path is not a real directory.", call. = FALSE)
  }
  if (!.builder_release_mode_owner_only(control)) {
    stop("The release control directory must be owner-only.", call. = FALSE)
  }
  entries <- list.files(control, all.files = TRUE, no.. = TRUE)
  unknown <- setdiff(entries, .builder_release_allowed_control)
  if (length(unknown)) {
    stop(
      "The release has unknown control occupants: ",
      paste(unknown, collapse = ", "),
      ". Nothing was removed.",
      call. = FALSE
    )
  }
  known_paths <- file.path(control, entries)
  linked <- vapply(known_paths, .builder_release_link, logical(1))
  if (any(linked)) {
    stop(
      "The release control directory contains a symbolic link: ",
      entries[[which(linked)[[1L]]]],
      ". Nothing was removed.",
      call. = FALSE
    )
  }
  invisible(control)
}

.builder_release_ensure_control <- function(target) {
  control <- builder_release_control_path(target)
  if (!dir.exists(control)) {
    if (
      .builder_release_exists(control) || !dir.create(control, mode = "0700")
    ) {
      stop("The release control directory could not be created.", call. = FALSE)
    }
  }
  .builder_release_assert_control(control)
  control
}

.builder_release_owner <- function(token) {
  list(
    schema_version = 1L,
    token = token,
    host = .builder_release_host(),
    pid = as.integer(Sys.getpid()),
    acquired_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

.builder_release_acquire_lock <- function(control, token) {
  lock <- file.path(control, "lock")
  if (!dir.create(lock, mode = "0700", showWarnings = FALSE)) {
    stop(
      "Another coordinator owns this release, or recovery is required.",
      call. = FALSE
    )
  }
  owner_path <- file.path(lock, "owner.rds")
  owner <- .builder_release_owner(token)
  tryCatch(
    .builder_release_atomic_rds(owner, owner_path, token),
    error = function(error) {
      entries <- list.files(lock, all.files = TRUE, no.. = TRUE)
      if (!length(entries)) {
        unlink(lock, recursive = TRUE, force = TRUE)
      }
      stop(error)
    }
  )
  lock
}

.builder_release_lock_owner <- function(lock) {
  owner <- .builder_release_read_rds(
    file.path(lock, "owner.rds"),
    "The release lock owner"
  )
  valid <- is.list(owner) &&
    identical(owner$schema_version, 1L) &&
    .builder_release_text(owner$token) &&
    .builder_release_text(owner$host) &&
    is.integer(owner$pid) &&
    length(owner$pid) == 1L &&
    !is.na(owner$pid)
  if (!valid) {
    stop("The release lock owner is invalid.", call. = FALSE)
  }
  owner
}

.builder_release_assert_lock <- function(lock, token) {
  if (!dir.exists(lock) || .builder_release_link(lock)) {
    stop("The coordinator no longer owns the release lock.", call. = FALSE)
  }
  owner <- .builder_release_lock_owner(lock)
  if (!identical(owner$token, token)) {
    stop("The coordinator release lock identity changed.", call. = FALSE)
  }
  invisible(owner)
}

.builder_release_lock_known <- function(lock) {
  entries <- list.files(lock, all.files = TRUE, no.. = TRUE)
  identical(entries, "owner.rds")
}

.builder_release_release_lock <- function(control, lock, token) {
  .builder_release_assert_lock(lock, token)
  if (!.builder_release_lock_known(lock)) {
    stop(
      "The release lock contains unknown files and was preserved.",
      call. = FALSE
    )
  }
  isolated <- file.path(control, paste0(".released-lock-", token))
  if (.builder_release_exists(isolated) || !file.rename(lock, isolated)) {
    stop("The release lock could not be isolated safely.", call. = FALSE)
  }
  unlink(isolated, recursive = TRUE, force = TRUE)
  !dir.exists(lock)
}

.builder_release_journal_path <- function(control) {
  file.path(control, "journal.rds")
}

.builder_release_write_phase <- function(handle, phase, detail = NULL) {
  journal <- handle$record
  journal$phase <- phase
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$detail <- detail
  .builder_release_atomic_rds(journal, handle$journal, handle$token)
  handle$record <- journal
  handle
}

.builder_release_journal <- function(target, required = FALSE) {
  control <- builder_release_control_path(target)
  path <- .builder_release_journal_path(control)
  if (!file.exists(path)) {
    if (required) {
      stop("The release journal is missing.", call. = FALSE)
    }
    return(NULL)
  }
  journal <- .builder_release_read_rds(path, "The release journal")
  valid <- is.list(journal) &&
    identical(journal$schema_version, 1L) &&
    .builder_release_text(journal$target) &&
    .builder_release_text(journal$control) &&
    .builder_release_text(journal$build_id) &&
    .builder_release_text(journal$token) &&
    .builder_release_text(journal$phase) &&
    identical(
      .canonicalTargetPath(journal$target),
      .builder_release_path(target)
    ) &&
    identical(.canonicalTargetPath(journal$control), control)
  if (!valid) {
    stop("The release journal is invalid.", call. = FALSE)
  }
  journal
}

builder_discover_recovery <- function(target) {
  target <- .builder_release_path(target)
  control <- builder_release_control_path(target)
  if (!dir.exists(control)) {
    return(list(state = "ready", target = target, backup = NULL))
  }
  .builder_release_assert_control(control)
  journal <- .builder_release_journal(target)
  lock <- file.path(control, "lock")
  if (
    !is.null(journal) &&
      identical(journal$phase, "complete") &&
      dir.exists(lock)
  ) {
    return(list(
      state = "stale_lock",
      target = target,
      control = control,
      backup = NULL,
      journal = journal
    ))
  }
  if (
    is.null(journal) || journal$phase %in% c("complete", "aborted", "recovered")
  ) {
    return(list(
      state = "ready",
      target = target,
      backup = NULL,
      journal = journal
    ))
  }
  backup <- .builder_release_or(journal$backup, file.path(control, "backup"))
  list(
    state = "recovery_required",
    target = target,
    control = control,
    stage = journal$stage,
    backup = backup,
    phase = journal$phase,
    journal = journal,
    message = paste0(
      "Release recovery is required. Preserved backup: ",
      backup
    )
  )
}

builder_prepare_release <- function(
  target,
  build_id,
  expected_prior = NULL
) {
  target <- .builder_release_path(target)
  if (!.builder_release_text(build_id)) {
    stop("A release build id is required.", call. = FALSE)
  }
  stage_id <- gsub("[^A-Za-z0-9._-]", "-", build_id)
  stage_id <- .portableBundlePath(stage_id, "The release build id")
  control <- .builder_release_ensure_control(target)
  recovery <- builder_discover_recovery(target)
  if (identical(recovery$state, "stale_lock")) {
    .builder_release_isolate_stale_lock(control, recovery$journal)
    recovery <- builder_discover_recovery(target)
  }
  completed_journal <- recovery$journal
  completed_backup <- if (is.null(completed_journal)) {
    NULL
  } else {
    completed_journal$backup
  }
  if (
    identical(recovery$state, "ready") &&
      .builder_release_text(completed_backup) &&
      dir.exists(completed_backup)
  ) {
    backup_identity <- builder_release_identity(completed_backup)
    if (!identical(backup_identity, completed_journal$expected_prior)) {
      stop(
        "A completed release has an unrecognized preserved backup.",
        call. = FALSE
      )
    }
    unlink(completed_backup, recursive = TRUE, force = TRUE)
    if (dir.exists(completed_backup)) {
      stop("The completed release backup could not be removed.", call. = FALSE)
    }
  }
  if (identical(recovery$state, "recovery_required")) {
    stop(recovery$message, call. = FALSE)
  }
  token <- .builder_release_token("release")
  lock <- .builder_release_acquire_lock(control, token)
  prepared <- FALSE
  on.exit(
    {
      if (!prepared && dir.exists(lock)) {
        try(.builder_release_release_lock(control, lock, token), silent = TRUE)
      }
    },
    add = TRUE
  )
  stages <- file.path(control, "stages")
  if (!dir.exists(stages) && !dir.create(stages, mode = "0700")) {
    stop("The release stage registry could not be created.", call. = FALSE)
  }
  if (
    .builder_release_link(stages) || !.builder_release_mode_owner_only(stages)
  ) {
    stop("The release stage registry is unsafe.", call. = FALSE)
  }
  stage <- file.path(stages, paste0(stage_id, "-", token))
  if (.builder_release_exists(stage) || !dir.create(stage, mode = "0700")) {
    stop("The assigned release stage could not be created.", call. = FALSE)
  }
  if (is.null(expected_prior)) {
    expected_prior <- builder_release_identity(target)
  }
  if (!.builder_release_identity_valid(expected_prior)) {
    unlink(stage, recursive = TRUE, force = TRUE)
    stop("The expected prior release identity is invalid.", call. = FALSE)
  }
  journal <- .builder_release_journal_path(control)
  backup <- file.path(control, "backup")
  if (.builder_release_exists(backup)) {
    unlink(stage, recursive = TRUE, force = TRUE)
    stop("A preserved release backup requires recovery.", call. = FALSE)
  }
  record <- list(
    schema_version = 1L,
    target = target,
    control = control,
    stage = stage,
    backup = backup,
    journal = journal,
    lock = lock,
    build_id = build_id,
    token = token,
    host = .builder_release_host(),
    pid = as.integer(Sys.getpid()),
    expected_prior = expected_prior,
    phase = "prepared",
    updated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    detail = NULL
  )
  .builder_release_atomic_rds(record, journal, token)
  prepared <- TRUE
  structure(
    c(record, list(record = record)),
    class = c("builder_release_handle", "list")
  )
}

.builder_release_handle <- function(handle) {
  if (!inherits(handle, "builder_release_handle")) {
    stop("A release handle is required.", call. = FALSE)
  }
  .builder_release_assert_lock(handle$lock, handle$token)
  journal <- .builder_release_journal(handle$target, required = TRUE)
  if (!identical(journal$token, handle$token)) {
    stop("The release journal identity changed.", call. = FALSE)
  }
  handle$record <- journal
  handle
}

.builder_release_restore <- function(handle, detail, .move = file.rename) {
  target_exists <- .builder_release_exists(handle$target)
  backup_exists <- dir.exists(handle$backup)
  if (target_exists) {
    if (.builder_release_exists(handle$stage)) {
      handle <- .builder_release_write_phase(
        handle,
        "recovery_required",
        detail
      )
      return(list(handle = handle, restored = FALSE))
    }
    if (!isTRUE(.move(handle$target, handle$stage))) {
      handle <- .builder_release_write_phase(
        handle,
        "recovery_required",
        detail
      )
      return(list(handle = handle, restored = FALSE))
    }
  }
  if (backup_exists && !isTRUE(.move(handle$backup, handle$target))) {
    handle <- .builder_release_write_phase(handle, "recovery_required", detail)
    return(list(handle = handle, restored = FALSE))
  }
  handle <- .builder_release_write_phase(handle, "prepared", detail)
  list(handle = handle, restored = TRUE)
}

builder_publish_release <- function(
  handle,
  .move = file.rename,
  .after_phase = function(phase) invisible(NULL),
  .after_move = function(move) invisible(NULL)
) {
  handle <- .builder_release_handle(handle)
  if (!identical(handle$record$phase, "prepared")) {
    stop("The release is not in its prepared phase.", call. = FALSE)
  }
  if (!dir.exists(handle$stage) || .builder_release_link(handle$stage)) {
    stop("The assigned release stage is missing or unsafe.", call. = FALSE)
  }
  handle$record$prepared_identity <- builder_release_identity(handle$stage)
  handle <- .builder_release_write_phase(handle, "locked")
  current <- builder_release_identity(handle$target)
  if (!identical(current, handle$expected_prior)) {
    stop(
      "The release changed after Review; nothing was published.",
      call. = FALSE
    )
  }
  if (isTRUE(current$exists)) {
    moved <- tryCatch(
      .move(handle$target, handle$backup),
      error = function(error) FALSE
    )
    if (!isTRUE(moved)) {
      handle <- .builder_release_write_phase(
        handle,
        "prepared",
        "The prior release could not be protected."
      )
      stop("The prior release could not be protected.", call. = FALSE)
    }
    .after_move("old_to_backup")
  }
  handle <- .builder_release_write_phase(handle, "old_moved")
  .after_phase("old_moved")
  moved <- tryCatch(
    .move(handle$stage, handle$target),
    error = function(error) FALSE
  )
  if (!isTRUE(moved)) {
    restored <- .builder_release_restore(
      handle,
      paste0("Publication failed. Preserved backup: ", handle$backup),
      .move = .move
    )
    if (!isTRUE(restored$restored)) {
      stop(
        "Publication and restoration failed. Preserved backup: ",
        handle$backup,
        call. = FALSE
      )
    }
    stop("Publication failed; the prior release was restored.", call. = FALSE)
  }
  .after_move("new_to_target")
  handle <- .builder_release_write_phase(handle, "new_published")
  .after_phase("new_published")
  handle <- .builder_release_write_phase(handle, "complete")
  .after_phase("complete")
  .builder_release_release_lock(handle$control, handle$lock, handle$token)
  warning <- NULL
  if (dir.exists(handle$backup)) {
    unlink(handle$backup, recursive = TRUE, force = TRUE)
    if (dir.exists(handle$backup)) {
      warning <- paste0(
        "The release was published, but its prior backup remains: ",
        handle$backup
      )
    }
  }
  list(
    error = NULL,
    published = TRUE,
    target = handle$target,
    identity = builder_release_identity(handle$target),
    journal = handle$journal,
    warning = warning
  )
}

builder_abort_release <- function(handle) {
  handle <- .builder_release_handle(handle)
  if (
    handle$record$phase %in%
      c("old_moved", "new_published", "recovery_required")
  ) {
    stop("This release requires recovery and cannot be aborted.", call. = FALSE)
  }
  if (dir.exists(handle$stage)) {
    unlink(handle$stage, recursive = TRUE, force = TRUE)
  }
  handle <- .builder_release_write_phase(handle, "aborted")
  .builder_release_release_lock(handle$control, handle$lock, handle$token)
  list(aborted = TRUE, target = handle$target)
}

.builder_release_pid_alive <- function(pid) {
  isTRUE(tryCatch(tools::pskill(pid, signal = 0L), error = function(error) {
    FALSE
  }))
}

.builder_release_isolate_stale_lock <- function(control, journal) {
  lock <- file.path(control, "lock")
  owner <- .builder_release_lock_owner(lock)
  recoverable <- identical(owner$token, journal$token) &&
    identical(owner$host, .builder_release_host()) &&
    !.builder_release_pid_alive(owner$pid) &&
    .builder_release_lock_known(lock)
  if (!recoverable) {
    stop(
      "The release lock is not safely recoverable; manual inspection is required.",
      call. = FALSE
    )
  }
  isolated <- file.path(control, paste0(".stale-lock-", owner$token))
  if (.builder_release_exists(isolated) || !file.rename(lock, isolated)) {
    stop("The stale release lock could not be isolated.", call. = FALSE)
  }
  unlink(isolated, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

builder_recover_release <- function(target, action = c("restore", "abort")) {
  action <- match.arg(action)
  recovery <- builder_discover_recovery(target)
  if (!identical(recovery$state, "recovery_required")) {
    return(list(recovered = FALSE, state = recovery$state))
  }
  journal <- recovery$journal
  control <- recovery$control
  lock <- file.path(control, "lock")
  if (dir.exists(lock)) {
    .builder_release_isolate_stale_lock(control, journal)
  }
  token <- .builder_release_token("recovery")
  recovery_lock <- .builder_release_acquire_lock(control, token)
  on.exit(
    {
      if (dir.exists(recovery_lock)) {
        try(
          .builder_release_release_lock(control, recovery_lock, token),
          silent = TRUE
        )
      }
    },
    add = TRUE
  )
  journal$recovery_from <- if (
    startsWith(journal$phase, "recovering_") &&
      .builder_release_text(journal$recovery_from)
  ) {
    journal$recovery_from
  } else {
    journal$phase
  }
  journal$phase <- paste0("recovering_", action)
  journal$detail <- paste0("Recovery action started: ", action)
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$token <- token
  journal$host <- .builder_release_host()
  journal$pid <- as.integer(Sys.getpid())
  journal$lock <- recovery_lock
  .builder_release_atomic_rds(
    journal,
    .builder_release_journal_path(control),
    token
  )
  target <- recovery$target
  backup <- recovery$backup
  if (identical(action, "restore")) {
    if (!dir.exists(backup)) {
      already_restored <-
        .builder_release_exists(target) &&
        identical(
          builder_release_identity(target),
          journal$expected_prior
        )
      if (!already_restored) {
        stop("The preserved release backup is missing.", call. = FALSE)
      }
    }
    if (dir.exists(backup) && .builder_release_exists(target)) {
      published_identity <- journal$prepared_identity
      owned_published <-
        journal$recovery_from %in%
        c("old_moved", "new_published") &&
        .builder_release_identity_valid(published_identity) &&
        identical(builder_release_identity(target), published_identity) &&
        !.builder_release_exists(journal$stage)
      if (!owned_published || !file.rename(target, journal$stage)) {
        stop(
          "The release target is occupied; the backup was preserved.",
          call. = FALSE
        )
      }
    }
    if (dir.exists(backup) && !file.rename(backup, target)) {
      stop("The preserved release backup could not be restored.", call. = FALSE)
    }
  } else if (dir.exists(backup)) {
    stop(
      "A preserved prior release cannot be discarded by abort.",
      call. = FALSE
    )
  }
  stage <- journal$stage
  if (dir.exists(stage) && .pathWithin(stage, file.path(control, "stages"))) {
    unlink(stage, recursive = TRUE, force = TRUE)
  }
  journal$phase <- "recovered"
  journal$detail <- paste0("Recovery action: ", action)
  journal$recovery_from <- NULL
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$token <- token
  journal$lock <- recovery_lock
  .builder_release_atomic_rds(
    journal,
    .builder_release_journal_path(control),
    token
  )
  .builder_release_release_lock(control, recovery_lock, token)
  list(recovered = TRUE, action = action, target = target)
}
