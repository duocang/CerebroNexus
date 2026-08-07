##----------------------------------------------------------------------------##
## Publish a completed build as one rollback-capable batch.
##
## The worker builds inside a staging directory under the final output
## directory, so every rename stays on one filesystem. Existing targets are
## moved aside first and restored if any publication step fails.
##----------------------------------------------------------------------------##

builder_remove_path <- function(path) {
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
  } else if (file.exists(path)) {
    unlink(path, force = TRUE)
  }
  !file.exists(path) && !dir.exists(path)
}

builder_publish_batch <- function(
  staged,
  targets,
  overwrite = FALSE,
  .move = file.rename
) {
  staged <- as.character(staged)
  targets <- as.character(targets)
  if (!length(staged) || length(staged) != length(targets)) {
    return(list(error = "The staged and target batches do not match."))
  }
  if (anyDuplicated(targets)) {
    return(list(error = "The publication targets are not unique."))
  }

  missing <- staged[!file.exists(staged) & !dir.exists(staged)]
  if (length(missing)) {
    return(list(
      error = paste0(
        "The staged build is incomplete: ",
        paste(basename(missing), collapse = ", "),
        "."
      )
    ))
  }

  existing <- targets[file.exists(targets) | dir.exists(targets)]
  if (length(existing) && !isTRUE(overwrite)) {
    return(list(
      error = paste0(
        "These outputs already exist: ",
        paste(basename(existing), collapse = ", "),
        ". Confirm replacement before building."
      ),
      existing = existing
    ))
  }

  for (dir in unique(dirname(targets))) {
    if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE)) {
      return(list(error = paste0("Cannot create output directory: ", dir)))
    }
  }

  backup_root <- tempfile(".builder-backup-", tmpdir = dirname(targets[1]))
  if (!dir.create(backup_root)) {
    return(list(error = "Cannot create a publication backup directory."))
  }
  on.exit(builder_remove_path(backup_root), add = TRUE)

  backups <- rep(NA_character_, length(targets))
  published <- logical(length(targets))

  rollback <- function() {
    for (index in rev(seq_along(targets))) {
      if (published[index]) {
        builder_remove_path(targets[index])
      }
      if (
        !is.na(backups[index]) &&
          (file.exists(backups[index]) || dir.exists(backups[index]))
      ) {
        .move(backups[index], targets[index])
      }
    }
  }

  for (index in seq_along(targets)) {
    if (file.exists(targets[index]) || dir.exists(targets[index])) {
      backups[index] <- file.path(
        backup_root,
        paste0(sprintf("%04d", index), "-", basename(targets[index]))
      )
      if (!.move(targets[index], backups[index])) {
        rollback()
        return(list(
          error = paste0(
            "Could not protect existing output: ",
            targets[index]
          )
        ))
      }
    }
  }

  for (index in seq_along(targets)) {
    if (!.move(staged[index], targets[index])) {
      rollback()
      return(list(
        error = paste0(
          "Could not publish ",
          basename(targets[index]),
          "; existing outputs were restored."
        )
      ))
    }
    published[index] <- TRUE
  }

  list(error = NULL, published = targets)
}
