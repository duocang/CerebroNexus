#' Filesystem mechanics for replacing export artifacts safely
#'
#' An export with an external backend produces two things that only mean
#' something together: the `.crb` and its sibling matrix (`<stem>.h5` or
#' `<stem>.bpcells/`). The exporter used to delete and rewrite the sibling
#' before it had finished collecting the rest of the data, so a later failure
#' left the previous `.crb` in place next to a sibling belonging to the run
#' that failed. The pair still looked complete and described different data.
#'
#' These helpers keep every write inside a hidden staging directory on the
#' target filesystem, move the previous outputs aside only once the ordinary
#' export work has succeeded, and put them back if anything goes wrong while
#' publishing. Staging and backup live under the target directory so that
#' publication is a rename rather than a copy, which keeps the window in which
#' the two entries disagree as short as the filesystem allows.
#'
#' The guarantee is transactional replacement at the R call boundary: an
#' ordinary error, condition, or a single interrupt restores the previous
#' artifacts. It is not atomicity against process death or power loss -- no
#' operating system can swap a file and a sibling directory in one indivisible
#' step -- and interrupting the rollback itself will leave it half done. In
#' that case everything still exists: the backup directory is kept, with a
#' `RECOVERY.txt` naming where each file belongs.
#'
#' One writer at a time. Two exports running against the same target will
#' interleave their backup and publish steps and the loser's output is
#' discarded without a word.
#'
#' This file holds filesystem mechanics only. It knows nothing about Seurat,
#' matrix backends, or the Cerebro object.
#'
#' @keywords internal
#' @noRd
NULL

#' Describe where an export writes, stages and backs up its artifacts
#'
#' @param file The final `.crb` path requested by the caller.
#' @param mode One of `embedded`, `h5` or `bpcells`.
#'
#' @return A list describing target, staged and backup paths.
#'
#' @keywords internal
#' @noRd
.newExportArtifactLayout <- function(file, mode) {
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(file)
  ) {
    stop("`file` must be a single non-empty file path.", call. = FALSE)
  }
  if (
    !is.character(mode) ||
      length(mode) != 1L ||
      !mode %in% c("embedded", "h5", "bpcells")
  ) {
    stop(
      "`mode` must be one of \"embedded\", \"h5\" or \"bpcells\".",
      call. = FALSE
    )
  }

  ## A directory sitting where the .crb should go used to make `saveRDS()`
  ## refuse, leaving the directory alone. Backing it up and publishing over it
  ## would move somebody's folder into staging and delete it on success.
  if (dir.exists(file)) {
    stop(
      "`file` points at an existing directory: ",
      file,
      call. = FALSE
    )
  }

  out_dir <- dirname(file)
  if (!nzchar(out_dir)) {
    out_dir <- "."
  }
  ## `recursive = TRUE` on purpose: the exporter used to create only the last
  ## component, so a multi-level output path failed at the very end, after the
  ## whole export had been computed.
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  if (!dir.exists(out_dir)) {
    stop("Could not create output directory: ", out_dir, call. = FALSE)
  }

  crb_name <- basename(file)
  sibling_name <- switch(
    mode,
    embedded = NULL,
    h5 = paste0(tools::file_path_sans_ext(crb_name), ".h5"),
    bpcells = paste0(tools::file_path_sans_ext(crb_name), ".bpcells")
  )

  ## `tempfile()` only supplies a unique token here; the directories are
  ## created next to the target so every publish stays a same-filesystem
  ## rename. It draws from its own counter, not the user's RNG stream, so an
  ## export stays reproducible under `set.seed()`.
  token <- basename(tempfile(pattern = ""))
  stage_dir <- file.path(out_dir, paste0(".cerebro-export-stage-", token))
  backup_dir <- file.path(out_dir, paste0(".cerebro-export-backup-", token))
  dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(stage_dir) || !dir.exists(backup_dir)) {
    stop(
      "Could not create staging directories under: ",
      out_dir,
      call. = FALSE
    )
  }

  layout <- list(
    mode = mode,
    out_dir = out_dir,
    stage_dir = stage_dir,
    backup_dir = backup_dir,
    crb_name = crb_name,
    sibling_name = sibling_name,
    target_crb = file.path(out_dir, crb_name),
    staged_crb = file.path(stage_dir, crb_name),
    backup_crb = file.path(backup_dir, crb_name),
    target_sibling = NULL,
    staged_sibling = NULL,
    backup_sibling = NULL
  )
  if (!is.null(sibling_name)) {
    layout$target_sibling <- file.path(out_dir, sibling_name)
    layout$staged_sibling <- file.path(stage_dir, sibling_name)
    layout$backup_sibling <- file.path(backup_dir, sibling_name)
  }
  layout
}

#' Move one artifact, falling back to copy when rename is refused
#'
#' @param .rename Injection seam. The copy fallback only runs when a rename is
#'   refused, which needs two filesystems to reproduce; tests pass a rename
#'   that always fails instead.
#'
#' @return `TRUE` when something was moved, `FALSE` when `from` did not exist.
#'
#' @keywords internal
#' @noRd
.moveExportArtifact <- function(from, to, .rename = file.rename) {
  if (is.null(from) || is.null(to) || !file.exists(from)) {
    return(FALSE)
  }
  ok <- suppressWarnings(.rename(from, to))
  if (isTRUE(ok)) {
    return(TRUE)
  }
  ## Rename is refused across filesystems. Staging sits next to the target so
  ## this should not happen, but a copy keeps the contract rather than
  ## silently losing the artifact.
  ##
  ## `file.copy(recursive = TRUE)` copies a directory *into* an existing
  ## directory rather than *to* a path, so a bpcells sibling has to be created
  ## first and copied entry by entry.
  if (dir.exists(from)) {
    dir.create(to, showWarnings = FALSE, recursive = TRUE)
    entries <- list.files(
      from,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE
    )
    copied <- suppressWarnings(
      file.copy(entries, to, recursive = TRUE, copy.date = TRUE)
    )
    complete <- isTRUE(all(copied)) &&
      length(list.files(to, all.files = TRUE, no.. = TRUE)) == length(entries)
    if (!complete) {
      unlink(to, recursive = TRUE, force = TRUE)
      stop("Could not move ", from, " to ", to, call. = FALSE)
    }
  } else {
    copied <- suppressWarnings(file.copy(from, to, copy.date = TRUE))
    if (!isTRUE(all(copied))) {
      stop("Could not move ", from, " to ", to, call. = FALSE)
    }
  }
  unlink(from, recursive = TRUE, force = TRUE)
  TRUE
}

#' Move existing final artifacts aside, all or nothing
#'
#' If the second move fails, the first is put back before the error travels on,
#' so the caller never sees a half-backed-up state.
#'
#' @keywords internal
#' @noRd
.backupExportArtifacts <- function(layout) {
  done <- list()
  pairs <- list(
    list(from = layout$target_crb, to = layout$backup_crb),
    list(from = layout$target_sibling, to = layout$backup_sibling)
  )
  for (pair in pairs) {
    if (is.null(pair$from)) {
      next
    }
    moved <- tryCatch(
      .moveExportArtifact(pair$from, pair$to),
      error = function(e) {
        for (undo in rev(done)) {
          suppressWarnings(try(
            .moveExportArtifact(undo$to, undo$from),
            silent = TRUE
          ))
        }
        stop(
          "Could not set aside the existing export artifacts: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    if (isTRUE(moved)) {
      done[[length(done) + 1L]] <- pair
    }
  }

  ## If the process is killed outright, or an interrupt lands while the
  ## rollback is running, these files are all that is left of the previous
  ## export -- inside a hidden directory nobody would think to look in. Say
  ## where each one belongs.
  if (length(done) > 0) {
    writeLines(
      c(
        "This directory holds the previous CerebroNexus export.",
        "",
        "It was moved aside so a new export could take its place, and it is",
        "kept because that replacement did not finish. To restore it by hand,",
        "move each file back to the path listed next to it, then delete this",
        "directory along with any .cerebro-export-stage-* beside it.",
        "",
        vapply(
          done,
          function(x) paste0("  ", basename(x$to), "  ->  ", x$from),
          character(1)
        )
      ),
      file.path(layout$backup_dir, "RECOVERY.txt")
    )
  }
  invisible(vapply(done, function(x) x$from, character(1)))
}

#' Move a staged artifact into its final place
#'
#' Refuses to overwrite: by the time this runs, anything previously at `to`
#' must already be in the backup directory. Overwriting here would destroy the
#' only copy.
#'
#' @keywords internal
#' @noRd
.publishStagedArtifact <- function(from, to) {
  if (is.null(from) || is.null(to)) {
    return(invisible(FALSE))
  }
  if (!file.exists(from)) {
    stop("Nothing staged at: ", from, call. = FALSE)
  }
  if (file.exists(to)) {
    stop(
      "Refusing to overwrite an artifact that was not backed up first: ",
      to,
      call. = FALSE
    )
  }
  .moveExportArtifact(from, to)
  invisible(TRUE)
}

#' Undo a partial publish
#'
#' Removes whatever this run published, then moves the backups back.
#'
#' Reports with `message()` rather than `warning()` on purpose. It runs while
#' another error is already travelling, and under `options(warn = 2)` -- common
#' in batch pipelines -- a warning here would be promoted to an error and take
#' the place of the real cause.
#'
#' @return The target paths that could **not** be restored, invisibly. The
#'   caller must keep the backup directory when this is non-empty.
#'
#' @keywords internal
#' @noRd
.restoreExportArtifacts <- function(layout) {
  targets <- list(
    list(target = layout$target_crb, backup = layout$backup_crb),
    list(target = layout$target_sibling, backup = layout$backup_sibling)
  )
  unrestored <- character()
  for (item in targets) {
    if (is.null(item$target)) {
      next
    }
    if (file.exists(item$backup)) {
      ## This run published something over the gap left by the backup; take it
      ## away before putting the original back.
      if (file.exists(item$target)) {
        unlink(item$target, recursive = TRUE, force = TRUE)
      }
      restored <- tryCatch(
        .moveExportArtifact(item$backup, item$target),
        error = function(e) e
      )
      if (inherits(restored, "condition")) {
        unrestored <- c(unrestored, item$target)
        message(
          "Could not put the previous export artifact back. It is still ",
          "readable at: ",
          item$backup,
          " and belongs at: ",
          item$target
        )
      }
    } else if (file.exists(item$target)) {
      ## Nothing was there before this run, so a published artifact is part of
      ## the failed attempt and must not be left behind.
      unlink(item$target, recursive = TRUE, force = TRUE)
    }
  }
  invisible(unrestored)
}

#' Remove the staging and backup directories
#'
#' Idempotent: it is registered with `on.exit()` and may also be called on the
#' success path.
#'
#' @param keep_backup Keep the backup directory. Set this whenever a rollback
#'   could not put something back -- deleting the only surviving copy of the
#'   previous export, right after saying where to find it, would turn a failed
#'   export into lost data.
#'
#' @keywords internal
#' @noRd
.cleanupExportArtifactLayout <- function(layout, keep_backup = FALSE) {
  if (is.null(layout)) {
    return(invisible(NULL))
  }
  dirs <- layout$stage_dir
  if (!isTRUE(keep_backup)) {
    dirs <- c(dirs, layout$backup_dir)
  }
  for (dir in dirs) {
    if (!is.null(dir) && dir.exists(dir)) {
      unlink(dir, recursive = TRUE, force = TRUE)
    }
  }
  invisible(NULL)
}
