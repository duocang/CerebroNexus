## Shared by the large-demo preparation scripts (Ren, CosMx, T-cell).
##
## A source file is described by `list(url, size, md5)` or `list(url, size,
## sha256)`. The size is always checked; the checksum is checked once, when the
## file has just been downloaded.

.demoLargeCacheDir <- function(path = NULL) {
  if (is.null(path)) {
    configured <- Sys.getenv("CEREBRO_LARGE_CACHE", "")
    if (nzchar(configured)) path <- configured
  }
  if (!is.null(path)) {
    if (
      !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
    ) {
      stop("CEREBRO_LARGE_CACHE must be one non-empty path.", call. = FALSE)
    }
    return(path)
  }
  file.path(tools::R_user_dir("CerebroNexus", "cache"), "large-examples")
}

.demoRequire <- function(packages, demo) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "The ",
      demo,
      " demo requires: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

.demoFileChecksum <- function(path, spec) {
  if (!is.null(spec$sha256)) {
    tool <- Sys.which("sha256sum")
    args <- shQuote(path)
    if (!nzchar(tool)) {
      tool <- Sys.which("shasum")
      args <- c("-a", "256", shQuote(path))
    }
    if (!nzchar(tool)) {
      return(NULL)
    }
    output <- system2(tool, args, stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L) {
      stop("Could not checksum ", basename(path), ".", call. = FALSE)
    }
    return(list(
      observed = tolower(strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]),
      expected = tolower(spec$sha256)
    ))
  }
  if (!is.null(spec$md5)) {
    return(list(
      observed = unname(tools::md5sum(path)),
      expected = tolower(spec$md5)
    ))
  }
  NULL
}

.demoValidateFile <- function(path, spec, checksum = FALSE) {
  if (!file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  size <- unname(file.info(path)$size)
  if (!is.finite(size) || size != spec$size) {
    return(FALSE)
  }
  if (isTRUE(checksum)) {
    sums <- .demoFileChecksum(path, spec)
    if (!is.null(sums) && !identical(sums$observed, sums$expected)) {
      stop("Checksum mismatch for ", basename(path), ".", call. = FALSE)
    }
  }
  TRUE
}

## Downloads `spec$url` to `destination` unless a file of the expected size is
## already there. A partial download (`<destination>.part`) is resumed.
.demoDownload <- function(spec, destination) {
  if (.demoValidateFile(destination, spec)) {
    return(destination)
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  partial <- paste0(destination, ".part")
  if (file.exists(destination)) {
    unlink(destination, force = TRUE)
  }

  curl <- Sys.which("curl")
  if (nzchar(curl)) {
    status <- system2(
      curl,
      c(
        "-fL",
        "--retry",
        "8",
        "--retry-delay",
        "5",
        "--continue-at",
        "-",
        "-o",
        shQuote(partial),
        shQuote(spec$url)
      )
    )
    if (!identical(status, 0L)) {
      stop("Download failed for ", basename(destination), ".", call. = FALSE)
    }
  } else {
    if (file.exists(partial)) {
      unlink(partial, force = TRUE)
    }
    old_options <- options(timeout = max(getOption("timeout"), 86400))
    on.exit(options(old_options), add = TRUE)
    utils::download.file(spec$url, partial, mode = "wb", method = "libcurl")
  }

  if (!.demoValidateFile(partial, spec, checksum = TRUE)) {
    stop(
      "Downloaded file is incomplete: ",
      basename(destination),
      call. = FALSE
    )
  }
  if (!file.rename(partial, destination)) {
    stop("Could not publish ", basename(destination), ".", call. = FALSE)
  }
  destination
}

## Builds in `stage`, then swaps it in as `target`. A failed or interrupted
## build leaves the previous prepared directory (if any) untouched.
.demoPublishStage <- function(stage, target, root, label) {
  backup <- NULL
  if (dir.exists(target)) {
    backup <- tempfile(paste0(basename(target), "-previous-"), root)
    if (!file.rename(target, backup)) {
      stop("Cannot preserve previous ", label, " cache.", call. = FALSE)
    }
  }
  if (!file.rename(stage, target)) {
    if (!is.null(backup)) {
      file.rename(backup, target)
    }
    stop(
      "Cannot publish prepared ",
      label,
      " cache; previous cache was preserved.",
      call. = FALSE
    )
  }
  if (!is.null(backup)) {
    unlink(backup, recursive = TRUE)
  }
  invisible(target)
}
