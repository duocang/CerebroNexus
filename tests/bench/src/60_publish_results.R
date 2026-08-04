# Publish a validated result directory without replacing prior runs.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop("need <stage_dir> <result_root> <run_id>", call. = FALSE)
}
stage <- normalizePath(args[1], mustWork = TRUE)
result_root <- args[2]
run_id <- args[3]

if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", run_id)) {
  stop("unsafe run id: ", run_id, call. = FALSE)
}

tree_fingerprint <- function(path) {
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[!dir.exists(files)]
  relative <- substring(files, nchar(path) + 2L)
  order <- order(relative)
  data.frame(
    path = relative[order],
    md5 = unname(tools::md5sum(files[order])),
    stringsAsFactors = FALSE
  )
}

source_fingerprint <- tree_fingerprint(stage)
if (!nrow(source_fingerprint)) {
  stop("staged result directory is empty", call. = FALSE)
}

runs <- file.path(result_root, "runs")
dir.create(runs, recursive = TRUE, showWarnings = FALSE)
destination <- file.path(runs, run_id)

if (dir.exists(destination)) {
  if (!identical(source_fingerprint, tree_fingerprint(destination))) {
    stop("conflicting existing run: ", run_id, call. = FALSE)
  }
} else {
  staged_destination <- tempfile(
    paste0(".stage-", run_id, "-"),
    tmpdir = runs
  )
  dir.create(staged_destination)
  on.exit(
    {
      if (dir.exists(staged_destination)) {
        unlink(staged_destination, recursive = TRUE)
      }
    },
    add = TRUE
  )
  entries <- list.files(stage, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  copied <- file.copy(
    entries,
    staged_destination,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (
    !all(copied) ||
      !identical(
        source_fingerprint,
        tree_fingerprint(staged_destination)
      )
  ) {
    stop("failed to stage an exact result copy", call. = FALSE)
  }
  if (!file.rename(staged_destination, destination)) {
    stop("failed to publish immutable result directory", call. = FALSE)
  }
}

if (identical(Sys.getenv("BENCH_PUBLISH_FAIL_AT"), "before-pointer")) {
  stop("injected failure before CURRENT update", call. = FALSE)
}

pointer <- file.path(result_root, "CURRENT")
pointer_stage <- tempfile(".CURRENT-", tmpdir = result_root)
writeLines(run_id, pointer_stage, useBytes = TRUE)
if (!file.rename(pointer_stage, pointer)) {
  copied <- file.copy(pointer_stage, pointer, overwrite = TRUE)
  unlink(pointer_stage)
  if (!copied) {
    stop("failed to update CURRENT result pointer", call. = FALSE)
  }
}
message("published immutable benchmark run ", run_id)
