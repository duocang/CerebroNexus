# End-to-end Viewer validation helpers for full-source C2 runs.

bench_viewer_schedule <- function(schedule) {
  required <- c("profile", "source", "n_cells", "backend", "export_repeat")
  if (!all(required %in% names(schedule))) {
    stop("Viewer schedule is missing required columns", call. = FALSE)
  }
  rows <- schedule[
    schedule$profile == "panel_c2" & schedule$export_repeat == 1L,
    required,
    drop = FALSE
  ]
  key <- paste(rows$source, rows$n_cells, rows$backend, rows$export_repeat)
  if (!nrow(rows) || anyDuplicated(key)) {
    stop("Viewer schedule must contain unique C2 rows", call. = FALSE)
  }
  rownames(rows) <- NULL
  rows
}

bench_validate_viewer_results <- function(schedule, results, run_id = NULL) {
  expected <- bench_viewer_schedule(schedule)
  timings <- c(
    "bundle_secs",
    "launch_secs",
    "hover_secs",
    "selection_secs",
    "zoom_secs",
    "gene_secs"
  )
  required <- c(
    "run_id",
    names(expected),
    "status",
    "correctness",
    timings
  )
  if (!all(required %in% names(results))) {
    stop("Viewer results are missing required columns", call. = FALSE)
  }
  key <- function(rows) {
    paste(rows$source, rows$n_cells, rows$backend, rows$export_repeat)
  }
  expected_keys <- key(expected)
  result_keys <- key(results)
  if (!setequal(expected_keys, result_keys) || anyDuplicated(result_keys)) {
    stop(
      "Viewer result set does not cover the scheduled C2 rows",
      call. = FALSE
    )
  }
  if (!is.null(run_id) && any(results$run_id != run_id)) {
    stop("Viewer results do not share the phase run id", call. = FALSE)
  }
  if (any(results$status != "OK") || any(results$correctness != "OK")) {
    stop("one or more Viewer validations failed", call. = FALSE)
  }
  values <- as.matrix(results[timings])
  storage.mode(values) <- "double"
  if (any(!is.finite(values)) || any(values < 0)) {
    stop("Viewer timings must be finite and non-negative", call. = FALSE)
  }
  invisible(TRUE)
}
