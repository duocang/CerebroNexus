# Shared measurement helpers.

# Resident set size of the current process, in MB. R's own gc() figures only
# account for R's heap, which misses the BPCells/HDF5 buffers that are the whole
# point of comparing backends, so ask the OS instead.
bench_rss_mb <- function() {
  out <- suppressWarnings(system2(
    "ps",
    c("-o", "rss=", "-p", Sys.getpid()),
    stdout = TRUE,
    stderr = FALSE
  ))
  val <- suppressWarnings(as.numeric(trimws(paste(out, collapse = ""))))
  if (is.na(val)) NA_real_ else val / 1024
}

# Peak resident set size on Linux. VmHWM includes native allocations that R's
# gc() accounting misses. Other platforms return NA rather than a false proxy.
bench_peak_rss_mb <- function(status_path = "/proc/self/status") {
  if (!file.exists(status_path)) {
    return(NA_real_)
  }
  line <- grep(
    "^VmHWM:",
    readLines(status_path, warn = FALSE),
    value = TRUE
  )
  value_kb <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", line[1L])))
  if (!length(value_kb) || !is.finite(value_kb)) NA_real_ else value_kb / 1024
}

bench_path_mb <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NA_real_)
  }
  if (dir.exists(path)) {
    files <- list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE
    )
    files <- files[!dir.exists(files)]
    return(sum(file.size(files), na.rm = TRUE) / 2^20)
  }
  file.size(path) / 2^20
}

# Wall clock of one expression, in seconds.
bench_time <- function(expr) {
  t0 <- Sys.time()
  force(expr)
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

# Append one row to a CSV, writing the header only when creating the file. Rows
# land on disk as soon as they are measured so an aborted sweep still leaves
# every tier it did finish.
bench_append_row <- function(path, row) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    row,
    path,
    sep = ",",
    row.names = FALSE,
    qmethod = "double",
    col.names = !file.exists(path),
    append = file.exists(path)
  )
  invisible(row)
}

bench_msg <- function(...) {
  message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), sprintf(...)))
}
