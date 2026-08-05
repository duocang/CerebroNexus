# Run repository-only benchmark contract tests without loading CerebroNexus.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("run this file with Rscript", call. = FALSE)
}
bench_root <- normalizePath(dirname(sub("^--file=", "", script_arg)))
Sys.setenv(BENCH_ROOT = bench_root)

required_packages <- c("rhdf5", "Matrix", "testthat")
for (package in required_packages) {
  load_error <- NULL
  loaded <- tryCatch(
    requireNamespace(package, quietly = TRUE),
    error = function(error) {
      load_error <<- conditionMessage(error)
      FALSE
    }
  )
  if (!isTRUE(loaded)) {
    detail <- if (is.null(load_error)) "not found" else load_error
    stop(
      "benchmark contract dependency cannot be loaded: ",
      package,
      " (",
      detail,
      ")",
      call. = FALSE
    )
  }
}

source(file.path(bench_root, "tests", "testthat.R"), chdir = TRUE)
