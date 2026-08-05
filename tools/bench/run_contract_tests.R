# Run repository-only benchmark contract tests without loading CerebroNexus.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("run this file with Rscript", call. = FALSE)
}
bench_root <- normalizePath(dirname(sub("^--file=", "", script_arg)))
Sys.setenv(BENCH_ROOT = bench_root)
source(file.path(bench_root, "tests", "testthat.R"), chdir = TRUE)
