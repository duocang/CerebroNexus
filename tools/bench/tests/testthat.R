# Entry point for the benchmark's repository-only tests.

bench_root <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(bench_root)) {
  bench_root <- normalizePath("..")
}
testthat::test_dir(
  file.path(bench_root, "tests", "testthat"),
  reporter = "summary",
  stop_on_failure = TRUE,
  stop_on_warning = TRUE
)
