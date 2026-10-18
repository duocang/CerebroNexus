bench_root_supplement <- normalizePath(file.path("..", "benchmarks", "expression-backends", "scripts"), mustWork = FALSE)
published_scale <- normalizePath(file.path(
  "..", "benchmarks", "results", "expression-backends", "scale", "runs",
  "20261016T224351Z-63d5337c3647-scale"
), mustWork = FALSE)

# Build a supplement stage from a published Scale run: one embedded tier, both
# fetched sources, a memory-only preflight failure, and the filter recorded.
supplement_stage <- function(root, filter = "source=human_pfc_hbcc;n_cells=300000;backend=embedded") {
  stage <- file.path(root, "stage")
  dir.create(file.path(stage, "logs"), recursive = TRUE)
  read <- function(name) utils::read.csv(file.path(published_scale, name), stringsAsFactors = FALSE, check.names = FALSE)
  write <- function(d, name) utils::write.csv(d, file.path(stage, name), row.names = FALSE)
  tier <- function(d) d$source == "human_pfc_hbcc" & d$n_cells == 300000
  for (name in c("05_schedule.csv", "10_export.csv", "20_access.csv", "21_access_timings.csv", "crashes.csv")) {
    d <- read(name)
    write(d[tier(d) & d$backend == "embedded", , drop = FALSE], name)
  }
  for (name in c("query_plan_manifest.csv", "query_panel.csv")) {
    d <- read(name)
    write(d[tier(d), , drop = FALSE], name)
  }
  check <- read("resource_check.csv")
  check <- check[tier(check), , drop = FALSE]
  check$embedded_safe <- FALSE
  check$embedded_reason <- "estimated memory exceeds safe budget"
  write(check, "resource_check.csv")
  write(read("00_probe.csv"), "00_probe.csv")
  write(read("source_manifest.csv"), "source_manifest.csv")
  manifest <- read("run_manifest.csv")
  manifest <- rbind(manifest, data.frame(key = "schedule_filter", value = filter))
  write(manifest, "run_manifest.csv")
  stage
}

validate_stage <- function(stage) {
  bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(bench_root_supplement, "benchmark_cli.R"), "validate", stage),
    stdout = TRUE, stderr = TRUE,
    env = c(paste0("BENCH_ROOT=", bench_root_supplement), "BENCH_PROFILE=scale")
  )
}

test_that("a filtered supplement validates against its own schedule", {
  testthat::skip_if_not(dir.exists(published_scale), "published Scale run not present")
  root <- tempfile("supplement-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  output <- validate_stage(supplement_stage(root))
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "validated 5 exports and 10 access processes")
})

test_that("a full study must still fetch exactly its scheduled sources", {
  testthat::skip_if_not(dir.exists(published_scale), "published Scale run not present")
  root <- tempfile("supplement-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  # Without the recorded filter, the extra mouse source is an inconsistency.
  output <- suppressWarnings(validate_stage(supplement_stage(root, filter = "")))
  expect_identical(attr(output, "status"), 1L)
  expect_match(paste(output, collapse = "\n"), "source manifest does not cover the scheduled sources")
})
