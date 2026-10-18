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

published_supplement <- normalizePath(file.path(
  "..", "benchmarks", "results", "expression-backends", "supplements", "scale", "runs",
  "20261018T072255Z-30a6295c85f5-scale-supplement"
), mustWork = FALSE)

skip_unless_published_pair <- function() {
  testthat::skip_if_not(
    dir.exists(published_scale) && dir.exists(published_supplement),
    "published Scale study and supplement not present"
  )
}

copy_run <- function(from, root) {
  dir.create(root, recursive = TRUE)
  file.copy(from, root, recursive = TRUE)
  file.path(root, basename(from))
}

test_that("assembly fills only resource skips and keeps them on record", {
  skip_unless_published_pair()
  source(file.path(bench_root_supplement, "benchmark.R"), local = TRUE)
  got <- bench_assemble_supplement(published_scale, published_supplement, "fixture-collection")

  supplement_id <- "20261018T072255Z-30a6295c85f5-scale-supplement"
  filled <- got$exports$source == "human_pfc_hbcc" & got$exports$n_cells == 500000 &
    got$exports$backend == "embedded"
  expect_equal(nrow(got$exports), 300L)
  expect_true(all(got$exports$status == "OK"))
  expect_equal(sum(filled), 5L)
  expect_true(all(got$exports$source_run_id[filled] == supplement_id))
  expect_true(all(got$exports$run_id[filled] == supplement_id))
  expect_true(all(got$exports$source_run_id[!filled] == "20261016T224351Z-63d5337c3647-scale"))
  expect_equal(nrow(got$access), 600L)
  expect_equal(nrow(got$resource_skips), 5L)
  expect_true(all(startsWith(got$resource_skips$status, "SKIPPED(resource)")))
  expect_true(all(got$exports$collection_id == "fixture-collection"))
  # Assembly only relabels: every numerical value is the acquisition's own.
  supplement_rows <- utils::read.csv(file.path(published_supplement, "10_export.csv"))
  expect_equal(sort(got$exports$r_peak_mb[filled]), sort(supplement_rows$r_peak_mb))
})

test_that("assembly refuses to replace successful builds or mix hosts", {
  skip_unless_published_pair()
  source(file.path(bench_root_supplement, "benchmark.R"), local = TRUE)
  root <- tempfile("assembly-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  # Point the supplement at a tier the study measured successfully.
  moved <- copy_run(published_supplement, file.path(root, "moved"))
  rows <- utils::read.csv(file.path(moved, "10_export.csv"), stringsAsFactors = FALSE)
  rows$n_cells <- 300000
  utils::write.csv(rows, file.path(moved, "10_export.csv"), row.names = FALSE)
  expect_error(
    bench_assemble_supplement(published_scale, moved, "x"),
    "may only replace builds the study skipped"
  )

  # A supplement measured on another CPU cannot fill this study.
  other <- copy_run(published_supplement, file.path(root, "other"))
  manifest <- utils::read.csv(file.path(other, "run_manifest.csv"), stringsAsFactors = FALSE)
  manifest$value[manifest$key == "cpu"] <- "another host"
  utils::write.csv(manifest, file.path(other, "run_manifest.csv"), row.names = FALSE)
  expect_error(
    bench_assemble_supplement(published_scale, other, "x"),
    "not measured like the study: cpu"
  )
})
