bench_resources <- file.path("..", "benchmarks", "expression-backends", "scripts", "benchmark.R")

skip_unless_bench_resources <- function() {
  testthat::skip_if_not(
    file.exists(bench_resources),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("streaming tiers remain runnable when embedded exceeds the budget", {
  skip_unless_bench_resources()
  root <- normalizePath(dirname(bench_resources))
  stage <- tempfile("scale-resource-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  paths <- file.path(stage, c("inventory.csv", "plan.csv", "manifest.csv", "resource_check.csv"))
  write.csv(data.frame(source = "human", nnz_per_cell = 4112,
    nnz = 4112 * 1486324, n_cells = 1486324, source_bytes = 14.2e9), paths[1], row.names = FALSE)
  write.csv(data.frame(source = "human", n_cells = c(100000, 500000)), paths[2], row.names = FALSE)
  write.csv(data.frame(key = c("memory_mb", "r_vector_limit_mb", "memory_available_bytes"),
    value = c("112640", "Inf", as.character(90 * 2^30))), paths[3], row.names = FALSE)
  output <- bench_test_system2(file.path(R.home("bin"), "Rscript"),
    c(file.path(root, "benchmark_cli.R"), "full-resources", paths),
    stdout = TRUE, stderr = TRUE,
    env = c(paste0("BENCH_ROOT=", root), "BENCH_PROFILE=scale", "BENCH_FREE_DISK_BYTES=200000000000"))
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  got <- read.csv(paths[4])
  expect_true(all(got$safe))
  expect_identical(got$embedded_safe, c(TRUE, FALSE))
  expect_match(got$embedded_reason[2], "memory")
  expect_equal(got$memory_budget_mb, rep(90 * 1024 * .99, 2), tolerance = 1)
})

test_that("Scale budgets available RAM and the tightest cgroup ancestor", {
  skip_unless_bench_resources()
  source(bench_resources, local = TRUE)
  root <- tempfile("memory-fixture-")
  dir.create(file.path(root, "cg", "parent", "child"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(c("MemTotal: 100000 kB", "MemAvailable: 80000 kB"), file.path(root, "meminfo"))
  writeLines("0::/parent/child", file.path(root, "cgroup"))
  writeLines("max", file.path(root, "cg", "parent", "child", "memory.max"))
  writeLines("50000000", file.path(root, "cg", "parent", "memory.max"))
  writeLines("10000000", file.path(root, "cg", "parent", "memory.current"))
  available <- bench_available_memory_bytes(file.path(root, "meminfo"), file.path(root, "cgroup"), file.path(root, "cg"))
  expect_equal(available, 40000000)
  expect_equal(bench_scale_memory_budget(available), 39600000)
  expect_equal(bench_scale_memory_budget(available, 10), 10 * 2^20)
  expect_error(bench_scale_memory_budget(NA_real_), "invalid")
  writeLines("50000000", file.path(root, "cg", "parent", "memory.current"))
  expect_error(bench_available_memory_bytes(file.path(root, "meminfo"), file.path(root, "cgroup"), file.path(root, "cg")), "no available")
})

test_that("embedded preflight skips before reading or materialising source data", {
  skip_unless_bench_resources()
  skip_if_not_installed("CerebroNexus")
  root <- normalizePath(dirname(bench_resources))
  stage <- tempfile("embedded-skip-")
  dir.create(file.path(stage, "result"), recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  assessment <- data.frame(source = "human_pfc", n_cells = 500000,
    estimated_nnz = 2e9, memory_budget_mb = 10000,
    embedded_safe = FALSE, embedded_reason = "estimated memory exceeds safe budget")
  # Use a real configured source name, but deliberately provide no source file.
  source(bench_resources, local = TRUE)
  assessment$source <- names(BENCH_SOURCES)[1]
  output_csv <- file.path(stage, "result", "exports.csv")
  plan_path <- file.path(stage, "plan.rds")
  for (preflight in c(TRUE, FALSE)) {
    assessment$embedded_safe <- !preflight
    write.csv(assessment, file.path(stage, "result", "resource_check.csv"), row.names = FALSE)
    saveRDS(list(nnz = 2e9, query_plan_fingerprint = "dense-prefix"), plan_path)
    output <- bench_test_system2(file.path(R.home("bin"), "Rscript"),
      c(file.path(root, "benchmark_cli.R"), "export", assessment$source,
        "500000", "embedded", "1", "1", stage, output_csv, plan_path),
      stdout = TRUE, stderr = TRUE,
      env = c(paste0("BENCH_ROOT=", root), "BENCH_PROFILE=scale", "BENCH_RUN_ID=fixture"))
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
    got <- tail(read.csv(output_csv), 1L)
    expect_match(got$status, "SKIPPED(resource):", fixed = TRUE)
    expect_true(is.na(got$read_secs))
    expect_true(is.na(got$export_secs))
    expect_match(got$status, if (preflight) "preflight:" else "source-tier estimate")
  }
})

test_that("a Scale embedded build killed at the memory cap is a resource skip", {
  skip_unless_bench_resources()
  skip_if_not_installed("CerebroNexus")
  root <- normalizePath(dirname(bench_resources))
  stage <- tempfile("embedded-killed-")
  dir.create(file.path(stage, "result"), recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  source(bench_resources, local = TRUE)
  src <- names(BENCH_SOURCES)[1]
  # The assessment allowed the build; only the observed kill makes it a skip.
  write.csv(data.frame(source = src, n_cells = 500000, estimated_nnz = 1e9,
    memory_budget_mb = 1e6, embedded_safe = TRUE, embedded_reason = "safe"),
    file.path(stage, "result", "resource_check.csv"), row.names = FALSE)
  plan_path <- file.path(stage, "plan.rds")
  saveRDS(list(nnz = 2e9, query_plan_fingerprint = "dense-prefix"), plan_path)
  output_csv <- file.path(stage, "result", "exports.csv")
  output <- bench_test_system2(file.path(R.home("bin"), "Rscript"),
    c(file.path(root, "benchmark_cli.R"), "export", src,
      "500000", "embedded", "2", "7", stage, output_csv, plan_path),
    stdout = TRUE, stderr = TRUE,
    env = c(paste0("BENCH_ROOT=", root), "BENCH_PROFILE=scale", "BENCH_RUN_ID=fixture",
      "BENCH_EXPORT_OOM_KILLED=killed at the Scale memory cap (cgroup oom_kill, exit 137)"))
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  got <- read.csv(output_csv)
  expect_equal(nrow(got), 1L)
  expect_identical(got$status,
    "SKIPPED(resource): killed at the Scale memory cap (cgroup oom_kill, exit 137)")
  expect_identical(c(got$export_repeat, got$order_position), c(2L, 7L))
  expect_equal(got$nnz, 2e9)
  expect_identical(got$query_plan_fingerprint, "dense-prefix")
  expect_true(is.na(got$read_secs))
})

test_that("resource planning explains safe and unsafe memory tiers", {
  skip_unless_bench_resources()
  source(bench_resources, local = TRUE)
  inventory <- data.frame(
    source = c("mouse", "human"),
    nnz_per_cell = c(2010, 4112),
    source_bytes = c(4.2e9, 14.2e9)
  )
  plan <- data.frame(
    source = c("mouse", "mouse", "human", "human"),
    n_cells = c(150e3, 400e3, 50e3, 150e3)
  )
  got <- bench_assess_resources(
    inventory,
    plan,
    memory_mb = 32768,
    vector_limit_mb = 32768,
    free_disk_bytes = 100e9
  )

  expect_true(got$safe[got$source == "mouse" & got$n_cells == 150e3])
  expect_false(got$safe[got$source == "mouse" & got$n_cells == 400e3])
  expect_true(got$safe[got$source == "human" & got$n_cells == 50e3])
  expect_false(got$safe[got$source == "human" & got$n_cells == 150e3])
  expect_match(
    got$reason[got$source == "human" & got$n_cells == 150e3],
    "memory"
  )
})

test_that("resource planning checks disk and sparse-index limits", {
  skip_unless_bench_resources()
  source(bench_resources, local = TRUE)
  inventory <- data.frame(
    source = "fixture",
    nnz_per_cell = 5000,
    source_bytes = 20e9
  )
  plan <- data.frame(source = "fixture", n_cells = c(100e3, 500e3))

  disk <- bench_assess_resources(
    inventory,
    plan[1, , drop = FALSE],
    memory_mb = 131072,
    vector_limit_mb = 131072,
    free_disk_bytes = 10e9
  )
  expect_false(disk$safe)
  expect_match(disk$reason, "disk")

  index <- bench_assess_resources(
    inventory,
    plan[2, , drop = FALSE],
    memory_mb = 262144,
    vector_limit_mb = 262144,
    free_disk_bytes = 100e9
  )
  expect_false(index$safe)
  expect_match(index$reason, "32-bit sparse index")
})

test_that("unsafe plans fail unless explicitly overridden", {
  skip_unless_bench_resources()
  source(bench_resources, local = TRUE)
  assessment <- data.frame(
    source = "fixture",
    n_cells = 1,
    safe = FALSE,
    reason = "estimated memory exceeds safe budget"
  )
  expect_error(bench_require_safe_plan(assessment), "unsafe benchmark plan")
  expect_true(bench_require_safe_plan(assessment, allow_unsafe = TRUE))
})

test_that("resource checker rejects unsafe plans before a run", {
  skip_unless_bench_resources()
  bench_root <- normalizePath(file.path("..", "benchmarks", "expression-backends", "scripts"))
  stage <- tempfile("bench-resource-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  inventory_path <- file.path(stage, "data_inventory.csv")
  plan_path <- file.path(stage, "run_plan.csv")
  manifest_path <- file.path(stage, "run_manifest.csv")
  output_path <- file.path(stage, "resource_check.csv")
  utils::write.csv(
    data.frame(
      source = "fixture",
      nnz_per_cell = 5000,
      source_bytes = 1e9
    ),
    inventory_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(source = "fixture", n_cells = 500e3),
    plan_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      key = c("memory_mb", "r_vector_limit_mb"),
      value = c("32768", "32768")
    ),
    manifest_path,
    row.names = FALSE
  )
  output <- suppressWarnings(bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "benchmark_cli.R"),
      "resources",
      inventory_path,
      plan_path,
      manifest_path,
      output_path
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("BENCH_ROOT=", bench_root),
      "BENCH_FREE_DISK_BYTES=100000000000"
    )
  ))
  expect_false(is.null(attr(output, "status")))
  expect_true(file.exists(output_path))
  expect_match(paste(output, collapse = "\n"), "unsafe benchmark plan")
})

test_that("resource checker accepts an unlimited R vector heap", {
  skip_unless_bench_resources()
  bench_root <- normalizePath(file.path("..", "benchmarks", "expression-backends", "scripts"))
  stage <- tempfile("bench-resource-unlimited-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  inventory_path <- file.path(stage, "data_inventory.csv")
  plan_path <- file.path(stage, "run_plan.csv")
  manifest_path <- file.path(stage, "run_manifest.csv")
  output_path <- file.path(stage, "resource_check.csv")
  utils::write.csv(
    data.frame(
      source = "fixture",
      nnz_per_cell = 100,
      source_bytes = 1e6
    ),
    inventory_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(source = "fixture", n_cells = 1000),
    plan_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      key = c("memory_mb", "r_vector_limit_mb"),
      value = c("32768", "Inf")
    ),
    manifest_path,
    row.names = FALSE
  )
  output <- suppressWarnings(bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "benchmark_cli.R"),
      "resources",
      inventory_path,
      plan_path,
      manifest_path,
      output_path
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("BENCH_ROOT=", bench_root),
      "BENCH_FREE_DISK_BYTES=100000000000"
    )
  ))
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true(file.exists(output_path))
})
