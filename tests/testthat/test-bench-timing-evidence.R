test_that("publication requires complete, valid raw timing evidence", {
  protocol <- file.path(
    "..",
    "benchmarks",
    "expression-backends",
    "scripts",
    "benchmark.R"
  )
  skip_if_not(file.exists(protocol))
  source(protocol, local = TRUE)
  access <- data.frame(
    run_id = "new",
    source = "fixture",
    n_cells = 4,
    backend = "h5",
    export_repeat = 1L,
    access_repeat = 1L,
    status = "OK",
    timing_contract = "monotonic-v1",
    n_hot = 2L,
    startup_secs = 0.1,
    first_query_secs = 0.1,
    hot_p50_secs = 0.1,
    hot_p95_secs = 0.1,
    block_secs = 0.1,
    subset_row_secs = 0.1,
    subset_block_secs = 0.1
  )
  timings <- access[
    rep(1, 7),
    c(
      "run_id",
      "source",
      "n_cells",
      "backend",
      "export_repeat",
      "access_repeat"
    )
  ]
  timings$sequence <- 1:7
  timings$phase <- c(
    "startup",
    "first",
    "hot",
    "hot",
    "block",
    "subset_row",
    "subset_block"
  )
  timings$clock <- "CLOCK_MONOTONIC"
  timings$resolution_secs <- 1e-9
  timings$monotonic_start <- 1:7
  timings$monotonic_end <- 1:7 + 0.1
  timings$wall_start <- 100:106
  timings$wall_end <- 100:106 + 0.1
  timings$seconds <- 0.1
  exports <- data.frame(
    status = "OK",
    read_secs = 0.1,
    seurat_secs = 0,
    export_secs = 0.1,
    shell_secs = NA_real_,
    serialize_secs = NA_real_
  )
  expect_silent(bench_validate_timing_evidence(exports, access, timings))
  stage <- withr::local_tempdir()
  target <- withr::local_tempdir()
  writeLines("previous", file.path(target, "CURRENT"))
  bad_access <- access
  bad_access$subset_block_secs <- -1
  utils::write.csv(
    exports,
    file.path(stage, "10_export.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    bad_access,
    file.path(stage, "20_access.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    timings,
    file.path(stage, "21_access_timings.csv"),
    row.names = FALSE
  )
  expect_error(
    bench_publish_result(stage, target, "invalid"),
    "invalid elapsed"
  )
  expect_identical(readLines(file.path(target, "CURRENT")), "previous")
  bad <- access
  bad$subset_block_secs <- -1
  expect_error(
    bench_validate_timing_evidence(exports, bad, timings),
    "invalid elapsed"
  )
  expect_error(
    bench_validate_timing_evidence(exports, access, timings[-3, ]),
    "incomplete"
  )
  bad <- timings
  bad$seconds[3] <- Inf
  expect_error(
    bench_validate_timing_evidence(exports, access, bad),
    "invalid elapsed"
  )
  bad <- timings
  bad$seconds[3] <- 0.2
  expect_error(
    bench_validate_timing_evidence(exports, access, bad),
    "clock endpoints"
  )
  bad <- access
  bad$hot_p50_secs <- 0.2
  expect_error(
    bench_validate_timing_evidence(exports, bad, timings),
    "summary differs"
  )
  bad <- access
  bad$timing_contract <- NULL
  expect_error(
    bench_validate_timing_evidence(exports, bad, timings),
    "monotonic-v1"
  )
})
