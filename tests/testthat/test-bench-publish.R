bench_root <- normalizePath(file.path("..", "benchmarks", "expression-backends", "scripts"), mustWork = FALSE)

skip_unless_bench_publish <- function() {
  testthat::skip_if_not(
    file.exists(file.path(bench_root, "benchmark_cli.R")),
    "benchmark tree not present (expected when checking a built package)"
  )
}

run_publisher <- function(stage, target, run_id, fail_at = "") {
  out <- tempfile("bench-publish-stdout-")
  err <- tempfile("bench-publish-stderr-")
  on.exit(unlink(c(out, err)), add = TRUE)
  status <- bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "benchmark_cli.R"),
      "publish",
      stage,
      target,
      run_id
    ),
    stdout = out,
    stderr = err,
    env = c(
      paste0("BENCH_ROOT=", bench_root),
      paste0("BENCH_PUBLISH_FAIL_AT=", fail_at)
    )
  )
  list(
    status = status,
    stdout = readLines(out, warn = FALSE),
    stderr = readLines(err, warn = FALSE)
  )
}

test_that("failed publish preserves the prior current result", {
  skip_unless_bench_publish()
  root <- tempfile("bench-results-")
  stage <- tempfile("bench-stage-")
  dir.create(file.path(root, "runs", "old-run"), recursive = TRUE)
  dir.create(stage)
  on.exit(unlink(c(root, stage), recursive = TRUE), add = TRUE)
  writeLines("old-run", file.path(root, "CURRENT"))
  writeLines("old evidence", file.path(root, "runs", "old-run", "summary.md"))
  writeLines("new evidence", file.path(stage, "summary.md"))

  failed <- run_publisher(stage, root, "new-run", fail_at = "before-pointer")
  expect_false(identical(failed$status, 0L))
  expect_equal(readLines(file.path(root, "CURRENT")), "old-run")
  expect_equal(
    readLines(file.path(root, "runs", "old-run", "summary.md")),
    "old evidence"
  )

  recovered <- run_publisher(stage, root, "new-run")
  expect_equal(
    recovered$status,
    0L,
    info = paste(recovered$stderr, collapse = "\n")
  )
  expect_equal(readLines(file.path(root, "CURRENT")), "new-run")
  expect_equal(
    readLines(file.path(root, "runs", "new-run", "summary.md")),
    "new evidence"
  )
  expect_true(dir.exists(file.path(root, "runs", "old-run")))
})

test_that("publisher rejects unsafe and conflicting run identities", {
  skip_unless_bench_publish()
  root <- tempfile("bench-results-")
  stage <- tempfile("bench-stage-")
  dir.create(root)
  dir.create(stage)
  on.exit(unlink(c(root, stage), recursive = TRUE), add = TRUE)
  writeLines("evidence", file.path(stage, "summary.md"))

  unsafe <- run_publisher(stage, root, "../escape")
  expect_false(identical(unsafe$status, 0L))
  expect_match(paste(unsafe$stderr, collapse = "\n"), "unsafe run id")

  first <- run_publisher(stage, root, "same-run")
  expect_equal(first$status, 0L)
  writeLines("different", file.path(stage, "summary.md"))
  conflict <- run_publisher(stage, root, "same-run")
  expect_false(identical(conflict$status, 0L))
  expect_match(
    paste(conflict$stderr, collapse = "\n"),
    "conflicting existing run"
  )
})

test_that("full and scale publish archive images and replace the latest image set", {
  skip_unless_bench_publish()
  for (study in c("full", "scale")) {
    root <- tempfile(paste0("bench-", study, "-"))
    stage <- tempfile("bench-image-stage-")
    dir.create(file.path(stage, "figures", "qa"), recursive = TRUE)
    on.exit(unlink(c(root, stage), recursive = TRUE), add = TRUE)
    writeLines("first", file.path(stage, "figures", "overview.png"))
    writeLines("vector", file.path(stage, "figures", "overview.pdf"))
    writeLines("private checks", file.path(stage, "figures", "qa", "checks.json"))
    writeLines("notes", file.path(stage, "figures", "README.md"))
    first <- run_publisher(stage, root, "run-1")
    expect_equal(first$status, 0L, info = paste(first$stderr, collapse = "\n"))
    expect_equal(list.files(file.path(root, "figures")), c("overview.pdf", "overview.png"))
    expect_equal(readLines(file.path(root, "CURRENT")), "run-1")
    archived <- file.path(root, "runs", "run-1", "figures", "overview.png")
    expect_equal(readLines(archived), "first")

    # Old image names must disappear from the latest set, but stay in history.
    unlink(file.path(stage, "figures", "overview.pdf"))
    writeLines("second", file.path(stage, "figures", "overview.png"))
    writeLines("new image", file.path(stage, "figures", "ceiling.png"))
    second <- run_publisher(stage, root, "run-2")
    expect_equal(second$status, 0L, info = paste(second$stderr, collapse = "\n"))
    expect_equal(readLines(file.path(root, "CURRENT")), "run-2")
    expect_equal(list.files(file.path(root, "figures")), c("ceiling.png", "overview.png"))
    expect_equal(readLines(archived), "first")
    for (name in c("overview.png", "ceiling.png")) {
      expect_equal(unname(tools::md5sum(file.path(root, "figures", name))),
                   unname(tools::md5sum(file.path(root, "runs", "run-2", "figures", name))))
    }
    expect_false(dir.exists(file.path(root, ".publish")))
  }
})

test_that("publication errors and interrupts restore both CURRENT and latest figures", {
  skip_unless_bench_publish()
  for (fail_at in c("before-pointer", "after-figures", "after-pointer", "interrupt-after-figures")) {
    root <- tempfile("bench-rollback-")
    stage <- tempfile("bench-image-stage-")
    dir.create(file.path(stage, "figures"), recursive = TRUE)
    on.exit(unlink(c(root, stage), recursive = TRUE), add = TRUE)
    writeLines("old image", file.path(stage, "figures", "overview.png"))
    expect_equal(run_publisher(stage, root, "old")$status, 0L)
    writeLines("new image", file.path(stage, "figures", "overview.png"))
    failed <- suppressWarnings(run_publisher(stage, root, "new", fail_at))
    expect_false(identical(failed$status, 0L), info = fail_at)
    expect_equal(readLines(file.path(root, "CURRENT")), "old", info = fail_at)
    expect_equal(readLines(file.path(root, "figures", "overview.png")), "old image", info = fail_at)
    expect_false(dir.exists(file.path(root, ".publish")), info = fail_at)
    expect_equal(run_publisher(stage, root, "new")$status, 0L)
    expect_equal(readLines(file.path(root, "CURRENT")), "new")
    expect_equal(readLines(file.path(root, "figures", "overview.png")), "new image")
  }
})

test_that("a killed publication is rolled back before the next attempt", {
  skip_unless_bench_publish()
  root <- tempfile("bench-recover-")
  stage <- tempfile("bench-image-stage-")
  dir.create(file.path(stage, "figures"), recursive = TRUE)
  on.exit(unlink(c(root, stage), recursive = TRUE), add = TRUE)
  writeLines("old", file.path(stage, "figures", "overview.png"))
  expect_equal(run_publisher(stage, root, "old")$status, 0L)
  writeLines("new", file.path(stage, "figures", "overview.png"))
  crashed <- run_publisher(stage, root, "new", "crash-after-figures")
  expect_equal(crashed$status, 99L)
  expect_true(dir.exists(file.path(root, ".publish")))
  # Stop the retry before activation, proving that it first recovered the old pair.
  retry <- run_publisher(stage, root, "new", "before-pointer")
  expect_false(identical(retry$status, 0L))
  expect_equal(readLines(file.path(root, "CURRENT")), "old")
  expect_equal(readLines(file.path(root, "figures", "overview.png")), "old")
  expect_false(dir.exists(file.path(root, ".publish")))
})

test_that("output checker requires raw evidence, figures, and checksums", {
  skip_unless_bench_publish()
  stage <- tempfile("bench-output-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  utils::write.csv(
    data.frame(key = "profile", value = "scale"),
    file.path(stage, "run_manifest.csv"),
    row.names = FALSE
  )
  checker <- file.path(bench_root, "benchmark_cli.R")

  missing <- suppressWarnings(bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(checker, "check", stage),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("BENCH_ROOT=", bench_root)
  ))
  expect_false(is.null(attr(missing, "status")))
  expect_match(paste(missing, collapse = "\n"), "missing staged output")

  writeLines("report", file.path(stage, "summary.md"))
  dir.create(file.path(stage, "figures"))
  writeLines(
    "png",
    file.path(
      stage,
      "figures",
      "expression_backend_benchmark_overview.png"
    )
  )
  writeLines(
    "png",
    file.path(
      stage,
      "figures",
      "expression_backend_benchmark_ceiling.png"
    )
  )
  for (name in c(
    "00_probe.csv",
    "05_schedule.csv",
    "10_export.csv",
    "20_access.csv",
    "crashes.csv",
    "query_panel.csv",
    "query_plan_manifest.csv",
    "resource_check.csv",
    "source_manifest.csv"
  )) {
    writeLines("evidence", file.path(stage, name))
  }
  dir.create(file.path(stage, "logs"))
  writeLines("local process log", file.path(stage, "logs", "run.log"))
  inventory <- bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(bench_root, "benchmark_cli.R"), "evidence", stage),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("BENCH_ROOT=", bench_root)
  )
  expect_null(
    attr(inventory, "status"),
    info = paste(inventory, collapse = "\n")
  )
  unlink(file.path(stage, "logs"), recursive = TRUE)
  complete <- bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(checker, "check", stage),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("BENCH_ROOT=", bench_root)
  )
  expect_null(attr(complete, "status"), info = paste(complete, collapse = "\n"))

  writeLines("tampered", file.path(stage, "10_export.csv"))
  tampered <- suppressWarnings(bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(checker, "check", stage),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("BENCH_ROOT=", bench_root)
  ))
  expect_false(is.null(attr(tampered, "status")))
  expect_match(paste(tampered, collapse = "\n"), "inventory does not match")
})
