local_validation_api <- new.env(parent = globalenv())
sys.source(
  test_path("..", "..", "scripts", "run-local-validation.R"),
  envir = local_validation_api
)

test_that("local validation arguments have safe bounded defaults", {
  parsed <- local_validation_api$local_validation_parse_args(character())

  expect_identical(parsed$mode, "full")
  expect_identical(parsed$logic_workers, 3L)
  expect_identical(parsed$browser_workers, 2L)
  expect_false(parsed$dry_run)
  expect_true(nzchar(parsed$output_dir))

  explicit <- local_validation_api$local_validation_parse_args(c(
    "--mode",
    "tests",
    "--logic-workers",
    "4",
    "--browser-workers",
    "1",
    "--output-dir",
    "validation-logs",
    "--dry-run"
  ))
  expect_identical(explicit$mode, "tests")
  expect_identical(explicit$logic_workers, 4L)
  expect_identical(explicit$browser_workers, 1L)
  expect_identical(explicit$output_dir, "validation-logs")
  expect_true(explicit$dry_run)
})

test_that("local validation arguments reject unsafe concurrency", {
  expect_error(
    local_validation_api$local_validation_parse_args(c("--mode", "quick")),
    "mode"
  )
  expect_error(
    local_validation_api$local_validation_parse_args(
      c("--logic-workers", "5")
    ),
    "logic-workers"
  )
  expect_error(
    local_validation_api$local_validation_parse_args(
      c("--browser-workers", "0")
    ),
    "browser-workers"
  )
  expect_error(
    local_validation_api$local_validation_parse_args(c("--unknown")),
    "Unknown"
  )
})

test_that("test schedule preserves isolation and bounded parallelism", {
  output_dir <- withr::local_tempdir()
  schedule <- local_validation_api$local_validation_schedule(
    logic_workers = 3L,
    browser_workers = 2L,
    mode = "tests",
    output_dir = output_dir
  )

  expect_identical(
    unique(schedule$phase),
    c("logic", "process-sensitive", "browser")
  )
  expect_identical(
    vapply(
      c("logic", "process-sensitive", "browser"),
      function(phase) sum(schedule$phase == phase),
      integer(1)
    ),
    c(logic = 4L, `process-sensitive` = 1L, browser = 6L)
  )
  expect_true(all(schedule$cap[schedule$phase == "logic"] == 3L))
  expect_identical(
    schedule$cap[schedule$phase == "process-sensitive"],
    1L
  )
  expect_true(all(schedule$cap[schedule$phase == "browser"] == 2L))
  expect_true(all(grepl(
    "--strategy weighted",
    schedule$command[schedule$phase %in% c("logic", "browser")],
    fixed = TRUE
  )))
  expect_false(grepl(
    "--strategy weighted",
    schedule$command[schedule$phase == "process-sensitive"],
    fixed = TRUE
  ))
  browser_artifacts <- schedule$artifact_dir[schedule$phase == "browser"]
  expect_length(unique(browser_artifacts), 6L)
  expect_true(all(startsWith(browser_artifacts, output_dir)))
})

test_that("full schedule appends check and pkgdown serially", {
  schedule <- local_validation_api$local_validation_schedule(
    logic_workers = 2L,
    browser_workers = 1L,
    mode = "full",
    output_dir = withr::local_tempdir()
  )

  expect_identical(
    unique(schedule$phase),
    c("logic", "process-sensitive", "browser", "check", "pkgdown")
  )
  expect_identical(schedule$cap[schedule$phase == "check"], 1L)
  expect_identical(schedule$cap[schedule$phase == "pkgdown"], 1L)
  expect_match(
    schedule$command[schedule$phase == "check"],
    "--no-tests",
    fixed = TRUE
  )
  expect_match(
    schedule$command[schedule$phase == "pkgdown"],
    "pkgdown-site",
    fixed = TRUE
  )
})

test_that("aggregate status remains nonzero after later successes", {
  expect_identical(
    local_validation_api$local_validation_exit_code(c(0L, 1L, 0L)),
    1L
  )
  expect_identical(
    local_validation_api$local_validation_exit_code(c(0L, 0L)),
    0L
  )
})
