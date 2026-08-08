test_that("the fixture generator refuses a non-repository working directory", {
  builder_dir <- normalizePath(builder_profile_inst_path("builder"))
  repo <- dirname(dirname(builder_dir))
  script <- file.path(
    repo,
    "data-raw",
    "build_builder_fixtures.R"
  )
  skip_if_not(
    file.exists(script),
    "fixture generator source not present (installed-package layout)"
  )
  script <- normalizePath(script)
  outside <- withr::local_tempdir(pattern = "builder-wrong-cwd-")
  result <- processx::run(
    file.path(R.home("bin"), "Rscript"),
    script,
    wd = outside,
    error_on_status = FALSE,
    echo = FALSE
  )
  expect_false(identical(result$status, 0L))
  expect_match(
    paste(result$stdout, result$stderr),
    "repository root",
    fixed = TRUE
  )
})
