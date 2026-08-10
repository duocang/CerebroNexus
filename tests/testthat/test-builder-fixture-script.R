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

test_that("the installed Builder runtime does not manufacture fixtures", {
  io_path <- builder_profile_inst_path("builder", "io.R")
  io_source <- readLines(io_path, warn = FALSE)
  generator_definitions <- grep(
    paste0(
      "^(?:\\.builder_fixture_[[:alnum:]_]+|",
      "builder_(?:make|write)_permanent_fixture)\\s*<-\\s*function"
    ),
    io_source,
    value = TRUE,
    perl = TRUE
  )

  expect_identical(generator_definitions, character())
})
