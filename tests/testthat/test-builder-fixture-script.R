builder_fixture_script_layout <- function() {
  builder_dir <- normalizePath(builder_profile_inst_path("builder"))
  repo <- dirname(dirname(builder_dir))
  list(
    repo = repo,
    script = file.path(repo, "data-raw", "build_builder_fixtures.R"),
    committed = file.path(builder_dir, "fixtures")
  )
}

builder_run_fixture_script <- function(layout, output, wd = layout$repo) {
  processx::run(
    file.path(R.home("bin"), "Rscript"),
    c(layout$script, output),
    wd = wd,
    error_on_status = FALSE,
    echo = FALSE
  )
}

test_that("the fixture generator refuses a non-repository working directory", {
  layout <- builder_fixture_script_layout()
  skip_if_not(
    file.exists(layout$script),
    "fixture generator source not present (installed-package layout)"
  )
  outside <- withr::local_tempdir(pattern = "builder-wrong-cwd-")
  result <- builder_run_fixture_script(layout, outside, wd = outside)
  expect_false(identical(result$status, 0L))
  expect_match(
    paste(result$stdout, result$stderr),
    "repository root",
    fixed = TRUE
  )
})

test_that("the Builder runtime does not manufacture fixtures", {
  io_path <- builder_profile_inst_path("builder", "io.R")
  runtime <- new.env(parent = baseenv())
  sys.source(io_path, envir = runtime)
  generator_symbols <- grep(
    "^(?:[.]builder_fixture_|builder_(?:make|write)_permanent_fixture$)",
    ls(runtime, all.names = TRUE),
    value = TRUE,
    perl = TRUE
  )

  expect_identical(generator_symbols, character())
})

test_that("the fixture script exactly reproduces committed gallery inputs", {
  layout <- builder_fixture_script_layout()
  skip_if_not(
    file.exists(layout$script),
    "fixture generator source not present (installed-package layout)"
  )
  expected_names <- sort(c(
    "all_content.rds",
    "section_a_1_he.png",
    "section_a_1_dapi.png",
    "section_a_2_he.png",
    "section_a_2_dapi.png",
    "section_b_1_he.png",
    "section_b_1_if.png",
    "section_b_1_pas.png"
  ))
  first_dir <- withr::local_tempdir(pattern = "builder-fixtures-first-")
  second_dir <- withr::local_tempdir(pattern = "builder-fixtures-second-")

  first <- builder_run_fixture_script(layout, first_dir)
  second <- builder_run_fixture_script(layout, second_dir)
  expect_identical(first$status, 0L, info = first$stderr)
  expect_identical(second$status, 0L, info = second$stderr)
  expect_identical(sort(list.files(first_dir)), expected_names)
  expect_identical(sort(list.files(second_dir)), expected_names)
  expect_identical(sort(list.files(layout$committed)), expected_names)

  read_bytes <- function(directory) {
    lapply(file.path(directory, expected_names), function(path) {
      readBin(path, what = "raw", n = as.integer(file.info(path)$size))
    })
  }
  generated <- read_bytes(first_dir)
  expect_identical(generated, read_bytes(second_dir))
  committed <- read_bytes(layout$committed)
  rds_index <- match("all_content.rds", expected_names)
  expect_identical(generated[-rds_index], committed[-rds_index])
  expect_equal(
    readRDS(file.path(first_dir, "all_content.rds")),
    readRDS(file.path(layout$committed, "all_content.rds")),
    tolerance = 0
  )
})

test_that("the serialized fixture avoids compressor-dependent bytes", {
  layout <- builder_fixture_script_layout()
  skip_if_not(
    file.exists(layout$script),
    "fixture generator source not present (installed-package layout)"
  )
  script <- paste(readLines(layout$script, warn = FALSE), collapse = "\n")

  expect_match(
    script,
    "saveRDS(object, fixture_path, version = 3L, compress = FALSE)",
    fixed = TRUE
  )
})
