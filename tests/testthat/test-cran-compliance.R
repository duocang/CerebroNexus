test_that("shipped R code does not use private package APIs", {
  package_root <- normalizePath(testthat::test_path("..", ".."))
  roots <- file.path(package_root, c("R", file.path("inst", "shiny")))
  files <- unlist(lapply(
    roots,
    list.files,
    pattern = "\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  ))
  source <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)

  expect_false(any(grepl(":::", source, fixed = TRUE)))
})

test_that("legacy launchers do not default exports to the desktop", {
  package_root <- normalizePath(testthat::test_path("..", ".."))
  files <- list.files(
    file.path(package_root, "R"),
    pattern = "^launchCerebroV1[.]",
    full.names = TRUE
  )
  source <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)

  expect_false(any(grepl("Desktop", source, fixed = TRUE)))
})

test_that("test setup does not override CRAN detection", {
  setup <- readLines(testthat::test_path("setup.R"), warn = FALSE)

  expect_false(any(grepl("Sys.setenv(NOT_CRAN", setup, fixed = TRUE)))
})

test_that("browser test files are skipped on CRAN", {
  files <- list.files(
    testthat::test_path(),
    pattern = "^test-app-.*[.]R$",
    full.names = TRUE
  )
  browser_files <- files[vapply(
    files,
    function(path) {
      any(grepl(
        "library(shinytest2)",
        readLines(path, warn = FALSE),
        fixed = TRUE
      ))
    },
    logical(1)
  )]

  has_skip <- vapply(
    browser_files,
    function(path) {
      any(grepl("skip_on_cran()", readLines(path, warn = FALSE), fixed = TRUE))
    },
    logical(1)
  )
  expect_true(
    all(has_skip),
    info = paste(basename(browser_files[!has_skip]), collapse = ", ")
  )
})

test_that("DESCRIPTION uses a repository-backed optional BPCells dependency", {
  description <- packageDescription("CerebroNexus")

  expect_null(description$Remotes)
  expect_match(
    description$Additional_repositories,
    "https://bnprks[.]r-universe[.]dev"
  )
})
