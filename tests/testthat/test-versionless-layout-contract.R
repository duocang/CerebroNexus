test_that("the package, its directories and examples carry no product version", {
  inst_dir <- viewer_app_test_path()
  expect_true(dir.exists(file.path(inst_dir, "viewer")))
  expect_true(dir.exists(file.path(inst_dir, "extdata", "examples")))
  segments <- unlist(strsplit(list.dirs(inst_dir, full.names = FALSE), "/"))
  expect_false(any(grepl("^v[0-9]+(?:\\.[0-9]+)+$", segments)))

  exports <- getNamespaceExports("CerebroNexus")
  expect_true(all(c("launchCerebro", "Cerebro") %in% exports))
  expect_false(any(grepl("^(launchCerebroV[0-9]|Cerebro_v)", exports)))
  expect_s3_class(Cerebro$new(), "Cerebro")

  examples <- list.files(
    file.path(inst_dir, "extdata", "examples"),
    pattern = "[.]crb$",
    full.names = TRUE
  )
  expect_gt(length(examples), 0L)
  for (path in examples) {
    expect_s3_class(readCerebro(path), "Cerebro")
  }
})

test_that("published documentation does not describe a versioned viewer", {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  roots <- file.path(root, c("man", "vignettes"))
  testthat::skip_if_not(
    all(dir.exists(roots)),
    "current source tree is not present in the installed-package layout"
  )
  files <- list.files(
    roots,
    "[.](R|Rd|Rmd)$",
    recursive = TRUE,
    full.names = TRUE
  )
  ## The retired Cerebro v1.3 class may be named as a format convertCerebro()
  ## upgrades; the viewer itself is never versioned.
  legacy <- paste0(
    "v1[.]4|launchCerebroV[0-9]|",
    "(shiny|extdata)/v[0-9]|version *= *['\"]v[0-9]"
  )
  hits <- files[vapply(
    files,
    function(path) {
      any(grepl(legacy, readLines(path, warn = FALSE), ignore.case = TRUE))
    },
    logical(1)
  )]
  expect_identical(hits, character(), info = paste(hits, collapse = "\n"))
  expect_false(file.exists(file.path(root, "pkgdown", "_pkgdown.yml")))
})
