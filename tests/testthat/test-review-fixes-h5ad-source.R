## Reader details found in review ---------------------------------------------

test_that("a float tissue image is not darkened, an integer one is scaled", {
  half <- array(0.5, dim = c(3, 4, 5))
  half[1, 1, 1] <- 1.000001
  testthat::local_mocked_bindings(
    .h5adRead = function(ctx, path) half,
    .package = "CerebroNexus"
  )
  scaled <- CerebroNexus:::.h5adReadImageArray(NULL, "spatial/x/hires")
  expect_equal(median(scaled), 0.5)

  bytes <- array(128L, dim = c(3, 4, 5))
  bytes[1, 1, 1] <- 255L
  testthat::local_mocked_bindings(
    .h5adRead = function(ctx, path) bytes,
    .package = "CerebroNexus"
  )
  scaled <- CerebroNexus:::.h5adReadImageArray(NULL, "spatial/x/hires")
  expect_equal(median(scaled), 128 / 255)
  expect_equal(max(scaled), 1)
})

test_that("columns in an encoding that is not read are counted in a message", {
  testthat::local_mocked_bindings(
    .h5adHas = function(ctx, path) !grepl("__categories", path),
    .h5adIsGroup = function(ctx, path) TRUE,
    .h5adAttrs = function(ctx, path) {
      list(`_index` = "_index", `column-order` = c("a", "bad", "short"))
    },
    .h5adChildren = function(ctx, path) c("_index", "a", "bad", "short"),
    .h5adReadElement = function(ctx, path) {
      switch(
        basename(path),
        `_index` = c("c1", "c2", "c3"),
        a = 1:3,
        short = 1:2,
        stop("unsupported encoding")
      )
    },
    .package = "CerebroNexus"
  )
  expect_message(
    table <- CerebroNexus:::.h5adReadDataFrame(NULL, "obs"),
    "Skipped 2 column\\(s\\) of `obs`.*bad, short"
  )
  expect_identical(names(table$columns), "a")
  noted <- testthat::capture_messages(
    CerebroNexus:::.h5adReadDataFrame(NULL, "obs", verbose = TRUE)
  )
  expect_false(any(grepl("^Skipped 2", noted)))
})

test_that("CEREBRO_PACKAGE_SOURCE is used only for a CerebroNexus checkout", {
  checkout <- function(package, with_inst = TRUE) {
    dir <- withr::local_tempdir(.local_envir = parent.frame())
    writeLines(
      paste0("Package: ", package, "\nVersion: 9.9.9"),
      file.path(dir, "DESCRIPTION")
    )
    dir.create(file.path(dir, "R"))
    writeLines("f <- function() 1", file.path(dir, "R", "f.R"))
    if (with_inst) {
      dir.create(file.path(dir, "inst"))
    }
    dir
  }
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = "")
  expect_null(CerebroNexus:::.cerebroSourceRoot())

  real <- checkout("CerebroNexus")
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = real)
  expect_identical(
    CerebroNexus:::.cerebroSourceRoot(),
    normalizePath(real, winslash = "/")
  )
  expect_identical(CerebroNexus:::.cerebroRuntimeVersion(), "9.9.9")

  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = checkout("somethingElse"))
  expect_null(CerebroNexus:::.cerebroSourceRoot())
  expect_false(identical(CerebroNexus:::.cerebroRuntimeVersion(), "9.9.9"))

  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = checkout("CerebroNexus", FALSE))
  expect_null(CerebroNexus:::.cerebroSourceRoot())
})
