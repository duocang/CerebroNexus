test_that("run-demo loads the worktree and avoids a busy default port", {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  skip_if_not(file.exists(launcher), "repository-only launcher")
  withr::local_envvar(c(CEREBRO_1M_DEMO_CRB = NA, CEREBRO_REN_DEMO_CRB = NA))
  root <- normalizePath(testthat::test_path("..", ".."))
  datasets <- tempfile(c("one-", "ren-"))
  file.create(datasets)
  withr::defer(unlink(datasets))
  loaded <- NULL
  testthat::local_mocked_bindings(
    load_all = function(path, ...) loaded <<- path,
    .package = "pkgload"
  )
  ports <- 0L
  testthat::local_mocked_bindings(
    randomPort = function(...) {
      ports <<- ports + 1L
      if (length(list(...))) {
        stop("port in use")
      }
      18765L
    },
    .package = "httpuv"
  )
  launched <- NULL
  testthat::local_mocked_bindings(
    runApp = function(appDir, host, port, launch.browser) {
      launched <<- list(
        appDir = appDir,
        host = host,
        port = port,
        browser = launch.browser
      )
    },
    .package = "shiny"
  )
  for (ren_only in c(FALSE, TRUE)) {
    prepared <- character()
    scope <- new.env(parent = globalenv())
    scope$commandArgs <- function(trailingOnly) {
      if (!trailingOnly) {
        paste0("--file=", normalizePath(launcher))
      } else {
        c("--no-browser", if (ren_only) "--ren-only")
      }
    }
    scope$source <- function(file, local) {
      scope$.viewerRenVersion <- 2L
      scope$prepareViewer1mBenchmarkData <- function() {
        prepared <<- c(prepared, "one")
        datasets[[1]]
      }
      scope$prepareViewerRenDemoData <- function() {
        prepared <<- c(prepared, "ren")
        datasets[[2]]
      }
    }
    sys.source(launcher, scope)
    expect_identical(loaded, root)
    expect_identical(prepared, if (ren_only) "ren" else c("one", "ren"))
    expect_identical(
      Sys.getenv("CEREBRO_REN_DEMO_CRB"),
      normalizePath(datasets[[2]])
    )
    expect_identical(
      Sys.getenv("CEREBRO_1M_DEMO_CRB"),
      if (ren_only) "" else normalizePath(datasets[[1]])
    )
    expect_identical(
      launched,
      list(
        appDir = file.path(root, "inst"),
        host = "127.0.0.1",
        port = 18765L,
        browser = FALSE
      )
    )
  }
  expect_identical(ports, 4L)
})
