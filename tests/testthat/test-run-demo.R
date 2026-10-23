test_that("run-demo loads the worktree and avoids a busy default port", {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  skip_if_not(file.exists(launcher), "repository-only launcher")
  withr::local_envvar(c(
    CEREBRO_1M_DEMO_CRB = NA,
    CEREBRO_REN_DEMO_CRB = NA,
    CEREBRO_COSMX_DEMO_CRB = NA,
    CEREBRO_TCELL_DEMO_CRB = NA
  ))
  root <- normalizePath(testthat::test_path("..", ".."))
  keys <- c("one", "ren", "cosmx", "tcell")
  datasets <- stats::setNames(tempfile(paste0(keys, "-")), keys)
  file.create(datasets)
  withr::defer(unlink(datasets))
  variables <- c(
    one = "CEREBRO_1M_DEMO_CRB",
    ren = "CEREBRO_REN_DEMO_CRB",
    cosmx = "CEREBRO_COSMX_DEMO_CRB",
    tcell = "CEREBRO_TCELL_DEMO_CRB"
  )
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
  ## Options and the data sets they prepare. Without a selecting option the
  ## 1M mouse brain and Ren are prepared, as before.
  runs <- list(
    list(options = character(), expected = c("one", "ren")),
    list(options = "--ren-only", expected = "ren"),
    list(options = "--cosmx", expected = c("one", "ren", "cosmx")),
    list(options = c("--tcell", "--cosmx"), expected = keys),
    list(options = "--cosmx-only", expected = "cosmx"),
    list(options = "--tcell-only", expected = "tcell"),
    list(options = c("--tcell-only", "--cosmx-only"), expected = c("cosmx", "tcell")),
    list(options = c("--ren-only", "--tcell"), expected = c("ren", "tcell")),
    list(options = c("--ren-only", "--no-compressed"), expected = "ren")
  )
  for (run in runs) {
    prepared <- character()
    packs <- character()
    launched <- NULL
    scope <- new.env(parent = globalenv())
    scope$commandArgs <- function(trailingOnly) {
      if (!trailingOnly) {
        paste0("--file=", normalizePath(launcher))
      } else {
        c("--no-browser", run$options)
      }
    }
    scope$source <- function(file, local) {
      scope$.viewerRenVersion <- 2L
      scope$.viewerCosMxVersion <- 1L
      scope$.viewerTcellVersion <- 1L
      scope$prepareViewer1mBenchmarkData <- function() {
        prepared <<- c(prepared, "one")
        datasets[["one"]]
      }
      scope$prepareViewerRenDemoData <- function() {
        prepared <<- c(prepared, "ren")
        datasets[["ren"]]
      }
      scope$prepareViewerCosMxDemoData <- function() {
        prepared <<- c(prepared, "cosmx")
        datasets[["cosmx"]]
      }
      scope$prepareViewerTcellDemoData <- function() {
        prepared <<- c(prepared, "tcell")
        datasets[["tcell"]]
      }
      ## The launcher builds a Viewer Pack for each demo data set it prepares,
      ## and for the compressed twin it makes of it.
      scope$prepareDemoViewerPack <- function(crb) {
        packs <<- c(packs, basename(crb))
        invisible(crb)
      }
      scope$prepareCompressedDemoVariant <- function(crb) {
        paste0(crb, "_compressed")
      }
    }
    sys.source(launcher, scope)
    label <- paste(c("run-demo", run$options), collapse = " ")
    expect_identical(loaded, root, label = label)
    expect_identical(prepared, run$expected, label = label)
    expected_packs <- unlist(lapply(
      basename(datasets[run$expected]),
      function(name) {
        if ("--no-compressed" %in% run$options) {
          name
        } else {
          c(name, paste0(name, "_compressed"))
        }
      }
    ), use.names = FALSE)
    expect_identical(packs, expected_packs, label = label)
    for (name in keys) {
      expect_identical(
        Sys.getenv(variables[[name]]),
        if (name %in% run$expected) normalizePath(datasets[[name]]) else "",
        label = paste(label, variables[[name]])
      )
    }
    expect_identical(
      launched,
      list(
        appDir = file.path(root, "inst"),
        host = "127.0.0.1",
        port = 18765L,
        browser = FALSE
      ),
      label = label
    )
  }
  expect_identical(ports, 2L * length(runs))
})

test_that("run-demo rejects an unknown option", {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  skip_if_not(file.exists(launcher), "repository-only launcher")
  scope <- new.env(parent = globalenv())
  scope$commandArgs <- function(trailingOnly) {
    if (!trailingOnly) {
      paste0("--file=", normalizePath(launcher))
    } else {
      "--everything"
    }
  }
  expect_error(
    sys.source(launcher, scope),
    "Supported options: --ren-only, --cosmx-only, --tcell-only, --cosmx, --tcell",
    fixed = TRUE
  )
})

test_that("demo preparation reuses valid packs and rebuilds missing or stale first frames", {
  helper <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_viewer_pack.R"
  )
  skip_if_not(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, env)
  result <- list(valid = FALSE)
  builds <- 0L
  testthat::local_mocked_bindings(
    validateViewerPack = function(file, full) {
      expect_true(full)
      result
    },
    buildViewerPack = function(file, viewer_binary, overwrite) {
      expect_identical(file, "fixture.crb")
      expect_identical(viewer_binary, "always")
      expect_true(overwrite)
      builds <<- builds + 1L
      "fixture.viewer"
    },
    .package = "CerebroNexus"
  )
  expect_message(
    env$prepareDemoViewerPack("fixture.crb"),
    "Preparing Viewer Pack"
  )
  expect_identical(builds, 1L)
  result <- list(
    valid = TRUE,
    path = "fixture.viewer",
    manifest = list(
      hla_chains = c("TRA", "TRB"),
      assets = data.frame(
        path = c("hla_tcr/TRA.first.qs2", "hla_tcr/TRB.first.qs2")
      )
    )
  )
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 1L)
  result$manifest$assets <- result$manifest$assets[1L, , drop = FALSE]
  expect_message(
    env$prepareDemoViewerPack("fixture.crb"),
    "Preparing Viewer Pack"
  )
  expect_identical(builds, 2L)
  result$manifest$hla_chains <- character()
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 2L)
})
