test_that("run-demo loads the worktree and avoids a busy default port", {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  skip_if_not(
    file.exists(launcher),
    "repository-only demo launcher is not installed with the package"
  )
  script <- paste(
    readLines(launcher, warn = FALSE),
    collapse = "\n"
  )

  expect_match(script, "pkgload::load_all\\([[:space:]]*repo_root")
  expect_match(script, "httpuv::randomPort", fixed = TRUE)
  expect_match(script, "port = port", fixed = TRUE)
  expect_match(script, "prepareDemoViewerPack(crb)", fixed = TRUE)
  expect_match(script, "prepareDemoViewerPack(ren_crb)", fixed = TRUE)
})

test_that("demo preparation reuses valid packs and rebuilds missing or stale first frames", {
  helper <- testthat::test_path("..", "bench", "prepare_viewer_pack.R")
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
  expect_message(env$prepareDemoViewerPack("fixture.crb"), "Preparing Viewer Pack")
  expect_identical(builds, 1L)
  result <- list(valid = TRUE, path = "fixture.viewer", manifest = list(
    hla_chains = c("TRA", "TRB"),
    assets = data.frame(path = c("hla_tcr/TRA.first.qs2", "hla_tcr/TRB.first.qs2"))
  ))
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 1L)
  result$manifest$assets <- result$manifest$assets[1L, , drop = FALSE]
  expect_message(env$prepareDemoViewerPack("fixture.crb"), "Preparing Viewer Pack")
  expect_identical(builds, 2L)
  result$manifest$hla_chains <- character()
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 2L)
})
