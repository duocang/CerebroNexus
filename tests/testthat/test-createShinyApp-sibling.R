write_bundle_crb <- function(
  directory,
  name = "dataset.crb",
  backend = list(type = "embedded", location = NULL),
  legacy = FALSE
) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, name)
  payload <- new.env(parent = emptyenv())
  class(payload) <- c("Cerebro_v1.3", "R6")
  payload$marker <- name
  if (!legacy) {
    payload$getExpressionBackend <- base::local({
      value <- backend
      function() value
    })
  }
  saveRDS(payload, path)
  path
}

write_backend_artifact <- function(directory, backend, contents = "MATRIX") {
  path <- file.path(directory, backend$location)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (identical(backend$type, "bpcells")) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    writeLines(contents, file.path(path, "payload"))
  } else {
    writeLines(contents, path)
  }
  path
}

build_test_app <- function(cerebro_data, result_dir, ...) {
  createShinyApp(
    cerebro_data = cerebro_data,
    result_dir = result_dir,
    launch_browser = FALSE,
    verbose = FALSE,
    ...
  )
}

test_that("tagged H5 and BPCells backends are copied to their exact paths", {
  root <- withr::local_tempdir()
  backends <- list(
    list(type = "h5", location = "matrices/original-name.h5"),
    list(type = "bpcells", location = "matrices/expression.bpcells")
  )

  for (index in seq_along(backends)) {
    source <- file.path(root, paste0("source-", index))
    backend <- backends[[index]]
    crb <- write_bundle_crb(
      source,
      name = "renamed-dataset.crb",
      backend = backend
    )
    write_backend_artifact(source, backend)
    app <- file.path(root, paste0("app-", index))

    build_test_app(c("Dataset" = crb), app)

    target <- file.path(app, "data", backend$location)
    if (identical(backend$type, "bpcells")) {
      expect_true(dir.exists(target))
      expect_identical(
        readLines(file.path(target, "payload")),
        "MATRIX"
      )
    } else {
      expect_true(file.exists(target))
      expect_identical(readLines(target), "MATRIX")
    }
    expect_false(file.exists(file.path(app, "data", "renamed-dataset.h5")))
  }
})

test_that("backend locations must be portable relative paths", {
  root <- withr::local_tempdir()
  invalid <- c(
    "../matrix.h5",
    "./matrix.h5",
    "/tmp/matrix.h5",
    "C:/matrix.h5",
    "nested//matrix.h5",
    "nested\\matrix.h5",
    "matrix?.h5",
    "nested/a:b.h5",
    "NUL",
    "nested/COM1.bin",
    "nested/name.",
    "nested/name "
  )

  for (index in seq_along(invalid)) {
    source <- file.path(root, paste0("source-", index))
    crb <- write_bundle_crb(
      source,
      backend = list(type = "h5", location = invalid[[index]])
    )
    expect_error(
      build_test_app(
        c("Dataset" = crb),
        file.path(root, paste0("app-", index))
      ),
      "portable relative path"
    )
  }
})

test_that("malformed backend descriptors are rejected", {
  root <- withr::local_tempdir()
  broken <- new.env(parent = emptyenv())
  class(broken) <- c("Cerebro_v1.3", "R6")
  broken$getExpressionBackend <- "not a function"
  broken_path <- file.path(root, "broken.crb")
  saveRDS(broken, broken_path)
  inconsistent <- write_bundle_crb(
    root,
    "inconsistent.crb",
    list(type = "embedded", location = "matrix.h5")
  )

  expect_error(
    build_test_app(c("Broken" = broken_path), file.path(root, "broken-app")),
    "unsupported expression-backend descriptor"
  )
  expect_error(
    build_test_app(
      c("Inconsistent" = inconsistent),
      file.path(root, "inconsistent-app")
    ),
    "unsupported expression-backend descriptor"
  )
})

test_that("arbitrary RDS objects are not accepted as Cerebro data", {
  root <- withr::local_tempdir()
  path <- file.path(root, "number.rds")
  saveRDS(42, path)

  expect_error(
    build_test_app(c("Number" = path), file.path(root, "app")),
    "recognized Cerebro"
  )
  expect_false(dir.exists(file.path(root, "app")))
})

test_that("a missing tagged backend stops the build", {
  root <- withr::local_tempdir()
  backend <- list(type = "h5", location = "matrix.h5")
  crb <- write_bundle_crb(root, backend = backend)

  expect_error(
    build_test_app(c("Dataset" = crb), file.path(root, "app")),
    "matrix.h5"
  )
})

test_that("failed preflight leaves an existing deployment untouched", {
  root <- withr::local_tempdir()
  app <- file.path(root, "app")
  dir.create(app)
  sentinel <- file.path(app, "sentinel.txt")
  writeLines("KEEP", sentinel)
  crb <- write_bundle_crb(
    file.path(root, "source"),
    backend = list(type = "h5", location = "missing.h5")
  )

  expect_error(
    build_test_app(c("Dataset" = crb), app, overwrite = TRUE),
    "missing.h5"
  )
  expect_true(file.exists(sentinel))
  if (file.exists(sentinel)) {
    expect_identical(readLines(sentinel), "KEEP")
  }
  expect_false(dir.exists(file.path(app, "shiny")))
})

test_that("inputs inside result_dir survive until the staged copy completes", {
  root <- withr::local_tempdir()
  app <- file.path(root, "app")
  input_dir <- file.path(app, "inputs")
  crb <- write_bundle_crb(input_dir)

  build_test_app(c("Dataset" = crb), app, overwrite = TRUE)

  expect_true(file.exists(file.path(app, "data", "dataset.crb")))
  expect_false(dir.exists(file.path(app, "inputs")))
})

test_that("a configured runtime override skips the tagged backend copy", {
  root <- withr::local_tempdir()
  backend <- list(type = "h5", location = "missing.h5")
  crb <- write_bundle_crb(root, backend = backend)
  override <- file.path(root, "host-matrix.h5")
  writeLines("HOST", override)
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    cerebro_options = list(expression_matrix_h5 = override)
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(config$expression_matrix_h5, override)
  expect_false(file.exists(file.path(app, "data", backend$location)))
})

test_that("one global override cannot serve multiple Cerebro data files", {
  root <- withr::local_tempdir()
  override <- file.path(root, "host-matrix.h5")
  writeLines("HOST", override)
  tagged_backend <- list(type = "h5", location = "missing.h5")
  first <- write_bundle_crb(
    file.path(root, "first"),
    "first.crb",
    tagged_backend
  )
  second <- write_bundle_crb(
    file.path(root, "second"),
    "second.crb",
    tagged_backend
  )

  expect_error(
    build_test_app(
      c("First" = first, "Second" = second),
      file.path(root, "tagged-app"),
      cerebro_options = list(expression_matrix_h5 = override)
    ),
    "same expression matrix"
  )

  legacy_first <- write_bundle_crb(
    file.path(root, "legacy-first"),
    "legacy-first.crb",
    legacy = TRUE
  )
  legacy_second <- write_bundle_crb(
    file.path(root, "legacy-second"),
    "legacy-second.crb",
    legacy = TRUE
  )
  expect_error(
    build_test_app(
      c("First" = legacy_first, "Second" = legacy_second),
      file.path(root, "legacy-app"),
      cerebro_options = list(expression_matrix_h5 = override)
    ),
    "same expression matrix"
  )
})

test_that("different sources cannot write the same bundle target", {
  root <- withr::local_tempdir()
  backend <- list(type = "h5", location = "matrix.h5")
  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  first <- write_bundle_crb(first_dir, "first.crb", backend)
  second <- write_bundle_crb(second_dir, "second.crb", backend)
  write_backend_artifact(first_dir, backend, "FIRST")
  write_backend_artifact(second_dir, backend, "SECOND")

  expect_error(
    build_test_app(
      c("First" = first, "Second" = second),
      file.path(root, "app")
    ),
    "same bundle target"
  )

  upper_backend <- list(type = "h5", location = "MATRIX.H5")
  upper <- write_bundle_crb(second_dir, "upper.crb", upper_backend)
  write_backend_artifact(second_dir, upper_backend, "UPPER")
  expect_error(
    build_test_app(
      c("First" = first, "Upper" = upper),
      file.path(root, "case-app")
    ),
    "same bundle target"
  )

  cross_backend <- list(type = "h5", location = "second.crb")
  cross_first <- write_bundle_crb(first_dir, "cross-first.crb", cross_backend)
  write_backend_artifact(first_dir, cross_backend)
  expect_error(
    build_test_app(
      c("First" = cross_first, "Second" = second),
      file.path(root, "cross-app")
    ),
    "same bundle target"
  )

  image <- file.path(root, "first.crb")
  writeLines("IMAGE", image)
  expect_error(
    build_test_app(
      c("First" = first),
      file.path(root, "image-app"),
      spatial_images = list("First" = image)
    ),
    "same bundle target"
  )
})

test_that("one spatial image can be shared by multiple data sets", {
  root <- withr::local_tempdir()
  first <- write_bundle_crb(file.path(root, "first"), "first.crb")
  second <- write_bundle_crb(file.path(root, "second"), "second.crb")
  image <- file.path(root, "histology.png")
  writeLines("IMAGE", image)
  app <- file.path(root, "app")

  build_test_app(
    c("First" = first, "Second" = second),
    app,
    spatial_images = list("First" = image, "Second" = image)
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(
    unname(unlist(config$spatial_images, use.names = FALSE)),
    rep(file.path("data", "histology.png"), 2L)
  )
  expect_identical(
    readLines(file.path(app, "data", "histology.png")),
    "IMAGE"
  )
})

test_that("parent and child bundle targets conflict before copying", {
  root <- withr::local_tempdir()
  tree_backend <- list(type = "bpcells", location = "tree")
  nested_backend <- list(type = "h5", location = "tree/injected.h5")
  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  first <- write_bundle_crb(first_dir, "first.crb", tree_backend)
  second <- write_bundle_crb(second_dir, "second.crb", nested_backend)
  write_backend_artifact(first_dir, tree_backend, "FIRST")
  write_backend_artifact(second_dir, nested_backend, "SECOND")

  expect_error(
    build_test_app(
      c("First" = first, "Second" = second),
      file.path(root, "app")
    ),
    "parent or child"
  )
})

test_that("backend paths cannot resolve through symbolic links", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  outside <- file.path(root, "outside")
  dir.create(source)
  dir.create(outside)
  writeLines("OUTSIDE", file.path(outside, "matrix.h5"))
  linked <- file.symlink(outside, file.path(source, "alias"))
  if (!isTRUE(linked)) {
    skip("Symbolic links are not available on this platform")
  }
  crb <- write_bundle_crb(
    source,
    backend = list(type = "h5", location = "alias/matrix.h5")
  )

  expect_error(
    build_test_app(c("Dataset" = crb), file.path(root, "app")),
    "symbolic link"
  )

  app <- file.path(root, "existing-app")
  dir.create(app)
  destination_linked <- file.symlink(outside, file.path(app, "data"))
  if (!isTRUE(destination_linked)) {
    skip("Destination symbolic links are not available on this platform")
  }
  embedded <- write_bundle_crb(file.path(root, "embedded"))

  build_test_app(c("Dataset" = embedded), app, overwrite = TRUE)

  expect_identical(
    readLines(file.path(outside, "matrix.h5")),
    "OUTSIDE"
  )
  expect_false(.pathIsSymbolicLink(file.path(app, "data")))
  expect_true(file.exists(file.path(app, "data", "dataset.crb")))
})

test_that("embedded and legacy CRBs do not require sibling files", {
  root <- withr::local_tempdir()
  embedded <- write_bundle_crb(
    file.path(root, "embedded"),
    "embedded.crb"
  )
  legacy <- write_bundle_crb(
    file.path(root, "legacy"),
    "legacy.crb",
    legacy = TRUE
  )
  app <- file.path(root, "app")

  build_test_app(
    c("Embedded" = embedded, "Legacy" = legacy),
    app
  )

  expect_true(file.exists(file.path(app, "data", "embedded.crb")))
  expect_true(file.exists(file.path(app, "data", "legacy.crb")))
})

test_that("overwrite FALSE rejects a non-empty destination without mutation", {
  root <- withr::local_tempdir()
  app <- file.path(root, "app")
  dir.create(app)
  sentinel <- file.path(app, "sentinel.txt")
  writeLines("KEEP", sentinel)
  crb <- write_bundle_crb(file.path(root, "source"))

  expect_error(
    build_test_app(
      c("Dataset" = crb),
      app,
      overwrite = FALSE
    ),
    "non-empty"
  )
  expect_identical(
    list.files(app, all.files = TRUE, no.. = TRUE),
    "sentinel.txt"
  )
  expect_identical(readLines(sentinel), "KEEP")
})

test_that("staged replacement preserves deployment root permissions", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  app <- file.path(root, "app")
  dir.create(app)
  Sys.chmod(app, mode = "0750")
  expect_identical(as.character(file.info(app)$mode), "750")
  crb <- write_bundle_crb(file.path(root, "source"))

  build_test_app(c("Dataset" = crb), app, overwrite = TRUE)

  expect_identical(as.character(file.info(app)$mode), "750")
})

test_that("named lists of scalar paths are accepted", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(file.path(root, "source"))
  app <- file.path(root, "app")

  build_test_app(list("Dataset" = crb), app)

  expect_true(file.exists(file.path(app, "data", "dataset.crb")))
})

test_that("at least one Cerebro data set is required", {
  root <- withr::local_tempdir()

  expect_error(
    build_test_app(
      setNames(character(), character()),
      file.path(root, "app")
    ),
    "at least one"
  )
})

test_that("the runtime rejects an H5 backend that is a directory", {
  skip_if_not_installed("HDF5Array")
  runtime <- new.env(parent = globalenv())
  source_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "shiny",
    "v1.4",
    "utility_functions.R"
  )
  if (!file.exists(source_path)) {
    source_path <- system.file(
      "shiny",
      "v1.4",
      "utility_functions.R",
      package = "CerebroNexus"
    )
  }
  expect_true(file.exists(source_path))
  sys.source(source_path, envir = runtime)
  root <- withr::local_tempdir()
  dir.create(file.path(root, "matrix.h5"))
  object <- new.env(parent = emptyenv())
  class(object) <- "Cerebro"
  object$getExpressionBackend <- function() {
    list(type = "h5", location = "matrix.h5")
  }
  had_options <- exists(
    "Cerebro.options",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (had_options) {
    old_options <- get("Cerebro.options", envir = .GlobalEnv)
  }
  withr::defer(
    if (had_options) {
      assign("Cerebro.options", old_options, envir = .GlobalEnv)
    } else {
      rm(list = "Cerebro.options", envir = .GlobalEnv)
    }
  )
  assign("Cerebro.options", list(), envir = .GlobalEnv)

  expect_error(
    runtime$.attachExternalExpression(
      object,
      file.path(root, "dataset.crb")
    ),
    "missing or not a file"
  )
})
