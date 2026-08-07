test_that("generated apps expose no bundled artifacts over HTTP", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")

  root <- withr::local_tempdir()
  sources <- privacy_test_sources(root)
  port <- httpuv::randomPort(host = "127.0.0.1")
  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = sources$cerebro_data,
    result_dir = app_dir,
    port = port,
    host = "127.0.0.1",
    launch_browser = FALSE,
    quiet = TRUE,
    spatial_images = list("H5" = sources$image),
    verbose = FALSE
  )

  private_paths <- c(
    file.path(app_dir, "private-data", "h5-data.crb"),
    file.path(app_dir, "private-data", "bpcells-data.crb"),
    file.path(app_dir, "private-data", "matrix.h5"),
    file.path(app_dir, "private-data", "matrix.bpcells", "payload")
  )
  expect_true(all(file.exists(private_paths)))
  expect_false(dir.exists(file.path(app_dir, "data")))
  spatial_image <- file.path(app_dir, "spatial-assets", "histology.png")
  expect_true(file.exists(spatial_image))

  app <- privacy_start_app(app_dir, port, root)
  on.exit(privacy_stop_app(app), add = TRUE)
  privacy_wait_for_app(app)

  private_urls <- c(
    "/data/h5-data.crb",
    "/data/bpcells-data.crb",
    "/data/matrix.h5",
    "/data/matrix.bpcells/payload",
    "/private-data/h5-data.crb",
    "/private-data/bpcells-data.crb",
    "/private-data/matrix.h5",
    "/private-data/matrix.bpcells/payload",
    "/spatial-assets/histology.png"
  )
  statuses <- vapply(
    private_urls,
    function(path) httr::status_code(privacy_get_from_app(app, path)),
    integer(1)
  )
  expect_identical(unname(statuses), rep(404L, length(statuses)))
})

test_that("a running legacy data mapping cannot expose replacement data", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  skip_on_os("windows")

  root <- withr::local_tempdir()
  sources <- privacy_test_sources(root)
  app_dir <- file.path(root, "app")
  privacy_write_legacy_app(app_dir)
  port <- httpuv::randomPort(host = "127.0.0.1")
  legacy_app <- privacy_start_app(app_dir, port, root)
  on.exit(privacy_stop_app(legacy_app), add = TRUE)
  privacy_wait_for_app(legacy_app)

  legacy_response <- privacy_get_from_app(legacy_app, "/data/legacy.txt")
  expect_identical(httr::status_code(legacy_response), 200L)

  createShinyApp(
    cerebro_data = sources$cerebro_data,
    result_dir = app_dir,
    overwrite = TRUE,
    port = port,
    host = "127.0.0.1",
    launch_browser = FALSE,
    quiet = TRUE,
    spatial_images = list("H5" = sources$image),
    verbose = FALSE
  )
  expect_true(legacy_app$process$is_alive())

  legacy_private_urls <- c(
    "/data/h5-data.crb",
    "/data/bpcells-data.crb",
    "/data/matrix.h5",
    "/data/matrix.bpcells/payload"
  )
  statuses <- vapply(
    legacy_private_urls,
    function(path) {
      httr::status_code(privacy_get_from_app(legacy_app, path))
    },
    integer(1)
  )
  expect_identical(unname(statuses), rep(404L, length(statuses)))
})
