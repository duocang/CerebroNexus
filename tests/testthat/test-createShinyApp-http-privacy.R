test_that("generated apps expose only bundled spatial images over HTTP", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")

  root <- withr::local_tempdir()
  source_dir <- file.path(root, "source")
  dir.create(source_dir)

  h5 <- Cerebro_v1.3$new()
  h5$setExpressionBackend(type = "h5", location = "matrix.h5")
  h5_crb <- file.path(source_dir, "h5-data.crb")
  saveRDS(h5, h5_crb)
  writeLines("H5 PAYLOAD", file.path(source_dir, "matrix.h5"))

  bpcells <- Cerebro_v1.3$new()
  bpcells$setExpressionBackend(
    type = "bpcells",
    location = "matrix.bpcells"
  )
  bpcells_crb <- file.path(source_dir, "bpcells-data.crb")
  saveRDS(bpcells, bpcells_crb)
  dir.create(file.path(source_dir, "matrix.bpcells"))
  writeLines(
    "BPCELLS PAYLOAD",
    file.path(source_dir, "matrix.bpcells", "payload")
  )

  image <- write_dummy_png(file.path(root, "histology.png"))
  image_bytes <- readBin(
    image,
    what = "raw",
    n = as.integer(file.info(image)$size)
  )
  port <- httpuv::randomPort(host = "127.0.0.1")
  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = c("H5" = h5_crb, "BPCells" = bpcells_crb),
    result_dir = app_dir,
    port = port,
    host = "127.0.0.1",
    launch_browser = FALSE,
    quiet = TRUE,
    spatial_images = list("H5" = image),
    verbose = FALSE
  )

  private_paths <- c(
    file.path(app_dir, "data", "h5-data.crb"),
    file.path(app_dir, "data", "bpcells-data.crb"),
    file.path(app_dir, "data", "matrix.h5"),
    file.path(app_dir, "data", "matrix.bpcells", "payload")
  )
  expect_true(all(file.exists(private_paths)))
  public_image <- file.path(app_dir, "spatial-assets", "histology.png")
  expect_true(file.exists(public_image))

  stdout <- file.path(root, "app.stdout")
  stderr <- file.path(root, "app.stderr")
  process <- callr::r_bg(
    function(app_dir, port) {
      shiny::runApp(
        appDir = app_dir,
        port = port,
        host = "127.0.0.1",
        launch.browser = FALSE,
        quiet = TRUE
      )
    },
    args = list(app_dir = app_dir, port = port),
    stdout = stdout,
    stderr = stderr,
    supervise = TRUE
  )
  on.exit(
    {
      if (process$is_alive()) {
        process$kill_tree()
      }
      try(process$wait(timeout = 5000), silent = TRUE)
    },
    add = TRUE
  )

  process_logs <- function() {
    read_log <- function(path) {
      if (file.exists(path)) {
        paste(readLines(path, warn = FALSE), collapse = "\n")
      } else {
        "<missing>"
      }
    }
    paste(
      "stdout:",
      read_log(stdout),
      "stderr:",
      read_log(stderr),
      sep = "\n"
    )
  }
  base_url <- paste0("http://127.0.0.1:", port)
  ready <- FALSE
  deadline <- Sys.time() + 30
  while (Sys.time() < deadline) {
    if (!process$is_alive()) {
      testthat::fail(paste("Generated app exited before ready", process_logs()))
    }
    response <- tryCatch(
      httr::GET(base_url, httr::timeout(1)),
      error = function(error) NULL
    )
    if (!is.null(response) && httr::status_code(response) == 200L) {
      ready <- TRUE
      break
    }
    Sys.sleep(0.05)
  }
  if (!ready) {
    testthat::fail(paste("Generated app did not become ready", process_logs()))
  }

  get_from_app <- function(path) {
    if (!process$is_alive()) {
      testthat::fail(paste("Generated app exited early", process_logs()))
    }
    tryCatch(
      httr::GET(paste0(base_url, path), httr::timeout(5)),
      error = function(error) {
        if (!process$is_alive()) {
          testthat::fail(paste("Generated app exited early", process_logs()))
        }
        stop(error)
      }
    )
  }

  statuses <- vapply(
    c(
      "/data/h5-data.crb",
      "/data/bpcells-data.crb",
      "/data/matrix.h5",
      "/data/matrix.bpcells/payload"
    ),
    function(path) httr::status_code(get_from_app(path)),
    integer(1)
  )
  expect_identical(unname(statuses), rep(404L, length(statuses)))

  image_response <- get_from_app("/spatial-assets/histology.png")
  expect_identical(httr::status_code(image_response), 200L)
  expect_identical(httr::content(image_response, as = "raw"), image_bytes)
})
