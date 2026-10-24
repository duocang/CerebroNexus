test_that("a background process writes the cell-name file once per cell order", {
  skip_if_not_installed("mirai")
  scope <- new.env(parent = globalenv())
  viewer_server_load(
    viewer_server_statements("shiny_server.R"),
    scope,
    names = c(
      ".viewer_cell_names_files",
      ".viewer_cell_names_job",
      "viewerCellNamesFile"
    )
  )
  pack <- list(
    manifest = list(cell_order_fingerprint = "md5-order:test"),
    canonical_order = TRUE,
    cells = c("c1", paste0("c", intToUtf8(233L))),
    cell_count = 2L
  )
  entry <- scope$viewerCellNamesFile(pack)
  result <- mirai::call_mirai(entry$task)$data
  expect_identical(result, list(cells = 2L, bytes = 7L))
  connection <- gzfile(entry$file, "rb")
  on.exit(close(connection), add = TRUE)
  expect_identical(
    readBin(connection, "raw", 64L),
    c(
      charToRaw("c1"),
      as.raw(0L),
      charToRaw(enc2utf8(paste0("c", intToUtf8(233L)))),
      as.raw(0L)
    )
  )
  ## Every later session of the process reuses the same file.
  expect_identical(scope$viewerCellNamesFile(pack), entry)
  expect_null(scope$viewerCellNamesFile(list(manifest = list())))
})

test_that("the cell-name resource describes the finished file", {
  skip_if_not_installed("mirai")
  scope <- new.env(parent = globalenv())
  viewer_server_load(
    viewer_server_statements("shiny_server.R"),
    scope,
    names = c(
      ".viewer_cell_names_files",
      ".viewer_cell_names_job",
      "viewerCellNamesFile"
    )
  )
  viewer_server_load(
    viewer_server_statements("shiny_server.R", server = TRUE),
    scope,
    names = "viewerCellNamesResource"
  )
  pack <- list(
    manifest = list(cell_order_fingerprint = "md5-order:resource"),
    canonical_order = TRUE,
    cells = c("a", "b", "c"),
    cell_count = 3L
  )
  scope$viewerPackCurrent <- function() pack
  scope$viewerStaticResourcePrefix <- function(kind, directory) "cell-names-x"
  scope$viewerCanonicalResourceIdentity <- function(pack) list(identity = "p")
  scope$input <- list()
  scope$isolate <- shiny::isolate
  descriptor <- scope$viewerCellNamesResource()
  expect_identical(descriptor$protocol, "canonical-cell-names-v1")
  expect_identical(descriptor$cells, 3L)
  expect_identical(descriptor$bytes, 6L)
  expect_match(descriptor$url, "^cell-names-x/md5-order_resource[.]bin[.]gz$")
  expect_identical(descriptor$identity, "p")
  ## A browser that could not read the file gets the names in messages.
  scope$input <- list(cell_view_names_resource_failed = descriptor$url)
  expect_null(scope$viewerCellNamesResource())
})
