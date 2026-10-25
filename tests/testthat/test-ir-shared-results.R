## Immune repertoire results that depend only on a data set's files are shared
## by the sessions of a process; everything else stays per session.

ir_shared_scope <- function() {
  scope <- new.env(parent = asNamespace("shiny"))
  scope$`%||%` <- function(a, b) if (is.null(a)) b else a
  for (name in c("ir_dataset_process_key", "ir_process_memo")) {
    eval(
      viewer_r_assignment(
        viewer_test_path("immune_repertoire", "data.R"),
        name
      ),
      scope
    )
  }
  scope$ir_process_cache <- new.env(parent = emptyenv())
  scope
}

test_that("a data set's process key follows its files", {
  scope <- ir_shared_scope()
  file <- withr::local_tempfile(fileext = ".crb")
  writeLines("one", file)
  ds <- structure(list(), cerebro_dataset_path = file)
  key <- scope$ir_dataset_process_key(ds)
  expect_identical(key$path, normalizePath(file, winslash = "/"))
  expect_identical(scope$ir_dataset_process_key(ds), key)
  writeLines("a longer second version", file)
  expect_false(identical(scope$ir_dataset_process_key(ds), key))
  expect_null(scope$ir_dataset_process_key(NULL))
  expect_null(scope$ir_dataset_process_key(list()))
  expect_null(scope$ir_dataset_process_key(
    structure(
      list(),
      cerebro_dataset_path = file.path(tempdir(), "missing.crb")
    )
  ))
})

test_that("process memo computes once per key and never without one", {
  scope <- ir_shared_scope()
  runs <- 0L
  compute <- function() {
    runs <<- runs + 1L
    runs
  }
  expect_identical(scope$ir_process_memo("units", list("a", 1), compute), 1L)
  expect_identical(scope$ir_process_memo("units", list("a", 1), compute), 1L)
  expect_identical(scope$ir_process_memo("units", list("b", 1), compute), 2L)
  expect_identical(scope$ir_process_memo("other", list("a", 1), compute), 3L)
  expect_identical(scope$ir_process_memo("units", NULL, compute), 4L)
  expect_identical(scope$ir_process_memo("units", NULL, compute), 5L)
})

test_that("sharing units match the barcodes once, not once per sample", {
  scope <- new.env(parent = globalenv())
  eval(
    viewer_r_assignment(
      viewer_test_path("immune_repertoire", "data.R"),
      "ir_discover_sharing_units"
    ),
    scope
  )
  scope$ir_scr_cols <- c("barcode", "CTgene")
  calls <- 0L
  scope$match <- function(...) {
    calls <<- calls + 1L
    base::match(...)
  }
  data <- list(
    a = data.frame(barcode = c("c3", "c1"), CTgene = "TRB-a"),
    empty = data.frame(barcode = character(), CTgene = character()),
    b = data.frame(barcode = "c2", CTgene = "TRB-a")
  )
  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3", "unmatched"),
    patient = c("p1", "p2", "p1", "p3"),
    site = c("t", "t", "t", "n")
  )
  expect_identical(
    scope$ir_discover_sharing_units(data, metadata),
    c("sample", "patient")
  )
  expect_identical(calls, 1L)
})

test_that("sharing discovery labels first-frame rows with the Pack's cells", {
  scope <- new.env(parent = globalenv())
  eval(
    viewer_r_assignment(
      viewer_test_path("immune_repertoire", "data.R"),
      "ir_sharing_unit_metadata"
    ),
    scope
  )
  first_frame <- data.frame(patient = c("p1", "p2", "p1"))
  pack <- list(canonical_order = TRUE)
  scope$viewerProjectionFirstFrameCache <- function() {
    list(meta_data = first_frame)
  }
  scope$viewerPackCurrent <- function() pack
  scope$viewerPackCellBarcodes <- function(pack) c("c1", "c2", "c3")
  scope$getMetaData <- function() stop("unexpected metadata hydration")
  metadata <- scope$ir_sharing_unit_metadata()
  expect_identical(metadata$cell_barcode, c("c1", "c2", "c3"))
  expect_identical(metadata$patient, first_frame$patient)

  hydrated <- data.frame(cell_barcode = c("c1", "c2", "c3"), patient = "p")
  scope$getMetaData <- function() hydrated
  pack$canonical_order <- FALSE
  expect_identical(scope$ir_sharing_unit_metadata(), hydrated)
  pack$canonical_order <- TRUE
  scope$viewerPackCellBarcodes <- function(pack) c("c1", "c2")
  expect_identical(scope$ir_sharing_unit_metadata(), hydrated)
  scope$viewerProjectionFirstFrameCache <- function() NULL
  scope$viewerPackCellBarcodes <- function(pack) c("c1", "c2", "c3")
  expect_identical(scope$ir_sharing_unit_metadata(), hydrated)
})
