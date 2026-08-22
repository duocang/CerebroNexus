test_that("extra tables receive stable keys", {
  root <- withr::local_tempdir()
  csv <- file.path(root, "metrics.csv")
  utils::write.csv(data.frame(value = 1:2), csv, row.names = FALSE)

  bundled <- .bundleExtraTables(c(Metrics = csv))

  expect_named(bundled, "files")
  expect_identical(bundled$files$Metrics$key, "external-file:1")
  expect_identical(bundled$files$Metrics$sheets[[1]]$key, "external:1:1")
  expect_identical(bundled$files$Metrics$sheets[[1]]$table$value, 1:2)
})

test_that("Excel extra tables import non-empty sheets and only rename mappings", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  workbook <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      Summary = data.frame(total = 3),
      Empty = data.frame(value = numeric()),
      Details = data.frame(sample = c("a", "b"))
    ),
    workbook
  )

  bundled <- .bundleExtraTables(
    c(Workbook = workbook),
    list(Workbook = list(`Sample details` = "Details"))
  )

  expect_identical(
    vapply(bundled$files$Workbook$sheets, `[[`, character(1), "label"),
    c("Summary", "Sample details")
  )
})

test_that("omitted extra tables produce no bundle", {
  expect_null(.bundleExtraTables())
})

test_that("extra table declarations reject invalid paths, extensions, and mappings", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(value = 1), csv, row.names = FALSE)

  expect_error(.bundleExtraTables(c(Missing = "no-such-file.csv")), "not found")
  expect_error(
    .bundleExtraTables(c(Bad = tempfile(fileext = ".rds"))),
    "unsupported"
  )
  expect_error(.bundleExtraTables(c(csv)), "named")
  expect_error(
    .bundleExtraTables(c(Table = csv), list(Unknown = list(New = "Sheet"))),
    "not present"
  )
  expect_error(
    .bundleExtraTables(c(Table = csv), list(Table = list(New = "Sheet"))),
    "Excel"
  )
})

test_that("extra table workbook mappings reject duplicate or missing sheets", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  workbook <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(One = data.frame(x = 1), Two = data.frame(x = 2)),
    workbook
  )

  expect_error(
    .bundleExtraTables(
      c(Book = workbook),
      list(Book = structure(list("One", "Two"), names = c("Same", "Same")))
    ),
    "duplicate final label"
  )
  expect_error(
    .bundleExtraTables(
      c(Book = workbook),
      list(Book = list(A = "One", B = "One"))
    ),
    "duplicate source sheet"
  )
  expect_error(
    .bundleExtraTables(c(Book = workbook), list(Book = list(A = "Missing"))),
    "not found"
  )
})

extra_tables_app_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  crb <- file.path(root, "dataset.crb")
  saveRDS(Cerebro$new(), crb)
  list(root = root, crb = crb)
}

test_that("createShinyApp writes table assets and keeps values out of config", {
  fixture <- extra_tables_app_fixture()
  csv <- file.path(fixture$root, "qc.csv")
  utils::write.csv(data.frame(score = 1:2), csv, row.names = FALSE)
  app <- file.path(fixture$root, "app")

  createShinyApp(
    cerebro_data = c(Study = fixture$crb),
    extra_tables = c(QC = csv),
    result_dir = app,
    launch_browser = FALSE,
    verbose = FALSE
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  table_file <- config$extra_tables$files$QC
  expect_true(file.exists(file.path(app, table_file$sheets[[1]]$path)))
  expect_identical(table_file$sheets[[1]]$key, "external:1:1")
  expect_false("table" %in% names(table_file$sheets[[1]]))
  expect_false(grepl(
    normalizePath(csv),
    paste(capture.output(str(config)), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("createShinyApp validates external tables before preparing result target", {
  fixture <- extra_tables_app_fixture()
  prepare_calls <- 0L
  testthat::local_mocked_bindings(
    .prepareBundleResultTarget = function(result_dir) {
      prepare_calls <<- prepare_calls + 1L
      stop("result target preparation reached", call. = FALSE)
    },
    .package = "CerebroNexus"
  )

  expect_error(
    createShinyApp(
      cerebro_data = c(Study = fixture$crb),
      extra_tables = c(Missing = file.path(fixture$root, "missing.csv")),
      result_dir = file.path(fixture$root, "app"),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "not found"
  )
  expect_identical(prepare_calls, 0L)
})
