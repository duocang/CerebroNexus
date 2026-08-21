test_that("extra tables receive stable external keys without source paths", {
  root <- withr::local_tempdir()
  csv <- file.path(root, "metrics.csv")
  tsv <- file.path(root, "annotations.tsv")
  utils::write.csv(data.frame(value = 1:2), csv, row.names = FALSE)
  utils::write.table(
    data.frame(group = c("a", "b")),
    tsv,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  bundled <- .bundleExtraTables(c(Metrics = csv, Annotations = tsv))

  expect_named(bundled, "files")
  expect_named(bundled$files, c("Metrics", "Annotations"))
  expect_named(bundled$files$Metrics, "sheets")
  expect_identical(bundled$files$Metrics$sheets[[1]]$key, "external:1:1")
  expect_identical(bundled$files$Annotations$sheets[[1]]$key, "external:2:1")
  expect_identical(bundled$files$Metrics$sheets[[1]]$label, "Metrics")
  expect_identical(bundled$files$Annotations$sheets[[1]]$label, "Annotations")
  expect_identical(bundled$files$Metrics$sheets[[1]]$table$value, 1:2)
  expect_false(any(grepl(root, unlist(bundled, recursive = TRUE))))
})

test_that("Excel extra tables import every non-empty sheet and mappings only rename", {
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
  expect_identical(
    vapply(bundled$files$Workbook$sheets, `[[`, character(1), "key"),
    c("external:1:1", "external:1:3")
  )
  expect_identical(
    bundled$files$Workbook$sheets[[2]]$table$sample,
    c("a", "b")
  )
})

test_that("omitted extra tables produce no manifest", {
  expect_null(.bundleExtraTables())
})

test_that("malformed workbooks report only their external label", {
  skip_if_not_installed("readxl")
  workbook <- file.path(withr::local_tempdir(), "private-input.xlsx")
  writeLines("not an Excel workbook", workbook)

  error <- tryCatch(
    .bundleExtraTables(c(Workbook = workbook)),
    error = identity
  )

  expect_match(conditionMessage(error), "Workbook")
  expect_false(grepl(workbook, conditionMessage(error), fixed = TRUE))
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
  directory <- file.path(withr::local_tempdir(), "not-a-table.csv")
  dir.create(directory)
  error <- tryCatch(
    .bundleExtraTables(c(Directory = directory)),
    error = identity
  )
  expect_match(conditionMessage(error), "regular file")
  expect_false(grepl(directory, conditionMessage(error), fixed = TRUE))
})

test_that("extra table workbook mappings reject duplicate labels and source sheets", {
  skip_if_not_installed("writexl")
  workbook <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(One = data.frame(x = 1), Two = data.frame(x = 2)),
    workbook
  )

  expect_error(
    .bundleExtraTables(
      c(Book = workbook),
      list(
        Book = structure(
          list("One", "Two"),
          names = c("Same", "Same")
        )
      )
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
  expect_error(
    .bundleExtraTables(c(Book = workbook), list(Book = list(One = "Two"))),
    "collides with an unmapped source sheet"
  )
})

extra_tables_app_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  crb <- file.path(root, "dataset.crb")
  saveRDS(Cerebro$new(), crb)
  list(root = root, crb = crb)
}

test_that("createShinyApp freezes table values but not source paths", {
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
  expect_named(config$extra_tables, "files")
  expect_identical(config$extra_tables$files$QC$sheets[[1]]$key, "external:1:1")
  expect_identical(config$extra_tables$files$QC$sheets[[1]]$table$score, 1:2)
  expect_false(grepl(
    normalizePath(csv),
    paste(capture.output(str(config)), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("createShinyApp omits external table config without external tables", {
  fixture <- extra_tables_app_fixture()
  app <- file.path(fixture$root, "app")

  createShinyApp(
    cerebro_data = c(Study = fixture$crb),
    result_dir = app,
    launch_browser = FALSE,
    verbose = FALSE
  )

  expect_null(readRDS(file.path(app, "cerebro_config.rds"))$extra_tables)
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
