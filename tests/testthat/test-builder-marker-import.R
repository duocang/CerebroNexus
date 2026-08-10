builder_plan_contract_source_runtime(environment())
builder_repo_source("marker_import.R")

builder_marker_import_xlsx <- function(sheets) {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path), envir = parent.frame())
  writexl::write_xlsx(sheets, path)
  path
}

test_that("marker import inventories delimited files and XLSX sheets", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(cluster = "B", gene = "MS4A1"),
    csv,
    row.names = FALSE
  )
  xlsx <- builder_marker_import_xlsx(list(
    all_cells = data.frame(
      cluster = c("B", "T"),
      gene = c("MS4A1", "CD3D")
    ),
    NK = data.frame(gene = "NKG7")
  ))

  got <- builder_marker_import_inventory(c(csv, xlsx))

  expect_identical(
    vapply(got, `[[`, character(1), "source_name"),
    c(basename(csv), "all_cells", "NK")
  )
  expect_true(all(vapply(got, `[[`, logical(1), "valid")))
})
