builder_repo_source("help.R")

test_that("every advanced term resolves to plain-language help", {
  terms <- c(
    "assay",
    "layer",
    "embedded",
    "H5",
    "BPCells",
    "default group",
    "default projection",
    "cell_barcode",
    "show_upload_ui"
  )
  for (term in terms) {
    entry <- builder_help_resolve(term)
    expect_true(nzchar(entry$plain), info = term)
    expect_false(grepl("S4|slot|data.frame|reactive", entry$plain), info = term)
  }
})
