test_that("HLA motif card uses the shared fill with its action-tail allowance", {
  inst_candidates <- c(
    "inst",
    "../../inst",
    testthat::test_path("../../inst"),
    system.file(package = "CerebroNexus")
  )
  inst_dir <- inst_candidates[file.exists(file.path(
    inst_candidates,
    "viewer"
  ))][1]
  skip_if(is.na(inst_dir))
  ui <- paste(
    readLines(file.path(inst_dir, "viewer/hla_tcr_motifs/UI.R")),
    collapse = "\n"
  )
  js <- paste(
    readLines(file.path(inst_dir, "viewer/www/fill_height.js")),
    collapse = "\n"
  )

  expect_match(ui, '`data-cerebro-fill-tail` = "22"', fixed = TRUE)
  expect_match(js, "cerebroFillTail", fixed = TRUE)
})
