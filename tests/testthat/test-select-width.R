test_that("viewer select controls have a shared 320px width cap", {
  css_candidates <- c(
    "inst/viewer/www/custom.css",
    "../../inst/viewer/www/custom.css",
    testthat::test_path("../../inst/viewer/www/custom.css"),
    system.file("viewer/www/custom.css", package = "CerebroNexus")
  )
  css_file <- css_candidates[file.exists(css_candidates)][1]
  skip_if(is.na(css_file))
  css <- paste(readLines(css_file, warn = FALSE), collapse = "\n")

  expect_match(css, "body .shiny-input-container:has(select)", fixed = TRUE)
  expect_match(css, "width: min(100%, 320px)", fixed = TRUE)
})
