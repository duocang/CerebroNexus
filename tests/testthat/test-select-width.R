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
  expect_match(
    css,
    "body .shiny-input-container:has(.bootstrap-select)",
    fixed = TRUE
  )
  expect_match(css, "body .bootstrap-select > .dropdown-toggle", fixed = TRUE)
  expect_match(
    css,
    "body .shiny-input-container .selectize-control",
    fixed = TRUE
  )
  expect_false(grepl(
    "body .shiny-input-container > .selectize-control",
    css,
    fixed = TRUE
  ))
  expect_match(css, "width: min(100%, 320px)", fixed = TRUE)
})

test_that("slow output feedback is delayed and keeps existing content", {
  js_candidates <- c(
    "inst/viewer/www/viewer-shell.js",
    "../../inst/viewer/www/viewer-shell.js",
    testthat::test_path("../../inst/viewer/www/viewer-shell.js"),
    system.file("viewer/www/viewer-shell.js", package = "CerebroNexus")
  )
  js_file <- js_candidates[file.exists(js_candidates)][1]
  skip_if(is.na(js_file))
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  expect_match(js, "shiny:outputinvalidated.cerebroMotion", fixed = TRUE)
  expect_match(js, "cerebro-output-waiting", fixed = TRUE)
  expect_match(js, "}, 120);", fixed = TRUE)
})
