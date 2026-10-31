test_that("Cartesian toolbars use lasso selection", {
  skip_if_not_installed("plotly")
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("plotting_functions.R"), envir = env)

  fig <- env$cerebro_plotly_toolbar(plotly::plot_ly(x = 1, y = 1))
  expect_identical(
    suppressMessages(plotly::plotly_build(fig))$x$layout$dragmode,
    "lasso"
  )
})

test_that("the declared Shiny minimum supplies Font Awesome 6", {
  description <- gsub(
    "[[:space:]]+",
    " ",
    packageDescription("CerebroNexus")[["Imports"]]
  )
  declared <- sub(
    ".*shiny \\(>= ([^)]+)\\).*",
    "\\1",
    description
  )
  expect_gte(utils::compareVersion(declared, "1.7.2.1"), 0L)
})
