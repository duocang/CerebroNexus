read_viewer_file <- function(...) {
  paste(readLines(viewer_test_path(...), warn = FALSE), collapse = "\n")
}

test_that("native Plotly outputs use the shared toolbar", {
  paths <- list(
    c("trajectory", "distribution_along_pseudotime.R"),
    c("gene_expression", "UI_expression_by_pseudotime.R"),
    c("coordinated_views", "server.R"),
    c("extra_material", "content.R")
  )

  for (path in paths) {
    expect_match(
      do.call(read_viewer_file, as.list(path)),
      "cerebro_plotly_toolbar",
      fixed = TRUE
    )
  }
})

test_that("native Plotly SVG icons are not hidden globally", {
  css <- read_viewer_file("www", "custom.css")

  expect_false(grepl(
    "[.]modebar-btn svg[[:space:]]*\\{[^}]*display:[[:space:]]*none",
    css
  ))
})

test_that("shared Plotly controls use the viewer icon language", {
  css <- read_viewer_file("www", "custom.css")
  glyphs <- c(
    "Box Select" = "\\f5cb",
    "Lasso Select" = "\\f5ee",
    "Pan" = "\\f0b2",
    "Zoom in" = "\\f00e",
    "Zoom out" = "\\f010",
    "Reset axes" = "\\f015",
    "Download plot as a png" = "\\f019"
  )

  for (title in names(glyphs)) {
    expect_match(
      css,
      sprintf(
        '.modebar-btn[data-title="%s"]::before { content: "%s"; }',
        title,
        glyphs[[title]]
      ),
      fixed = TRUE
    )
  }
  expect_match(
    css,
    '.modebar-btn[data-title="Box Select"] svg,',
    fixed = TRUE
  )
})

test_that("Sankey plots request a download-only toolbar", {
  plotting <- read_viewer_file("plotting_functions.R")

  expect_match(
    plotting,
    'cerebro_plotly_toolbar(plot, "toImage")',
    fixed = TRUE
  )
})

test_that("Cartesian toolbars default to lasso selection", {
  skip_if_not_installed("plotly")
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("plotting_functions.R"), envir = env)

  fig <- env$cerebro_plotly_toolbar(plotly::plot_ly(x = 1, y = 1))
  expect_identical(
    suppressMessages(plotly::plotly_build(fig))$x$layout$dragmode,
    "lasso"
  )

  download_only <- env$cerebro_plotly_toolbar(
    plotly::plot_ly(x = 1, y = 1),
    "toImage"
  )
  expect_null(
    suppressMessages(plotly::plotly_build(download_only))$x$layout$dragmode
  )
})

test_that("the declared Shiny minimum supplies Font Awesome 6", {
  description <- read.dcf(
    testthat::test_path("..", "..", "DESCRIPTION"),
    fields = "Imports"
  )[[1]]

  expect_match(description, "shiny \\(>= 1[.]7[.]2[.]1\\)")
})
