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

test_that("Sankey plots request a download-only toolbar", {
  plotting <- read_viewer_file("plotting_functions.R")

  expect_match(
    plotting,
    'cerebro_plotly_toolbar(plot, "toImage")',
    fixed = TRUE
  )
})

test_that("the declared Shiny minimum supplies Font Awesome 6", {
  description <- read.dcf(
    testthat::test_path("..", "..", "DESCRIPTION"),
    fields = "Imports"
  )[[1]]

  expect_match(description, "shiny \\(>= 1[.]7[.]2[.]1\\)")
})
