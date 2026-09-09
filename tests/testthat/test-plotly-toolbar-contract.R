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

test_that("shared Plotly toolbars render their icons and charts", {
  skip_if_not_installed("shinytest2")
  inst_dir <- testthat::test_path("../../inst")
  shinytest2::local_app_support(inst_dir)
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = "plotly_toolbar",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())

  app$click(selector = 'a[href="#shiny-tab-groups"]')
  app$wait_for_js(
    "document.querySelectorAll('#shiny-tab-groups .modebar-btn').length >= 7",
    timeout = 20000
  )

  glyphs <- app$get_js(paste0(
    "(() => {",
    "const buttons=[...document.querySelectorAll(",
    "'#shiny-tab-groups .modebar-btn')];",
    "const styles=buttons.map(button => getComputedStyle(button, '::before'));",
    "return {count:buttons.length, visible:styles.every(style => ",
    "style.opacity === '1' && style.display !== 'none'), ",
    "aligned:styles.every(style => style.position === 'static')};",
    "})()"
  ))

  expect_gte(glyphs$count, 7)
  expect_true(glyphs$visible)
  expect_true(glyphs$aligned)

  app$run_js(paste0(
    "document.querySelector(",
    "'input[name=\"groups_by_other_group_plot_type\"]",
    "[value=\"Sankey plot\"]'",
    ").parentElement.click();"
  ))
  app$wait_for_js(
    paste0(
      "document.getElementById('groups_by_other_group_plot')",
      "?._fullData?.[0]?.type === 'sankey'"
    ),
    timeout = 10000
  )

  sankey <- app$get_js(paste0(
    "(() => {",
    "const plot=document.getElementById('groups_by_other_group_plot');",
    "return {links:plot._fullData[0].link.value.length, buttons:",
    "[...plot.querySelectorAll('.modebar-btn')].map(button => ",
    "button.dataset.title)};",
    "})()"
  ))

  expect_gt(sankey$links, 0)
  expect_identical(
    unlist(sankey$buttons, use.names = FALSE),
    "Download plot as a png"
  )
})
