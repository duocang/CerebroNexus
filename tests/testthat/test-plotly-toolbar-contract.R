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

test_that("Groups charts render their download toolbar, bars and Sankey", {
  skip_if_not_installed("shinytest2")
  inst_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(inst_dir))
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = "groups_chart_toolbar",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())

  app$click(selector = 'a[href="#shiny-tab-groups"]')
  app$wait_for_js(
    paste0(
      "document.querySelectorAll(",
      "'#groups_by_other_group_plot .cc-plot svg rect').length > 0"
    ),
    timeout = 20000
  )

  bar <- app$get_js(paste0(
    "(() => {",
    "const plot=document.getElementById('groups_by_other_group_plot');",
    "const buttons=[...plot.querySelectorAll('.cc-btn')];",
    "const glyphs=buttons.map(button => getComputedStyle(button, '::before'));",
    "return {buttons:buttons.map(button => button.title),",
    "glyphs:glyphs.every(style => style.content !== 'none'),",
    "legend:plot.querySelectorAll('.cc-legend-item').length,",
    "plotly:document.querySelectorAll('#shiny-tab-groups .js-plotly-plot').length};",
    "})()"
  ))

  expect_identical(
    unlist(bar$buttons, use.names = FALSE),
    c("Download plot as SVG", "Download plot as a png")
  )
  expect_true(bar$glyphs)
  expect_gt(bar$legend, 0)
  expect_equal(bar$plotly, 0)

  app$run_js(paste0(
    "document.querySelector(",
    "'#groups_by_other_group_plot .cc-legend-item').click();"
  ))
  expect_equal(
    app$get_js(paste0(
      "document.querySelectorAll(",
      "'#groups_by_other_group_plot .cc-legend-item.is-hidden').length"
    )),
    1
  )

  app$run_js(paste0(
    "document.querySelector(",
    "'input[name=\"groups_by_other_group_plot_type\"]",
    "[value=\"Sankey plot\"]'",
    ").parentElement.click();"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelectorAll(",
      "'#groups_by_other_group_plot path.cc-link').length > 0"
    ),
    timeout = 10000
  )
})
