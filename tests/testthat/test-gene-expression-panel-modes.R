test_that("gene expression summary modes keep their intended series", {
  helper <- viewer_test_path(
    "gene_expression",
    "func_expression_summary.R"
  )
  expect_true(file.exists(helper))
  if (!file.exists(helper)) {
    return(invisible())
  }
  source(helper, local = TRUE)

  combined <- expressionSummarySpec(
    "combined",
    c("MS4A1", "CD3D")
  )
  expect_identical(combined$kind, "mean")
  expect_identical(combined$series[[1]]$genes, c("MS4A1", "CD3D"))

  separate <- expressionSummarySpec(
    "separate",
    c("MS4A1", "CD3D")
  )
  expect_identical(separate$kind, "separate")
  expect_identical(
    vapply(separate$series, `[[`, character(1), "label"),
    c("MS4A1", "CD3D")
  )

  expect_identical(
    expressionSummarySpec(
      "separate",
      paste0("Gene", seq_len(10))
    )$kind,
    "mean"
  )
  expect_identical(
    expressionSummarySpec(
      "separate",
      c("MS4A1", "CD3D"),
      dimensions = 3
    )$kind,
    "mean"
  )

  rgb <- expressionSummarySpec(
    "rgb",
    c("MS4A1", "CD3D"),
    list(r = "MS4A1", g = "CD3D", b = "")
  )
  expect_identical(rgb$kind, "rgb")
  expect_identical(
    vapply(rgb$series, `[[`, character(1), "label"),
    c("R · MS4A1", "G · CD3D")
  )
  expect_identical(
    vapply(rgb$series, `[[`, character(1), "genes"),
    c("MS4A1", "CD3D")
  )
})

test_that("RGB summaries preserve repeated channels and omit empty ones", {
  helper <- viewer_test_path(
    "gene_expression",
    "func_expression_summary.R"
  )
  skip_if_not(file.exists(helper))
  source(helper, local = TRUE)

  rgb <- expressionSummarySpec(
    "rgb",
    c("MS4A1", "CD3D"),
    list(r = "MS4A1", g = "MS4A1", b = "CD3D")
  )
  expect_identical(
    vapply(rgb$series, `[[`, character(1), "label"),
    c("R · MS4A1", "G · MS4A1", "B · CD3D")
  )
})

test_that("RGB violin outliers use the channel color", {
  source(
    viewer_test_path("plotting_functions.R"),
    local = TRUE
  )
  source(
    viewer_test_path("gene_expression", "func_expression_summary.R"),
    local = TRUE
  )

  plot <- plotExpressionSummary(
    list(list(
      label = "R · MS4A1",
      key = "r",
      genes = "MS4A1",
      color = "#dc2626",
      values = c(0, 0, 0, 1, 4)
    )),
    factor(rep("sample_1", 5)),
    c(sample_1 = "#f59e0b")
  )
  trace <- plotly::plotly_build(plot)$x$data[[1]]

  expect_identical(trace$marker$color, "#dc2626")
})

test_that("gene expression panels follow gene, selection, and display mode", {
  skip_if_not_installed("shinytest2")
  inst_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(inst_dir))
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = "gene_expression_panel_modes",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())

  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  app$wait_for_js(
    "document.getElementById('expression_genes_input') !== null",
    timeout = 20000
  )
  expect_false(app$get_js(
    "document.querySelector('#expression_details_selected_cells_UI h3') !== null"
  ))
  expect_false(app$get_js(
    "document.querySelector('#expression_in_selected_cells_UI h3') !== null"
  ))

  app$set_inputs(expression_genes_input = "MS4A1", wait_ = FALSE)
  app$wait_for_js(
    "document.querySelector('#expression_by_group_UI h3') !== null",
    timeout = 20000
  )
  expect_false(app$get_js(
    "document.querySelector('#expression_by_gene_UI h3') !== null"
  ))

  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_projection_cell_view_host canvas:not(.cv-mini)') !== null"
    ),
    timeout = 20000
  )
  app$run_js(paste0(
    "Shiny.setInputValue('expression_projection_persistent_selection',",
    "{x:[0],y:[0],ids:['missing-cell']},{priority:'event'});"
  ))
  selection <- app$wait_for_value(
    input = "expression_projection_persistent_selection",
    timeout = 20000
  )
  expect_identical(
    unlist(selection$ids, use.names = FALSE),
    "missing-cell"
  )
  app$wait_for_idle(timeout = 20000)
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_in_selected_cells_UI h3') !== null"
    ),
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.querySelector('#expression_details_selected_cells_UI h3') !== null"
  ))
  app$run_js(paste0(
    "Shiny.setInputValue('expression_projection_persistent_selection',",
    "null,{priority:'event'});"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_in_selected_cells_UI h3') === null"
    ),
    timeout = 10000
  )

  app$set_inputs(
    expression_genes_input = c("MS4A1", "CD3D"),
    wait_ = FALSE
  )
  app$wait_for_js(
    paste0(
      "document.getElementById(",
      "'expression_projection_genes_in_separate_panels')?.disabled === false"
    ),
    timeout = 10000
  )
  app$set_inputs(
    expression_projection_genes_in_separate_panels = "separate",
    wait_ = FALSE
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('expression_by_group')?.innerText",
      ".includes('MS4A1')"
    ),
    timeout = 20000
  )
  panel_text <- app$get_js(
    "document.getElementById('expression_by_group').innerText"
  )
  expect_true(grepl("MS4A1", panel_text, fixed = TRUE))
  expect_true(grepl("CD3D", panel_text, fixed = TRUE))

  app$set_inputs(
    expression_projection_genes_in_separate_panels = "rgb",
    wait_ = FALSE
  )
  app$wait_for_js(
    "document.getElementById('expression_rgb_gene_r') !== null",
    timeout = 10000
  )
  app$set_inputs(
    expression_rgb_gene_r = "MS4A1",
    expression_rgb_gene_g = "CD3D",
    expression_rgb_gene_b = "",
    wait_ = FALSE
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('expression_by_group')?.innerText",
      ".includes('R · MS4A1') && ",
      "document.getElementById('expression_by_group')?.innerText",
      ".includes('G · CD3D')"
    ),
    timeout = 20000
  )
  rgb_text <- app$get_js(
    "document.getElementById('expression_by_group').innerText"
  )
  expect_false(grepl("B ·", rgb_text, fixed = TRUE))
})
