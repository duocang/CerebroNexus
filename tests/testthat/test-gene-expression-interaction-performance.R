test_that("Gene expression hydrates auxiliary cell data only on interaction", {
  source <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(source, "function requestSingleAux()", fixed = TRUE)
  expect_match(
    source,
    "p.canvas.addEventListener('pointerenter', requestSingleAux)",
    fixed = TRUE
  )
  expect_false(grepl("scheduleSingleAux();", source, fixed = TRUE))
  expect_false(grepl("}, 5000);", source, fixed = TRUE))
})

test_that("Gene selection settles before expression extraction starts", {
  selected <- paste(
    readLines(
      viewer_test_path("gene_expression", "obj_selected_genes.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    selected,
    "expression_selected_genes_input <- reactive",
    fixed = TRUE
  )
  expect_match(
    selected,
    "expression_selected_genes <- debounceAfterFirst(",
    fixed = TRUE
  )
})

test_that("Gene projection host is present before dynamic controls bind", {
  ui <- paste(
    readLines(viewer_test_path("gene_expression", "UI.R"), warn = FALSE),
    collapse = "\n"
  )
  parameters <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "obj_projection_parameters_plot.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    ui,
    'cerebroCellViewOutput("expression_projection")',
    fixed = TRUE
  )
  expect_false(grepl('uiOutput("expression_projection_UI")', ui, fixed = TRUE))
  expect_match(parameters, 'input[["coordviews_shared_base"]]', fixed = TRUE)
  expect_match(parameters, "available_projections[[1L]]", fixed = TRUE)
})
