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

test_that("RGB expression reads all channels in one backend call", {
  scope <- new.env(parent = globalenv())
  scope$reactive <- shiny::reactive
  scope$req <- shiny::req
  scope$withProgress <- function(expr, ...) force(expr)
  scope$incProgress <- function(...) NULL
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  scope$input <- shiny::reactiveValues(
    expression_projection_genes_in_separate_panels = "rgb"
  )
  scope$session <- shiny::MockShinySession$new()

  values <- matrix(
    c(1, 2, 3, 4, 5, 6),
    nrow = 3,
    dimnames = list(c("g1", "g2", "g3"), c("c1", "c2"))
  )
  reads <- 0L
  dataset <- new.env(parent = emptyenv())
  dataset$expression <- values
  dataset$getExpressionMatrix <- function(cells = NULL, genes = NULL) {
    reads <<- reads + 1L
    values[genes, cells, drop = FALSE]
  }
  scope$data_set <- shiny::reactive(dataset)
  scope$getGeneNames <- function() rownames(values)
  scope$expression_projection_cells_to_show <- shiny::reactive(c(1L, 2L))
  scope$expression_projection_coordinates <- shiny::reactive(data.frame(
    x = c(0, 1),
    y = c(1, 0)
  ))
  scope$expression_selected_genes <- shiny::reactive(list(
    genes_to_display_present = c("g1", "g2", "g3"),
    rgb_genes = list(r = "g1", g = "g2", b = "g3")
  ))

  sys.source(
    viewer_test_path("gene_expression", "func_expression_summary.R"),
    envir = scope
  )
  sys.source(
    viewer_test_path(
      "gene_expression",
      "obj_projection_expression_levels.R"
    ),
    envir = scope
  )
  levels <- shiny::isolate(scope$expression_projection_expression_levels())

  expect_identical(reads, 1L)
  expect_equal(levels, list(r = c(1, 4), g = c(2, 5), b = c(3, 6)))
  expect_equal(
    shiny::isolate(scope$expressionProjectionValues(
      c(1L, 2L),
      c("g3", "g1")
    )),
    list(g3 = c(3, 6), g1 = c(1, 4))
  )
  expect_identical(reads, 1L)
})

test_that("gene expression work is debounced before backend reads", {
  levels <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "obj_projection_expression_levels.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    levels,
    "expression_projection_request <- debounceAfterFirst(",
    fixed = TRUE
  )
  expect_match(
    levels,
    "request <- expression_projection_request()",
    fixed = TRUE
  )
})

test_that("gene display modes reuse specialist geometry", {
  scope <- new.env(parent = globalenv())
  scope$expressionProjectionProgressSet <- function(...) NULL
  scope$expressionColorScale <- function(...) list(c(0, "#fff"), c(1, "#f70"))
  scope$expressionPanelColorScales <- function(...) NULL
  scope$expressionReverseColorScale <- function(...) FALSE
  sent <- list()
  scope$cerebroCellViewRender <- function(id, meta, data, hover, extra) {
    sent[[length(sent) + 1L]] <<- list(
      type = "full",
      id = id,
      meta = meta,
      data = data,
      hover = hover,
      extra = extra
    )
  }
  scope$cerebroCellViewRecolor <- function(id, meta, data) {
    sent[[length(sent) + 1L]] <<- list(
      type = "recolor",
      id = id,
      meta = meta,
      data = data
    )
  }
  sys.source(
    viewer_test_path(
      "gene_expression",
      "func_projection_update_plot.R"
    ),
    envir = scope
  )
  base <- list(
    coordinates = list(x = c(0, 1), y = c(1, 0)),
    render_token = 1L,
    reset_axes = FALSE,
    expression_levels = c(1, 2),
    plot_parameters = list(
      draw_border = FALSE,
      keep_square = FALSE,
      point_size = 2,
      point_opacity = 1,
      x_range = c(0, 1),
      y_range = c(0, 1),
      plot_order = "Highest expression on top",
      is_trajectory = FALSE,
      projection = "UMAP",
      n_dimensions = 2,
      hover_info = FALSE
    ),
    color_settings = list(
      color_scale = "Cerebro orange",
      color_range = c(0, 2),
      color_mode = "shared",
      genes = c("g1", "g2"),
      rgb_genes = list(r = "g1", g = "g2", b = NULL)
    ),
    selection_keys = c("c1", "c2"),
    hover_columns = list(),
    trajectory = list(),
    display_mode = "mean",
    separate_panels = FALSE
  )

  scope$expression_projection_update_plot(base)
  changed <- base
  changed$render_token <- 2L
  changed$expression_levels <- c(2, 3)
  scope$expression_projection_update_plot(changed)
  rgb <- changed
  rgb$render_token <- 3L
  rgb$display_mode <- "rgb"
  rgb$expression_levels <- list(r = c(1, 2), g = c(2, 1), b = c(0, 0))
  scope$expression_projection_update_plot(rgb)

  expect_identical(
    vapply(sent, `[[`, character(1), "type"),
    c(
      "full",
      "recolor",
      "recolor"
    )
  )
  expect_false(any(c("x", "y", "selection_key") %in% names(sent[[2L]]$data)))
  expect_null(sent[[3L]]$data$color)
  expect_named(sent[[3L]]$data$rgb, c("r", "g", "b"))
  expect_true(sent[[3L]]$data$rgb_scaled)
  expect_true(all(unlist(sent[[3L]]$data$rgb) >= 0L))
  expect_true(all(unlist(sent[[3L]]$data$rgb) <= 255L))
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

  viewer_set_selectize(app, "expression_genes_input", "MS4A1")
  app$wait_for_idle(timeout = 60000)
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
    "{x:[0],y:[0],ids:['test-cell']},{priority:'event'})"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_in_selected_cells_UI h3') !== null"
    ),
    timeout = 20000
  )
  expect_true(app$get_js(
    "document.querySelector('#expression_details_selected_cells_UI h3') !== null"
  ))
  app$run_js(paste0(
    "Shiny.setInputValue('expression_projection_persistent_selection',",
    "null,{priority:'event'})"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_in_selected_cells_UI h3') === null"
    ),
    timeout = 20000
  )

  viewer_set_selectize(
    app,
    "expression_genes_input",
    c("MS4A1", "CD3D")
  )
  app$wait_for_js(
    paste0(
      "document.getElementById(",
      "'expression_projection_genes_in_separate_panels')?.disabled === false"
    ),
    timeout = 10000
  )
  app$wait_for_js(
    "document.querySelector('[id^=\"shiny-progress-\"]') === null",
    timeout = 20000
  )
  app$run_js(paste0(
    "window.__geneModeTokens=[];",
    "window.addEventListener('cerebro:cell-view-ready',function(e){",
    "if(e.detail?.id==='expression_projection' && ",
    "Number.isFinite(Number(e.detail.renderToken)))",
    "window.__geneModeTokens.push(Number(e.detail.renderToken));});"
  ))
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
  app$wait_for_js(
    "document.querySelector('[id^=\"shiny-progress-\"]') === null",
    timeout = 20000
  )
  Sys.sleep(0.75)
  expect_identical(
    app$get_js(
      "Array.from(new Set(window.__geneModeTokens)).length"
    ),
    1L
  )

  app$run_js(paste0(
    "window.__geneModeTokens=[];",
    "window.addEventListener('cerebro:cell-view-ready',function(e){",
    "if(e.detail?.id==='expression_projection' && ",
    "Number.isFinite(Number(e.detail.renderToken)))",
    "window.__geneModeTokens.push(Number(e.detail.renderToken));});"
  ))
  app$set_inputs(
    expression_projection_genes_in_separate_panels = "rgb",
    wait_ = FALSE
  )
  app$wait_for_js(
    "document.getElementById('expression_rgb_gene_r') !== null",
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "(() => {const text=document.getElementById(",
      "'expression_by_group')?.innerText || '';",
      "return text.includes('R · MS4A1') && ",
      "text.includes('G · CD3D') && !text.includes('B ·');})()"
    ),
    timeout = 20000
  )
  Sys.sleep(0.75)
  expect_identical(
    app$get_js(
      "Array.from(new Set(window.__geneModeTokens)).length"
    ),
    1L
  )
})
