library(shinytest2)

inst_dir <- system.file(package = "CerebroNexus")

## shinytest2's wait_for_idle() tracks server reactivity, NOT the async
## client-side projection renderer (www/projection_scatter.js). After it returns,
## a projection output can still be mid-paint and a renderUI-created input can be
## unbound, so reading a value or setting such an input the instant wait_for_idle
## returns races the render and intermittently fails (a 500 on the value URL, a
## NULL output, or "input binding not found"). These helpers poll for the
## condition instead of reading once, which is what de-flakes these recordings.

## Poll get_value(...) until it returns a non-NULL result or the timeout expires.
## A query that errors (server 500 while the output is still rendering) is
## retried; if no value arrives, report the last real error instead of replacing
## it with an unexplained NULL. `validate`, when supplied, is a predicate the
## value must also satisfy: an async output can return a non-NULL but transient
## error state (e.g. a shiny.silent.error while req() is unmet) that is not yet
## the real payload, so keep polling until validate(val) is TRUE. A predicate
## that errors counts as not-yet-ready and is retried.
retry_get_value <- function(
  app,
  ...,
  timeout = 20000,
  interval = 300,
  validate = NULL
) {
  deadline <- Sys.time() + timeout / 1000
  last_error <- NULL
  repeat {
    val <- tryCatch(
      app$get_value(...),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    ok <- !is.null(val)
    if (ok && !is.null(validate)) {
      ok <- tryCatch(
        isTRUE(validate(val)),
        error = function(e) {
          last_error <<- e
          FALSE
        }
      )
    }
    if (ok) {
      return(val)
    }
    if (Sys.time() > deadline) {
      if (!is.null(last_error)) {
        stop(last_error)
      }
      return(val)
    }
    Sys.sleep(interval / 1000)
  }
}

test_that("retry_get_value reports the last error after timing out", {
  attempts <- 0L
  app <- list(get_value = function(...) {
    attempts <<- attempts + 1L
    stop(sprintf("render attempt %d failed", attempts), call. = FALSE)
  })

  expect_error(
    retry_get_value(app, timeout = 5, interval = 1),
    "render attempt [0-9]+ failed"
  )
  expect_gt(attempts, 1L)
})

## Wait until an input element exists in the DOM (its Shiny binding is registered)
## before set_inputs — a renderUI-created selectize is not present the instant
## wait_for_idle returns.
wait_for_input <- function(app, id, timeout = 20000) {
  app$wait_for_js(
    sprintf("document.getElementById('%s') !== null", id),
    timeout = timeout
  )
}

## Activate a sidebar tab by its real link. Conditional items are inserted
## asynchronously, while static workspaces such as Linked views are available
## immediately; the same helper handles both.
activate_tab <- function(app, tab_name, timeout = 20000) {
  selector <- sprintf("a[href=\"#shiny-tab-%s\"]", tab_name)
  app$wait_for_js(
    sprintf("document.querySelector('%s') !== null", selector),
    timeout = timeout
  )
  app$run_js(sprintf("document.querySelector('%s').click();", selector))
}

test_that("{shinytest2} recording: overview", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "overview", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  ## Data Info tab: verify key values from the loaded example.crb
  cells_box <- retry_get_value(app, output = "load_data_number_of_cells")
  expect_true(grepl("1,?476", cells_box$html))

  organism_box <- retry_get_value(app, output = "load_data_organism")
  expect_true(grepl("hg", organism_box$html))

  date_box <- retry_get_value(app, output = "load_data_date_of_export")
  expect_true(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", date_box$html))

  app$stop()
})

test_that("the native sidebar scrolls and its mobile toggle still works", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "native_sidebar_scroll",
    height = 420,
    width = 600
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 20000)

  layout <- app$get_js(paste0(
    "(function () {",
    "var sidebar = document.querySelector('.main-sidebar');",
    "var links = Array.from(document.querySelectorAll('.sidebar-menu a'));",
    "var last = links[links.length - 1];",
    "var style = getComputedStyle(sidebar);",
    "return {",
    "fixedClass: document.body.classList.contains('fixed'),",
    "position: style.position, overflowY: style.overflowY,",
    "clientHeight: sidebar.clientHeight, scrollHeight: sidebar.scrollHeight,",
    "lastText: last ? last.textContent.trim() : '', viewport: innerHeight",
    "};",
    "})()"
  ))
  expect_false(layout$fixedClass)
  expect_identical(layout$position, "fixed")
  expect_match(layout$overflowY, "auto|scroll")
  expect_lte(layout$clientHeight, layout$viewport)
  expect_gt(layout$scrollHeight, layout$clientHeight)
  expect_identical(layout$lastText, "About")

  app$run_js(paste0(
    "document.querySelector('.main-sidebar').scrollTop = ",
    "document.querySelector('.main-sidebar').scrollHeight;"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelector('.main-sidebar').scrollTop > 0 && ",
      "Array.from(document.querySelectorAll('.sidebar-menu a')).slice(-1)[0]",
      ".getBoundingClientRect().bottom <= innerHeight"
    ),
    timeout = 5000
  )

  app$wait_for_js(
    "getComputedStyle(document.querySelector('.sidebar-toggle')).display !== 'none'",
    timeout = 5000
  )
  app$run_js("document.querySelector('.sidebar-toggle').click();")
  app$wait_for_js(
    "document.body.classList.contains('sidebar-open')",
    timeout = 5000
  )
  app$run_js("document.querySelector('.sidebar-toggle').click();")
  app$wait_for_js(
    "!document.body.classList.contains('sidebar-open')",
    timeout = 5000
  )

  logs <- app$get_logs()
  expect_false(any(grepl(
    "fixed layout requires the slimscroll plugin",
    as.character(logs$message),
    fixed = TRUE
  )))
})

test_that("Linked-view backgrounds and FOVs reset with the dataset", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "spatial_background_isolation",
    height = 950,
    width = 1619
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(
    crb_file_selector = "extdata/examples/demo_spatial_visium.crb",
    wait_ = FALSE
  )
  app$wait_for_idle(timeout = 30000)
  activate_tab(app, "coordinated_views", timeout = 30000)
  app$wait_for_js(
    paste0(
      "(function () {",
      "var spatial = document.getElementById('cv-pick-spatial');",
      "var image = document.getElementById('cv-img-pick');",
      "if (!spatial || !image) return false;",
      "var fovs = Array.from(spatial.options).map(function (x) { return x.value; });",
      "var images = Array.from(image.options).map(function (x) { return x.textContent; });",
      "return fovs.length === 1 && fovs[0] === 'anterior1' && ",
      "images.length === 2 && images[0] === 'None' && ",
      "images[1] === 'Tissue background' && image.value !== '__none__';",
      "})()"
    ),
    timeout = 30000
  )
  expect_null(app$get_js(
    "document.querySelector('a[href=\"#shiny-tab-spatial\"]')"
  ))
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.cv-ptitle')).some(",
    "function (x) { return x.textContent.indexOf('anterior1') >= 0; })"
  )))
  expect_identical(
    unlist(
      app$get_js(paste0(
        "Array.from(document.getElementById('cv-img-pick').options).map(",
        "function (x) { return x.textContent; })"
      )),
      use.names = FALSE
    ),
    c("None", "Tissue background")
  )

  app$set_inputs(
    crb_file_selector = "extdata/examples/demo_spatial_slideseq.crb",
    wait_ = FALSE
  )
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    paste0(
      "(function () {",
      "var spatial = document.getElementById('cv-pick-spatial');",
      "var image = document.getElementById('cv-img-pick');",
      "if (!spatial || !image) return false;",
      "var fovs = Array.from(spatial.options).map(function (x) { return x.value; });",
      "var images = Array.from(image.options).map(function (x) { return x.textContent; });",
      "return fovs.length === 1 && fovs[0] === 'image' && ",
      "images.length === 1 && images[0] === 'None' && ",
      "image.value === '__none__' && image.disabled;",
      "})()"
    ),
    timeout = 30000
  )
  expect_identical(
    unlist(
      app$get_js(paste0(
        "Array.from(document.getElementById('cv-pick-spatial').options).map(",
        "function (x) { return x.value; })"
      )),
      use.names = FALSE
    ),
    "image"
  )
  expect_identical(
    unlist(
      app$get_js(paste0(
        "Array.from(document.getElementById('cv-img-pick').options).map(",
        "function (x) { return x.textContent; })"
      )),
      use.names = FALSE
    ),
    "None"
  )
  expect_false(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.cv-ptitle')).some(",
    "function (x) { return x.textContent.indexOf('anterior1') >= 0; })"
  )))
})


test_that("{shinytest2} recording: Linked views", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "main", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  activate_tab(app, "coordinated_views")
  app$wait_for_js(
    paste0(
      "document.querySelectorAll(",
      "'.cv-panes .cv-pane'",
      ").length > 0"
    ),
    timeout = 20000
  )
  bundles <- retry_get_value(app, export = "coordviews_bundles_built")
  expect_gt(bundles, 0L)
  app$stop()
})


test_that("{shinytest2} recording: groups", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "groups", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "groups")
  app$wait_for_idle(timeout = 10000)

  ## composition plot renders
  plot_val <- retry_get_value(app, output = "groups_by_other_group_plot")
  expect_false(is.null(plot_val))

  ## switch to percent view
  app$set_inputs(groups_by_other_group_show_as_percent = TRUE)
  app$wait_for_idle(timeout = 10000)
  plot_pct <- retry_get_value(app, output = "groups_by_other_group_plot")
  expect_false(is.null(plot_pct))

  ## show table
  app$set_inputs(groups_by_other_group_show_table = TRUE)
  app$wait_for_idle(timeout = 10000)
  table_val <- retry_get_value(app, output = "groups_by_other_group_table")
  expect_false(is.null(table_val))

  app$expect_values(
    input = c(
      "groups_by_other_group_show_as_percent",
      "groups_by_other_group_show_table"
    ),
    output = FALSE,
    export = FALSE
  )
  app$stop()
})

test_that("{shinytest2} recording: marker_genes", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "marker_genes",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  # Marker genes is a conditionally + asynchronously inserted sidebar item
  # (insertConditionalTab): wait for it, then click, so it activates on a slow
  # runner instead of navigating before it exists.
  activate_tab(app, "markerGenes")
  app$wait_for_idle(timeout = 10000)

  ## select seurat_clusters (only group with actual marker genes)
  app$set_inputs(marker_genes_selected_table = "seurat_clusters", wait_ = FALSE)
  app$wait_for_idle(timeout = 10000)

  ## table renders — marker gene results render asynchronously, so the output can
  ## momentarily hold a shiny.silent.error (req() not yet satisfied) that
  ## serialises to non-JSON. Poll until the value parses as the expected DT
  ## payload instead of reading once and letting fromJSON choke on that transient
  ## error state.
  table_val <- retry_get_value(
    app,
    output = "marker_genes_table",
    validate = function(v) {
      !is.null(jsonlite::fromJSON(v, simplifyVector = FALSE)$x$container)
    }
  )
  expect_false(is.null(table_val))

  ## verify expected columns are present in the table header
  parsed <- jsonlite::fromJSON(table_val, simplifyVector = FALSE)
  container_html <- parsed$x$container
  for (col in c(
    "gene",
    "p_val",
    "avg_log2FC",
    "pct.1",
    "pct.2",
    "p_val_adj",
    "on_cell_surface"
  )) {
    expect_true(
      grepl(col, container_html, fixed = TRUE),
      label = paste("column present:", col)
    )
  }

  ## "no markers found" and "no data" messages should not be shown —
  ## table_or_text_UI should contain the table, not a text message
  ui_val <- retry_get_value(app, output = "marker_genes_table_or_text_UI")
  expect_false(grepl("no_markers_found|no_data", ui_val$html, fixed = FALSE))

  app$expect_values(
    input = c(
      "marker_genes_selected_method",
      "marker_genes_selected_table",
      "marker_genes_table_filter_switch"
    ),
    output = FALSE,
    export = FALSE
  )
  app$stop()
})


test_that("app startup does not eagerly load scRepertoire", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "no_screp_at_startup",
    height = 950,
    width = 1619
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 20000)

  ## Deferred-loading contract: the IR settings render on the first flush
  ## (suspendWhenHidden = FALSE), but startup must probe availability with
  ## system.file() only and never load the ~90-package scRepertoire tree. Only a
  ## real repertoire plot render is allowed to pull it in, so at a fresh startup
  ## with no repertoire tab opened the namespace must be absent.
  ##
  ## Strict identity (not `expect_false(isTRUE(...))`): a missing or NULL export
  ## must FAIL, not silently pass as though it were FALSE.
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_data_builds"), 0L)
  expect_identical(app$get_value(export = "ir_server_loaded"), FALSE)
  expect_identical(app$get_value(export = "trajectory_server_loaded"), FALSE)

  ## Delayed re-check: wait past the former one-second background-prewarm timer,
  ## so any deferred/prewarm-style loader would have fired by now. Still unloaded
  ## => genuinely lazy, not merely sampled before a callback ran.
  Sys.sleep(1.5)
  app$wait_for_idle(timeout = 10000)
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_data_builds"), 0L)
  expect_identical(app$get_value(export = "ir_server_loaded"), FALSE)
  expect_identical(app$get_value(export = "trajectory_server_loaded"), FALSE)
})

test_that("{shinytest2} recording: gene_expression", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "gene_expression",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  activate_tab(app, "geneExpression")
  app$wait_for_idle(timeout = 10000)

  ## projection UI renders without any gene selected
  proj_ui <- retry_get_value(app, output = "expression_projection_UI")
  expect_false(is.null(proj_ui))

  ## The gene selectize lives in a renderUI, so it is not bound the instant the
  ## tab goes idle. Wait for its element before set_inputs, or the input binding
  ## is "not found" and no gene is ever selected.
  wait_for_input(app, "expression_genes_input")
  Sys.sleep(1.1)
  expect_gt(
    app$get_js(paste0(
      "Object.keys(document.getElementById('expression_genes_input')",
      ".selectize.options).length"
    )),
    0
  )

  ## select MS4A1 and verify it is found in the data set
  app$set_inputs(expression_genes_input = "MS4A1", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)

  ## projection plot renders after gene selection
  proj_val <- retry_get_value(app, output = "expression_projection")
  expect_false(is.null(proj_val))

  ## verify expression levels have some non-zero values (cells with color)
  expr_levels <- retry_get_value(app, export = "expression_levels")
  expect_true(length(expr_levels) > 0)
  expect_true(any(expr_levels > 0))

  app$stop()
})

test_that("gene expression uses the race-safe server-side gene selector", {
  selector_source <- paste(
    readLines(file.path(
      inst_dir,
      "viewer/gene_expression/UI_projection_input_type.R"
    )),
    collapse = "\n"
  )
  expect_match(selector_source, "serverSideGeneSelector", fixed = TRUE)
})

test_that("{shinytest2} recording: gene_id_conversion", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "gene_id_conversion",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "geneIdConversion")
  app$wait_for_idle(timeout = 10000)

  table_val <- retry_get_value(app, output = "gene_info")
  expect_false(is.null(table_val))

  app$stop()
})

test_that("{shinytest2} recording: color_management", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "color_management",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "color_management")
  app$wait_for_idle(timeout = 10000)

  ui_val <- retry_get_value(app, output = "color_assignments_UI")
  expect_false(is.null(ui_val))
  app$wait_for_js(
    paste0(
      "document.querySelectorAll(",
      "'[id^=\"color_assignments_info_group_\"]').length > 0"
    ),
    timeout = 20000
  )
  info_ids <- app$get_js(paste0(
    "Array.from(document.querySelectorAll(",
    "'[id^=\"color_assignments_info_group_\"]')).map(function(button){",
    "return button.id;})"
  ))
  info_ids <- as.character(unlist(info_ids, use.names = FALSE))
  expect_gt(length(info_ids), 0L)
  expect_identical(length(unique(info_ids)), length(info_ids))
  expect_gt(
    app$get_js(paste0(
      "document.querySelectorAll('",
      ".cerebro-color-card .shiny-input-container[data-shiny-input-type=colour]",
      "').length"
    )),
    0L
  )
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('",
    ".cerebro-color-card input.shiny-colour-input",
    "')).every(function(input){return input.dataset.showColour==='both' && ",
    "/^#[0-9A-F]{6}$/i.test(input.value);})"
  )))
  app$run_js(
    "document.querySelector('[id^=\"color_assignments_info_group_\"]').click()"
  )
  app$wait_for_js(
    paste0(
      "(function(){var title=document.querySelector('.modal-title');return !!(",
      "title && title.textContent.indexOf('Colors for groups')!==-1);})()"
    ),
    timeout = 20000
  )

  app$stop()
})

test_that("{shinytest2} recording: about", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "about", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "about")
  app$wait_for_idle(timeout = 10000)

  about_text <- retry_get_value(app, output = "about")
  expect_false(is.null(about_text))
  expect_true(nchar(about_text) > 0)

  app$stop()
})

test_that("createShinyApp bundles a working app", {
  example <- system.file(
    "extdata/examples/example.crb",
    package = "CerebroNexus"
  )
  skip_if_not(nzchar(example), "example.crb not found")

  tmp <- file.path(tempdir(), "demo.crb")
  app_dir <- file.path(tempdir(), "test_create_app")
  file.copy(example, tmp, overwrite = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)

  createShinyApp(
    cerebro_data = c("mydata" = tmp),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  )

  # Freshly bundled createShinyApp app loads demo.crb at startup, so it is the
  # heaviest to initialise; the default 15s load_timeout is too tight on slow CI
  # runners. Give it 30s (other tabs use pre-built inst apps and idle faster).
  app <- AppDriver$new(
    app_dir,
    height = 950,
    width = 1619,
    load_timeout = 30000
  )
  app$wait_for_idle(timeout = 20000)

  cells <- retry_get_value(app, output = "load_data_number_of_cells")
  expect_true(grepl("1,?476", cells$html))

  app$stop()
})
