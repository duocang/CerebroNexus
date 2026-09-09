# End-to-end Viewer validation helpers for full-source C2 runs.

bench_viewer_schedule <- function(schedule) {
  required <- c("profile", "source", "n_cells", "backend", "export_repeat")
  if (!all(required %in% names(schedule))) {
    stop("Viewer schedule is missing required columns", call. = FALSE)
  }
  rows <- schedule[
    schedule$profile == "panel_c2" & schedule$export_repeat == 1L,
    required,
    drop = FALSE
  ]
  key <- paste(rows$source, rows$n_cells, rows$backend, rows$export_repeat)
  if (!nrow(rows) || anyDuplicated(key)) {
    stop("Viewer schedule must contain unique C2 rows", call. = FALSE)
  }
  rownames(rows) <- NULL
  rows
}

bench_validate_viewer_results <- function(schedule, results, run_id = NULL) {
  expected <- bench_viewer_schedule(schedule)
  timings <- c(
    "bundle_secs",
    "launch_secs",
    "hover_secs",
    "selection_secs",
    "zoom_secs",
    "gene_secs"
  )
  required <- c(
    "run_id",
    names(expected),
    "status",
    "correctness",
    timings
  )
  if (!all(required %in% names(results))) {
    stop("Viewer results are missing required columns", call. = FALSE)
  }
  key <- function(rows) {
    paste(rows$source, rows$n_cells, rows$backend, rows$export_repeat)
  }
  expected_keys <- key(expected)
  result_keys <- key(results)
  if (!setequal(expected_keys, result_keys) || anyDuplicated(result_keys)) {
    stop(
      "Viewer result set does not cover the scheduled C2 rows",
      call. = FALSE
    )
  }
  if (!is.null(run_id) && any(results$run_id != run_id)) {
    stop("Viewer results do not share the phase run id", call. = FALSE)
  }
  if (any(results$status != "OK") || any(results$correctness != "OK")) {
    stop("one or more Viewer validations failed", call. = FALSE)
  }
  values <- as.matrix(results[timings])
  storage.mode(values) <- "double"
  if (any(!is.finite(values)) || any(values < 0)) {
    stop("Viewer timings must be finite and non-negative", call. = FALSE)
  }
  invisible(TRUE)
}

bench_run_viewer_validation <- function(
  crb,
  app_dir,
  gene,
  timeout = 300000
) {
  now <- function() unname(proc.time()[["elapsed"]])
  stage <- "bundle"
  run <- function() {
    started <- now()
    CerebroNexus::createShinyApp(
      cerebro_data = c(benchmark = crb),
      result_dir = app_dir,
      launch_browser = FALSE,
      verbose = FALSE,
      cerebro_options = list(
        exclude_trivial_metadata = TRUE,
        projections_show_hover_info = TRUE
      ),
      initial_page = "projection"
    )
    bundle_secs <- now() - started

    stage <<- "launch"
    shinytest2::local_app_support(app_dir)
    app <- NULL
    on.exit(if (!is.null(app)) app$stop(), add = TRUE)
    started <- now()
    app <- shinytest2::AppDriver$new(
      app_dir,
      name = "c2_viewer",
      height = 950,
      width = 1619,
      load_timeout = timeout
    )
    canvas <- paste0(
      "#overview_projection_cell_view_host ",
      ".cv-canvas-wrap > canvas:not(.cv-mini)"
    )
    app$wait_for_js(
      sprintf(
        paste0(
          "document.querySelector('%s')?.getBoundingClientRect().width > 0 && ",
          "window.cerebroCellViews?.captureState(",
          "'overview_projection') !== null"
        ),
        canvas
      ),
      timeout = timeout
    )
    launch_secs <- now() - started

    stage <<- "hover"
    started <- now()
    geometry <- app$get_js(sprintf(
      paste0(
        "(() => { const canvas = document.querySelector('%s'); ",
        "const r = canvas.getBoundingClientRect(); ",
        "const pixels = canvas.getContext('2d').getImageData(",
        "0, 0, canvas.width, canvas.height).data; ",
        "const hits = []; ",
        "for (let gy = 0; gy < 8; gy++) { ",
        "for (let gx = 0; gx < 10; gx++) { ",
        "let found = false; ",
        "for (let py = Math.floor(gy*canvas.height/8); ",
        "py < Math.floor((gy+1)*canvas.height/8) && !found; py += 2) { ",
        "for (let px = Math.floor(gx*canvas.width/10); ",
        "px < Math.floor((gx+1)*canvas.width/10); px += 2) { ",
        "if (pixels[(py*canvas.width+px)*4+3] > 0) { ",
        "hits.push([r.left+px/canvas.width*r.width,",
        "r.top+py/canvas.height*r.height]); found = true; break; } } } } } ",
        "hits.sort((a,b) => ",
        "Math.hypot(a[0]-r.left-r.width/2,a[1]-r.top-r.height/2) - ",
        "Math.hypot(b[0]-r.left-r.width/2,b[1]-r.top-r.height/2)); ",
        "return {left:r.left,top:r.top,width:r.width,height:r.height,",
        "hits:hits}; })()"
      ),
      canvas
    ))
    mouse <- app$get_chromote_session()$Input$dispatchMouseEvent
    pixel_points <- do.call(rbind, lapply(geometry$hits, unlist))
    points <- rbind(
      pixel_points,
      c(
        geometry$left + geometry$width * .5,
        geometry$top + geometry$height * .5
      ),
      c(
        geometry$left + geometry$width * .35,
        geometry$top + geometry$height * .35
      ),
      c(
        geometry$left + geometry$width * .65,
        geometry$top + geometry$height * .65
      )
    )
    hovered <- FALSE
    for (i in seq_len(nrow(points))) {
      mouse(
        type = "mouseMoved",
        x = points[i, 1],
        y = points[i, 2],
        button = "none",
        buttons = 0
      )
      hovered <- isTRUE(app$get_js(paste0(
        "Array.from(document.querySelectorAll(",
        "'#overview_projection_cell_view_host .cv-tip')).some(",
        "tip => Number(getComputedStyle(tip).opacity) > 0 && ",
        "tip.textContent.trim().length > 0)"
      )))
      if (hovered) break
    }
    if (!hovered) {
      stop("Canvas hover did not show a tooltip", call. = FALSE)
    }
    hover_secs <- now() - started

    stage <<- "selection"
    started <- now()
    drag <- app$get_js(sprintf(
      paste0(
        "(() => { const host = document.getElementById(",
        "'overview_projection_cell_view_host'); ",
        "host.querySelector('.cv-tbtn[data-act=\"box\"]').click(); ",
        "const r = document.querySelector('%s').getBoundingClientRect(); ",
        "return {x1:r.left+r.width*.3,y1:r.top+r.height*.3,",
        "x2:r.right-r.width*.3,y2:r.bottom-r.height*.3}; })()"
      ),
      canvas
    ))
    mouse(
      type = "mousePressed",
      x = drag$x1,
      y = drag$y1,
      button = "left",
      buttons = 1,
      clickCount = 1
    )
    mouse(
      type = "mouseMoved",
      x = drag$x2,
      y = drag$y2,
      button = "left",
      buttons = 1
    )
    mouse(
      type = "mouseReleased",
      x = drag$x2,
      y = drag$y2,
      button = "left",
      buttons = 0,
      clickCount = 1
    )
    app$wait_for_js(
      paste0(
        "(() => { const active = document.getElementById(",
        "'overview_projection_selection_active'); ",
        "const count = document.getElementById(",
        "'overview_number_of_selected_cells'); ",
        "return active && !active.classList.contains(",
        "'cerebro-selection-status-hidden') && ",
        "count?.textContent.includes('Selected'); })()"
      ),
      timeout = timeout
    )
    selection_secs <- now() - started

    stage <<- "zoom"
    started <- now()
    app$click(selector = "#overview_projection_zoom_to_selection")
    app$wait_for_js(
      paste0(
        "document.getElementById(",
        "'overview_projection_zoom_to_selection')?.",
        "getAttribute('aria-pressed') === 'true' && ",
        "document.querySelector(",
        "'#overview_projection_cell_view_host .cv-mini.is-on') !== null"
      ),
      timeout = timeout
    )
    zoom_secs <- now() - started

    stage <<- "gene"
    started <- now()
    app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
    app$wait_for_js(
      "document.getElementById('expression_genes_input') !== null",
      timeout = timeout
    )
    app$set_inputs(expression_genes_input = gene, wait_ = FALSE)
    app$wait_for_js(
      paste0(
        "document.querySelector(",
        "'#expression_projection_cell_view_host ",
        "canvas:not(.cv-mini)') !== null"
      ),
      timeout = timeout
    )
    deadline <- Sys.time() + timeout / 1000
    expression <- NULL
    repeat {
      expression <- tryCatch(
        app$get_value(export = "expression_levels"),
        error = function(error) NULL
      )
      if (length(expression) && any(unlist(expression) > 0)) {
        break
      }
      if (Sys.time() > deadline) {
        stop("Gene switch returned no positive expression", call. = FALSE)
      }
      Sys.sleep(.25)
    }
    gene_secs <- now() - started

    stage <<- "logs"
    logs <- app$get_logs()
    known_browser <- grepl(
      "the fixed layout requires the slimscroll plugin",
      logs$message,
      fixed = TRUE
    )
    bad_browser <- logs$location %in%
      c("browser", "chromote") &
      tolower(logs$level) %in% c("error", "severe") &
      !known_browser
    bad_server <- logs$location == "shiny" &
      logs$level == "stderr" &
      grepl("(^Error|Execution halted|Unhandled)", logs$message)
    if (any(bad_browser | bad_server)) {
      stop(
        paste(logs$message[bad_browser | bad_server], collapse = " | "),
        call. = FALSE
      )
    }

    stage <<- "shutdown"
    app$stop()
    app <- NULL

    list(
      correctness = "OK",
      bundle_secs = bundle_secs,
      launch_secs = launch_secs,
      hover_secs = hover_secs,
      selection_secs = selection_secs,
      zoom_secs = zoom_secs,
      gene_secs = gene_secs
    )
  }

  tryCatch(
    run(),
    error = function(error) {
      error$stage <- stage
      class(error) <- c("bench_viewer_error", class(error))
      stop(error)
    }
  )
}
