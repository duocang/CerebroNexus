# End-to-end Viewer validation helpers for full-source C2 runs.

bench_viewer_schedule <- function(schedule) {
  required <- c("profile", "source", "n_cells", "backend", "export_repeat")
  if (!all(required %in% names(schedule))) {
    stop("Viewer schedule is missing required columns", call. = FALSE)
  }
  rows <- schedule[
    schedule$profile == "panel_c2",
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
    "gene_secs",
    "linked_secs"
  )
  required <- c(
    "run_id",
    names(expected),
    "gene",
    "browser",
    "status",
    "correctness",
    "rendered_point_count",
    "navigator_gpu",
    "renderer_backend",
    "renderer_adapter",
    "renderer_context_lost",
    "renderer_error",
    "js_heap_mb",
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
  expected_profile <- expected$profile[match(result_keys, expected_keys)]
  if (any(is.na(results$profile) | results$profile != expected_profile)) {
    stop("Viewer result profile does not match the schedule", call. = FALSE)
  }
  if (!is.null(run_id) && any(results$run_id != run_id)) {
    stop("Viewer results do not share the phase run id", call. = FALSE)
  }
  if (any(results$status != "OK") || any(results$correctness != "OK")) {
    stop("one or more Viewer validations failed", call. = FALSE)
  }
  if (
    any(is.na(results$gene) | !nzchar(results$gene)) ||
      any(is.na(results$browser) | !nzchar(results$browser))
  ) {
    stop("Viewer gene or browser provenance is missing", call. = FALSE)
  }
  values <- as.matrix(results[timings])
  storage.mode(values) <- "double"
  if (any(!is.finite(values)) || any(values < 0)) {
    stop("Viewer timings must be finite and non-negative", call. = FALSE)
  }
  if (
    any(!is.finite(results$rendered_point_count)) ||
      any(results$rendered_point_count != results$n_cells)
  ) {
    stop("Viewer did not render every scheduled cell", call. = FALSE)
  }
  if (
    any(is.na(results$navigator_gpu) | !results$navigator_gpu) ||
      any(
        is.na(results$renderer_backend) |
          results$renderer_backend != "webgpu"
      )
  ) {
    stop("publication Viewer evidence requires WebGPU", call. = FALSE)
  }
  renderer_error <- ifelse(
    is.na(results$renderer_error),
    "",
    as.character(results$renderer_error)
  )
  if (
    any(
      is.na(results$renderer_context_lost) |
        results$renderer_context_lost
    ) ||
      any(nzchar(renderer_error))
  ) {
    stop("Viewer reported a GPU error or context loss", call. = FALSE)
  }
  if (any(!is.finite(results$js_heap_mb) | results$js_heap_mb <= 0)) {
    stop("Viewer JavaScript heap measurement is invalid", call. = FALSE)
  }
  invisible(TRUE)
}

bench_viewer_renderer <- function(app, root_selector) {
  app$get_js(sprintf(
    paste0(
      "(() => {const root=document.querySelector(%s);",
      "const canvases=Array.from(root?.querySelectorAll(",
      "'canvas:not(.cv-mini)')||[]).filter(canvas=>",
      "canvas.offsetParent!==null&&canvas.width>0&&canvas.height>0);",
      "const gpu=canvases.find(canvas=>canvas.classList.contains(",
      "'cv-gpu-layer')&&canvas.style.display!=='none');",
      "const renderer=gpu?._cerebroPointRenderer;",
      "const stats=renderer?.stats?.()||{};",
      "const points=(gpu||canvases.find(canvas=>canvas.dataset.pointCount))",
      "?.dataset.pointCount;return {navigatorGpu:!!navigator.gpu,",
      "pointCount:points==null?null:Number(points),",
      "backend:gpu?(stats.backend||'webgpu'):'canvas2d',",
      "adapter:String(stats.adapter||''),contextLost:!!stats.contextLost,",
      "error:String(stats.error||'')};})()"
    ),
    jsonlite::toJSON(root_selector, auto_unbox = TRUE)
  ))
}

bench_wait_viewer_renderer <- function(
  app,
  root_selector,
  expected_cells = NULL,
  require_webgpu = FALSE,
  timeout = 300000
) {
  expected <- if (is.null(expected_cells)) {
    "null"
  } else {
    format(as.numeric(expected_cells), scientific = FALSE, trim = TRUE)
  }
  app$wait_for_js(
    sprintf(
      paste0(
        "(() => {const root=document.querySelector(%s);",
        "const canvases=Array.from(root?.querySelectorAll(",
        "'canvas:not(.cv-mini)')||[]).filter(canvas=>",
        "canvas.offsetParent!==null&&canvas.width>0&&canvas.height>0);",
        "const gpu=canvases.find(canvas=>canvas.classList.contains(",
        "'cv-gpu-layer')&&canvas.style.display!=='none');",
        "const points=(gpu||canvases.find(canvas=>canvas.dataset.pointCount))",
        "?.dataset.pointCount;const count=Number(points);",
        "return Number.isFinite(count)&&(%s===null||count===%s)&&",
        "(!%s||!!gpu);})()"
      ),
      jsonlite::toJSON(root_selector, auto_unbox = TRUE),
      expected,
      expected,
      if (isTRUE(require_webgpu)) "true" else "false"
    ),
    timeout = timeout
  )
  diagnostics <- bench_viewer_renderer(app, root_selector)
  bench_require_viewer_renderer(
    diagnostics,
    expected_cells,
    require_webgpu,
    root_selector
  )
  diagnostics
}

bench_require_viewer_renderer <- function(
  diagnostics,
  expected_cells = NULL,
  require_webgpu = FALSE,
  label = "Viewer"
) {
  point_count <- as.numeric(diagnostics$pointCount)
  if (
    !is.null(expected_cells) &&
      (!is.finite(point_count) || point_count != expected_cells)
  ) {
    stop(label, " did not render every cell", call. = FALSE)
  }
  if (
    isTRUE(require_webgpu) &&
      (!isTRUE(diagnostics$navigatorGpu) ||
        !identical(diagnostics$backend, "webgpu"))
  ) {
    stop(label, " did not use WebGPU", call. = FALSE)
  }
  if (
    isTRUE(diagnostics$contextLost) ||
      nzchar(as.character(diagnostics$error))
  ) {
    stop(label, " reported a GPU error or context loss", call. = FALSE)
  }
  invisible(diagnostics)
}

bench_viewer_bad_logs <- function(logs) {
  message <- ifelse(is.na(logs$message), "", as.character(logs$message))
  location <- ifelse(is.na(logs$location), "", as.character(logs$location))
  level <- tolower(ifelse(is.na(logs$level), "", as.character(logs$level)))
  known_browser <- grepl(
    "the fixed layout requires the slimscroll plugin",
    message,
    fixed = TRUE
  )
  bad_browser <- location %in%
    c("browser", "chromote") &
    level %in% c("error", "severe") &
    !known_browser
  bad_server <- location == "shiny" &
    level == "stderr" &
    grepl(
      "(^|[[:space:]])(Warning: )?Error( in|:)|Execution halted|Unhandled",
      message
    )
  bad_browser | bad_server
}

bench_run_viewer_validation <- function(
  crb,
  app_dir,
  gene,
  expected_cells = NULL,
  require_webgpu = FALSE,
  timeout = 300000
) {
  now <- function() unname(proc.time()[["elapsed"]])
  stage <- "bundle"
  run <- function() {
    chrome <- chromote::find_chrome()
    browser <- paste(
      suppressWarnings(system2(
        chrome,
        "--version",
        stdout = TRUE,
        stderr = TRUE
      )),
      collapse = " "
    )
    if (!nzchar(browser)) {
      stop("Browser version is unavailable", call. = FALSE)
    }
    started <- now()
    CerebroNexus::createShinyApp(
      cerebro_data = c(benchmark = crb),
      result_dir = app_dir,
      launch_browser = FALSE,
      port = httpuv::randomPort(),
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
    overview_renderer <- bench_wait_viewer_renderer(
      app,
      "#shiny-tab-overview",
      expected_cells,
      require_webgpu,
      timeout
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
        "const i=(py*canvas.width+px)*4; ",
        "if (pixels[i+3] > 0 && ",
        "(Math.max(pixels[i],pixels[i+1],pixels[i+2])-",
        "Math.min(pixels[i],pixels[i+1],pixels[i+2]) > 40 || ",
        "Math.max(Math.abs(pixels[i]-pixels[0]),",
        "Math.abs(pixels[i+1]-pixels[1]),",
        "Math.abs(pixels[i+2]-pixels[2])) > 40)) { ",
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
    hover_point <- NULL
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
      if (hovered) {
        hover_point <- points[i, ]
        break
      }
    }
    if (!hovered) {
      stop("Canvas hover did not show a tooltip", call. = FALSE)
    }
    hover_secs <- now() - started

    stage <<- "selection"
    started <- now()
    app$get_js(paste0(
      "(() => { const host = document.getElementById(",
      "'overview_projection_cell_view_host'); ",
      "host.querySelector('.cv-tbtn[data-act=\"box\"]').click(); ",
      "return true; })()"
    ))
    padding <- min(20, geometry$width / 20, geometry$height / 20)
    drag <- list(
      x1 = max(geometry$left + 1, hover_point[[1L]] - padding),
      y1 = max(geometry$top + 1, hover_point[[2L]] - padding),
      x2 = min(geometry$left + geometry$width - 1, hover_point[[1L]] + padding),
      y2 = min(geometry$top + geometry$height - 1, hover_point[[2L]] + padding)
    )
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
        "Number(count?.querySelector('b')?.textContent.replace(/,/g, '')) > 0; ",
        "})()"
      ),
      timeout = timeout
    )
    selection_secs <- now() - started

    stage <<- "zoom"
    started <- now()
    zoom_button <- paste0(
      "#overview_projection_cell_view_host ",
      ".cv-pane:not(.cv-hidden) .cv-zsel-btn"
    )
    app$click(selector = zoom_button)
    app$wait_for_js(
      paste0(
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
    gene_json <- jsonlite::toJSON(gene, auto_unbox = TRUE)
    app$run_js(sprintf(
      paste0(
        "(() => {const s=document.getElementById(",
        "'expression_genes_input')?.selectize;",
        "if(!s)throw new Error('Gene input is not ready');",
        "s.addOption({value:%s,text:%s});s.setValue(%s);",
        "Shiny.setInputValue('expression_genes_input',%s,",
        "{priority:'event'});})()"
      ),
      gene_json,
      gene_json,
      gene_json,
      gene_json
    ))
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
    gene_renderer <- bench_wait_viewer_renderer(
      app,
      "#shiny-tab-geneExpression",
      expected_cells,
      require_webgpu,
      timeout
    )
    gene_secs <- now() - started

    stage <<- "linked"
    started <- now()
    app$click(selector = 'a[href="#shiny-tab-coordinated_views"]')
    app$wait_for_js(
      paste0(
        "(() => {const summary=window.cerebroLinkedViewsState?.summary?.();",
        "return summary?.ready===true&&summary?.complete===true;})()"
      ),
      timeout = timeout
    )
    linked_renderer <- bench_wait_viewer_renderer(
      app,
      "#shiny-tab-coordinated_views",
      expected_cells,
      require_webgpu,
      timeout
    )
    linked_secs <- now() - started

    js_heap_mb <- as.numeric(app$get_js(
      "Number(performance.memory?.usedJSHeapSize||0)/1048576"
    ))
    if (!is.finite(js_heap_mb) || js_heap_mb <= 0) {
      stop("JavaScript heap measurement is unavailable", call. = FALSE)
    }

    stage <<- "logs"
    logs <- app$get_logs()
    bad_logs <- bench_viewer_bad_logs(logs)
    if (any(bad_logs)) {
      stop(
        paste(logs$message[bad_logs], collapse = " | "),
        call. = FALSE
      )
    }

    stage <<- "shutdown"
    app$stop()
    app <- NULL

    list(
      correctness = "OK",
      browser = browser,
      bundle_secs = bundle_secs,
      launch_secs = launch_secs,
      hover_secs = hover_secs,
      selection_secs = selection_secs,
      zoom_secs = zoom_secs,
      gene_secs = gene_secs,
      linked_secs = linked_secs,
      rendered_point_count = as.numeric(overview_renderer$pointCount),
      navigator_gpu = isTRUE(overview_renderer$navigatorGpu),
      renderer_backend = as.character(overview_renderer$backend),
      renderer_adapter = as.character(overview_renderer$adapter),
      renderer_context_lost = isTRUE(overview_renderer$contextLost),
      renderer_error = as.character(overview_renderer$error),
      js_heap_mb = js_heap_mb
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
