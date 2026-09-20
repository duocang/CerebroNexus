# A deliberately small, independent Viewer smoke validation.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("usage: validate.R <result-dir> <file.crb> [more.crb ...]", call. = FALSE)
}
result_dir <- args[[1L]]
crbs <- args[-1L]
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
validation_library <- Sys.getenv("VIEWER_VALIDATION_LIBRARY")
if (nzchar(validation_library)) {
  .libPaths(c(validation_library, .libPaths()))
}

suppressPackageStartupMessages({
  library(CerebroNexus)
  library(shinytest2)
})

timeout <- as.numeric(Sys.getenv("VIEWER_VALIDATION_TIMEOUT", "300000"))
if (!is.finite(timeout) || timeout < 10000) {
  stop("VIEWER_VALIDATION_TIMEOUT must be at least 10000 milliseconds")
}

browser_version <- function() {
  chrome <- chromote::find_chrome()
  paste(suppressWarnings(system2(chrome, "--version", stdout = TRUE)), collapse = " ")
}

bad_logs <- function(logs) {
  if (!nrow(logs)) return(logical())
  message <- ifelse(is.na(logs$message), "", as.character(logs$message))
  location <- ifelse(is.na(logs$location), "", as.character(logs$location))
  level <- tolower(ifelse(is.na(logs$level), "", as.character(logs$level)))
  browser <- location %in% c("browser", "chromote") & level %in% c("error", "severe")
  server <- location == "shiny" & level == "stderr" &
    grepl("(^|[[:space:]])(Warning: )?Error( in|:)|Execution halted|Unhandled", message)
  browser | server
}

validation_state <- new.env(parent = emptyenv())

validate_one <- function(crb, index) {
  crb <- normalizePath(crb, mustWork = TRUE)
  label <- tools::file_path_sans_ext(basename(crb))
  safe_label <- gsub("[^A-Za-z0-9._-]", "-", label)
  app_dir <- tempfile(paste0("viewer-", index, "-"))
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)

  validation_state$stage <- "read"
  started <- proc.time()[["elapsed"]]
  object <- readCerebro(crb)
  n_cells <- ncol(object$expression)
  genes <- rownames(object$expression)
  if (!is.finite(n_cells) || n_cells < 1 || !length(genes)) {
    stop("CRB has no expression matrix dimensions", call. = FALSE)
  }
  gene <- genes[[max(1L, as.integer(length(genes) / 2L))]]
  read_secs <- proc.time()[["elapsed"]] - started

  validation_state$stage <- "bundle"
  started <- proc.time()[["elapsed"]]
  createShinyApp(
    cerebro_data = stats::setNames(crb, label),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE,
    cerebro_options = list(
      exclude_trivial_metadata = TRUE,
      projections_show_hover_info = TRUE
    ),
    initial_page = "projection"
  )
  bundle_secs <- proc.time()[["elapsed"]] - started

  validation_state$stage <- "launch"
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = paste0("viewer-validation-", index),
    height = 950,
    width = 1619,
    load_timeout = timeout,
    timeout = timeout,
    check_names = FALSE
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  started <- proc.time()[["elapsed"]]
  app$wait_for_js(
    paste0(
      "(() => {const host=document.getElementById('overview_projection_cell_view_host');",
      "const canvases=Array.from(host?.querySelectorAll('canvas:not(.cv-mini)')||[]);",
      "return canvases.some(c=>c.offsetParent!==null&&c.width>0&&c.height>0&&",
      "Number(c.dataset.pointCount)===", n_cells, ");})()"
    ),
    timeout = timeout
  )
  launch_secs <- proc.time()[["elapsed"]] - started
  screenshot <- file.path(
    result_dir,
    paste0(sprintf("%02d-", index), safe_label, "-overview.png")
  )
  app$get_screenshot(screenshot, selector = "#shiny-tab-overview")

  validation_state$stage <- "selection"
  geometry <- app$get_js(paste0(
    "(() => {const host=document.getElementById('overview_projection_cell_view_host');",
    "const canvas=Array.from(host?.querySelectorAll('canvas:not(.cv-mini)')||[])",
    ".find(c=>c.offsetParent!==null&&c.width>0&&c.height>0);",
    "if(!canvas)return null;const r=canvas.getBoundingClientRect();",
    "return {left:r.left,top:r.top,width:r.width,height:r.height};})()"
  ))
  if (is.null(geometry) || !is.finite(as.numeric(geometry$width))) {
    stop("Viewer exposed no interactive canvas", call. = FALSE)
  }
  app$get_js(paste0(
    "document.querySelector('#overview_projection_cell_view_host ",
    ".cv-tbtn[data-act=\"box\"]')?.click()"
  ))
  mouse <- app$get_chromote_session()$Input$dispatchMouseEvent
  x1 <- geometry$left + geometry$width * .2
  y1 <- geometry$top + geometry$height * .2
  x2 <- geometry$left + geometry$width * .8
  y2 <- geometry$top + geometry$height * .8
  mouse(type = "mouseMoved", x = x1, y = y1, button = "none", buttons = 0)
  mouse(type = "mousePressed", x = x1, y = y1, button = "left", buttons = 1)
  mouse(type = "mouseMoved", x = x2, y = y2, button = "left", buttons = 1)
  mouse(type = "mouseReleased", x = x2, y = y2, button = "left", buttons = 0)
  app$wait_for_js(
    paste0(
      "Number(document.getElementById('overview_number_of_selected_cells')?",
      ".querySelector('b')?.textContent.replace(/,/g,''))>0"
    ),
    timeout = timeout
  )

  validation_state$stage <- "zoom"
  app$click(selector = paste0(
    "#overview_projection_cell_view_host ",
    ".cv-pane:not(.cv-hidden) .cv-zsel-btn"
  ))
  app$wait_for_js(
    "document.querySelector('#overview_projection_cell_view_host .cv-mini.is-on')!==null",
    timeout = timeout
  )

  validation_state$stage <- "gene"
  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  app$wait_for_js(
    "document.getElementById('expression_genes_input')?.selectize!=null",
    timeout = timeout
  )
  gene_json <- jsonlite::toJSON(gene, auto_unbox = TRUE)
  app$run_js(sprintf(
    paste0(
      "(() => {const s=document.getElementById('expression_genes_input').selectize;",
      "s.addOption({value:%s,text:%s});s.setValue(%s);",
      "Shiny.setInputValue('expression_genes_input',%s,{priority:'event'});})()"
    ),
    gene_json, gene_json, gene_json, gene_json
  ))
  app$wait_for_js(
    paste0(
      "Array.from(document.querySelectorAll('#expression_projection_cell_view_host ",
      "canvas:not(.cv-mini)')).some(c=>c.offsetParent!==null&&c.width>0&&c.height>0&&",
      "Number(c.dataset.pointCount)===", n_cells, ")"
    ),
    timeout = timeout
  )

  validation_state$stage <- "logs"
  logs <- app$get_logs()
  bad <- bad_logs(logs)
  if (any(bad)) {
    stop(paste(logs$message[bad], collapse = " | "), call. = FALSE)
  }

  data.frame(
    artifact = crb,
    cells = n_cells,
    gene = gene,
    browser = browser_version(),
    status = "OK",
    failed_stage = "",
    detail = "",
    read_secs = read_secs,
    bundle_secs = bundle_secs,
    launch_secs = launch_secs,
    screenshot = normalizePath(screenshot),
    stringsAsFactors = FALSE
  )
}

rows <- lapply(seq_along(crbs), function(i) {
  validation_state$stage <- "setup"
  tryCatch(
    validate_one(crbs[[i]], i),
    error = function(error) data.frame(
      artifact = normalizePath(crbs[[i]], mustWork = FALSE),
      cells = NA_real_, gene = NA_character_, browser = NA_character_,
      status = "FAILED", failed_stage = validation_state$stage,
      detail = conditionMessage(error), read_secs = NA_real_,
      bundle_secs = NA_real_, launch_secs = NA_real_, screenshot = NA_character_,
      stringsAsFactors = FALSE
    )
  )
})
results <- do.call(rbind, rows)
utils::write.csv(results, file.path(result_dir, "viewer_validation.csv"), row.names = FALSE)

lines <- c(
  "# Viewer validation",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  "",
  "This is an independent functional smoke validation. It is not part of the expression-backend benchmark and does not update its `CURRENT` pointer.",
  "",
  "| artifact | cells | status | failed stage |",
  "|---|---:|---|---|"
)
for (i in seq_len(nrow(results))) {
  lines <- c(lines, sprintf(
    "| %s | %s | %s | %s |",
    basename(results$artifact[[i]]),
    if (is.na(results$cells[[i]])) "" else format(results$cells[[i]], big.mark = ","),
    results$status[[i]], results$failed_stage[[i]]
  ))
}
writeLines(lines, file.path(result_dir, "summary.md"))
print(results[, c("artifact", "cells", "status", "failed_stage")], row.names = FALSE)
if (any(results$status != "OK")) quit(status = 1L)
