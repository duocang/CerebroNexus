# Supplemental operation protocol v2. Separate series from the frozen page-entry harness.
main <- function() {
  args <- commandArgs(TRUE)
  stopifnot(length(args) == 3L)
  root <- normalizePath(args[[1L]])
  crb <- normalizePath(args[[2L]])
  out <- normalizePath(args[[3L]], mustWork = FALSE)
  Sys.setenv(NOT_CRAN = "true")
  chromote::set_chrome_args(setdiff(chromote::get_chrome_args(), "--disable-gpu"))
  pkgload::load_all(root, quiet = TRUE)
  object <- readCerebro(crb)
  genes <- intersect(c("Gad1", "Slc17a7", "Snap25", "MALAT1", "ACTB", "GAPDH"), object$getGeneNames())
  if (length(genes) < 3L) genes <- head(object$getGeneNames(), 3L)
  genes <- head(genes, 3L)
  expected <- nrow(object$getMetaData())
  rm(object)
  q <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  app_dir <- tempfile("viewer-operations-")
  dir.create(app_dir)
  writeLines(c(sprintf("devtools::load_all(%s,quiet=TRUE)", q(root)),
    sprintf("launchCerebro(mode='closed',crb_file_to_load=c(Operations=%s),percentage_cells_to_show=100,projections_show_hover_info=TRUE)", q(crb))),
    file.path(app_dir, "app.R"))
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  shinytest2::local_app_support(app_dir)
  app <- shinytest2::AppDriver$new(app_dir, width = 1619, height = 950,
    name = "gene_operations_v2", timeout = 120000, load_timeout = 120000)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  chrome <- app$get_chromote_session()
  on.exit(try(chrome$parent$close(), silent = TRUE), add = TRUE)
  git_args <- c("-C", root)
  marker <- file.path(root, ".git")
  if (.Platform$OS.type == "unix" && file.exists(marker) && !dir.exists(marker)) {
    git_dir <- sub("^gitdir: ", "", readLines(marker, n = 1L, warn = FALSE))
    if (grepl("^[A-Za-z]:/", git_dir)) git_dir <- paste0("/mnt/", tolower(substr(git_dir, 1L, 1L)), substring(git_dir, 3L))
    git_args <- c("-c", "core.autocrlf=true", paste0("--git-dir=", git_dir), paste0("--work-tree=", root))
  }
  manifest <- list(protocol = "gene-operations-v2", source_sha = system2("git", c(shQuote(git_args), "rev-parse", "HEAD"), stdout = TRUE),
    dataset_sha256 = digest::digest(file = crb, algo = "sha256"),
    harness_sha256 = digest::digest(file = file.path(root, "tests/bench/viewer-final-protocol/probe-gene-operations.R"), algo = "sha256"),
    browser = chrome$Browser$getVersion()$product, viewport = c(1619, 950),
    readiness = "new primary draw event plus expected label and panel count; complete unmeasured", session_info = capture.output(sessionInfo()))
  jsonlite::write_json(manifest, paste0(out, ".manifest.json"), pretty = TRUE, auto_unbox = TRUE)
  app$wait_for_value(output = "load_data_number_of_cells", timeout = 120000)
  app$run_js(paste0(
    "window.__ops={events:[],bytes:0,messages:0,last:performance.now()};",
    "window.addEventListener('cerebro:specialist-state',e=>{if(e.detail.viewId==='expression_projection')__ops.events.push({at:performance.now(),detail:e.detail,text:document.getElementById('expression_projection_cell_view_host')?.innerText||'',panels:Array.from(document.querySelectorAll('#expression_projection_cell_view_host .cv-pane')).filter(p=>p.offsetParent!==null&&!p.classList.contains('cv-hidden')).length});});",
    "Shiny.shinyapp.$socket.addEventListener('message',e=>{__ops.bytes+=typeof e.data==='string'?new TextEncoder().encode(e.data).length:(e.data.byteLength||e.data.size||0);__ops.messages++;__ops.last=performance.now();});",
    "document.querySelector(\"a[href='#shiny-tab-geneExpression']\").click();"))
  app$wait_for_js("__ops.events.some(e=>e.detail.eventKind==='primary')", timeout = 120000)
  app$wait_for_js("!!document.getElementById('expression_genes_input')?.selectize", timeout = 30000)
  app$wait_for_js("performance.now()-__ops.last>1000", timeout = 120000)
  rows <- list()
  select <- function(id, value) paste0("(()=>{const e=document.getElementById(", q(id),
    ");if(!e?.selectize)throw new Error('selector unavailable');const v=", q(value),
    ";(Array.isArray(v)?v:[v]).filter(Boolean).forEach(x=>e.selectize.addOption({value:x,text:x}));",
    "e.selectize.setValue(v);})()")
  measure <- function(name, action, text = NULL, panels = 1L) {
    app$run_js(paste0("__ops.start=performance.now();__ops.events=[];__ops.bytes=0;__ops.messages=0;", action))
    condition <- paste0("e.at>=__ops.start&&e.detail.eventKind==='primary'&&e.detail.renderedPointCount===", expected)
    if (!is.null(text)) condition <- paste0(condition, "&&e.text.includes(", q(text), ")")
    if (panels > 1L) condition <- paste0(condition, "&&e.panels===", panels)
    status <- tryCatch({app$wait_for_js(paste0("__ops.events.some(e=>", condition, ")"), timeout = 120000); "ok"}, error = function(e) conditionMessage(e))
    primary <- app$get_js(paste0("__ops.events.find(e=>", condition, ")||null"))
    if (identical(status, "ok")) app$wait_for_js("performance.now()-__ops.last>1000", timeout = 120000)
    state <- app$get_js("({start:__ops.start,quiet:__ops.last+1000,bytes:__ops.bytes,messages:__ops.messages,errors:Array.from(document.querySelectorAll('.shiny-output-error')).filter(e=>e.offsetParent!==null).map(e=>e.innerText)})")
    timing <- primary$detail$timing
    val <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
    row <- data.frame(operation = name, status = status, genes = paste(genes, collapse = ","),
      cells = expected, first_usable_ms = val(primary$at) - state$start,
      complete_ms = NA_real_, quiet_ms = state$quiet - state$start,
      # Existing specialist timing survives color updates and is page-scoped.
      # It is not an operation-stage measurement, so never reuse it here.
      click_to_request_ms = NA_real_, server_prepare_ms = NA_real_, transfer_ms = NA_real_,
      decode_ms = NA_real_, first_draw_ms = NA_real_, primary_payload = NA_real_,
      websocket_bytes = state$bytes,
      message_count = state$messages, visible_errors = paste(state$errors, collapse = " | "))
    rows[[length(rows) + 1L]] <<- row
    write.table(do.call(rbind, rows), out, sep = "\t", row.names = FALSE, quote = TRUE)
    print(row)
    if (!identical(status, "ok") || nzchar(row$visible_errors)) stop("Operation failed: ", name)
  }
  measure("single_first", select("expression_genes_input", genes[[1L]]), genes[[1L]])
  measure("single_second_gene", select("expression_genes_input", genes[[2L]]), genes[[2L]])
  measure("single_cached_return", select("expression_genes_input", genes[[1L]]), genes[[1L]])
  measure("mean_three", select("expression_genes_input", genes), "Mean expression (3 genes)")
  measure("separate_three", select("expression_projection_genes_in_separate_panels", "separate"), panels = 3L)
  measure("rgb_three", select("expression_projection_genes_in_separate_panels", "rgb"), "Each channel is scaled independently")
}
main()
