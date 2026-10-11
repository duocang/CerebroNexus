# Functional probe: real Gene hover request and exact full tooltip recovery.
main <- function() {
  Sys.setenv(NOT_CRAN = "true")
  args <- commandArgs(TRUE)
  stopifnot(length(args) == 3L)
  root <- normalizePath(args[[1L]])
  crb <- normalizePath(args[[2L]])
  pkgload::load_all(root, quiet = TRUE)
  q <- function(x) encodeString(x, quote = '"')
  app_dir <- tempfile("gene-hover-"); dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  writeLines(c(sprintf("devtools::load_all(%s,quiet=TRUE)", q(root)),
    sprintf("launchCerebro(mode='closed',crb_file_to_load=c(Demo=%s),projections_show_hover_info=TRUE)", q(crb))), file.path(app_dir,"app.R"))
  shinytest2::local_app_support(app_dir)
  app <- shinytest2::AppDriver$new(app_dir, width = 1619, height = 950, timeout = 120000, load_timeout = 120000)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  browser <- app$get_chromote_session()$parent
  on.exit(try(browser$close(), silent = TRUE), add = TRUE)
  app$run_js(paste0("window.__hoverRequests=[];window.__hoverDraws=[];",
    "const old=Shiny.setInputValue;Shiny.setInputValue=function(n,v,...rest){",
    "if(n==='cell_view_aux_request')__hoverRequests.push(v);return old.call(this,n,v,...rest);};",
    "window.addEventListener('cerebro:specialist-state',e=>{if(e.detail.viewId==='expression_projection')__hoverDraws.push(e.detail);});",
    "document.querySelector(\"a[href='#shiny-tab-geneExpression']\").click();"))
  app$wait_for_js("__hoverDraws.some(e=>e.eventKind==='primary')", timeout = 120000)
  # Wait for the selection core, whose auxiliary draw enables exact cell identity.
  app$wait_for_js("__hoverDraws.some(e=>e.eventKind==='aux')", timeout = 120000)
  stopifnot(identical(app$get_js("__hoverRequests.filter(x=>x.stage==='metadata').length"), 0L))
  found <- app$get_js(paste0("(()=>{const host=document.getElementById('expression_projection_cell_view_host');",
    "for(const c of host.querySelectorAll('.cv-canvas-wrap>canvas')){const r=c.getBoundingClientRect();",
    "for(let y=20;y<r.height-20;y+=10)for(let x=20;x<r.width-20;x+=10){",
    "c.dispatchEvent(new MouseEvent('mousemove',{clientX:r.left+x,clientY:r.top+y,bubbles:true}));",
    "if(Array.from(host.querySelectorAll('.cv-tip')).some(t=>Number(t.style.opacity)>0)){",
    "window.__hoverPoint={canvas:c,x:r.left+x,y:r.top+y};return true;}}}return false;})()"))
  stopifnot(isTRUE(found))
  app$wait_for_js("__hoverRequests.some(x=>x.stage==='metadata')", timeout = 30000)
  app$wait_for_js(paste0("Array.from(document.querySelectorAll('#expression_projection_cell_view_host .cv-tip'))",
    ".some(t=>Number(t.style.opacity)>0&&t.innerText.includes('Transcripts')&&t.innerText.includes('Expressed genes'))"), timeout = 120000)
  app$run_js("__hoverPoint.canvas.dispatchEvent(new MouseEvent('mousemove',{clientX:__hoverPoint.x,clientY:__hoverPoint.y,bubbles:true}));")
  stopifnot(identical(app$get_js("__hoverRequests.filter(x=>x.stage==='metadata').length"), 1L))
  tooltip <- app$get_js("Array.from(document.querySelectorAll('#expression_projection_cell_view_host .cv-tip')).find(t=>Number(t.style.opacity)>0)?.innerText")
  lines <- strsplit(tooltip, "\n", fixed = TRUE)[[1L]]
  cell <- sub("^Cell: ", "", lines[grepl("^Cell: ", lines)])
  metadata <- readCerebro(crb)$getMetaData()
  index <- match(cell, metadata$cell_barcode)
  stopifnot(length(index) == 1L, !is.na(index))
  for (field in c(nUMI = "Transcripts", nGene = "Expressed genes")) {
    line <- lines[startsWith(lines, paste0(field, ": "))]
    displayed <- as.numeric(gsub(",", "", substring(line, nchar(field) + 3L), fixed = TRUE))
    source <- if (field == "Transcripts") "nUMI" else "nGene"
    stopifnot(length(displayed) == 1L, displayed == round(metadata[[source]][index]))
  }
  writeLines(c("PASS: no eager Gene metadata; actual hover restores exact cell/count values; repeat deduplicated", tooltip), args[[3L]])
  cat("Gene hover: PASS\n")
}
main()
