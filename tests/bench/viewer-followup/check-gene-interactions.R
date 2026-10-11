# Functional probe: real Gene hover request and exact full tooltip recovery.
main <- function() {
  Sys.setenv(NOT_CRAN = "true")
  chromote::set_chrome_args(setdiff(chromote::get_chrome_args(),"--disable-gpu"))
  args <- commandArgs(TRUE)
  stopifnot(length(args) == 3L)
  root <- normalizePath(args[[1L]])
  crb <- normalizePath(args[[2L]])
  pkgload::load_all(root, quiet = TRUE)
  q <- function(x) encodeString(x, quote = '"')
  app_dir <- tempfile("gene-hover-"); dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  writeLines(c(sprintf("devtools::load_all(%s,quiet=TRUE)", q(root)),
    sprintf("launchCerebro(mode='closed',crb_file_to_load=c(Demo=%s),projections_show_hover_info=TRUE,percentage_cells_to_show=100)", q(crb))), file.path(app_dir,"app.R"))
  shinytest2::local_app_support(app_dir)
  app <- shinytest2::AppDriver$new(app_dir, width = 1619, height = 950, timeout = 120000, load_timeout = 120000)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  browser <- app$get_chromote_session()$parent
  on.exit(try(browser$close(), silent = TRUE), add = TRUE)
  app$run_js("window.__colorHash=null;const wire=window.CBViewWire;window.CBViewWire=Object.assign({},wire,{unpack:function(x){const m=wire.unpack(x);if(m.id==='expression_projection'&&m.data?.render_key?.includes('Gad1')){const s=m.data.sparse_color;if(s){const values=new Float64Array(s.length);s.index.forEach((i,k)=>{values[i]=s.color[k];});crypto.subtle.digest('SHA-256',values.buffer).then(h=>window.__colorHash=Array.from(new Uint8Array(h),x=>x.toString(16).padStart(2,'0')).join(''));}}return m;}});")
  app$run_js(paste0("window.__hoverRequests=[];window.__hoverDraws=[];",
    "const old=Shiny.setInputValue;Shiny.setInputValue=function(n,v,...rest){",
    "if(n==='cell_view_aux_request')__hoverRequests.push(v);return old.call(this,n,v,...rest);};",
    "window.addEventListener('cerebro:specialist-state',e=>{if(e.detail.viewId==='expression_projection')__hoverDraws.push(e.detail);});",
    "document.querySelector(\"a[href='#shiny-tab-geneExpression']\").click();"))
  app$wait_for_js("__hoverDraws.some(e=>e.eventKind==='primary')", timeout = 120000)
  app$wait_for_js("!!document.getElementById('expression_genes_input')?.selectize",timeout=30000)
  app$run_js("(()=>{const s=document.getElementById('expression_genes_input').selectize;s.addOption({value:'Gad1',text:'Gad1'});s.setValue('Gad1');})()")
  app$wait_for_js("__hoverDraws.some(e=>e.eventKind==='primary'&&e.renderKey?.includes('Gad1'))",timeout=120000)
  # Exercise demand loading before waiting for stable IDs. Blank-space pointer
  # motion does not count; scan canvas locations to hit an actual point.
  app$run_js("(()=>{const host=document.getElementById('expression_projection_cell_view_host');for(const c of host.querySelectorAll('.cv-canvas-wrap>canvas')){const r=c.getBoundingClientRect();for(let y=20;y<r.height-20;y+=20)for(let x=20;x<r.width-20;x+=20)c.dispatchEvent(new MouseEvent('mousemove',{clientX:r.left+x,clientY:r.top+y,bubbles:true}));}})()")
  app$wait_for_js("__hoverDraws.some(e=>e.eventKind==='aux')",timeout=120000)
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
  object <- readCerebro(crb)
  metadata <- object$getMetaData()
  expression <- as.numeric(object$getExpressionRow(gene="Gad1",cells=NULL))
  expected_hash <- digest::digest(writeBin(expression,raw(),size=8L,endian="little"),algo="sha256",serialize=FALSE)
  app$wait_for_js("!!__colorHash",timeout=30000)
  stopifnot(identical(app$get_js("__colorHash"),expected_hash))
  index <- match(cell, metadata$cell_barcode)
  stopifnot(length(index) == 1L, !is.na(index))
  for (field in c(nUMI = "Transcripts", nGene = "Expressed genes")) {
    line <- lines[startsWith(lines, paste0(field, ": "))]
    displayed <- as.numeric(gsub(",", "", substring(line, nchar(field) + 3L), fixed = TRUE))
    source <- if (field == "Transcripts") "nUMI" else "nGene"
    stopifnot(length(displayed) == 1L, displayed == round(metadata[[source]][index]))
  }
  app$run_js("document.querySelector('#expression_projection_cell_view_host .cv-tbtn[data-act=box]').click();")
  point <- app$get_js("({x:__hoverPoint.x,y:__hoverPoint.y,rect:__hoverPoint.canvas.getBoundingClientRect().toJSON()})")
  chrome <- app$get_chromote_session()
  chrome$Input$dispatchMouseEvent(type="mousePressed",x=max(point$x-25,point$rect$left+1),y=max(point$y-25,point$rect$top+1),button="left",buttons=1L,clickCount=1L)
  chrome$Input$dispatchMouseEvent(type="mouseMoved",x=point$x+25,y=point$y+25,button="left",buttons=1L)
  chrome$Input$dispatchMouseEvent(type="mouseReleased",x=point$x+25,y=point$y+25,button="left",buttons=0L,clickCount=1L)
  tryCatch(app$wait_for_js("cerebroCellViews.captureState('expression_projection')?.cells.length>0",timeout=10000),error=function(e){jsonlite::write_json(app$get_js("({state:cerebroCellViews.captureState('expression_projection'),selection:Shiny.shinyapp.$inputValues.expression_projection_persistent_selection,point:{x:__hoverPoint.x,y:__hoverPoint.y},rect:__hoverPoint.canvas.getBoundingClientRect().toJSON()})"),paste0(args[[3]],".failure.json"),auto_unbox=TRUE,pretty=TRUE);stop(e)})
  selection <- unlist(app$get_js("cerebroCellViews.captureState('expression_projection').cells"))
  selected_indices <- match(selection,metadata$cell_barcode)
  stopifnot(length(selection)>0L,!anyNA(selected_indices),!anyDuplicated(selected_indices),!is.unsorted(selected_indices))
  jsonlite::write_json(list(color_sha256=expected_hash,selected=length(selection),indices=selected_indices,tooltip=tooltip),paste0(args[[3L]],".json"),auto_unbox=TRUE)
  writeLines(c("PASS: actual Gene hover restores exact cell/count values; repeat deduplicated; box selection identities and canonical order", tooltip), args[[3L]])
  cat("Gene hover: PASS\n")
}
main()
