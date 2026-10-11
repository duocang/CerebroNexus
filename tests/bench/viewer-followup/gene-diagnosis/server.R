# Read-only profiling probes in a test process. Production functions and readiness
# are unchanged; R trace()/Rprof() supply timing rather than replacement wrappers.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
options(shiny.reactlog=TRUE)
probe_dir <- file.path(root,"tests/bench/viewer-followup/gene-diagnosis")
out <- args[[5]]
dir.create(dirname(out), recursive=TRUE, showWarnings=FALSE)
pkgload::load_all(root, quiet=TRUE)
writeLines(as.character(Sys.getpid()),args[[3]])
stamp <- function(name,phase,detail=NULL) {
  cat(jsonlite::toJSON(list(name=name,phase=phase,mono=nanonext::mclock(),
    cpu=sum(proc.time()[1:2])*1000,detail=detail),auto_unbox=TRUE),'\n',
    file=paste0(out,'.stages.jsonl'),append=TRUE)
}
install_probes <- function(scope) {
  .bench_scope <<- scope
  targets <- c('viewerDatasetIdentity','viewerProjectionAsset','viewerExpressionCells',
    'viewerBpcellsValues','viewerExpressionValues','viewerExpressionRow',
    'expressionProjectionCachedRows','expression_projection_update_plot',
    'expressionSparseColor','expressionPackedColor','cv_wire_pack_message',
    'cerebroProjectionHoverColumns','cerebroCellViewDeferredAux',
    'plotExpressionSummary','compactViolinSummary','getMetaData','viewerProjectionFirstFrameMetadata','viewerPackValidateDescriptor')
  for (name in targets) {
    env <- scope
    while (!exists(name,envir=env,inherits=FALSE) && !identical(env,emptyenv())) env <- parent.env(env)
    if (identical(env,emptyenv()) || environmentIsLocked(env)) next
    try(trace(name,where=env,print=FALSE,
      tracer=if(name=='viewerExpressionRow') quote(stamp('viewerExpressionRow','start',list(gene=gene,n=length(cells),first=head(cells,1)))) else bquote(stamp(.(name),'start')),
      exit=bquote(stamp(.(name),'end'))),silent=TRUE)
  }
  shiny::observeEvent(scope$input$expression_genes_input, {
    stamp('gene_input','received',scope$input$expression_genes_input)
  },priority=9999,ignoreInit=TRUE)
}
app <- launchCerebro(mode='closed',crb_file_to_load=c(Mouse=args[[2]],
  Demo=file.path(root,'inst/extdata/examples/example.crb')),percentage_cells_to_show=100)
original_server <- app$serverFuncSource()
# trace's exit hook can inspect the session frame without altering its body.
trace('original_server',where=globalenv(),exit=quote(install_probes(environment())),print=FALSE)
app$serverFuncSource <- function() function(input,output,session) {
  shiny::observeEvent(input$remaining_clock,{
    session$sendCustomMessage('remaining_clock',list(id=input$remaining_clock,mono=nanonext::mclock()))
  },priority=10000,ignoreInit=TRUE)
  shiny::observeEvent(input$remaining_save,{
    Rprof(NULL)
    jsonlite::write_json(shiny::reactlog(),paste0(out,".reactlog.json"),auto_unbox=TRUE,pretty=FALSE)
    jsonlite::write_json(input$remaining_save,paste0(out,'.json'),auto_unbox=TRUE,pretty=TRUE)
    session$sendCustomMessage('remaining_saved',TRUE)
  },ignoreInit=TRUE)
  shiny::observeEvent(input$remaining_memory,{
    scope <- .bench_scope
    snapshot <- shiny::isolate({
      caches <- lapply(c('expression_projection_row_cache','.extra_material_process_cache'),function(n) {
        x <- get0(n,envir=scope,inherits=TRUE)
        if(!is.environment(x))return(NULL)
        keys <- ls(x,all.names=TRUE)
        list(name=n,entries=length(keys),bytes=sum(vapply(keys,function(k)as.numeric(object.size(get(k,x))),0)))
      })
      list(label=input$remaining_memory,gc=unname(gc()),rss=unname(ps::ps_memory_info()[['rss']]),caches=caches)
    })
    cat(jsonlite::toJSON(snapshot,auto_unbox=TRUE),'\n',file=paste0(out,'.memory.jsonl'),append=TRUE)
    session$sendCustomMessage('remaining_memory',snapshot)
  },ignoreInit=TRUE)
  Rprof(paste0(out,'.Rprof'),interval=.005)
  trace('manageInputs',where=session,print=FALSE,
    tracer=quote(stamp('manageInputs','start',list(names=names(data),gene=data[['expression_genes_input']],ack=data[['expression_projection_rendered_key']]))),
    exit=quote(stamp('manageInputs','end')))
  trace('sendBinaryMessage',where=session,print=FALSE,
    tracer=quote(stamp('binary_send','start',list(type=type,bytes=length(message)))),
    exit=quote(stamp('binary_send','end')))
  session$onFlush(function()stamp('flush','start'),once=FALSE)
  session$onFlushed(function()stamp('flush','end'),once=FALSE)
  original_server(input,output,session)
}
shiny::addResourcePath("gene-diag",probe_dir)
handler <- app$httpHandler
app$httpHandler <- function(req) {
  response <- handler(req)
  if(is.list(response)&&is.character(response$content)&&grepl('</body>',response$content,fixed=TRUE)) {
    script <- paste(readLines(file.path(probe_dir,'footer.js')),collapse='\n')
    response$content <- sub('<head>','<head><script src="gene-diag/head.js"></script>',response$content,fixed=TRUE)
    response$content <- gsub('src="[^"]*cell_views[.]js[^"]*"','src="gene-diag/cell_views.js"',response$content)
    response$content <- sub('</body>',paste0('<script>',script,'</script></body>'),response$content,fixed=TRUE)
  }
  response
}
monitor <- callr::r_bg(function(pid,out) {
  peak <- 0
  repeat {
    p <- tryCatch(ps::ps_handle(pid),error=function(e)NULL)
    if(is.null(p)||!ps::ps_is_running(p))break
    rss <- tryCatch(ps::ps_memory_info(p)[['rss']],error=function(e)0)
    if(rss<=0)break
    peak <- max(peak,rss);writeLines(paste(rss,peak),paste0(out,'.rss'));Sys.sleep(.1)
  }
},args=list(Sys.getpid(),out),supervise=TRUE)
shiny::runApp(app,host='0.0.0.0',port=as.integer(args[[4]]),launch.browser=FALSE)
