# Read-only profiling probes in a test process. Production functions and readiness
# are unchanged; R trace()/Rprof() supply timing rather than replacement wrappers.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
out <- args[[5]]
dir.create(dirname(out), recursive=TRUE, showWarnings=FALSE)
pkgload::load_all(root, quiet=TRUE)
writeLines(as.character(Sys.getpid()),args[[3]])
stamp <- function(name,phase,detail=NULL) {
  cat(jsonlite::toJSON(list(name=name,phase=phase,mono=nanonext::mclock(),
    cpu=sum(proc.time()[1:2])*1000,detail=detail),auto_unbox=TRUE),'\n',
    file=paste0(out,'.stages.jsonl'),append=TRUE)
}
wire_header <- function(value) {
  if(!is.raw(value)||length(value)<4)return(NULL)
  n <- readBin(value[1:4],integer(),n=1,size=4,endian='little')
  list(bytes=length(value),header=jsonlite::fromJSON(rawToChar(value[seq.int(5,4+n)]),simplifyVector=FALSE))
}
install_probes <- function(scope) {
  .bench_scope <<- scope
  trace('load_deferred_viewer_server',where=scope,print=FALSE,
    tracer=quote(stamp('module_load','start',server_file)),
    exit=quote({
      stamp('module_load','end',server_file)
      for(name in c('cv_prepare_progressive_supplement',
        'cv_build_progressive_supplement','cv_build_compact_supplement',
        'cv_build_trajectories','cv_canonical_metadata','cv_cell_ids',
        'cv_build_deferred_metadata','cv_build_spatial','cv_build_trekker',
        'cv_build_clone','cv_build_clone_rows','cv_async_clone_spec',
        'cv_start_clone_task','cv_send_progressive_supplement','cv_build_bundle_safe')) {
        if(exists(name,envir=server_scope,inherits=FALSE))
          trace(name,where=server_scope,print=FALSE,
            tracer=if(name=='cv_canonical_metadata') quote({
              stamp('cv_canonical_metadata','start'); force(metadata)
              stamp('metadata_argument','forced',list(rows=nrow(metadata),columns=ncol(metadata)))
            }) else bquote(stamp(.(name),'start')),
            exit=bquote(stamp(.(name),'end')))
      }
    }))

  targets <- c('cv_wire_pack_message','.hydrateThinCrbFields',
    'viewerProjectionFirstFrameMetadata','viewerPackTrajectoryFrame','viewerDatasetIdentity')
  for (name in targets) {
    env <- scope
    while (!exists(name,envir=env,inherits=FALSE) && !identical(env,emptyenv())) env <- parent.env(env)
    if (identical(env,emptyenv()) || environmentIsLocked(env)) next
    try(trace(name,where=env,print=FALSE,
      tracer=bquote(stamp(.(name),'start')),
      exit=if(name=='cv_wire_pack_message') quote(stamp('cv_wire_pack_message','end',wire_header(returnValue()))) else bquote(stamp(.(name),'end'))),silent=TRUE)
  }
  shiny::observeEvent(scope$input$coordviews_primary_ready, {
    stamp('supplement_input','received',scope$input$coordviews_primary_ready)
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
    jsonlite::write_json(input$remaining_save,paste0(out,'.json'),auto_unbox=TRUE,pretty=TRUE)
    session$sendCustomMessage('remaining_saved',TRUE)
  },ignoreInit=TRUE)
  Rprof(paste0(out,'.Rprof'),interval=.005,gc.profiling=TRUE)
  original_server(input,output,session)
}
shiny::addResourcePath("linked-diag",file.path(root,"tests/bench/viewer-followup/linked-tail"))
handler <- app$httpHandler
app$httpHandler <- function(req) {
  response <- handler(req)
  if(is.list(response)&&is.character(response$content)&&grepl('</body>',response$content,fixed=TRUE)) {
    script <- paste(readLines(file.path(root,'tests/bench/viewer-followup/linked-tail/probe.js')),collapse='\n')
    response$content <- sub('<head>','<head><script src="linked-diag/head.js"></script>',response$content,fixed=TRUE)
    response$content <- gsub('src="[^"]*cell_views[.]js[^"]*"','src="linked-diag/cell_views.js"',response$content)
    response$content <- sub('</body>' ,paste0('<script>',script,'</script></body>'),response$content,fixed=TRUE)
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
