args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
pkgload::load_all(root, quiet=TRUE)
writeLines(as.character(Sys.getpid()), args[[3]])
app <- launchCerebro(mode='closed', crb_file_to_load=c(Mouse=args[[2]]), percentage_cells_to_show=100)
ui <- app$httpHandler
out <- args[[5]]
original_server <- app$serverFuncSource()
app$serverFuncSource <- function() function(input,output,session) {
  shiny::observeEvent(input$bench_save, {
    jsonlite::write_json(input$bench_save,paste0(out,'.json'),auto_unbox=TRUE,pretty=TRUE)
    session$sendCustomMessage('bench_saved',TRUE)
  },ignoreInit=TRUE)
  original_server(input,output,session)
}
monitor <- callr::r_bg(function(pid,out) {
  peak <- 0
  repeat {
    p <- tryCatch(ps::ps_handle(pid),error=function(e)NULL)
    if(is.null(p)||!ps::ps_is_running(p)) break
    handles <- c(list(p),ps::ps_children(p,recursive=TRUE))
    rss <- sum(vapply(handles,function(x)tryCatch(ps::ps_memory_info(x)[['rss']],error=function(e)0),0))
    if (rss == 0) break
    peak <- max(peak,rss)
    writeLines(paste(rss,peak),paste0(out,'.rss'))
    Sys.sleep(.1)
  }
},args=list(Sys.getpid(),out))
on.exit(monitor$kill())
# This test wrapper only adds measurement listeners; all actions use the real UI.
original_ui <- app$ui
if (is.null(original_ui)) {
  # launchCerebro returns a Shiny app with a static UI handler. Wrap its HTML.
  app$httpHandler <- function(req) {
    response <- ui(req)
    if (is.list(response) && is.character(response$content) &&
        grepl('</body>', response$content, fixed=TRUE)) {
      script <- paste(readLines(file.path(root,'tests/bench/viewer-followup/brave/measure.js')),collapse='\n')
      response$content <- sub('</body>',paste0('<script>',script,'</script></body>'),response$content,fixed=TRUE)
    }
    response
  }
}
shiny::runApp(app, host='0.0.0.0', port=as.integer(args[[4]]), launch.browser=FALSE)
