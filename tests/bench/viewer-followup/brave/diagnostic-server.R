# Reuse the frozen Linked wrapper, replacing only its final blocking runApp call.
expr <- parse('tests/bench/viewer-followup/brave/server.R')
stopifnot(identical(tail(expr,1)[[1]][[1]],quote(shiny::runApp)))
if(identical(Sys.getenv('BENCH_MULTI_DATASET'),'true')) {
  at <- which(vapply(expr,function(x)is.call(x)&&identical(x[[1]],quote(`<-`))&&identical(x[[2]],quote(app)),FALSE))
  stopifnot(length(at)==1L)
  expr[[at]][[3]][['crb_file_to_load']] <- quote(c(Mouse=args[[2]],Demo=file.path(root,'inst/extdata/examples/example.crb')))
}
setup <- head(expr,-1)
# on.exit in the reused top-level script would fire at the end of eval(),
# prematurely stopping its monitor in this wrapper.
setup <- setup[!vapply(setup,function(x)is.call(x)&&identical(x[[1]],quote(on.exit)),FALSE)]
eval(setup,envir=globalenv())
handler <- app$httpHandler
app$httpHandler <- function(req) {
  response <- handler(req)
  if(is.list(response)&&is.character(response$content)&&grepl('</body>',response$content,fixed=TRUE)) {
    script <- paste(readLines(file.path(root,'tests/bench/viewer-followup/brave/diagnostic.js')),collapse='\n')
    response$content <- sub('</body>',paste0('<script>',script,'</script></body>'),response$content,fixed=TRUE)
  }
  response
}
diagnostic_original <- app$serverFuncSource()
app$serverFuncSource <- function() function(input,output,session) {
  shiny::observeEvent(input$bench_diagnostic,{
    jsonlite::write_json(input$bench_diagnostic,paste0(out,'.diagnostic.json'),auto_unbox=TRUE,pretty=TRUE)
  },ignoreInit=TRUE)
  diagnostic_original(input,output,session)
}
shiny::runApp(app,host='0.0.0.0',port=as.integer(args[[4]]),launch.browser=FALSE)
