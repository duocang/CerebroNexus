# Short native-Brave functional check; serve the production renderer unchanged.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
pkgload::load_all(root, quiet=TRUE)
writeLines(as.character(Sys.getpid()), args[[3]])
app <- launchCerebro(mode='closed', crb_file_to_load=c(Mouse=args[[2]],
  Demo=file.path(root,'inst/extdata/examples/example.crb')), percentage_cells_to_show=100)
handler <- app$httpHandler
app$httpHandler <- function(req) {
  response <- handler(req)
  if(is.list(response) && is.character(response$content) &&
      grepl('</body>',response$content,fixed=TRUE)) {
    script <- paste(readLines(file.path(root,
      'tests/bench/viewer-followup/linked-tooltip/probe.js')),collapse='\n')
    response$content <- sub('</body>',paste0('<script>',script,'</script></body>'),
      response$content,fixed=TRUE)
  }
  response
}
shiny::runApp(app,host='0.0.0.0',port=as.integer(args[[4]]),launch.browser=FALSE)
