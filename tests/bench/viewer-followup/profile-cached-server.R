# Sampling only: leave all production functions and their bindings intact.
bench_cached_install <- function(scope) {
 shiny::observeEvent(scope$input$bench_cached_profile,{
  if(isTRUE(scope$input$bench_cached_profile)) Rprof(Sys.getenv("VIEWER_TRACE_FILE"),interval=.005)
  else Rprof(NULL)
  scope$session$sendCustomMessage("bench_cached_ack",scope$input$bench_cached_profile)
 },ignoreInit=TRUE,priority=9999)
}
trace("shinyApp",where=asNamespace("shiny"),print=FALSE,tracer=quote({
 body(server) <- bquote({.(body(server));bench_cached_install(environment())})
}))
