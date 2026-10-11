# Production app plus passive click/Plotly/paint observations. No ready changes.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
out <- args[[4]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(root, quiet = TRUE)
writeLines(as.character(Sys.getpid()), file.path(out, "pid"))
files <- file.path(root, "inst/viewer/immune_repertoire", c("data.R", "settings.R", "visualizations.R"))
write.csv(data.frame(file = files, md5 = unname(tools::md5sum(files))), file.path(out, "source.csv"), row.names = FALSE)
app <- launchCerebro(mode = "closed", crb_file_to_load = c(Ren = args[[2]], Demo = file.path(root, "inst/extdata/examples/example.crb")), percentage_cells_to_show = 100)
server <- app$serverFuncSource()
app$serverFuncSource <- function() function(input, output, session) {
  shiny::observeEvent(input$ir_latency_evidence, {
    cat(jsonlite::toJSON(input$ir_latency_evidence, auto_unbox = TRUE), "\n", file = file.path(out, "browser.jsonl"), append = TRUE)
  }, ignoreInit = TRUE)
  server(input, output, session)
}
handler <- app$httpHandler
app$httpHandler <- function(req) {
  response <- handler(req)
  if (is.list(response) && is.character(response$content) && grepl("</body>", response$content, fixed = TRUE)) {
    script <- paste(readLines(file.path(root, "tests/bench/viewer-followup/ir-latency/probe.js")), collapse = "\n")
    response$content <- sub("</body>", paste0("<script>", script, "</script></body>"), response$content, fixed = TRUE)
  }
  response
}
shiny::runApp(app, host = "127.0.0.1", port = as.integer(args[[3]]), launch.browser = FALSE)
