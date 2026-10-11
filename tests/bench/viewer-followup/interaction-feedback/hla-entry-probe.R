# Narrow, read-only HLA entry timing; no browser injection or readiness changes.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
pkgload::load_all(root, quiet = TRUE)
out <- args[[3]]
stamp <- function(name, phase, detail = NULL) {
  cat(jsonlite::toJSON(list(name = name, phase = phase,
    elapsed = unname(proc.time()[[3]]), detail = detail), auto_unbox = TRUE),
    "\n", file = out, append = TRUE)
}
install_hla_probes <- function(scope) {
  if (!exists("hla_tcr_chains", scope, inherits = FALSE) ||
      exists(".hla_probe_installed", scope, inherits = FALSE)) return()
  scope$.hla_probe_installed <- TRUE
  for (name in c("hla_tcr_chains", "hla_segments", "hla_default_min_nodes",
      "hla_params_ready", "hla_motif_graph", "hla_motif_graph_raw_cached",
      "hla_visnet")) {
    trace(".func", where = attr(scope[[name]], "observable"), print = FALSE,
      tracer = bquote(stamp(.(name), "start")),
      exit = bquote(stamp(.(name), "end")))
  }
}
install_probes <- function(scope) {
  trace("load_deferred_viewer_server", where = scope, print = FALSE,
    tracer = quote(stamp("module", "start", server_file)),
    exit = quote({stamp("module", "end", server_file); install_hla_probes(server_scope)}))
  for (key in c("sidebar", "hla_color_by", "hla_min_nodes", "hla_by_v", "hla_show_isolated")) local({
    id <- key
    shiny::observeEvent(scope$input[[id]], {
      stamp(id, "input", scope$input[[id]])
    }, priority = 1000, ignoreInit = FALSE)
  })
}
app <- launchCerebro(mode = "closed", crb_file_to_load = c(
  Demo = file.path(root, "inst/extdata/examples/example.crb")), percentage_cells_to_show = 100)
original_server <- app$serverFuncSource()
trace("original_server", where = globalenv(), print = FALSE,
  exit = quote(install_probes(environment())))
app$serverFuncSource <- function() function(input, output, session) {
  session$onFlush(function() stamp("flush", "start"), once = FALSE)
  session$onFlushed(function() stamp("flush", "end"), once = FALSE)
  original_server(input, output, session)
}
shiny::runApp(app, host = "127.0.0.1", port = as.integer(args[[2]]), launch.browser = FALSE)
