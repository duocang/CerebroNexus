# Same production launch as interaction-feedback/server.R, with Ren for IR.
# No injected delays, renderer hooks or readiness instrumentation.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
data_dir <- normalizePath(args[[2]])
pkgload::load_all(root, quiet = TRUE)
app <- launchCerebro(mode = "closed", crb_file_to_load = c(
  Demo = file.path(root, "inst/extdata/examples/example.crb"),
  Mouse = file.path(data_dir, "cerebro_mouse_brain_1m_pages_clean_v1.crb"),
  Ren = file.path(data_dir, "cerebro_ren_covid_1.46m.crb")
), percentage_cells_to_show = 100)
shiny::runApp(app, host = "127.0.0.1", port = as.integer(args[[3]]),
  launch.browser = FALSE)
