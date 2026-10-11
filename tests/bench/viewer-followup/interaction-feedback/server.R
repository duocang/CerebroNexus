# Production UI smoke server. No injected timing/ready/renderer instrumentation.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
pkgload::load_all(root, quiet = TRUE)
datasets <- c(
  Mouse = args[[2]], Demo = file.path(root, "inst/extdata/examples/example.crb")
)
if (length(args) >= 4L) datasets <- c(datasets, Spatial = args[[4]])
if (length(args) >= 5L) datasets <- c(datasets, Trekker = args[[5]])
if (length(args) >= 6L) datasets <- c(datasets, HLA = args[[6]])
app <- launchCerebro(mode = "closed", crb_file_to_load = datasets,
  percentage_cells_to_show = 100)
shiny::runApp(app, host = "127.0.0.1", port = as.integer(args[[3]]), launch.browser = FALSE)
