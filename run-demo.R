#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  script <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    mustWork = TRUE
  )
  ## dirname() turns Windows separators into "/"; normalise again so both
  ## branches return the same form.
  normalizePath(dirname(script), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
## --ren-only, --cosmx-only and --tcell-only restrict the data sets to the ones
## named; --cosmx and --tcell add a data set to the default selection (the
## 1M mouse brain and Ren). Without any of them, behaviour is unchanged.
## Every prepared data set also gets a compressed twin of its H5 file (opened
## from the same menu as "<name> [compressed]"); --no-compressed skips it.
supported <- c(
  "--ren-only",
  "--cosmx-only",
  "--tcell-only",
  "--cosmx",
  "--tcell",
  "--prepare-only",
  "--no-browser",
  "--no-compressed"
)
if (any(!args %in% supported)) {
  stop("Supported options: ", paste(supported, collapse = ", "), call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first: install.packages('pkgload')", call. = FALSE)
}
## Always run this checkout, including after switching branches. An installed
## package from an earlier PR must not supply the data/backend implementation.
pkgload::load_all(repo_root, export_all = FALSE, quiet = TRUE, helpers = FALSE)
message("Viewer source: ", repo_root)
source(
  file.path(repo_root, "data-raw", "large-demo", "h5_transposed_write.R"),
  local = TRUE
)
source(
  file.path(repo_root, "data-raw", "large-demo", "prepare_viewer_1m_data.R"),
  local = TRUE
)
source(
  file.path(repo_root, "data-raw", "large-demo", "prepare_viewer_ren_data.R"),
  local = TRUE
)
source(
  file.path(repo_root, "data-raw", "large-demo", "download_helpers.R"),
  local = TRUE
)
source(
  file.path(repo_root, "data-raw", "large-demo", "prepare_viewer_cosmx_data.R"),
  local = TRUE
)
source(
  file.path(repo_root, "data-raw", "large-demo", "prepare_viewer_tcell_data.R"),
  local = TRUE
)
source(
  file.path(repo_root, "data-raw", "large-demo", "prepare_viewer_pack.R"),
  local = TRUE
)
source(
  file.path(repo_root, "data-raw", "large-demo", "h5_compressed_variant.R"),
  local = TRUE
)
## The uncompressed data set and, unless --no-compressed, its compressed twin.
prepareDemoPair <- function(crb) {
  prepareDemoViewerPack(crb)
  if (!"--no-compressed" %in% args) {
    prepareDemoViewerPack(prepareCompressedDemoVariant(crb))
  }
  invisible(crb)
}

only <- args[args %in% c("--ren-only", "--cosmx-only", "--tcell-only")]
selected <- if (length(only)) {
  c(
    ren = "--ren-only" %in% only,
    cosmx = "--cosmx-only" %in% only,
    tcell = "--tcell-only" %in% only
  )
} else {
  c(ren = TRUE, cosmx = FALSE, tcell = FALSE)
}
selected[["cosmx"]] <- selected[["cosmx"]] || "--cosmx" %in% args
selected[["tcell"]] <- selected[["tcell"]] || "--tcell" %in% args
selected_1m <- !length(only)

if (selected_1m) {
  crb <- prepareViewer1mBenchmarkData()
  prepareDemoPair(crb)
  Sys.setenv(CEREBRO_1M_DEMO_CRB = normalizePath(crb, mustWork = TRUE))
} else {
  Sys.unsetenv("CEREBRO_1M_DEMO_CRB")
}
## Each demo: its preparation function, the variable the Viewer reads and the
## version stamp (rebuilt when it changes).
demos <- list(
  ren = list(
    prepare = function() prepareViewerRenDemoData(),
    variable = "CEREBRO_REN_DEMO_CRB",
    label = "Ren data version",
    version = function() .viewerRenVersion
  ),
  cosmx = list(
    prepare = function() prepareViewerCosMxDemoData(),
    variable = "CEREBRO_COSMX_DEMO_CRB",
    label = "CosMx data version",
    version = function() .viewerCosMxVersion
  ),
  tcell = list(
    prepare = function() prepareViewerTcellDemoData(),
    variable = "CEREBRO_TCELL_DEMO_CRB",
    label = "T-cell data version",
    version = function() .viewerTcellVersion
  )
)
for (name in names(demos)) {
  demo <- demos[[name]]
  if (!selected[[name]]) {
    Sys.unsetenv(demo$variable)
    next
  }
  demo_crb <- demo$prepare()
  prepareDemoPair(demo_crb)
  do.call(Sys.setenv, stats::setNames(
    list(normalizePath(demo_crb, mustWork = TRUE)),
    demo$variable
  ))
  message(demo$label, ": ", demo$version(), "; file: ", demo_crb)
}
if ("--prepare-only" %in% args) {
  quit(save = "no", status = 0)
}

port <- tryCatch(
  httpuv::randomPort(min = 7451L, max = 7451L, n = 1L),
  error = function(error) httpuv::randomPort()
)
message(sprintf("Opening CerebroNexus at http://127.0.0.1:%d", port))

shiny::runApp(
  file.path(repo_root, "inst"),
  host = "127.0.0.1",
  port = port,
  launch.browser = !"--no-browser" %in% args
)
