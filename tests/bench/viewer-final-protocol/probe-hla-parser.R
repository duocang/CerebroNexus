args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L || length(args) > 4L) {
  stop("usage: Rscript probe-hla-parser.R ROOT REN_CRB BASELINE_SOURCE [baseline|candidate|both]")
}
perf_root <- normalizePath(args[[1L]])
crb <- normalizePath(args[[2L]])
baseline_file <- normalizePath(args[[3L]])
mode <- if (length(args) == 4L) args[[4L]] else "both"
stopifnot(mode %in% c("baseline", "candidate", "both"))
pkgload::load_all(perf_root, quiet = TRUE)
baseline <- new.env(parent = globalenv())
candidate <- new.env(parent = globalenv())
sys.source(baseline_file, envir = baseline)
sys.source(file.path(perf_root, "R/hla_motif_core.R"), envir = candidate)
object <- readCerebro(crb)
data <- object$getImmuneRepertoire()
if (!is.list(data) || !length(data)) stop("Ren repertoire unavailable")
cat("samples:", length(data), "rows:", sum(vapply(data, nrow, integer(1))), "\n")
for (chain in c("TRB", "TRA")) {
  if (mode == "both") {
    start <- proc.time()[["elapsed"]]
    before <- baseline$hla_parse_ir_segments(data, chain)
    before_s <- proc.time()[["elapsed"]] - start
    start <- proc.time()[["elapsed"]]
    after <- candidate$hla_parse_ir_segments(data, chain)
    after_s <- proc.time()[["elapsed"]] - start
    if (!identical(before, after)) stop("Parser output differs for ", chain)
    cat(chain, "baseline", before_s, "candidate", after_s,
        "identical TRUE rows", nrow(after), "\n")
  } else {
    parser <- if (mode == "baseline") baseline else candidate
    start <- proc.time()[["elapsed"]]
    value <- parser$hla_parse_ir_segments(data, chain)
    cat(chain, mode, proc.time()[["elapsed"]] - start,
        "rows", if (is.null(value)) 0L else nrow(value),
        "bytes", as.numeric(object.size(value)), "\n")
  }
}
