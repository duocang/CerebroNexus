args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("usage: Rscript probe-hla-guard.R ROOT REN_CRB BASELINE_SOURCE")
}
root <- normalizePath(args[[1L]])
crb <- normalizePath(args[[2L]])
baseline_file <- normalizePath(args[[3L]])
pkgload::load_all(root, quiet = TRUE)
baseline <- new.env(parent = globalenv())
candidate <- new.env(parent = globalenv())
sys.source(baseline_file, envir = baseline)
sys.source(file.path(root, "R/hla_motif_core.R"), envir = candidate)
data <- readCerebro(crb)$getImmuneRepertoire()
segments <- candidate$hla_parse_ir_segments(data, "TRB")
stopifnot(nrow(segments) > 20000L)
meta_cols <- intersect(c("sample", "seurat_clusters"), names(segments))
start <- proc.time()[["elapsed"]]
before <- baseline$hla_build_motif_graph_raw(segments, meta_cols = meta_cols)
before_s <- proc.time()[["elapsed"]] - start
start <- proc.time()[["elapsed"]]
after <- candidate$hla_build_motif_graph_raw(segments, meta_cols = meta_cols)
after_s <- proc.time()[["elapsed"]] - start
cat("rows", nrow(segments), "distinct", length(unique(segments$cdr3)),
  "baseline", before_s, "candidate", after_s,
  "identical", identical(before, after), "\n")
if (!identical(before, after)) stop("Graph guard output differs")
