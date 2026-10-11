# Exact distance equivalence and bounded timing on the shipped Demo repertoire.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
pkgload::load_all(root, quiet = TRUE)
obj <- qs2::qs_read(file.path(root, "inst/extdata/examples/example.crb"))
seg <- hla_parse_ir_segments(obj$getImmuneRepertoire(), "TRB")
seqs <- unique(seg$cdr3)
bins <- split(seqs, nchar(seqs))
bins <- bins[lengths(bins) > 1L]
measure <- function(threads) {
  elapsed <- system.time(result <- lapply(bins, function(x) {
    if (is.null(threads)) stringdist::stringdistmatrix(x, x, method = "hamming")
    else stringdist::stringdistmatrix(x, x, method = "hamming", nthread = threads)
  }))[["elapsed"]]
  list(elapsed = elapsed, result = result)
}
a <- measure(NULL)
b <- measure(1L)
stopifnot(identical(a$result, b$result))
jsonlite::write_json(list(cells = nrow(seg), nodes = length(seqs),
  bin_sizes = lengths(bins), configured_threads = getOption("sd_num_thread"),
  default_seconds = a$elapsed, one_thread_seconds = b$elapsed,
  identical = identical(a$result, b$result)), args[[2]], pretty = TRUE, auto_unbox = TRUE)
