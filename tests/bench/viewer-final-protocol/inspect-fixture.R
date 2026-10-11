args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("usage: Rscript inspect-fixture.R ROOT CRB")
pkgload::load_all(normalizePath(args[[1L]]), quiet = TRUE)
object <- readCerebro(normalizePath(args[[2L]]))
schema <- object$crb_schema
cat("schema_version", schema$version, "\n")
cat("schema_cells", schema$n_cells, "\n")
cat("cell_fingerprint", object$cell_fingerprint, "\n")
cat("backend", object$getExpressionBackend()$type, "\n")
