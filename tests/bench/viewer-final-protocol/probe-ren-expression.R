args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("usage: Rscript probe-ren-expression.R ROOT REN_CRB")
}
root <- normalizePath(args[[1L]])
crb <- normalizePath(args[[2L]])
pkgload::load_all(root, quiet = TRUE)
object <- readCerebro(crb)
all_genes <- object$getGeneNames()
genes <- intersect(c("MALAT1", "ACTB", "GAPDH"), all_genes)
if (length(genes) < 3L) genes <- head(all_genes, 3L)
stopifnot(length(genes) == 3L)
started <- proc.time()[["elapsed"]]
block <- as.matrix(object$getExpressionBlock(genes))
block_s <- proc.time()[["elapsed"]] - started
stopifnot(nrow(block) == 3L, ncol(block) == 1462702L)
started <- proc.time()[["elapsed"]]
single <- as.numeric(object$getExpressionRow(genes[[1L]]))
single_s <- proc.time()[["elapsed"]] - started
stopifnot(identical(single, as.numeric(block[1L, ])))
separate <- lapply(seq_len(3L), function(i) as.numeric(block[i, ]))
stopifnot(identical(separate[[1L]], single),
  identical(separate[[2L]], as.numeric(block[2L, ])))
duplicate <- separate[c(1L, 1L, 2L)]
stopifnot(identical(duplicate[[1L]], duplicate[[2L]]))
mean_values <- colMeans(block)
stopifnot(length(mean_values) == length(single), all(is.finite(mean_values)))
runtime <- new.env(parent = globalenv())
sys.source(file.path(root, "inst/viewer/gene_expression/func_projection_update_plot.R"),
  envir = runtime)
packets <- lapply(separate, runtime$expressionPackedColor)
stopifnot(all(vapply(packets, function(x) grepl("-f64-v1$", x$protocol),
  logical(1))))
restored <- lapply(packets, function(packet) {
  if (identical(packet$protocol, "dense-f64-v1")) return(packet$color)
  out <- numeric(packet$length)
  out[packet$index + 1L] <- packet$color
  out
})
stopifnot(all(vapply(seq_along(restored), function(i)
  identical(restored[[i]], separate[[i]]), logical(1))))
cat("genes", paste(genes, collapse = ","), "cells", ncol(block),
  "block_s", block_s, "single_s", single_s,
  "nonzero", paste(vapply(separate, function(x) sum(x != 0), integer(1)),
    collapse = ","),
  "protocol", paste(vapply(packets, `[[`, character(1), "protocol"),
    collapse = ","),
  "actual_values_exact", TRUE, "duplicate", TRUE,
  "mean", length(mean_values), "separate", length(separate), "\n")
