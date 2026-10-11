# Full-data numerical equivalence, separate from browser timing runs.
args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
obj <- readCerebro(args[[2]])
raw <- obj$getImmuneRepertoire()
env <- new.env(parent = globalenv())
env$`%||%` <- function(a, b) if (is.null(a)) b else a
env$viewerPackCurrent <- function() NULL
env$viewerPackReadAsset <- function(...) NULL
for (expr in parse(file.path(args[[1]], "inst/viewer/immune_repertoire/data.R"))) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) eval(expr, env)
}
packed <- qs2::qs_read(file.path(sub("[.]crb$", ".viewer", args[[2]]), "immune/homeostasis.qs2"))
breaks <- c(Rare = .0001, Small = .001, Medium = .01, Large = .1, Hyperexpanded = 1)
for (call in c("gene", "nt", "aa", "strict")) {
  column <- paste0("CT", call)
  expected <- scRepertoire::clonalHomeostasis(raw, cloneCall = call,
    chain = "both", cloneSize = breaks, exportTable = TRUE)
  actual <- env$ir_homeostasis_matrix(packed[[column]], breaks)
  fallback <- env$ir_homeostasis_matrix(env$ir_homeostasis_counts(raw, column), breaks)
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-12)),
    isTRUE(all.equal(fallback, expected, tolerance = 1e-12)))
  cat(call, "PASS", nrow(actual), "samples; maximum absolute error",
    max(abs(actual - expected)), "\n")
}
cat("Direct unchanged scRepertoire fallback, strict / TRB: ")
tryCatch({
  scRepertoire::clonalHomeostasis(raw, cloneCall = "strict", chain = "TRB",
    exportTable = TRUE)
  cat("PASS\n")
}, error = function(e) cat(conditionMessage(e), "\n"))
