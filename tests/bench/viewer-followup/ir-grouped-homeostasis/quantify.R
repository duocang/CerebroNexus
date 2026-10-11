args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
out <- "tests/bench/viewer-followup/ir-grouped-homeostasis"
env <- new.env(parent = globalenv())
env$`%||%` <- function(a, b) if (is.null(a)) b else a
for (x in parse("inst/viewer/immune_repertoire/data.R")) {
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.call(x[[3]]) &&
      identical(x[[3]][[1]], as.name("function"))) eval(x, env)
}
obj <- readCerebro(args[[2]])
raw <- obj$getImmuneRepertoire()
raw <- env$ir_annotate_metadata(raw, obj$getMetaData(), c("severity", "patient"))
breaks <- c(Rare = .0001, Small = .001, Medium = .01, Large = .1, Hyperexpanded = 1)
current <- function(call = "gene", group = "severity", thresholds = breaks) {
  env$ir_homeostasis_plot(env$ir_grouped_homeostasis_counts(raw, paste0("CT", call), group), thresholds, "inferno")
}
cat("BEFORE_NATIVE_NAMESPACE", "scRepertoire" %in% loadedNamespaces(), "\n")
first <- nanonext::mclock()
native_first <- ggplot2::ggplot_build(current())
cat("NATIVE_FIRST_MS", nanonext::mclock() - first, "\n")
cat("AFTER_NATIVE_NAMESPACE", "scRepertoire" %in% loadedNamespaces(), "\n")
stopifnot(!"scRepertoire" %in% loadedNamespaces())
start <- nanonext::mclock()
loadNamespace("scRepertoire")
cat("UNUSED_NAMESPACE_LOAD_MS", nanonext::mclock() - start, "\n")
before <- function() scRepertoire::clonalHomeostasis(raw, cloneCall = "gene", chain = "both",
  group.by = "severity", cloneSize = breaks, palette = "inferno")
timings <- data.frame(pair = integer(), arm = character(), ms = numeric())
measure <- function(pair, arm, fn) {
  gc()
  start <- nanonext::mclock()
  result <- ggplot2::ggplot_build(fn())
  timings <<- rbind(timings, data.frame(pair, arm, ms = nanonext::mclock() - start))
  write.csv(timings, file.path(out, "timings.csv"), row.names = FALSE)
  print(tail(timings, 1), row.names = FALSE)
  result
}
for (i in 0:3) {
  a <- measure(i, "baseline", before)
  b <- measure(i, "candidate", current)
  cols <- c("x", "y", "ymin", "ymax", "fill")
  stopifnot(isTRUE(all.equal(a$data[[1]][cols], b$data[[1]][cols], tolerance = 1e-12)))
  if (i == 0 && timings$ms[[2]] >= timings$ms[[1]]) stop("No Level 1 improvement")
}
cat("FORMAL_MEDIANS\n")
print(aggregate(ms ~ arm, timings[timings$pair > 0, ], median))
# Whole Ren numerical equivalence, all four clone definitions, two groupings.
if (length(args) < 3L || args[[3]] != "timing-only") {
for (group in c("severity", "patient")) for (call in c("gene", "nt", "aa", "strict")) {
  counts <- env$ir_grouped_homeostasis_counts(raw, paste0("CT", call), group)
  thresholds <- if (group == "patient") c(Small = .002, Large = .02, Expanded = 1) else breaks
  actual <- env$ir_homeostasis_matrix(counts, thresholds)
  expected <- scRepertoire::clonalHomeostasis(raw, cloneCall = call, chain = "both",
    group.by = group, cloneSize = thresholds, exportTable = TRUE)
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-12)))
  cat("EQUAL", group, call, nrow(actual), "groups; max error", max(abs(actual - expected)),
    "compact bytes", as.numeric(object.size(counts)), "\n")
}
}
cat("ALL_CHECKS_PASSED\n")
print(sessionInfo())
