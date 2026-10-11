args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
out <- "tests/bench/viewer-followup/ir-grouped-homeostasis"
obj <- readCerebro(args[[2]])
raw <- obj$getImmuneRepertoire()
env <- new.env(parent = globalenv())
for (x in parse("inst/viewer/immune_repertoire/data.R")) {
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.call(x[[3]]) &&
      identical(x[[3]][[1]], as.name("function"))) eval(x, env)
}
raw <- env$ir_annotate_metadata(raw, obj$getMetaData(), "severity")
loadNamespace("scRepertoire")
Rprof(file.path(out, "grouped.Rprof"), interval = .01)
start <- nanonext::mclock()
p <- scRepertoire::clonalHomeostasis(raw, cloneCall = "gene", chain = "both",
  group.by = "severity", palette = "inferno")
cat("BASELINE_MS", nanonext::mclock() - start, "\n")
Rprof(NULL)
profile <- summaryRprof(file.path(out, "grouped.Rprof"))
write.csv(head(profile$by.total, 25), file.path(out, "profile-total.csv"))
write.csv(head(profile$by.self, 25), file.path(out, "profile-self.csv"))
print(head(profile$by.total, 12))
