# Bounded IR diagnosis. Run in a fresh WSL R process; no app/ready changes.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
dataset <- normalizePath(args[[2]])
out <- args[[3]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
timings <- data.frame(stage = character(), elapsed = numeric())
measure <- function(label, expr) {
  gc()
  Rprof(file.path(out, paste0(label, ".Rprof")), interval = .01)
  start <- nanonext::mclock()
  value <- tryCatch(force(expr), finally = Rprof(NULL))
  elapsed <- (nanonext::mclock() - start) / 1000
  timings <<- rbind(timings, data.frame(stage = label, elapsed = elapsed))
  write.csv(timings, file.path(out, "timings.csv"), row.names = FALSE)
  cat(label, elapsed, "s\n")
  flush.console()
  value
}
invisible(measure("package", pkgload::load_all(root, quiet = TRUE)))
obj <- measure("read-crb", readCerebro(dataset))
ir <- measure("repertoire", obj$getImmuneRepertoire())
writeLines(c(paste("samples", length(ir)), paste("rows", sum(vapply(ir, nrow, integer(1)))),
             paste("scRepertoire", as.character(utils::packageVersion("scRepertoire")))),
           file.path(out, "identity.txt"))
invisible(measure("namespace", loadNamespace("scRepertoire")))
p <- measure("homeostasis", scRepertoire::clonalHomeostasis(ir, cloneCall = "strict", chain = "both", palette = "inferno"))
grDevices::pdf(NULL)
fig <- measure("ggplotly", plotly::ggplotly(p))
invisible(measure("plotly-json", plotly::plotly_json(fig, jsonedit = FALSE)))
grDevices::dev.off()
invisible(measure("diversity", scRepertoire::clonalDiversity(ir, cloneCall = "strict", chain = "both", n.boots = 20, exportTable = TRUE)))
for (file in list.files(out, "[.]Rprof$", full.names = TRUE)) {
  summary <- summaryRprof(file)
  write.csv(head(summary$by.total, 30), sub("Rprof$", "summary.csv", file))
}
