# Fresh R workers avoid mistaking R's interned-string reuse for thread scaling.
args <- commandArgs(TRUE)
path <- normalizePath(args[[1]])
out <- args[[2]]
results <- data.frame()
for (pair in 1:3) for (threads in c(1L, 4L)) {
  row <- callr::r(function(path, threads) {
    loadNamespace("qs2")
    loadNamespace("nanonext")
    gc()
    started <- nanonext::mclock()
    value <- qs2::qs_read(path, nthreads = threads)
    elapsed <- (nanonext::mclock() - started) / 1000
    data.frame(threads, elapsed, rows = sum(vapply(value, nrow, integer(1))), bytes = as.numeric(object.size(value)))
  }, args = list(path, threads))
  results <- rbind(results, cbind(pair, row))
  write.csv(results, out, row.names = FALSE)
  print(tail(results, 1))
}
