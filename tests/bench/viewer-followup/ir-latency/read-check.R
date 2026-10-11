# One bounded reader check, not a production change or a browser benchmark.
args <- commandArgs(TRUE)
path <- args[[1]]
cat("qs2", as.character(packageVersion("qs2")), "default threads", qs2::qopt("nthreads"), "\n")
reference <- NULL
results <- data.frame()
for (threads in unique(c(qs2::qopt("nthreads"), 1L, 4L))) {
  gc()
  started <- nanonext::mclock()
  value <- qs2::qs_read(path, nthreads = threads)
  elapsed <- (nanonext::mclock() - started) / 1000
  if (is.null(reference)) reference <- value
  results <- rbind(results, data.frame(threads, elapsed, identical = identical(reference, value),
    bytes = as.numeric(object.size(value))))
}
write.csv(results, args[[2]], row.names = FALSE)
print(results)
