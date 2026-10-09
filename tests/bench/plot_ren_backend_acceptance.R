args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[[1L]] else "tests/bench/acceptance/ren-backends-20261008.tsv"
output <- if (length(args) >= 2L) args[[2L]] else "tests/bench/acceptance/ren-backends-20261008.png"
data <- utils::read.delim(input, check.names = FALSE)
stopifnot(all(data$correctness == "OK"), setequal(data$backend, c("bpcells", "h5")))
metrics <- c(
  startup_seconds = "Load source + CRB",
  first_gene_seconds = "First gene",
  hot_gene_p50_seconds = "Repeated gene",
  twelve_gene_seconds = "12-gene block"
)
grDevices::png(output, width = 1800, height = 600, res = 150)
on.exit(grDevices::dev.off(), add = TRUE)
graphics::par(mfrow = c(1, 4), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
for (metric in names(metrics)) {
  medians <- vapply(c("bpcells", "h5"), function(backend) {
    stats::median(data[data$backend == backend, metric])
  }, numeric(1))
  upper <- max(medians) * 1.27
  graphics::barplot(medians, names.arg = c("BPCells", "HDF5"),
    col = c("#D9A03C", "#3F7F93"), border = NA, ylim = c(0, upper),
    main = metrics[[metric]], ylab = "Seconds")
  graphics::text(c(0.7, 1.9), medians, sprintf("%.3f", medians), pos = 3, cex = 0.9)
}
graphics::mtext("Ren v2 expression backends: median of 3 fresh-process rounds",
  outer = TRUE, side = 3, line = 1, cex = 1.2)
