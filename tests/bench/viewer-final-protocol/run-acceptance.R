# Focused final functional acceptance; no timing claims from this runner.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
root <- normalizePath(args[[1L]])
group <- args[[2L]]
output <- normalizePath(args[[3L]], mustWork = FALSE)
groups <- list(
  data = c("cerebro-io", "viewer-pack", "clone-base-cache", "multi_crb",
    "export-data-integrity", "seurat-pipeline", "seurat-v5-split-layers",
    "seurat-v5-layered-public-functions", "seurat-v5-layer-partition"),
  viewer = c("viewer-dataset-context", "viewer-shared-selection",
    "gpu-selection-update", "selection-status", "trajectory-selection-valid",
    "single-expression-incremental", "gene-expression-panel-modes",
    "gene-expression-interaction-performance", "gene-hover-demand", "gene-expression-color-transport",
    "expression-rgb-dataset-switch", "projection-first-frame-performance",
    "groups-performance", "coordinated-views", "viewer-hidden-palette",
    "viewer-spatial-hidden-moran", "spatial", "spatial-morans-i",
    "hla-network-selection", "hla-app-contract", "hla-parser-equivalence",
    "hla-graph-guard-equivalence"),
  browser = c("app-inst", "app-new-modules", "app-trajectory", "app-immune_repertoire"),
  bundle = "smoke-production"
)
stopifnot(group %in% names(groups))
Sys.setenv(NOT_CRAN = "true")
filter <- paste0("^(", paste(groups[[group]], collapse = "|"), ")$")
results <- testthat::test_local(root, filter = filter, reporter = "summary",
  stop_on_failure = FALSE)
table <- as.data.frame(results)
write.table(table[setdiff(names(table), "result")], output,
  sep = "\t", row.names = FALSE, quote = TRUE)
saveRDS(results, paste0(output, ".rds"))
cat("\nGroup:", group, "tests:", nrow(table), "failed:", sum(table$failed),
  "errors:", sum(table$error), "skipped:", sum(table$skipped), "\n")
if (any(table$failed > 0L | table$error)) quit(status = 1L)
