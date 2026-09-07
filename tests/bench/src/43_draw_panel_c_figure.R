# Draw the combined incremental Panel C figure.

args <- commandArgs(trailingOnly = TRUE)
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "reporting.R"))
root <- if (length(args)) args[1L] else file.path(here, "result")
baseline <- bench_current_result_dir(root)
c1 <- bench_current_result_dir(file.path(root, "panel-c1"))
c2 <- bench_current_result_dir(file.path(root, "panel-c2"))
bench_validate_panel_c_baseline(baseline)
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

read_access <- function(path) {
  utils::read.csv(file.path(path, "20_access.csv"), stringsAsFactors = FALSE)
}
access <- rbind(read_access(baseline), read_access(c1))
access_long <- rbind(
  data.frame(
    access[c("source", "n_cells", "backend")],
    workload = "single gene",
    seconds = access$hot_p50_secs
  ),
  data.frame(
    access[c("source", "n_cells", "backend")],
    workload = "12-gene block",
    seconds = access$block_secs
  )
)
access_summary <- stats::aggregate(
  seconds ~ source + n_cells + backend + workload,
  access_long,
  stats::median
)
colors <- c(embedded = "#B4553F", bpcells = "#D9A03C", h5 = "#3F7F93")
p1 <- ggplot(access_summary, aes(n_cells, seconds, color = backend)) +
  geom_line() +
  geom_point(size = 2) +
  facet_grid(workload ~ source, scales = "free") +
  scale_color_manual(values = colors) +
  scale_x_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "k")
  ) +
  labs(
    title = "C1: three-backend scale bridge",
    x = "cells",
    y = "median seconds"
  ) +
  theme_bw(base_size = 10)

exports <- utils::read.csv(
  file.path(c2, "10_export.csv"),
  stringsAsFactors = FALSE
)
c2_access <- read_access(c2)
c2_long <- rbind(
  data.frame(
    exports[c("source", "backend")],
    metric = "build seconds",
    value = exports$export_secs
  ),
  data.frame(
    exports[c("source", "backend")],
    metric = "stored GiB",
    value = exports$total_mb / 1024
  ),
  data.frame(
    exports[c("source", "backend")],
    metric = "peak RSS GiB",
    value = exports$peak_rss_mb / 1024
  ),
  data.frame(
    c2_access[c("source", "backend")],
    metric = "single-gene seconds",
    value = c2_access$hot_p50_secs
  ),
  data.frame(
    c2_access[c("source", "backend")],
    metric = "12-gene block seconds",
    value = c2_access$block_secs
  )
)
c2_summary <- stats::aggregate(
  value ~ source + backend + metric,
  c2_long,
  stats::median
)
p2 <- ggplot(c2_summary, aes(backend, value, fill = backend)) +
  geom_col(width = 0.65) +
  facet_grid(metric ~ source, scales = "free_y") +
  scale_fill_manual(values = colors) +
  labs(
    title = "C2: complete-source out-of-core feasibility",
    x = NULL,
    y = "median"
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "none")

out_dir <- file.path(root, "panel-c", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(
  file.path(out_dir, "expression_backend_benchmark_panel_c.png"),
  p1 / p2 + plot_annotation(tag_levels = "A"),
  width = 13,
  height = 12,
  dpi = 180
)
