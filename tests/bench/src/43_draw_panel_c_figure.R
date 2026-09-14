# Draw the publication-full study from its frozen phase directories.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("need <result_root> <output_dir>", call. = FALSE)
}
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "reporting.R"))
root <- normalizePath(args[1L], mustWork = TRUE)
out_dir <- normalizePath(args[2L], mustWork = TRUE)
manifest <- utils::read.csv(
  file.path(out_dir, "study_manifest.csv"),
  stringsAsFactors = FALSE
)
if (!identical(manifest$panel, c("ab", "c1", "c2"))) {
  stop("frozen study manifest is incomplete", call. = FALSE)
}
paths <- stats::setNames(
  vapply(
    seq_len(nrow(manifest)),
    function(i) {
      normalizePath(file.path(root, manifest$panel[i]), mustWork = TRUE)
    },
    character(1)
  ),
  manifest$panel
)

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

read_results <- function(path, file) {
  utils::read.csv(file.path(path, file), stringsAsFactors = FALSE)
}
make_long <- function(data, panel, phase, metrics, labels, divisors) {
  do.call(
    rbind,
    lapply(metrics, function(metric) {
      data.frame(
        panel = panel,
        phase = phase,
        data[c("source", "n_cells", "backend")],
        metric = labels[[metric]],
        value = data[[metric]] / divisors[[metric]],
        stringsAsFactors = FALSE
      )
    })
  )
}
summarise <- function(data) {
  median <- stats::aggregate(
    value ~ source + n_cells + backend + metric + panel,
    data,
    stats::median
  )
  minimum <- stats::aggregate(
    value ~ source + n_cells + backend + metric + panel,
    data,
    min
  )
  maximum <- stats::aggregate(
    value ~ source + n_cells + backend + metric + panel,
    data,
    max
  )
  names(median)[names(median) == "value"] <- "median"
  names(minimum)[names(minimum) == "value"] <- "minimum"
  names(maximum)[names(maximum) == "value"] <- "maximum"
  Reduce(
    function(x, y) {
      merge(
        x,
        y,
        by = c("source", "n_cells", "backend", "metric", "panel")
      )
    },
    list(median, minimum, maximum)
  )
}

access_metrics <- c("hot_p50_secs", "block_secs")
access_labels <- c(
  hot_p50_secs = "Warmed single-gene latency (s)",
  block_secs = "12-gene block latency (s)"
)
access_divisors <- c(hot_p50_secs = 1, block_secs = 1)
rss_labels <- c(peak_rss_mb = "Build-process peak RSS (GiB)")
rss_divisors <- c(peak_rss_mb = 1024)
c1 <- rbind(
  make_long(
    read_results(paths[["ab"]], "20_access.csv"),
    "A/B",
    "access",
    access_metrics,
    access_labels,
    access_divisors
  ),
  make_long(
    read_results(paths[["c1"]], "20_access.csv"),
    "C1",
    "access",
    access_metrics,
    access_labels,
    access_divisors
  ),
  make_long(
    read_results(paths[["ab"]], "10_export.csv"),
    "A/B",
    "build",
    "peak_rss_mb",
    rss_labels,
    rss_divisors
  ),
  make_long(
    read_results(paths[["c1"]], "10_export.csv"),
    "C1",
    "build",
    "peak_rss_mb",
    rss_labels,
    rss_divisors
  )
)
c1_summary <- summarise(c1)
c1_summary$line_group <- c1_summary$backend

colors <- c(embedded = "#B4553F", bpcells = "#D9A03C", h5 = "#3F7F93")
p1 <- ggplot(c1, aes(n_cells, value, color = backend)) +
  geom_point(
    alpha = 0.28,
    position = position_jitter(width = 5000, height = 0)
  ) +
  geom_line(
    data = c1_summary,
    aes(y = median, group = line_group),
    linewidth = 0.5
  ) +
  geom_errorbar(
    data = c1_summary,
    aes(x = n_cells, ymin = minimum, ymax = maximum, color = backend),
    inherit.aes = FALSE,
    width = 8000,
    linewidth = 0.4
  ) +
  geom_point(data = c1_summary, aes(y = median, shape = panel), size = 2) +
  facet_grid(metric ~ source, scales = "free_y") +
  scale_color_manual(values = colors) +
  scale_x_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "k")
  ) +
  labs(
    title = "A/B and C1: three-backend scaling",
    subtitle = "One acquisition environment; points are independent processes",
    x = "cells",
    y = NULL
  ) +
  theme_bw(base_size = 10)

c2_exports <- read_results(paths[["c2"]], "10_export.csv")
c2_access <- read_results(paths[["c2"]], "20_access.csv")
c2 <- rbind(
  make_long(
    c2_exports,
    "C2",
    "build",
    c("export_secs", "total_mb", "peak_rss_mb"),
    c(
      export_secs = "Backend construction (s)",
      total_mb = "Stored size (GiB)",
      peak_rss_mb = "Build-process peak RSS (GiB)"
    ),
    c(export_secs = 1, total_mb = 1024, peak_rss_mb = 1024)
  ),
  make_long(
    c2_access,
    "C2",
    "access",
    access_metrics,
    access_labels,
    access_divisors
  )
)
c2_summary <- summarise(c2)
not_representable <- unique(c2_summary[c("source", "metric")])
not_representable$backend <- "embedded"
not_representable$label <- "not representable"

p2 <- ggplot(c2, aes(backend, value, color = backend)) +
  geom_point(
    alpha = 0.32,
    position = position_jitter(width = 0.08, height = 0)
  ) +
  geom_errorbar(
    data = c2_summary,
    aes(x = backend, ymin = minimum, ymax = maximum, color = backend),
    inherit.aes = FALSE,
    width = 0.18,
    linewidth = 0.4
  ) +
  geom_point(data = c2_summary, aes(y = median), size = 2.2) +
  geom_text(
    data = not_representable,
    aes(x = backend, y = Inf, label = label),
    inherit.aes = FALSE,
    angle = 90,
    vjust = 1.4,
    hjust = 1.05,
    size = 2.6
  ) +
  facet_grid(metric ~ source, scales = "free_y") +
  scale_color_manual(values = colors, limits = names(colors), drop = FALSE) +
  scale_x_discrete(limits = names(colors), drop = FALSE) +
  labs(
    title = "C2: million-cell out-of-core feasibility",
    subtitle = "Points are independent processes; bars show the observed range",
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "none")

figure_dir <- file.path(out_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(
  file.path(figure_dir, "expression_backend_benchmark_publication_full.png"),
  p1 / p2 + plot_annotation(tag_levels = "A"),
  width = 13,
  height = 14,
  dpi = 180
)
