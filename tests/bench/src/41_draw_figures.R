# Generate uncertainty-aware figures from the current publication-profile run.
#
# Usage: Rscript src/41_draw_figures.R <result_dir> <out_dir>

args <- commandArgs(trailingOnly = TRUE)
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "protocol.R"))
source(file.path(here, "lib", "reporting.R"))

result_dir <- if (length(args) >= 1L) {
  normalizePath(args[1], mustWork = TRUE)
} else {
  bench_current_result_dir(file.path(here, "result"))
}
out_dir <- if (length(args) >= 2L) args[2] else "vignettes/img"

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

manifest <- utils::read.csv(
  file.path(result_dir, "run_manifest.csv"),
  stringsAsFactors = FALSE
)
manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
profile <- bench_profile(manifest_values[["profile"]])
bench_require_article_profile(profile)

exports <- utils::read.csv(
  file.path(result_dir, "10_export.csv"),
  stringsAsFactors = FALSE
)
access <- utils::read.csv(
  file.path(result_dir, "20_access.csv"),
  stringsAsFactors = FALSE
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

exports$r_peak_mb[exports$r_peak_mb > 4e6] <- NA_real_
access$startup_secs <- access$load_secs + access$attach_secs
export_summary <- bench_summarise_metrics(
  exports,
  group = c("source", "n_cells", "backend"),
  metrics = c("total_mb", "export_secs", "r_peak_mb")
)
access_summary <- bench_summarise_metrics(
  access,
  group = c("source", "n_cells", "backend"),
  metrics = c(
    "startup_secs",
    "rss_mb",
    "first_query_secs",
    "hot_p50_secs",
    "block_secs"
  )
)

pretty_source <- c(
  mouse_brain_e18 = "10x mouse brain E18",
  human_pfc_hbcc = "human PFC cross-disorder (HBCC)",
  human_pfc_mssm = "human PFC cross-disorder (MSSM)"
)
add_source_label <- function(x) {
  label <- unname(pretty_source[x$source])
  label[is.na(label)] <- x$source[is.na(label)]
  x$src <- label
  x
}
export_summary <- add_source_label(export_summary)
access_summary <- add_source_label(access_summary)

backend_levels <- c("embedded", "bpcells", "h5")
backend_cols <- c(embedded = "#B4553F", bpcells = "#D9A03C", h5 = "#3F7F93")
export_summary$backend <- factor(
  export_summary$backend,
  levels = backend_levels
)
access_summary$backend <- factor(
  access_summary$backend,
  levels = backend_levels
)

base <- theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 10.5),
    plot.subtitle = element_text(size = 8.5, colour = "grey35")
  )

k_cells <- function(x) paste0(x / 1000, "k")

panel_line <- function(df, metric, title, subtitle, ylab, log_y = TRUE) {
  median <- paste0(metric, "_median")
  minimum <- paste0(metric, "_min")
  maximum <- paste0(metric, "_max")
  p <- ggplot(
    df,
    aes(
      .data[["n_cells"]],
      .data[[median]],
      colour = .data[["backend"]],
      group = .data[["backend"]]
    )
  ) +
    geom_errorbar(
      aes(ymin = .data[[minimum]], ymax = .data[[maximum]]),
      width = 0,
      linewidth = 0.45
    ) +
    geom_point(size = 1.9) +
    facet_wrap(~src) +
    scale_colour_manual(values = backend_cols) +
    scale_x_continuous(labels = k_cells) +
    labs(title = title, subtitle = subtitle, x = "cells", y = ylab) +
    base
  if (length(unique(df$n_cells)) > 1L) {
    p <- p + geom_line(linewidth = 0.6)
  }
  if (log_y) {
    p <- p + scale_y_log10()
  }
  p
}

p_hot <- panel_line(
  access_summary,
  "hot_p50_secs",
  "Warmed single-gene query",
  "median and range across fresh-process repeats; log scale",
  "seconds"
)
p_block <- panel_line(
  access_summary,
  "block_secs",
  "12-gene block read",
  "marker-view access pattern; median and range; log scale",
  "seconds"
)
p_rss <- panel_line(
  access_summary,
  "rss_mb",
  "Resident memory after load and attach",
  "one process per measurement; median and range",
  "MB",
  log_y = FALSE
)

p_disk <- ggplot(
  export_summary,
  aes(
    factor(.data[["n_cells"]]),
    .data[["total_mb_median"]],
    fill = .data[["backend"]]
  )
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_errorbar(
    aes(
      ymin = .data[["total_mb_min"]],
      ymax = .data[["total_mb_max"]]
    ),
    position = position_dodge(width = 0.75),
    width = 0.15,
    linewidth = 0.4
  ) +
  facet_wrap(~src, scales = "free_x") +
  guides(fill = "none") +
  scale_fill_manual(values = backend_cols) +
  scale_x_discrete(labels = function(x) k_cells(as.numeric(x))) +
  labs(
    title = "Total on-disk footprint",
    subtitle = "CRB plus external sibling; median and range",
    x = "cells",
    y = "MB"
  ) +
  base

overview <- (p_hot / p_block / p_rss / p_disk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "expression_backend_benchmark_overview.png"),
  overview,
  width = 8,
  height = 11,
  dpi = 150,
  bg = "white"
)

usable <- exports[
  exports$status == "OK" & is.finite(exports$r_peak_mb) & !is.na(exports$nnz),
  ,
  drop = FALSE
]
points <- bench_summarise_metrics(
  usable,
  group = c("source", "n_cells", "nnz"),
  metrics = "r_peak_mb"
)
points <- add_source_label(points)
points$bytes_per_nnz <- points$r_peak_mb_median * 2^20 / points$nnz
bytes_per_nnz <- stats::median(points$bytes_per_nnz)
limit_mb <- suppressWarnings(as.numeric(manifest_values[["r_vector_limit_mb"]]))
if (!is.finite(limit_mb)) {
  limit_mb <- max(points$r_peak_mb_max) * 1.1
}
ceiling_nnz <- limit_mb * 2^20 / bytes_per_nnz

failures <- exports[grepl("^FAILED", exports$status) & !is.na(exports$nnz), ]
failures <- failures[!duplicated(failures[c("source", "n_cells", "nnz")]), ]
failures <- add_source_label(failures)
plot_points <- rbind(
  data.frame(
    nnz = points$nnz,
    value = points$r_peak_mb_median,
    minimum = points$r_peak_mb_min,
    maximum = points$r_peak_mb_max,
    src = points$src,
    outcome = "built"
  ),
  data.frame(
    nnz = failures$nnz,
    value = rep(limit_mb * 1.06, nrow(failures)),
    minimum = rep(limit_mb * 1.06, nrow(failures)),
    maximum = rep(limit_mb * 1.06, nrow(failures)),
    src = failures$src,
    outcome = rep("could not be built", nrow(failures))
  )
)

ceiling <- ggplot(
  plot_points,
  aes(
    .data[["nnz"]],
    .data[["value"]],
    colour = .data[["src"]],
    shape = .data[["outcome"]]
  )
) +
  geom_abline(
    slope = bytes_per_nnz / 2^20,
    intercept = 0,
    colour = "grey55",
    linetype = "22",
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = .data[["minimum"]], ymax = .data[["maximum"]]),
    width = 0,
    linewidth = 0.45
  ) +
  geom_hline(yintercept = limit_mb, colour = "#A6342A", linewidth = 0.5) +
  geom_vline(xintercept = ceiling_nnz, colour = "grey55", linewidth = 0.4) +
  geom_point(size = 2.6, stroke = 0.9) +
  coord_cartesian(ylim = c(0, limit_mb * 1.15)) +
  scale_x_continuous(labels = function(x) sprintf("%.1fe9", x / 1e9)) +
  scale_shape_manual(values = c(built = 16, `could not be built` = 4)) +
  labs(
    title = "Host-specific export scale estimate",
    subtitle = sprintf(
      "%d distinct source/tier points; median %.1f B/nnz (range %.1f-%.1f)",
      nrow(points),
      bytes_per_nnz,
      min(points$bytes_per_nnz),
      max(points$bytes_per_nnz)
    ),
    x = "non-zeros in the tier",
    y = "peak R heap (MB)"
  ) +
  base

ggsave(
  file.path(out_dir, "expression_backend_benchmark_ceiling.png"),
  ceiling,
  width = 8,
  height = 4.4,
  dpi = 150,
  bg = "white"
)

message(
  sprintf(
    "wrote publication-profile figures from %s (%d scale points)",
    manifest_values[["run_id"]],
    nrow(points)
  )
)
