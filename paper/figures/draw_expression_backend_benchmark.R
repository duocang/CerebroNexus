#!/usr/bin/env Rscript

# Oxford Bioinformatics-ready expression-backend benchmark figure.
#
# Usage:
#   Rscript paper/figures/draw_expression_backend_benchmark.R \
#     tests/bench/result/publication-full/runs/<run-id> \
#     paper/figures/output/<run-id>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    paste(
      "usage: draw_expression_backend_benchmark.R",
      "<published-run-directory> <output-directory>"
    ),
    call. = FALSE
  )
}

run_dir <- normalizePath(args[[1L]], mustWork = TRUE)
output_dir <- normalizePath(args[[2L]], mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]))
repo <- normalizePath(file.path(dirname(script_path), "..", ".."))
plotting_git_sha <- paste(
  system2(
    "git",
    c("-C", shQuote(repo), "rev-parse", "HEAD"),
    stdout = TRUE
  ),
  collapse = ""
)
if (!grepl("^[0-9a-f]{40}$", plotting_git_sha)) {
  stop("could not record the plotting-code Git SHA", call. = FALSE)
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
if (!requireNamespace("systemfonts", quietly = TRUE)) {
  stop("the systemfonts package is required to verify Arial", call. = FALSE)
}

FONT_FAMILY <- "Arial"
FIGURE_WIDTH_MM <- 178
FIGURE_HEIGHT_MM <- 200
COLOUR_DPI <- 350
OKABE_ITO <- c(BPCells = "#0072B2", H5 = "#E69F00")

font_match <- systemfonts::match_fonts(FONT_FAMILY)
if (!nrow(font_match) || !file.exists(font_match$path[[1L]])) {
  stop("Arial is not installed or cannot be resolved by Fontconfig", call. = FALSE)
}
if (!grepl("arial", basename(font_match$path[[1L]]), ignore.case = TRUE)) {
  stop(
    "Fontconfig substituted a non-Arial font: ", font_match$path[[1L]],
    call. = FALSE
  )
}

read_required <- function(name) {
  path <- file.path(run_dir, name)
  if (!file.exists(path)) {
    stop("missing published benchmark file: ", name, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

exports <- read_required("10_export.csv")
access <- read_required("20_access.csv")
manifest <- read_required("run_manifest.csv")
source_manifest <- read_required("source_manifest.csv")

if (any(exports$status != "OK") || any(access$status != "OK")) {
  stop("paper figure requires an all-OK published run", call. = FALSE)
}
if (!"correctness" %in% names(access) || any(access$correctness != "OK")) {
  stop("paper figure requires correctness=OK for every access row", call. = FALSE)
}

required_export <- c("source", "backend", "export_secs", "total_mb")
required_access <- c(
  "source", "backend", "startup_secs", "rss_mb", "peak_rss_mb",
  "hot_p50_secs", "block_secs", "subset_row_secs", "subset_block_secs"
)
if (!all(required_export %in% names(exports))) {
  stop("10_export.csv lacks required plotting columns", call. = FALSE)
}
if (!all(required_access %in% names(access))) {
  stop("20_access.csv lacks required plotting columns", call. = FALSE)
}

pretty_source <- c(
  mouse_brain_e18 = "Mouse brain E18\n(1.31 million cells)",
  human_pfc_hbcc = "Human PFC HBCC\n(1.49 million cells)"
)
pretty_backend <- c(bpcells = "BPCells", h5 = "H5")

prepare_groups <- function(data) {
  data$source_label <- unname(pretty_source[data$source])
  data$source_label[is.na(data$source_label)] <-
    data$source[is.na(data$source_label)]
  data$source_label <- factor(
    data$source_label,
    levels = unname(pretty_source)
  )
  data$backend_label <- unname(pretty_backend[data$backend])
  data$backend_label[is.na(data$backend_label)] <-
    data$backend[is.na(data$backend_label)]
  data$backend_label <- factor(
    data$backend_label,
    levels = c("BPCells", "H5")
  )
  data
}
exports <- prepare_groups(exports)
access <- prepare_groups(access)

summarise_metric <- function(data, metric, scale = 1, label = metric) {
  value <- as.numeric(data[[metric]]) / scale
  if (any(!is.finite(value) | value < 0)) {
    stop("invalid values in metric: ", metric, call. = FALSE)
  }
  groups <- interaction(
    data$source_label,
    data$backend_label,
    drop = TRUE,
    lex.order = TRUE
  )
  pieces <- split(
    data.frame(
      source_label = data$source_label,
      backend_label = data$backend_label,
      value = value
    ),
    groups
  )
  rows <- lapply(pieces, function(x) data.frame(
    source_label = x$source_label[[1L]],
    backend_label = x$backend_label[[1L]],
    metric = label,
    median = stats::median(x$value),
    minimum = min(x$value),
    maximum = max(x$value),
    n = nrow(x),
    stringsAsFactors = FALSE
  ))
  result <- do.call(rbind, rows)
  result$source_label <- factor(
    result$source_label,
    levels = levels(data$source_label)
  )
  result$backend_label <- factor(
    result$backend_label,
    levels = levels(data$backend_label)
  )
  rownames(result) <- NULL
  result
}

theme_bioinformatics <- function(show_legend = FALSE) {
  theme_bw(base_family = FONT_FAMILY, base_size = 7) +
    theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black", linewidth = 0.35),
      axis.title = element_text(size = 9, colour = "black"),
      axis.text = element_text(size = 7, colour = "black"),
      axis.ticks = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks.length = grid::unit(1.4, "mm"),
      strip.background = element_blank(),
      strip.text = element_text(size = 7, face = "plain", colour = "black"),
      plot.margin = margin(3, 4, 3, 3, unit = "mm"),
      legend.position = if (show_legend) c(0.78, 0.83) else "none",
      legend.background = element_rect(
        fill = scales::alpha("white", 0.88),
        colour = "black",
        linewidth = 0.25
      ),
      legend.title = element_blank(),
      legend.text = element_text(size = 7),
      legend.key.size = grid::unit(3.5, "mm"),
      plot.tag = element_text(
        family = FONT_FAMILY,
        size = 8,
        face = "bold",
        colour = "black"
      ),
      plot.tag.position = c(0, 1)
    )
}

metric_plot <- function(
  summary,
  y_label,
  log_scale = FALSE,
  show_legend = FALSE
) {
  dodge <- position_dodge(width = 0.48)
  plot <- ggplot(
    summary,
    aes(
      x = source_label,
      y = median,
      colour = backend_label,
      shape = backend_label,
      group = backend_label
    )
  ) +
    geom_errorbar(
      aes(ymin = minimum, ymax = maximum),
      position = dodge,
      width = 0.12,
      linewidth = 0.42
    ) +
    geom_point(position = dodge, size = 2.1, stroke = 0.7) +
    scale_colour_manual(values = OKABE_ITO, drop = FALSE) +
    scale_shape_manual(values = c(BPCells = 16, H5 = 17), drop = FALSE) +
    labs(x = NULL, y = y_label, colour = NULL, shape = NULL) +
    theme_bioinformatics(show_legend)
  if (log_scale) {
    plot <- plot + scale_y_log10()
  } else {
    plot <- plot + expand_limits(y = 0)
  }
  plot
}

build <- summarise_metric(exports, "export_secs")
storage <- summarise_metric(exports, "total_mb", scale = 1024)
startup <- summarise_metric(access, "startup_secs")
hot <- summarise_metric(access, "hot_p50_secs")

memory <- rbind(
  summarise_metric(access, "rss_mb", label = "Resident RSS"),
  summarise_metric(access, "peak_rss_mb", label = "Peak RSS")
)
memory$metric <- factor(memory$metric, levels = c("Resident RSS", "Peak RSS"))

blocks <- rbind(
  summarise_metric(access, "block_secs", label = "12 genes, all cells"),
  summarise_metric(
    access,
    "subset_row_secs",
    label = "1 gene, 100k non-contiguous cells"
  ),
  summarise_metric(
    access,
    "subset_block_secs",
    label = "12 genes, 100k non-contiguous cells"
  )
)
blocks$metric <- factor(
  blocks$metric,
  levels = c(
    "12 genes, all cells",
    "1 gene, 100k non-contiguous cells",
    "12 genes, 100k non-contiguous cells"
  )
)

p_a <- metric_plot(build, "Backend construction time (s)", show_legend = TRUE)
p_b <- metric_plot(storage, "Stored size (GB)")
p_c <- metric_plot(startup, "Hydrated startup time (s)")
p_d <- metric_plot(memory, "Process memory (MB)") +
  facet_wrap(~metric, ncol = 1)
p_e <- metric_plot(hot, "Warmed single-gene latency (s)", log_scale = TRUE)
p_f <- metric_plot(blocks, "Expression access latency (s)", log_scale = TRUE) +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  theme(strip.text = element_text(size = 6.4, family = FONT_FAMILY))

figure <- (p_a | p_b) /
  (p_c | p_d) /
  (p_e | p_f) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(
      plot.tag = element_text(
        family = FONT_FAMILY,
        size = 8,
        face = "bold",
        colour = "black"
      )
    )
  ) +
  plot_layout(heights = c(1, 1.25, 1.42))

pdf_path <- file.path(output_dir, "expression_backend_benchmark.pdf")
tiff_path <- file.path(output_dir, "expression_backend_benchmark.tiff")

ggsave(
  pdf_path,
  figure,
  device = grDevices::cairo_pdf,
  width = FIGURE_WIDTH_MM,
  height = FIGURE_HEIGHT_MM,
  units = "mm",
  bg = "white",
  family = FONT_FAMILY
)
ggsave(
  tiff_path,
  figure,
  device = function(filename, width, height, ...) {
    grDevices::tiff(
      filename,
      width = width,
      height = height,
      units = "in",
      res = COLOUR_DPI,
      compression = "lzw",
      type = "cairo",
      family = FONT_FAMILY,
      ...
    )
  },
  width = FIGURE_WIDTH_MM,
  height = FIGURE_HEIGHT_MM,
  units = "mm",
  dpi = COLOUR_DPI,
  bg = "white"
)

if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0) {
  stop("PDF export failed", call. = FALSE)
}
if (!file.exists(tiff_path) || file.info(tiff_path)$size <= 0) {
  stop("TIFF export failed", call. = FALSE)
}

pdffonts <- Sys.which("pdffonts")
if (!nzchar(pdffonts)) {
  stop("pdffonts is required to verify embedded PDF fonts", call. = FALSE)
}
font_table <- suppressWarnings(system2(pdffonts, pdf_path, stdout = TRUE))
writeLines(font_table, file.path(output_dir, "pdffonts.txt"))
font_rows <- font_table[grepl("Arial", font_table, ignore.case = TRUE)]
if (!length(font_rows) || !any(grepl("[[:space:]]yes[[:space:]]", font_rows))) {
  stop("Arial was not embedded in the PDF; inspect pdffonts.txt", call. = FALSE)
}

manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
legend <- c(
  "**Figure X. Expression-backend performance on complete million-cell public datasets.**",
  paste0(
    "(a) Backend construction time. (b) Total stored size of the CRB and its ",
    "external expression-backend sibling. (c) Fresh-process hydrated startup ",
    "through `readCerebro()`. (d) Resident and peak process memory after ",
    "hydration. (e) Warmed full-cell single-gene expression access. (f) ",
    "Full-cell 12-gene access and deterministic reverse-ordered, ",
    "non-contiguous access over up to 100,000 cells."
  ),
  paste0(
    "Points show medians and error bars show the observed minimum-to-maximum ",
    "range across three independent builds in (a,b) and six independent ",
    "fresh access processes in (c-f). Error bars are observed ranges, not ",
    "95% confidence intervals. Both datasets were analysed at their complete ",
    "cell counts."
  )
)
writeLines(legend, file.path(output_dir, "figure_legend.md"))

inputs <- c(
  "10_export.csv", "20_access.csv", "run_manifest.csv", "source_manifest.csv"
)
input_paths <- file.path(run_dir, inputs)
outputs <- c(basename(pdf_path), basename(tiff_path), "figure_legend.md")
output_paths <- file.path(output_dir, outputs)
plot_manifest <- data.frame(
  kind = c(rep("input", length(inputs)), rep("output", length(outputs))),
  path = c(input_paths, output_paths),
  bytes = as.numeric(file.info(c(input_paths, output_paths))$size),
  md5 = unname(tools::md5sum(c(input_paths, output_paths))),
  benchmark_run_id = manifest_values[["run_id"]],
  benchmark_git_sha = manifest_values[["git_sha"]],
  plotting_git_sha = plotting_git_sha,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  stringsAsFactors = FALSE
)
utils::write.csv(
  plot_manifest,
  file.path(output_dir, "plot_manifest.csv"),
  row.names = FALSE,
  na = ""
)

message("wrote Bioinformatics-ready figure to ", output_dir)
