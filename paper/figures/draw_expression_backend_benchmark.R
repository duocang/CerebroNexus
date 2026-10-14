#!/usr/bin/env Rscript

# Expression-backend figure export; final submission requires rendered review.
#
# Usage:
#   Rscript paper/figures/draw_expression_backend_benchmark.R \
#     tests/benchmarks/results/expression-backends/full/runs/<run-id> \
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
resolve_output <- function(path) {
  path <- path.expand(path)
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = TRUE))
  parent <- resolve_output(dirname(path))
  switch(basename(path), "." = parent, ".." = dirname(parent), file.path(parent, basename(path)))
}
output_dir <- resolve_output(args[[2L]])
input_path <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
compare_output <- output_dir
if (.Platform$OS.type == "windows") {
  input_path <- tolower(input_path)
  compare_output <- tolower(compare_output)
}
if (identical(compare_output, input_path) || startsWith(compare_output, paste0(input_path, "/"))) {
  stop("figure output must be outside the immutable input run", call. = FALSE)
}
if (file.exists(output_dir) && (!dir.exists(output_dir) ||
    length(list.files(output_dir, all.files = TRUE, no.. = TRUE)))) {
  stop("figure output must be a new or empty directory; previous exports are preserved", call. = FALSE)
}
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

FONT_FAMILY <- "Arial"
FIGURE_WIDTH_MM <- 178
FIGURE_HEIGHT_MM <- 132
COLOUR_DPI <- 350
OKABE_ITO <- c(BPCells = "#0072B2", H5 = "#E69F00")

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
manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
if (!identical(manifest_values[["profile"]], "panel_c2")) {
  stop("paper figure requires the full-dataset profile (panel_c2), not quick or scale", call. = FALSE)
}

if (!nrow(exports) || !nrow(access) || !all(exports$status %in% "OK") ||
    !all(access$status %in% "OK")) {
  stop("paper figure requires an all-OK published run", call. = FALSE)
}
if (!"correctness" %in% names(access) || !all(access$correctness %in% "OK")) {
  stop(
    "paper figure requires correctness=OK for every access row",
    call. = FALSE
  )
}

if (!"access_contract" %in% names(access) || anyNA(access$access_contract) ||
    any(access$access_contract != "materialized-v2")) {
  stop("paper figure requires freshly measured materialized-v2 access results", call. = FALSE)
}

required_export <- c("source", "backend", "export_secs", "total_mb")
required_access <- c(
  "source",
  "backend",
  "startup_secs",
  "rss_mb",
  "peak_rss_mb",
  "hot_p50_secs",
  "block_secs",
  "subset_row_secs",
  "subset_block_secs"
)
if (!all(required_export %in% names(exports))) {
  stop("10_export.csv lacks required plotting columns", call. = FALSE)
}
if (!all(required_access %in% names(access))) {
  stop("20_access.csv lacks required plotting columns", call. = FALSE)
}

# Reuse the measurement and evidence checks, including build/access pairing.
bench_root <- file.path(repo, "tests", "benchmarks", "expression-backends", "scripts")
Sys.setenv(BENCH_ROOT = bench_root, BENCH_PROFILE = "panel_c2")
source(file.path(bench_root, "benchmark.R"), local = TRUE)
expected_cells <- vapply(BENCH_SOURCES[c("mouse_brain_e18", "human_pfc_hbcc")],
  function(x) x$full_cells, numeric(1))
for (data in list(exports, access)) {
  if (!setequal(data$source, names(expected_cells)) ||
      !setequal(data$backend, c("bpcells", "h5")) ||
      anyNA(data$n_cells) || any(data$n_cells != expected_cells[data$source])) {
    stop("paper figure requires both complete datasets and H5/BPCells only", call. = FALSE)
  }
}
for (stage in c("validate", "check")) {
  status <- system2(file.path(R.home("bin"), "Rscript"),
    shQuote(c(file.path(bench_root, "benchmark_cli.R"), stage, run_dir)))
  if (status != 0L) stop("paper figure input failed ", stage, call. = FALSE)
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
if (!requireNamespace("systemfonts", quietly = TRUE)) {
  stop("the systemfonts package is required to verify Arial", call. = FALSE)
}
font_match <- systemfonts::match_fonts(FONT_FAMILY)
if (!nrow(font_match) || !file.exists(font_match$path[[1L]]) ||
    !grepl("arial", basename(font_match$path[[1L]]), ignore.case = TRUE)) {
  stop("Arial is required; font substitution is not allowed", call. = FALSE)
}
pdffonts <- Sys.which("pdffonts")
if (!nzchar(pdffonts)) {
  stop("pdffonts is required to verify embedded PDF fonts", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
  rows <- lapply(pieces, function(x) {
    data.frame(
      source_label = x$source_label[[1L]],
      backend_label = x$backend_label[[1L]],
      metric = label,
      median = stats::median(x$value),
      minimum = min(x$value),
      maximum = max(x$value),
      n = nrow(x),
      stringsAsFactors = FALSE
    )
  })
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
      strip.placement = "outside",
      plot.margin = margin(5, 6, 3, 5, unit = "mm"),
      legend.position = if (show_legend) c(0.78, 0.22) else "none",
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
      plot.tag.position = c(0, 1.02)
    )
}

dumbbell_plot <- function(
  summary,
  x_label,
  log_scale = FALSE,
  show_legend = FALSE,
  facet_columns = NULL,
  facet_ncol = 1L
) {
  plot <- ggplot(
    summary,
    aes(
      x = median,
      y = source_label,
      colour = backend_label,
      shape = backend_label,
      group = source_label
    )
  ) +
    geom_line(
      colour = "grey68",
      linewidth = 0.5,
      aes(group = interaction(source_label, metric))
    ) +
    geom_errorbar(
      aes(xmin = minimum, xmax = maximum),
      orientation = "y",
      width = 0.11,
      linewidth = 0.45
    ) +
    geom_point(size = 2.35, stroke = 0.75) +
    scale_colour_manual(values = OKABE_ITO, drop = FALSE) +
    scale_shape_manual(values = c(BPCells = 16, H5 = 17), drop = FALSE) +
    labs(x = x_label, y = NULL, colour = NULL, shape = NULL) +
    theme_bioinformatics(show_legend)
  if (!is.null(facet_columns)) {
    plot <- plot +
      facet_wrap(
        stats::as.formula(paste("~", facet_columns)),
        ncol = facet_ncol,
        scales = "free_x"
      )
  }
  if (log_scale) {
    if (any(!is.finite(summary$minimum) | summary$minimum <= 0)) {
      stop("log-scale latency requires strictly positive measurements", call. = FALSE)
    }
    plot <- plot +
      scale_x_log10(
        breaks = scales::breaks_log(n = 3),
        labels = scales::label_number(accuracy = 0.01)
      )
  } else {
    plot <- plot + expand_limits(x = 0)
  }
  plot
}

build <- summarise_metric(exports, "export_secs")
storage <- summarise_metric(exports, "total_mb", scale = 1024)
startup <- summarise_metric(access, "startup_secs")
hot <- summarise_metric(access, "hot_p50_secs")

resources <- rbind(
  summarise_metric(access, "startup_secs", label = "Hydrated startup (s)"),
  summarise_metric(access, "rss_mb", label = "Resident RSS (MiB)"),
  summarise_metric(access, "peak_rss_mb", label = "Peak RSS (MiB)")
)
resources$metric <- factor(
  resources$metric,
  levels = c("Hydrated startup (s)", "Resident RSS (MiB)", "Peak RSS (MiB)")
)

blocks <- rbind(
  summarise_metric(
    access,
    "hot_p50_secs",
    label = "1 gene, all cells"
  ),
  summarise_metric(access, "block_secs", label = "12 genes, all cells"),
  summarise_metric(
    access,
    "subset_row_secs",
    label = "1 gene, 100k cells"
  ),
  summarise_metric(
    access,
    "subset_block_secs",
    label = "12 genes, 100k cells"
  )
)
blocks$metric <- factor(
  blocks$metric,
  levels = c(
    "1 gene, all cells",
    "12 genes, all cells",
    "1 gene, 100k cells",
    "12 genes, 100k cells"
  )
)

p_a <- dumbbell_plot(
  build,
  "Backend construction time (s)",
  show_legend = TRUE
)
p_b <- dumbbell_plot(storage, "Stored size (GiB)")
p_c <- dumbbell_plot(
  resources,
  NULL,
  facet_columns = "metric",
  facet_ncol = 1L
) +
  theme(strip.text = element_text(size = 6.6, family = FONT_FAMILY))
p_d <- dumbbell_plot(
  blocks,
  "Expression access latency (s; log scale)",
  log_scale = TRUE,
  facet_columns = "metric",
  facet_ncol = 2L
) +
  theme(strip.text = element_text(size = 6.2, family = FONT_FAMILY))

figure <- (p_a | p_b) /
  (p_c | p_d) +
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
  plot_layout(heights = c(0.72, 1.28))

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

font_table <- suppressWarnings(system2(pdffonts, shQuote(pdf_path), stdout = TRUE))
writeLines(font_table, file.path(output_dir, "pdffonts.txt"))
font_rows <- font_table[-seq_len(min(2L, length(font_table)))]
embedded <- vapply(strsplit(trimws(font_rows), "[[:space:]]+"), function(row) {
  length(row) >= 8L && identical(tail(row, 5L)[[1L]], "yes")
}, logical(1))
if (!is.null(attr(font_table, "status")) || !length(font_rows) || !all(embedded) ||
    !any(grepl("Arial", font_rows, ignore.case = TRUE))) {
  stop("PDF font embedding check failed; inspect pdffonts.txt", call. = FALSE)
}

repeat_counts <- function(data) {
  counts <- as.integer(table(interaction(data$source, data$backend, drop = TRUE)))
  paste(sort(unique(counts)), collapse = "/")
}
legend <- c(
  "**Figure X. Expression-backend performance on complete million-cell public datasets.**",
  paste0(
    "(a) Backend construction time. (b) Total stored size of the CRB and its ",
    "external expression-backend sibling. (c) Fresh-process hydrated startup ",
    "through `readCerebro()` and resident/peak process memory after hydration. ",
    "(d) Warmed full-cell expression access and deterministic reverse-ordered, ",
    "non-contiguous access over up to 100,000 cells. Grey connectors join the ",
    "two backend medians for the same complete dataset."
  ),
  paste0(
    "Points show medians and error bars show the observed minimum-to-maximum ",
    "range across ", repeat_counts(exports), " independent builds per source/backend in (a,b) and ",
    repeat_counts(access), " fresh access processes in (c,d), nested within those builds. ",
    "Error bars are observed ranges, not ",
    "95% confidence intervals. Both datasets were analysed at their complete ",
    "cell counts."
  )
)
writeLines(legend, file.path(output_dir, "figure_legend.md"))

inputs <- c(
  "10_export.csv",
  "20_access.csv",
  "run_manifest.csv",
  "source_manifest.csv"
)
input_paths <- file.path(run_dir, inputs)
outputs <- c(basename(pdf_path), basename(tiff_path), "figure_legend.md")
source_dir <- file.path(output_dir, "source-data")
dir.create(source_dir)
if (!all(file.copy(input_paths, source_dir)) ||
    !file.copy(script_path, file.path(output_dir, basename(script_path)))) {
  stop("failed to save figure source data and plotting code", call. = FALSE)
}
outputs <- c(outputs, "pdffonts.txt", file.path("source-data", inputs), basename(script_path))
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

message("wrote figure export to ", output_dir, "; review the rendered layout before submission")
