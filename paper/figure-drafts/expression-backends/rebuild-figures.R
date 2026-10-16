# Draft re-render of the expression-backend benchmark figures in one shared
# style (Genome Biology submission target).
#
# Data inputs (read-only):
#   scale: tests/benchmarks/results/expression-backends/scale/runs/
#          20261016T111536Z-81d11e8aedfa-scale-assembled/{10_export,20_access}.csv
#          (the published five-build assembled Scale collection)
#   full:  tests/benchmarks/results/expression-backends/full/runs/
#          20261015T213433Z-81d11e8aedfa-full/{10_export,20_access}.csv
#
# Statistical unit: one BUILD (export_repeat) per dataset/size/backend.
# Each build is read by two nested access processes; a build value is the
# median of its two process values. Across the five builds the plotted point
# is the median and the whisker is the observed min-max (descriptive range,
# not a confidence interval). Cell counts are not biological replicates.

args <- commandArgs(TRUE)
root <- normalizePath(if (length(args)) args[1] else getwd())
opts <- list(family = NULL, stdpdf = FALSE)
for (a in args) {
  if (grepl("^--family=", a)) opts$family <- sub("^--family=", "", a)
  if (identical(a, "--stdpdf")) opts$stdpdf <- TRUE
}

scale_dir <- file.path(root, "tests/benchmarks/results/expression-backends/scale/runs",
                       "20261016T111536Z-81d11e8aedfa-scale-assembled")
full_dir <- file.path(root, "tests/benchmarks/results/expression-backends/full/runs",
                      "20261015T213433Z-81d11e8aedfa-full")
out_dir <- file.path(root, "paper/figure-drafts/expression-backends")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# --- font selection and reporting (no silent substitution) -------------------
pick_font <- function(candidates) {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    families <- unique(systemfonts::system_fonts()$family)
    hit <- candidates[candidates %in% families]
    if (length(hit)) return(list(family = hit[1], matched = TRUE))
  }
  list(family = candidates[1], matched = FALSE)
}
font <- pick_font(c("Arial", "Helvetica", "Nimbus Sans", "Liberation Sans", "DejaVu Sans"))
cat(sprintf("cairo available: %s\n", isTRUE(capabilities("cairo"))))
cat(sprintf("font requested: Arial/Helvetica; used: '%s' (exact name match: %s)\n",
            font$family, font$matched))
cat(sprintf("R %s / %s; ggplot2 %s; patchwork %s\n",
            getRversion(), R.version$platform,
            as.character(packageVersion("ggplot2")),
            as.character(packageVersion("patchwork"))))

FAM <- if (!is.null(opts$family)) opts$family else font$family
if (opts$stdpdf) cat(sprintf("stdpdf pass: using '%s' text on the standard PDF device\n", FAM))

# --- shared style -----------------------------------------------------------
palette <- c(embedded = "#D55E00", h5 = "#0072B2", bpcells = "#E69F00")
shapes  <- c(embedded = 17,      h5 = 15,      bpcells = 16)
labels  <- c(embedded = "Embedded", h5 = "H5", bpcells = "BPCells")

source_levels <- c("mouse_brain_e18", "human_pfc_hbcc")
source_titles <- c(mouse_brain_e18 = "Mouse brain E18", human_pfc_hbcc = "Human PFC (HBCC)")

metrics <- list(
  list(key = "hot_p50_secs", title = "Warmed single-gene read",   unit = "Time (s)",        divisor = 1,    log = TRUE),
  list(key = "block_secs",   title = "12-gene block read",        unit = "Time (s)",        divisor = 1,    log = TRUE),
  list(key = "rss_mb",       title = "Memory after load",         unit = "RSS (GiB)",       divisor = 1024, log = FALSE),
  list(key = "total_mb",     title = "Total on-disk footprint",   unit = "Storage (GiB)",   divisor = 1024, log = TRUE)
)
y_label <- function(metric) paste0(metric$unit, if (metric$log) ", log10" else "")
y_scale <- function(metric) {
  if (metric$log) {
    scale_y_log10(labels = function(x) format(x, trim = TRUE, scientific = FALSE))
  } else {
    scale_y_continuous(labels = function(x) format(x, trim = TRUE, scientific = FALSE))
  }
}

base_theme <- function() {
  theme_bw(base_size = 7, base_family = FAM) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.border     = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#EDEDED", linewidth = 0.2),
      axis.line        = element_line(colour = "#202020", linewidth = 0.25),
      axis.ticks       = element_line(colour = "#202020", linewidth = 0.25),
      axis.ticks.length = unit(1.5, "pt"),
      axis.text        = element_text(colour = "#202020", size = 6.5),
      axis.title       = element_text(colour = "#202020", size = 7),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = 7),
      legend.title     = element_blank(),
      legend.text      = element_text(size = 7),
      legend.key.height = unit(3.2, "mm"),
      legend.key.width  = unit(4.0, "mm"),
      plot.tag         = element_text(size = 8, face = "bold"),
      plot.margin      = margin(2, 3, 2, 2, unit = "mm")
    )
}

# --- data loading -----------------------------------------------------------
read_ok <- function(dir, name) {
  d <- read.csv(file.path(dir, name), stringsAsFactors = FALSE)
  d <- d[d$status == "OK", , drop = FALSE]
  if ("correctness" %in% names(d)) d <- d[d$correctness == "OK", , drop = FALSE]
  d
}

scale_access  <- read_ok(scale_dir, "20_access.csv")
scale_exports <- read_ok(scale_dir, "10_export.csv")
full_access   <- read_ok(full_dir,  "20_access.csv")
full_exports  <- read_ok(full_dir,  "10_export.csv")

# --- scale figure data ------------------------------------------------------
# One value per build/backend/metric, then median and min-max across builds.
build_medians <- function(data, keys, metrics_keys) {
  do.call(rbind, lapply(metrics_keys, function(m) {
    agg <- aggregate(data[[m]], by = data[keys], FUN = median, na.rm = TRUE)
    names(agg)[ncol(agg)] <- "value"
    agg$metric <- m
    agg
  }))
}
keys_access <- c("source", "n_cells", "backend", "source_run_id", "export_repeat")

acc_build <- build_medians(scale_access, keys_access, c("hot_p50_secs", "block_secs", "rss_mb"))
exp_build <- build_medians(scale_exports, keys_access, c("total_mb"))
builds <- rbind(acc_build, exp_build)

# Five-build summary per combination; the feasibility build and the four
# repeats of Human 500k embedded share a key and therefore pool to five.
summary_rows <- do.call(rbind, lapply(split(builds, list(builds$source, builds$n_cells, builds$backend, builds$metric), drop = TRUE), function(d) {
  data.frame(source = d$source[1], n_cells = d$n_cells[1], backend = d$backend[1], metric = d$metric[1],
             n_builds = nrow(d), value = median(d$value),
             lo = min(d$value), hi = max(d$value), stringsAsFactors = FALSE)
}))
tier_of <- function(d) {
  is_full <- d$tier_type == "full"
  if (any(is_full)) "full endpoint" else "scale grid"
}
tiers <- do.call(rbind, lapply(split(scale_access, list(scale_access$source, scale_access$n_cells, scale_access$backend), drop = TRUE), function(d) {
  data.frame(source = d$source[1], n_cells = d$n_cells[1], backend = d$backend[1],
             tier = tier_of(d), stringsAsFactors = FALSE)
}))
tiers_exp <- do.call(rbind, lapply(split(scale_exports, list(scale_exports$source, scale_exports$n_cells, scale_exports$backend), drop = TRUE), function(d) {
  data.frame(source = d$source[1], n_cells = d$n_cells[1], backend = d$backend[1],
             tier = tier_of(d), stringsAsFactors = FALSE)
}))
tiers <- unique(rbind(tiers, tiers_exp))
summary_rows <- merge(summary_rows, tiers, by = c("source", "n_cells", "backend"), all.x = TRUE)
backends_scale <- c("embedded", "bpcells", "h5")
stopifnot(all(summary_rows$backend %in% backends_scale),
          all(summary_rows$n_builds == 5L))

# --- full figure data -------------------------------------------------------
full_acc <- full_access
full_exp <- full_exports
stopifnot(setequal(unique(full_acc$backend), c("h5", "bpcells")),
          all(table(full_acc$source, full_acc$backend) == 10L)) # 5 builds x 2 reads

# --- figure builder ---------------------------------------------------------
scale_panel <- function(metric, src) {
  d <- summary_rows[summary_rows$metric == metric$key & summary_rows$source == src, , drop = FALSE]
  grid <- d[d$tier == "scale grid", , drop = FALSE]
  endp <- d[d$tier == "full endpoint", , drop = FALSE]
  grid$y <- grid$value / metric$divisor; grid$lo <- grid$lo / metric$divisor; grid$hi <- grid$hi / metric$divisor
  endp$y <- endp$value / metric$divisor
  grid$backend <- factor(grid$backend, levels = c("h5", "bpcells", "embedded"))
  endp$backend <- factor(endp$backend, levels = c("h5", "bpcells", "embedded"))
  dmax <- max(grid$n_cells)
  bridge <- merge(grid[grid$n_cells == dmax, c("backend", "y", "n_cells")],
                  endp[, c("backend", "y", "n_cells")], by = "backend", suffixes = c("", "_end"))
  bridge$backend <- factor(bridge$backend, levels = c("h5", "bpcells", "embedded"))
  full_x <- unique(endp$n_cells)
  if (!length(full_x)) full_x <- NA_real_
  subtitle <- if (is.finite(full_x)) {
    sprintf("Full dataset: %s cells", format(full_x, big.mark = ",", trim = TRUE))
  }
  p <- ggplot() +
    geom_errorbar(data = grid, aes(n_cells, y, ymin = lo, ymax = hi, colour = backend),
                  width = 0.02, linewidth = 0.35) +
    geom_line(data = grid, aes(n_cells, y, colour = backend), linewidth = 0.5) +
    geom_point(data = grid, aes(n_cells, y, colour = backend, shape = backend), size = 1.7) +
    geom_segment(data = bridge, aes(n_cells, y, xend = n_cells_end, yend = y_end, colour = backend),
                 linetype = "dashed", linewidth = 0.4) +
    geom_point(data = endp, aes(n_cells, y, colour = backend), shape = 21, size = 2.0,
               stroke = 0.4, fill = "white") +
    scale_x_log10(breaks = c(1e3, 1e4, 5e4, 1e5, 3e5, 5e5, 1e6),
                  limits = if (is.finite(full_x)) c(800, full_x * 1.18),
                  labels = function(x) ifelse(x >= 1e6, paste0(x / 1e6, "M"), paste0(x / 1e3, "k"))) +
    y_scale(metric) +
    scale_colour_manual(values = palette, labels = labels, drop = FALSE) +
    scale_shape_manual(values = shapes, labels = labels, drop = FALSE) +
    labs(title = paste0(metric$title, " | ", source_titles[[src]]),
         subtitle = subtitle,
         x = "Cells (log10)", y = y_label(metric)) +
    base_theme() +
    theme(plot.title = element_text(size = 7, hjust = 0.5, margin = margin(b = 1)),
          plot.subtitle = element_text(size = 6.5, hjust = 0.5, colour = "#404040",
                                       margin = margin(b = 2)),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
          plot.margin = margin(2, 3, 3, 2, unit = "mm"))
  if (is.finite(full_x)) {
    p <- p + geom_vline(xintercept = full_x, linetype = "dotted",
                        colour = "#6b6b6b", linewidth = 0.3)
  }
  p
}

full_panel <- function(metric, src) {
  raw <- if (metric$key == "total_mb") {
    full_exp[full_exp$source == src, , drop = FALSE]
  } else {
    full_acc[full_acc$source == src, , drop = FALSE]
  }
  raw$value <- raw[[metric$key]] / metric$divisor
  raw$backend <- factor(raw$backend, levels = c("h5", "bpcells"))
  builds <- aggregate(raw["value"], by = raw[c("backend", "export_repeat")], FUN = median)
  centre <- do.call(rbind, lapply(levels(raw$backend), function(b) {
    v <- builds$value[builds$backend == b]
    data.frame(backend = b, y = median(v), lo = min(v), hi = max(v), stringsAsFactors = FALSE)
  }))
  centre$backend <- factor(centre$backend, levels = c("h5", "bpcells"))
  raw$x <- as.numeric(raw$backend)
  raw$offset <- ave(raw$x, raw$backend, FUN = function(z) seq(-0.16, 0.16, length.out = length(z)))
  centre$x <- as.numeric(centre$backend)
  cell_count <- unique(raw$n_cells)
  stopifnot(length(cell_count) == 1L)
  ggplot() +
    geom_point(data = raw, aes(x + offset, value, colour = backend), size = 1.2, alpha = 0.5) +
    geom_errorbar(data = centre, aes(x, y, ymin = lo, ymax = hi, colour = backend),
                  width = 0.07, linewidth = 0.5) +
    geom_point(data = centre, aes(x, y, colour = backend, shape = backend), size = 2.0) +
    scale_x_continuous(breaks = 1:2, labels = c("H5", "BPCells"), limits = c(0.6, 2.4)) +
    y_scale(metric) +
    scale_colour_manual(values = palette[c("h5", "bpcells")],
                        labels = labels[c("h5", "bpcells")], drop = TRUE) +
    scale_shape_manual(values = shapes[c("h5", "bpcells")],
                       labels = labels[c("h5", "bpcells")], drop = TRUE) +
    labs(title = paste0(metric$title, " | ", source_titles[[src]]),
         subtitle = sprintf("%s cells", format(cell_count, big.mark = ",", trim = TRUE)),
         x = NULL, y = y_label(metric)) +
    base_theme() +
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 7, hjust = 0.5, margin = margin(b = 1)),
          plot.subtitle = element_text(size = 6.5, hjust = 0.5, colour = "#404040",
                                       margin = margin(b = 2)))
}

make_figure <- function(panel_fun, tag) {
  panels <- list()
  for (m in metrics) for (src in source_levels) panels[[length(panels) + 1L]] <- panel_fun(m, src)
  wrap_plots(panels, ncol = 2, guides = "collect") +
    plot_annotation(tag_levels = "a") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0, 1),
          plot.tag = element_text(size = 8, face = "bold"))
}

scale_fig <- make_figure(scale_panel)
full_fig  <- make_figure(full_panel)

# --- export -----------------------------------------------------------------
save_pdf_png <- function(plot, stem, height_mm) {
  if (opts$stdpdf) {
    # Geometry-audit pass on R's standard PDF device. The collision auditor
    # merges Cairo glyph traces into artificial boxes; this twin exists only
    # so the same layout can be audited without that artifact.
    grDevices::pdf(paste0(stem, "-stdpdf.pdf"), width = 170 / 25.4, height = height_mm / 25.4,
                   family = "Helvetica", useDingbats = FALSE, bg = "white")
    print(plot)
    grDevices::dev.off()
    cat(sprintf("wrote %s-stdpdf.pdf (170 x %s mm)\n", stem, height_mm))
    return(invisible(NULL))
  }
  ggsave(paste0(stem, ".pdf"), plot, device = grDevices::cairo_pdf,
         width = 170, height = height_mm, units = "mm", scale = 1, bg = "white")
  ggsave(paste0(stem, ".png"), plot,
         width = 170, height = height_mm, units = "mm", dpi = 340, bg = "white")
  cat(sprintf("wrote %s.pdf / .png (170 x %s mm)\n", stem, height_mm))
}

save_pdf_png(full_fig,  file.path(out_dir, "fig-full-expression-backends"), 168)
save_pdf_png(scale_fig, file.path(out_dir, "fig-scale-expression-backends"), 182)
