# Scale plots. Called by benchmark_cli.R figure; writes images only.
#
# Statistical unit: one BUILD per dataset/size/backend. Each build is read by
# two nested access processes; a build value is the median of its two reads,
# the plotted point is the median across builds and the whiskers are the
# observed min-max (descriptive range, not a confidence interval). Cell counts
# are not biological replicates. No title or caption is written into the file.
bench_scale_style <- function() {
  list(
    palette = c(embedded = "#D55E00", h5 = "#0072B2", bpcells = "#E69F00"),
    shapes = c(embedded = 17, h5 = 15, bpcells = 16),
    labels = c(embedded = "Embedded", h5 = "H5", bpcells = "BPCells")
  )
}

bench_scale_family <- function() {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    candidates <- c("Arial", "Helvetica", "Nimbus Sans", "Liberation Sans", "DejaVu Sans")
    families <- unique(systemfonts::system_fonts()$family)
    hit <- candidates[candidates %in% families]
    if (length(hit)) return(hit[1])
  }
  "sans"
}

bench_scale_theme <- function(family) {
  ggplot2::theme_bw(base_size = 7, base_family = family) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#EDEDED", linewidth = 0.2),
      axis.line = ggplot2::element_line(colour = "#202020", linewidth = 0.25),
      axis.ticks = ggplot2::element_line(colour = "#202020", linewidth = 0.25),
      axis.ticks.length = grid::unit(1.5, "pt"),
      axis.text = ggplot2::element_text(colour = "#202020", size = 6.5),
      axis.title = ggplot2::element_text(colour = "#202020", size = 7),
      plot.title = ggplot2::element_text(size = 7, hjust = 0.5, margin = ggplot2::margin(b = 1.5)),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 7),
      legend.key.height = grid::unit(3.2, "mm"),
      legend.key.width = grid::unit(4.0, "mm"),
      plot.tag = ggplot2::element_text(size = 8, face = "bold"),
      plot.margin = ggplot2::margin(2, 3, 3, 2, unit = "mm")
    )
}

bench_scale_metrics <- function() {
  list(
    list(key = "hot_p50_secs", title = "Warmed single-gene read", unit = "Time (s), log10",
         divisor = 1, log = TRUE),
    list(key = "block_secs", title = "12-gene block read", unit = "Time (s), log10",
         divisor = 1, log = TRUE),
    list(key = "rss_mb", title = "Memory after load", unit = "RSS (GiB)",
         divisor = 1024, log = FALSE),
    list(key = "total_mb", title = "Total on-disk footprint", unit = "Storage (GiB), log10",
         divisor = 1024, log = TRUE)
  )
}

bench_scale_sources <- function(exports, access) {
  all_sources <- unique(c(access$source, exports$source))
  sources <- intersect(c("mouse_brain_e18", "human_pfc_hbcc"), all_sources)
  c(sources, setdiff(all_sources, sources))
}

bench_scale_full_cells <- function(source) {
  if (exists("BENCH_SOURCES", inherits = TRUE)) {
    value <- BENCH_SOURCES[[source]]$full_cells
    if (length(value) == 1L && is.finite(value)) return(value)
  }
  NA_real_
}

bench_scale_summary <- function(exports, access, metric) {
  raw <- if (metric$key == "total_mb") exports else access
  raw <- raw[raw$status == "OK", , drop = FALSE]
  raw <- raw[is.finite(raw[[metric$key]]), , drop = FALSE]
  keys <- c("source", "n_cells", "backend", "export_repeat")
  build <- aggregate(
    raw[[metric$key]], by = raw[keys], FUN = median, na.rm = TRUE
  )
  names(build)[ncol(build)] <- "value"
  summary <- do.call(rbind, lapply(
    split(build, list(build$source, build$n_cells, build$backend), drop = TRUE),
    function(d) {
      data.frame(
        source = d$source[1], n_cells = d$n_cells[1], backend = d$backend[1],
        n_builds = nrow(d), value = median(d$value),
        lo = min(d$value), hi = max(d$value), stringsAsFactors = FALSE
      )
    }
  ))
  summary
}

bench_scale_panels <- function(exports, access) {
  style <- bench_scale_style()
  sources <- bench_scale_sources(exports, access)
  source_titles <- c(mouse_brain_e18 = "Mouse brain E18", human_pfc_hbcc = "Human PFC (HBCC)")
  family <- bench_scale_family()
  plots <- list()
  for (metric in bench_scale_metrics()) {
    summary <- bench_scale_summary(exports, access, metric)
    stopifnot(nrow(summary) > 0L)
    tiers <- sort(unique(summary$n_cells))
    breaks <- if (length(tiers) <= 4L) tiers else c(1e3, 1e4, 5e4, 1e5, 3e5, 5e5, 1e6)
    rotate <- length(breaks) > 4L
    for (src in sources) {
      d <- summary[summary$source == src, , drop = FALSE]
      d$y <- d$value / metric$divisor
      d$lo <- d$lo / metric$divisor
      d$hi <- d$hi / metric$divisor
      present <- intersect(c("h5", "bpcells", "embedded"), unique(as.character(d$backend)))
      d$backend <- factor(as.character(d$backend), levels = present)
      title <- unname(source_titles[src])
      if (is.na(title)) title <- src
      full_x <- bench_scale_full_cells(src)
      subtitle <- if (is.finite(full_x)) {
        sprintf("Full dataset: %s cells", format(full_x, big.mark = ",", trim = TRUE))
      }
      p <- ggplot2::ggplot() +
        ggplot2::geom_errorbar(
          data = d, ggplot2::aes(n_cells, y, ymin = lo, ymax = hi, colour = backend),
          width = 0.02, linewidth = 0.35
        ) +
        ggplot2::geom_line(
          data = d, ggplot2::aes(n_cells, y, colour = backend), linewidth = 0.5
        ) +
        ggplot2::geom_point(
          data = d, ggplot2::aes(n_cells, y, colour = backend, shape = backend), size = 1.7
        ) +
        ggplot2::scale_x_log10(
          breaks = breaks,
          limits = if (is.finite(full_x)) c(800, full_x * 1.18),
          labels = function(x) ifelse(x >= 1e6, paste0(x / 1e6, "M"), paste0(x / 1e3, "k"))
        ) +
        ggplot2::scale_colour_manual(
          values = style$palette[present], labels = style$labels[present], drop = TRUE
        ) +
        ggplot2::scale_shape_manual(
          values = style$shapes[present], labels = style$labels[present], drop = TRUE
        ) +
        ggplot2::labs(title = paste0(metric$title, " | ", title), subtitle = subtitle,
                      x = "Cells (log10)", y = metric$unit) +
        bench_scale_theme(family) +
        ggplot2::theme(
          plot.subtitle = ggplot2::element_text(size = 6.5, hjust = 0.5, colour = "#404040",
                                                margin = ggplot2::margin(b = 2))
        )
      if (is.finite(full_x)) {
        p <- p + ggplot2::geom_vline(xintercept = full_x, linetype = "dotted",
                                     colour = "#6b6b6b", linewidth = 0.3)
      }
      if (rotate) {
        p <- p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5)
        )
      }
      p <- if (metric$log) {
        p + ggplot2::scale_y_log10(
          labels = function(x) format(x, trim = TRUE, scientific = FALSE)
        )
      } else {
        p + ggplot2::scale_y_continuous(
          labels = function(x) format(x, trim = TRUE, scientific = FALSE)
        )
      }
      plots[[length(plots) + 1L]] <- p
    }
  }
  list(plots = plots, sources = sources)
}

bench_scale_plot <- function(exports, access) {
  built <- bench_scale_panels(exports, access)
  patchwork::wrap_plots(
    built$plots, ncol = min(length(built$sources), 2L), guides = "collect"
  ) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(
      legend.position = "bottom",
      plot.tag.position = c(0, 1),
      plot.tag = ggplot2::element_text(size = 8, face = "bold")
    )
}

bench_scale_ceiling_plot <- function(exports) {
  style <- bench_scale_style()
  usable <- exports[
    exports$backend == "embedded" &
      exports$status == "OK" &
      is.finite(exports$r_peak_mb) &
      !is.na(exports$nnz),
    ,
    drop = FALSE
  ]
  if (!nrow(usable)) return(NULL)
  points <- do.call(rbind, lapply(
    split(usable, list(usable$source, usable$n_cells), drop = TRUE),
    function(d) {
      data.frame(
        source = d$source[1], n_cells = d$n_cells[1], nnz = d$nnz[1],
        n_builds = nrow(d), y = median(d$r_peak_mb) / 1024,
        lo = min(d$r_peak_mb) / 1024, hi = max(d$r_peak_mb) / 1024,
        stringsAsFactors = FALSE
      )
    }
  ))
  source_colours <- c(mouse_brain_e18 = "#0072B2", human_pfc_hbcc = "#D55E00")
  source_shapes <- c(mouse_brain_e18 = 16, human_pfc_hbcc = 17)
  source_titles <- c(mouse_brain_e18 = "Mouse brain E18", human_pfc_hbcc = "Human PFC (HBCC)")
  present <- intersect(names(source_colours), unique(points$source))
  points$src <- unname(source_titles[points$source])
  points$src[is.na(points$src)] <- points$source[is.na(points$src)]
  points$src <- factor(points$src, levels = unname(source_titles[present]))
  points$nnz_millions <- points$nnz / 1e6
  ggplot2::ggplot(points, ggplot2::aes(nnz_millions, y, colour = src, shape = src)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.5
    ) +
    ggplot2::geom_point(size = 2.0) +
    ggplot2::scale_colour_manual(
      values = unname(source_colours[present]),
      labels = unname(source_titles[present]), drop = TRUE
    ) +
    ggplot2::scale_shape_manual(
      values = unname(source_shapes[present]),
      labels = unname(source_titles[present]), drop = TRUE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, NA), breaks = scales::breaks_pretty(n = 5),
      labels = function(x) format(x, trim = TRUE, scientific = FALSE)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA), breaks = scales::breaks_pretty(n = 5),
      labels = function(x) format(x, trim = TRUE, scientific = FALSE)
    ) +
    ggplot2::labs(x = "Nonzero elements (millions)", y = "Peak R heap (GiB)") +
    bench_scale_theme(bench_scale_family()) +
    ggplot2::theme(legend.position = "bottom")
}

bench_figure_scale <- function(exports, access, manifest_values, out_dir) {
  if (!capabilities("cairo")) {
    message("note: cairo is unavailable; falling back to the standard PDF device")
  }
  plot <- bench_scale_plot(exports, access)
  sources <- bench_scale_sources(exports, access)
  rows <- ceiling(length(unique(c(access$source, exports$source))) * 4L /
                    min(length(sources), 2L))
  height_mm <- rows * 35 + 30
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  ggplot2::ggsave(
    file.path(out_dir, "expression_backend_benchmark_overview.pdf"), plot,
    device = device, width = 170, height = height_mm, units = "mm", scale = 1, bg = "white"
  )
  ggplot2::ggsave(
    file.path(out_dir, "expression_backend_benchmark_overview.png"), plot,
    width = 170, height = height_mm, units = "mm", dpi = 340, bg = "white"
  )
  ceiling <- bench_scale_ceiling_plot(exports)
  if (!is.null(ceiling)) {
    ggplot2::ggsave(
      file.path(out_dir, "expression_backend_benchmark_ceiling.png"), ceiling,
      width = 170, height = 115, units = "mm", dpi = 340, bg = "white"
    )
  }
  invisible(plot)
}
