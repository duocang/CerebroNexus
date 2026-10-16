# Full-source plots. Called by benchmark_cli.R figure; writes images only.
#
# Statistical unit: one BUILD per backend. Each build is read by two nested
# access processes; the plotted dots are those individual read processes (not
# independent replicates), the marker is the build median and the whiskers are
# the observed min-max across builds. Cell counts are not biological
# replicates. No figure title or caption is written into the file.
bench_full_style <- function() {
  palette <- c(embedded = "#D55E00", h5 = "#0072B2", bpcells = "#E69F00")
  shapes <- c(embedded = 17, h5 = 15, bpcells = 16)
  list(
    palette = palette,
    shapes = shapes,
    labels = c(embedded = "Embedded", h5 = "H5", bpcells = "BPCells")
  )
}

bench_full_family <- function() {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    candidates <- c("Arial", "Helvetica", "Nimbus Sans", "Liberation Sans", "DejaVu Sans")
    families <- unique(systemfonts::system_fonts()$family)
    hit <- candidates[candidates %in% families]
    if (length(hit)) return(hit[1])
  }
  "sans"
}

bench_full_theme <- function(family) {
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
      plot.margin = ggplot2::margin(2, 3, 2, 2, unit = "mm")
    )
}

bench_full_sources <- function(access) {
  sources <- intersect(c("mouse_brain_e18", "human_pfc_hbcc"), unique(access$source))
  c(sources, setdiff(unique(access$source), sources))
}

bench_full_panels <- function(exports, access) {
  stopifnot(
    all(access$status == "OK"), all(exports$status == "OK"),
    all(access$correctness == "OK"),
    all(access$access_contract == "materialized-v2")
  )
  style <- bench_full_style()
  sources <- bench_full_sources(access)
  source_titles <- c(mouse_brain_e18 = "Mouse brain E18", human_pfc_hbcc = "Human PFC (HBCC)")
  metrics <- list(
    list(key = "hot_p50_secs", title = "Warmed single-gene read", unit = "Time (s), log10",
         divisor = 1, log = TRUE),
    list(key = "block_secs", title = "12-gene block read", unit = "Time (s), log10",
         divisor = 1, log = TRUE),
    list(key = "rss_mb", title = "Memory after load", unit = "RSS (GiB)",
         divisor = 1024, log = FALSE),
    list(key = "total_mb", title = "Total on-disk footprint", unit = "Storage (GiB), log10",
         divisor = 1024, log = TRUE)
  )
  family <- bench_full_family()
  plots <- list()
  for (spec in metrics) {
    raw <- if (spec$key == "total_mb") exports else access
    for (src in sources) {
      dat <- raw[raw$source == src, , drop = FALSE]
      dat$value <- dat[[spec$key]] / spec$divisor
      stopifnot(all(is.finite(dat$value)), all(dat$value >= 0))
      if (spec$log) stopifnot(all(dat$value > 0))
      present <- intersect(c("h5", "bpcells", "embedded"), unique(as.character(dat$backend)))
      dat$backend <- factor(as.character(dat$backend), levels = present)
      # Reduce read processes to one value per build before summarizing builds.
      builds <- aggregate(dat["value"], by = dat[c("backend", "export_repeat")],
                          FUN = median)
      centre <- do.call(rbind, lapply(present, function(b) {
        values <- builds$value[builds$backend == b]
        data.frame(backend = b, x = match(b, present), y = median(values),
                   lo = min(values), hi = max(values), stringsAsFactors = FALSE)
      }))
      centre$backend <- factor(centre$backend, levels = present)
      dat$x <- match(as.character(dat$backend), present)
      dat$offset <- ave(dat$x, dat$backend, FUN = function(z) {
        if (length(z) == 1L) return(0)
        seq(-0.16, 0.16, length.out = length(z))
      })
      title <- unname(source_titles[src])
      if (is.na(title)) title <- src
      cell_count <- unique(dat$n_cells)
      stopifnot(length(cell_count) == 1L)
      p <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = dat, ggplot2::aes(x + offset, value, colour = backend),
          size = 1.2, alpha = 0.5
        ) +
        ggplot2::geom_errorbar(
          data = centre,
          ggplot2::aes(x, y, ymin = lo, ymax = hi, colour = backend),
          width = 0.07, linewidth = 0.5
        ) +
        ggplot2::geom_point(
          data = centre,
          ggplot2::aes(x, y, colour = backend, shape = backend), size = 2.0
        ) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(present),
          labels = unname(style$labels[present]),
          limits = c(0.6, length(present) + 0.4)
        ) +
        ggplot2::scale_colour_manual(
          values = style$palette[present], labels = style$labels[present], drop = TRUE
        ) +
        ggplot2::scale_shape_manual(
          values = style$shapes[present], labels = style$labels[present], drop = TRUE
        ) +
        ggplot2::labs(
          title = paste0(spec$title, " | ", title),
          subtitle = sprintf("%s cells", format(cell_count, big.mark = ",", trim = TRUE)),
          x = NULL, y = spec$unit
        ) +
        bench_full_theme(family) +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_text(size = 6.5, hjust = 0.5, colour = "#404040",
                                                margin = ggplot2::margin(b = 2))
        )
      p <- if (spec$log) {
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

bench_full_plot <- function(exports, access) {
  built <- bench_full_panels(exports, access)
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

bench_figure_full <- function(exports, access, out_dir) {
  if (!capabilities("cairo")) {
    message("note: cairo is unavailable; falling back to the standard PDF device")
  }
  plot <- bench_full_plot(exports, access)
  sources <- bench_full_sources(access)
  rows <- ceiling(length(unique(access$source)) * 4L / min(length(sources), 2L))
  height_mm <- rows * 33 + 28
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  stem <- file.path(out_dir, "expression_backend_benchmark_overview")
  device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  ggplot2::ggsave(paste0(stem, ".pdf"), plot, device = device,
                  width = 170, height = height_mm, units = "mm", scale = 1, bg = "white")
  ggplot2::ggsave(paste0(stem, ".png"), plot,
                  width = 170, height = height_mm, units = "mm", dpi = 340, bg = "white")
  invisible(plot)
}
