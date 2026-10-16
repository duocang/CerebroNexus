# Full-source plots. Called by benchmark_cli.R figure; writes images only.
# Read timings are nested within builds; the dots are not independent builds.
bench_full_plot <- function(exports, access) {
  sources <- c("mouse_brain_e18", "human_pfc_hbcc")
  source_names <- c("Mouse brain E18", "Human PFC (HBCC)")
  backends <- c("h5", "bpcells")
  colours <- c(h5 = "#3F7F93", bpcells = "#D9A03C")
  stopifnot(
    setequal(unique(access$source), sources),
    setequal(unique(exports$source), sources),
    all(access$backend %in% backends), all(exports$backend %in% backends),
    all(access$status == "OK"), all(exports$status == "OK"),
    all(access$correctness == "OK"),
    all(access$access_contract == "materialized-v2")
  )
  # Reuse the original four metrics without filtering or sampling any rows.
  specs <- list(
    list(data = access, metric = "hot_p50_secs", divisor = 1,
         title = "Warmed single-gene read", unit = "Time (s, log10 scale)", log = TRUE),
    list(data = access, metric = "block_secs", divisor = 1,
         title = "12-gene read across all cells", unit = "Time (s)", log = FALSE),
    list(data = access, metric = "rss_mb", divisor = 1,
         title = "Memory after load and attach", unit = "Process RSS (MiB)", log = FALSE),
    list(data = exports, metric = "total_mb", divisor = 1024,
         title = "Total on-disk footprint", unit = "Storage (GiB, log10 scale)", log = TRUE)
  )
  plots <- list()
  for (row in seq_along(specs)) {
    spec <- specs[[row]]
    raw <- spec$data
    raw$value <- raw[[spec$metric]] / spec$divisor
    stopifnot(all(is.finite(raw$value)), all(raw$value >= 0))
    if (spec$log) stopifnot(all(raw$value > 0))
    limits <- if (spec$log) {
      range(raw$value) * c(0.75, 1.8)
    } else if (spec$metric == "rss_mb") {
      range(raw$value) + diff(range(raw$value)) * c(-0.3, 0.5)
    } else c(0, max(raw$value) * 1.3)
    for (col in seq_along(sources)) {
      dat <- raw[raw$source == sources[col], , drop = FALSE]
      dat <- dat[order(dat$backend, dat$export_repeat,
                       if ("access_repeat" %in% names(dat)) dat$access_repeat else rep(0, nrow(dat))), ]
      dat$x <- match(dat$backend, backends)
      dat$offset <- ave(dat$x, dat$backend, FUN = function(x) seq(-0.17, 0.17, length.out = length(x)))
      centre <- do.call(rbind, lapply(backends, function(backend) {
        values <- dat$value[dat$backend == backend]
        stopifnot(length(values) > 0L)
        data.frame(backend = backend, x = match(backend, backends),
                   median = median(values), lower = min(values), upper = max(values))
      }))
      centre$label <- if (spec$metric == "rss_mb") {
        format(round(centre$median), big.mark = ",", trim = TRUE)
      } else vapply(centre$median, function(x) format(signif(x, 3), scientific = FALSE, trim = TRUE), character(1))
      cell_count <- unique(dat$n_cells)
      stopifnot(length(cell_count) == 1L)
      p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = value, colour = backend)) +
        ggplot2::geom_point(ggplot2::aes(x = x + offset), size = 1.6, alpha = 0.55) +
        ggplot2::geom_errorbar(data = centre,
          ggplot2::aes(x = x, ymin = lower, ymax = upper, colour = backend),
          inherit.aes = FALSE, width = 0.11, linewidth = 0.45) +
        ggplot2::geom_point(data = centre,
          ggplot2::aes(x = x, y = median, fill = backend), inherit.aes = FALSE,
          shape = 23, size = 2.8, stroke = 0.45, colour = "#202020") +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::scale_fill_manual(values = colours) +
        ggplot2::scale_x_continuous(breaks = 1:2,
                                   labels = paste0(c("H5", "BPCells"), "\n", centre$label),
                                   limits = c(0.55, 2.45)) +
        ggplot2::labs(
          title = if (col == 1L) paste0(letters[row], "  ", spec$title) else " ",
          subtitle = if (row == 1L) paste0(source_names[col], " | ",
            format(cell_count, big.mark = ",", scientific = FALSE, trim = TRUE), " cells") else NULL,
          x = NULL, y = spec$unit
        ) +
        ggplot2::theme_minimal(base_size = 10, base_family = "sans") +
        ggplot2::theme(
          legend.position = "none",
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(colour = "#E5E7EB", linewidth = 0.3),
          axis.text = ggplot2::element_text(colour = "#30343B", size = 9.5),
          axis.title.y = ggplot2::element_text(size = 9.5, margin = ggplot2::margin(r = 5)),
          plot.title = ggplot2::element_text(face = "bold", size = 11, margin = ggplot2::margin(b = 6)),
          plot.subtitle = ggplot2::element_text(size = 9, margin = ggplot2::margin(b = 6)),
          plot.margin = ggplot2::margin(5, 8, 5, 5)
        )
      p <- if (spec$log) {
        p + ggplot2::scale_y_log10(limits = limits,
          breaks = if (spec$metric == "hot_p50_secs") c(0.03, 0.1, 0.3, 1) else c(3, 10, 30, 100),
          labels = scales::label_number(), expand = ggplot2::expansion(mult = 0))
      } else {
        p + ggplot2::scale_y_continuous(limits = limits,
          expand = ggplot2::expansion(mult = 0), n.breaks = 4)
      }
      plots[[length(plots) + 1L]] <- p
    }
  }
  access_counts <- table(access$source, access$backend)
  build_counts <- table(exports$source, exports$backend)
  stopifnot(length(unique(as.vector(access_counts))) == 1L,
            length(unique(as.vector(build_counts))) == 1L)
  patchwork::wrap_plots(plots, ncol = 2) + patchwork::plot_annotation(
    title = "Full-dataset expression backend comparison",
    caption = paste0(
      "Small dots: all recorded processes. Diamonds: median. Whiskers: observed min-max (not confidence intervals).\n",
      "Per dataset/backend: ", as.vector(build_counts)[1], " independent builds; ",
      as.vector(access_counts)[1], " read processes (2 per build). Read processes share built artifacts.\n",
      "Values below backend names are medians. A and D: log10 axes; C: expanded linear axis (not from zero).\n",
      "Single-gene timing uses each process's warmed median. Embedded exceeds sparse-index capacity."
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0, lineheight = 1.2,
                                          margin = ggplot2::margin(t = 8)),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
  )
}

bench_figure_full <- function(exports, access, out_dir) {
  plot <- bench_full_plot(exports, access)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  stem <- file.path(out_dir, "expression_backend_benchmark_overview")
  grDevices::pdf(paste0(stem, ".pdf"), width = 220 / 25.4, height = 230 / 25.4,
                 family = "Helvetica", useDingbats = FALSE)
  print(plot)
  grDevices::dev.off()
  ggplot2::ggsave(paste0(stem, ".png"), plot, width = 220, height = 230,
                  units = "mm", dpi = 350, bg = "white")
}
