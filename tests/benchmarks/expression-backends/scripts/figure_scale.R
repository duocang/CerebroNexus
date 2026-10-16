# Scale plots. Called by benchmark_cli.R figure; writes images only.
bench_figure_scale <- function(exports, access, manifest_values, out_dir) {
  exports$r_peak_mb[exports$r_peak_mb > 4e6] <- NA_real_
  if (!"startup_secs" %in% names(access)) {
    access$startup_secs <- access$load_secs + access$attach_secs
  }
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
    human_pfc_hbcc = "Human PFC (HBCC)",
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

  base <- theme_minimal(base_size = 10.5, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, colour = "grey35"),
        axis.text = element_text(size = 9, colour = "#30343B"),
        axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing.x = grid::unit(8, "mm"),
      plot.margin = margin(6, 8, 6, 6)
    )

  k_cells <- function(x) ifelse(x >= 1e6, paste0(x / 1e6, "M"), paste0(x / 1000, "k"))

  panel_line <- function(df, metric, title, subtitle, ylab, log_y = TRUE, divisor = 1) {
    median <- paste0(metric, "_median")
    minimum <- paste0(metric, "_min")
    maximum <- paste0(metric, "_max")
    # Resource-skipped exports have no measurements. Do not invent a y value.
    df <- df[!is.na(df[[median]]), , drop = FALSE]
    for (column in c(median, minimum, maximum)) df[[column]] <- df[[column]] / divisor
    if (log_y && any(!is.finite(as.matrix(df[c(median, minimum, maximum)])) |
                     as.matrix(df[c(median, minimum, maximum)]) <= 0)) {
      stop("Log-scale figures require finite, positive measurements", call. = FALSE)
    }
    stopifnot(all(is.finite(df$n_cells)), all(df$n_cells > 0))
    p <- ggplot(
      df,
      aes(
        .data[["n_cells"]],
        .data[[median]],
        colour = .data[["backend"]],
        shape = .data[["backend"]],
        group = .data[["backend"]]
      )
    ) +
      geom_errorbar(
        aes(ymin = .data[[minimum]], ymax = .data[[maximum]]),
        width = 0,
        linewidth = 0.45
      ) +
      geom_point(size = 2.2) +
      facet_wrap(~src) +
      scale_colour_manual(values = backend_cols,
                          labels = c(embedded = "Embedded", bpcells = "BPCells", h5 = "H5")) +
      scale_shape_manual(values = c(embedded = 17, bpcells = 16, h5 = 15),
                         labels = c(embedded = "Embedded", bpcells = "BPCells", h5 = "H5")) +
      scale_x_log10(breaks = c(1e3, 1e4, 5e4, 1e5, 3e5, 5e5, 1e6), labels = k_cells,
                    expand = expansion(mult = c(0.045, 0.065))) +
      labs(title = title, subtitle = subtitle, x = "Cells (log10 scale)",
           y = if (log_y) paste0(ylab, ", log10 scale") else ylab) +
      base
    if (length(unique(df$n_cells)) > 1L) {
      p <- p + geom_line(linewidth = 0.6)
    }
    if (log_y) {
      p <- p + scale_y_log10(labels = function(x) vapply(x, format, character(1), trim = TRUE, scientific = FALSE),
                            breaks = if (metric == "rss_mb") c(0.5, 1, 2, 5, 10) else scales::breaks_log(),
                            expand = expansion(mult = c(0.07, 0.12)))
    }
    p
  }

  p_hot <- panel_line(
    access_summary,
    "hot_p50_secs",
    "Interactive single-gene latency",
    "warmed expression lookup; median and range; log scale",
    "Time (s)"
  )
  p_block <- panel_line(
    access_summary,
    "block_secs",
    "Marker-panel block latency",
    "12 genes across all cells; median and range; log scale",
    "Time (s)"
  )
  p_rss <- panel_line(
    access_summary,
    "rss_mb",
    "Resident memory after load and attach",
    "loaded process memory, not build-time peak; median and range",
    "RSS (GiB)",
    divisor = 1024
  )

  p_disk <- panel_line(
    export_summary, "total_mb", "Total on-disk footprint",
    "CRB plus external expression data; median and range",
    "Storage (GiB)", divisor = 1024
  )

  skipped <- unique(exports[
    exports$backend == "embedded" & grepl("^SKIPPED", exports$status),
    c("source", "n_cells", "status"), drop = FALSE
  ])
  skipped_note <- if (nrow(skipped)) paste(vapply(seq_len(nrow(skipped)), function(i) {
    label <- unname(pretty_source[skipped$source[i]])
    if (is.na(label)) label <- skipped$source[i]
    paste0(label, " ", k_cells(skipped$n_cells[i]),
           ": ", sub("^SKIPPED\\(resource\\): preflight: ", "", skipped$status[i]))
  }, character(1)), collapse = "; ") else "No resource-skipped builds."
  count_range <- function(data) {
    counts <- as.numeric(table(interaction(data$source, data$n_cells, data$backend, drop = TRUE)))
    paste(unique(range(counts)), collapse = "-")
  }

  overview <- (p_hot / p_block / p_rss / p_disk) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A",
      title = "Expression backend scaling",
      caption = paste0(
        "Both axes: log10. Points: medians. Whiskers: min-max, not confidence intervals. Lower is better.\n",
        "Per completed source/tier/backend: ", count_range(exports[exports$status == "OK", ]),
        " builds; ", count_range(access), " read processes (nested within builds).\n",
        "Embedded skipped: ", skipped_note, ".\n",
        "Full endpoints: Mouse 1,306,127; Human 1,486,324 cells. Embedded above 500k is not scheduled.\n",
        "Lines join measured tiers; gaps are not measurements."
      ),
      theme = theme(plot.title = element_text(size = 15, face = "bold"),
                    plot.caption = element_text(size = 8, hjust = 0, lineheight = 1.2),
                    plot.margin = margin(8, 8, 8, 8))) &
    theme(legend.position = "bottom")

  ggsave(
    file.path(out_dir, "expression_backend_benchmark_overview.png"),
    overview,
    width = 240,
    height = 270,
    units = "mm",
    dpi = 350,
    bg = "white"
  )
  ggsave(file.path(out_dir, "expression_backend_benchmark_overview.pdf"),
         overview, device = grDevices::pdf, useDingbats = FALSE,
         width = 240, height = 270, units = "mm", bg = "white")

  usable <- exports[
    exports$backend == "embedded" &
      exports$status == "OK" &
      is.finite(exports$r_peak_mb) &
      !is.na(exports$nnz),
    ,
    drop = FALSE
  ]
  points <- bench_summarise_metrics(
    usable,
    group = c("source", "n_cells", "nnz"),
    metrics = "r_peak_mb"
  )
  points <- add_source_label(points)
  points$nnz_millions <- points$nnz / 1e6
  for (column in c("r_peak_mb_median", "r_peak_mb_min", "r_peak_mb_max")) {
    points[[column]] <- points[[column]] / 1024
  }
  sources <- unique(points$src)
  source_colours <- setNames(rep(c("#3F7F93", "#B4553F", "#80749B"),
                                length.out = length(sources)), sources)
  source_shapes <- setNames(rep(c(16, 17, 15), length.out = length(sources)), sources)
  tier_note <- paste(vapply(sources, function(src) {
    paste0(src, ": ", paste(k_cells(sort(unique(points$n_cells[points$src == src]))),
                           collapse = ", "), " cells")
  }, character(1)), collapse = "\n")

  # Only measured peaks belong on this plot: a preflight skip is not a memory ceiling.
  build_memory <- ggplot(points, aes(
    .data[["nnz_millions"]], .data[["r_peak_mb_median"]],
    colour = .data[["src"]], shape = .data[["src"]]
  )) +
    geom_errorbar(aes(ymin = .data[["r_peak_mb_min"]], ymax = .data[["r_peak_mb_max"]]),
                  width = 0, linewidth = 0.55) +
    geom_point(size = 2.8) +
    scale_colour_manual(values = source_colours) +
    scale_shape_manual(values = source_shapes) +
    scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_pretty(n = 5),
                       labels = scales::label_number(big.mark = ","),
                       expand = expansion(mult = c(0.02, 0.06))) +
    scale_y_continuous(limits = c(0, NA), breaks = scales::breaks_pretty(n = 5),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(
      title = "Embedded build peak memory",
      subtitle = sprintf("%d measured dataset/size pairs; %s builds per pair; median and min-max range",
                         nrow(points), count_range(usable)),
      x = "Nonzero elements (millions)",
      y = "Peak R heap (GiB)",
      caption = paste0(tier_note, if (nrow(skipped)) paste0("\nResource-skipped: ", skipped_note, "."))
    ) + base +
    theme(plot.caption = element_text(size = 8, hjust = 0, lineheight = 1.2),
          plot.title.position = "plot", plot.caption.position = "plot")

  # Keep the established filename so existing benchmark archives remain compatible.
  ggsave(file.path(out_dir, "expression_backend_benchmark_ceiling.png"),
         build_memory, width = 210, height = 145, units = "mm", dpi = 350, bg = "white")
  invisible(build_memory)
}
