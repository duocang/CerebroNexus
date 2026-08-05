# Publication figure helpers for the real-data expression-backend benchmark.

BENCH_BACKEND_LEVELS <- c("embedded", "h5", "bpcells")
BENCH_BACKEND_COLOURS <- c(
  embedded = "#D55E00",
  h5 = "#0072B2",
  bpcells = "#009E73"
)
BENCH_BACKEND_SHAPES <- c(embedded = 16, h5 = 17, bpcells = 15)
BENCH_BACKEND_LABELS <- c(
  embedded = "Embedded",
  h5 = "H5",
  bpcells = "BPCells"
)
BENCH_SOURCE_LABELS <- c(
  mouse_brain_e18 = "Mouse E18",
  human_pfc_hbcc = "Human PFC · HBCC",
  human_pfc_mssm = "Human PFC · MSSM"
)

bench_format_cells <- function(x) {
  ifelse(
    x >= 1e6,
    paste0(format(round(x / 1e6, 1), trim = TRUE), "M"),
    paste0(format(round(x / 1e3), trim = TRUE), "k")
  )
}

bench_prepare_plot_data <- function(x) {
  if (!nrow(x)) {
    return(x)
  }
  source_label <- unname(BENCH_SOURCE_LABELS[x$source])
  source_label[is.na(source_label)] <- x$source[is.na(source_label)]
  x$source_label <- source_label
  x$backend <- factor(x$backend, levels = BENCH_BACKEND_LEVELS)

  keys <- unique(x[c("source", "source_label", "n_cells")])
  source_order <- match(keys$source, names(BENCH_SOURCE_LABELS))
  source_order[is.na(source_order)] <- length(BENCH_SOURCE_LABELS) +
    seq_len(sum(is.na(source_order)))
  keys <- keys[order(source_order, keys$n_cells), , drop = FALSE]
  keys$tier_label <- paste0(
    keys$source_label,
    "\n",
    bench_format_cells(keys$n_cells),
    " cells"
  )
  key_id <- paste(x$source, x$n_cells, sep = "\r")
  key_levels <- paste(keys$source, keys$n_cells, sep = "\r")
  x$tier_label <- factor(
    keys$tier_label[match(key_id, key_levels)],
    levels = keys$tier_label
  )
  x
}

bench_nature_theme <- function(base_size = 8.5) {
  ggplot2::theme_classic(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      axis.title = ggplot2::element_text(colour = "#222222"),
      axis.text = ggplot2::element_text(colour = "#333333"),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#333333"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "#333333"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", colour = "#222222"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.key.width = grid::unit(12, "pt"),
      panel.spacing = grid::unit(7, "pt"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(
        colour = "#555555",
        size = base_size - 0.5,
        margin = ggplot2::margin(b = 4)
      ),
      plot.caption = ggplot2::element_text(
        colour = "#666666",
        size = base_size - 1,
        hjust = 0
      ),
      plot.tag = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.margin = ggplot2::margin(5, 6, 5, 5)
    )
}

bench_backend_scales <- function() {
  list(
    ggplot2::scale_colour_manual(
      values = BENCH_BACKEND_COLOURS,
      labels = BENCH_BACKEND_LABELS,
      drop = FALSE
    ),
    ggplot2::scale_shape_manual(
      values = BENCH_BACKEND_SHAPES,
      labels = BENCH_BACKEND_LABELS,
      drop = FALSE
    )
  )
}

bench_missing_panel <- function(title, message) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.53,
      label = message,
      colour = "#555555",
      size = 2.7,
      lineheight = 1.05
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(title = title) +
    bench_nature_theme() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
}

bench_metric_panel <- function(
  x,
  metric,
  title,
  y_label,
  log_y = FALSE,
  missing_message = "No valid measurements"
) {
  if (!metric %in% names(x)) {
    return(bench_missing_panel(title, missing_message))
  }
  x <- x[is.finite(x[[metric]]), , drop = FALSE]
  if (log_y) {
    x <- x[x[[metric]] > 0, , drop = FALSE]
  }
  if (!nrow(x)) {
    return(bench_missing_panel(title, missing_message))
  }

  summary <- bench_summarise_metrics(
    x,
    group = c("tier_label", "backend"),
    metrics = metric
  )
  median_col <- paste0(metric, "_median")
  min_col <- paste0(metric, "_min")
  max_col <- paste0(metric, "_max")
  dodge <- ggplot2::position_dodge(width = 0.68)

  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = .data[["tier_label"]],
      y = .data[[metric]],
      colour = .data[["backend"]],
      shape = .data[["backend"]]
    )
  ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitterdodge(
        jitter.width = 0.07,
        jitter.height = 0,
        dodge.width = 0.68,
        seed = 1
      ),
      size = 1.1,
      alpha = 0.28,
      stroke = 0.2
    ) +
    ggplot2::geom_pointrange(
      data = summary,
      ggplot2::aes(
        y = .data[[median_col]],
        ymin = .data[[min_col]],
        ymax = .data[[max_col]],
        group = .data[["backend"]]
      ),
      position = dodge,
      linewidth = 0.42,
      size = 1.45
    ) +
    bench_backend_scales() +
    ggplot2::labs(title = title, x = NULL, y = y_label) +
    bench_nature_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 28, hjust = 1, vjust = 1),
      legend.position = "bottom"
    )
  if (log_y) {
    p <- p + ggplot2::scale_y_log10(labels = scales::label_number())
  } else {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_number())
  }
  p
}

bench_build_overview <- function(exports, access, run_id) {
  exports <- bench_prepare_plot_data(exports)
  access <- bench_prepare_plot_data(bench_normalize_access_metrics(access))
  access$backend_ready_secs <- access$load_secs + access$attach_secs

  panels <- list(
    bench_metric_panel(exports, "export_secs", "Export", "time (s)", TRUE),
    bench_metric_panel(exports, "total_mb", "Stored footprint", "MB", TRUE),
    bench_metric_panel(
      access,
      "backend_ready_secs",
      "Backend ready",
      "time (s)",
      TRUE
    ),
    bench_metric_panel(access, "rss_mb", "Resident memory", "RSS (MB)"),
    bench_metric_panel(
      access,
      "hot_p50_secs",
      "Warmed single-gene query",
      "time (s)",
      TRUE
    ),
    bench_metric_panel(
      access,
      "block_ready_secs",
      "Materialized 12-gene block",
      "time (s)",
      TRUE,
      "Legacy run: only lazy-view\npreparation was timed.\nRe-run publication for this panel."
    )
  )
  patchwork::wrap_plots(panels, ncol = 2, guides = "collect") +
    patchwork::plot_annotation(
      title = "Expression-backend trade-offs on real public datasets",
      subtitle = paste0(
        "Raw independent-process repeats with median and range · run ",
        run_id
      ),
      tag_levels = "a",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12),
        plot.subtitle = ggplot2::element_text(colour = "#555555", size = 8.5),
        plot.tag = ggplot2::element_text(face = "bold", size = 11)
      )
    ) &
    ggplot2::theme(legend.position = "bottom")
}

bench_build_observed_scaling <- function(exports) {
  x <- exports[
    exports$status == "OK" &
      is.finite(exports$r_peak_mb) &
      exports$r_peak_mb < 4e6 &
      is.finite(exports$nnz),
    ,
    drop = FALSE
  ]
  x <- bench_prepare_plot_data(x)
  summary <- bench_summarise_metrics(
    x,
    group = c("source_label", "backend", "n_cells", "nnz"),
    metrics = "r_peak_mb"
  )
  ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = .data[["nnz"]],
      y = .data[["r_peak_mb"]],
      colour = .data[["source_label"]]
    )
  ) +
    ggplot2::geom_point(size = 1.2, alpha = 0.3) +
    ggplot2::geom_line(
      data = summary,
      ggplot2::aes(
        y = .data[["r_peak_mb_median"]],
        group = .data[["source_label"]]
      ),
      linewidth = 0.45,
      alpha = 0.7
    ) +
    ggplot2::geom_pointrange(
      data = summary,
      ggplot2::aes(
        y = .data[["r_peak_mb_median"]],
        ymin = .data[["r_peak_mb_min"]],
        ymax = .data[["r_peak_mb_max"]]
      ),
      linewidth = 0.45,
      size = 1.45
    ) +
    ggplot2::facet_wrap(
      ~backend,
      nrow = 1,
      labeller = ggplot2::as_labeller(BENCH_BACKEND_LABELS)
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Mouse E18" = "#6A51A3",
        "Human PFC · HBCC" = "#1F78B4",
        "Human PFC · MSSM" = "#E6550D"
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(
      title = "Observed export-memory scaling",
      subtitle = "Successful runs only; lines connect observed medians and do not estimate a capacity boundary",
      x = "non-zero values in exported tier",
      y = "peak R heap (MB)",
      colour = "dataset"
    ) +
    bench_nature_theme(base_size = 9)
}

bench_build_repeat_plot <- function(exports) {
  x <- bench_prepare_plot_data(exports[exports$status == "OK", , drop = FALSE])
  ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = .data[["order_position"]],
      y = .data[["export_secs"]]
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        group = interaction(
          .data[["source"]],
          .data[["n_cells"]],
          .data[["export_repeat"]]
        )
      ),
      colour = "#C7C7C7",
      linewidth = 0.45
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        colour = .data[["backend"]],
        shape = .data[["backend"]]
      ),
      size = 2,
      stroke = 0.35
    ) +
    ggplot2::facet_wrap(~tier_label, scales = "free_y", ncol = 2) +
    bench_backend_scales() +
    ggplot2::scale_x_continuous(breaks = 1:3) +
    ggplot2::scale_y_log10(labels = scales::label_number()) +
    ggplot2::labs(
      title = "Order-balanced export repeats",
      subtitle = "Each grey path is one independent repeat; backend order rotates across repeats",
      x = "position within repeat",
      y = "export time (s)"
    ) +
    bench_nature_theme(base_size = 9)
}

bench_build_query_latency <- function(access) {
  x <- bench_prepare_plot_data(bench_normalize_access_metrics(access))
  metrics <- c(first_query_secs = "First gene", hot_p50_secs = "Warmed gene")
  if (any(is.finite(x$block_ready_secs))) {
    metrics <- c(metrics, block_ready_secs = "12-gene materialized")
  }
  long <- do.call(
    rbind,
    lapply(names(metrics), function(metric) {
      data.frame(
        tier_label = x$tier_label,
        backend = x$backend,
        metric = metrics[[metric]],
        seconds = x[[metric]],
        stringsAsFactors = FALSE
      )
    })
  )
  long <- long[is.finite(long$seconds) & long$seconds > 0, , drop = FALSE]
  long$metric <- factor(long$metric, levels = unname(metrics))
  summary <- bench_summarise_metrics(
    transform(long, status = "OK"),
    group = c("tier_label", "backend", "metric"),
    metrics = "seconds"
  )

  ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .data[["metric"]],
      y = .data[["seconds"]],
      colour = .data[["backend"]],
      shape = .data[["backend"]]
    )
  ) +
    ggplot2::geom_jitter(width = 0.08, height = 0, alpha = 0.25, size = 1) +
    ggplot2::geom_pointrange(
      data = summary,
      ggplot2::aes(
        y = .data[["seconds_median"]],
        ymin = .data[["seconds_min"]],
        ymax = .data[["seconds_max"]]
      ),
      position = ggplot2::position_dodge(width = 0.48),
      linewidth = 0.4,
      size = 1.4
    ) +
    ggplot2::facet_wrap(~tier_label, ncol = 2) +
    bench_backend_scales() +
    ggplot2::scale_y_log10(labels = scales::label_number()) +
    ggplot2::labs(
      title = "Fresh-process and warmed query latency",
      subtitle = if (length(metrics) < 3L) {
        "Legacy block preparation is omitted because it is not a materialized read"
      } else {
        "Block latency includes native-view preparation plus dense materialization"
      },
      x = NULL,
      y = "time (s)"
    ) +
    bench_nature_theme(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 22, hjust = 1))
}

bench_build_pareto <- function(exports, access) {
  exports <- bench_prepare_plot_data(exports)
  access <- bench_prepare_plot_data(access)
  disk <- bench_summarise_metrics(
    exports,
    group = c("source", "n_cells", "tier_label", "backend"),
    metrics = "total_mb"
  )
  memory <- bench_summarise_metrics(
    access,
    group = c("source", "n_cells", "tier_label", "backend"),
    metrics = "rss_mb"
  )
  x <- merge(
    disk,
    memory,
    by = c("source", "n_cells", "tier_label", "backend"),
    all = FALSE
  )
  x$backend <- factor(x$backend, levels = BENCH_BACKEND_LEVELS)

  ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = .data[["total_mb_median"]],
      y = .data[["rss_mb_median"]],
      colour = .data[["backend"]],
      shape = .data[["backend"]]
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data[["rss_mb_min"]],
        ymax = .data[["rss_mb_max"]]
      ),
      width = 0,
      linewidth = 0.4
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data[["total_mb_min"]],
        xmax = .data[["total_mb_max"]]
      ),
      orientation = "y",
      width = 0,
      linewidth = 0.4
    ) +
    ggplot2::geom_point(size = 2.6, stroke = 0.55) +
    ggplot2::facet_wrap(~tier_label, scales = "free", ncol = 2) +
    bench_backend_scales() +
    ggplot2::scale_x_log10(labels = scales::label_number()) +
    ggplot2::scale_y_log10(labels = scales::label_number()) +
    ggplot2::labs(
      title = "Storage–memory Pareto surface",
      subtitle = "Lower-left combines smaller bundles with lower runtime RSS",
      x = "stored footprint (MB)",
      y = "runtime RSS (MB)"
    ) +
    bench_nature_theme(base_size = 9)
}

bench_build_correctness <- function(access) {
  x <- bench_prepare_plot_data(access)
  correct <- x$correctness == "OK" &
    x$row_fingerprint == x$reference_row_fingerprint &
    x$block_fingerprint == x$reference_block_fingerprint
  groups <- split(
    seq_len(nrow(x)),
    interaction(x$tier_label, x$backend, drop = TRUE, lex.order = TRUE)
  )
  audit <- do.call(
    rbind,
    lapply(groups, function(index) {
      data.frame(
        tier_label = x$tier_label[index[1]],
        backend = x$backend[index[1]],
        passed = sum(correct[index], na.rm = TRUE),
        total = length(index),
        fraction = mean(correct[index], na.rm = TRUE)
      )
    })
  )
  audit$label <- paste0(audit$passed, "/", audit$total)
  audit$backend <- factor(audit$backend, levels = BENCH_BACKEND_LEVELS)

  ggplot2::ggplot(
    audit,
    ggplot2::aes(
      x = .data[["backend"]],
      y = .data[["tier_label"]],
      fill = .data[["fraction"]]
    )
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = .data[["label"]]), size = 3) +
    ggplot2::scale_x_discrete(labels = BENCH_BACKEND_LABELS) +
    ggplot2::scale_fill_gradientn(
      colours = c("#B2182B", "#F7F7F7", "#009E73"),
      limits = c(0, 1),
      labels = scales::label_percent(),
      name = "fingerprints matched"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(
        title.position = "top",
        barwidth = grid::unit(100, "pt"),
        barheight = grid::unit(6, "pt")
      )
    ) +
    ggplot2::labs(
      title = "Correctness and completeness audit",
      subtitle = sprintf(
        "%d/%d fresh access processes matched both source fingerprints",
        sum(audit$passed),
        sum(audit$total)
      ),
      x = NULL,
      y = NULL
    ) +
    bench_nature_theme(base_size = 9) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

bench_build_publication_figures <- function(exports, access, manifest_values) {
  exports$r_peak_mb[exports$r_peak_mb > 4e6] <- NA_real_
  list(
    expression_backend_benchmark_overview = list(
      plot = bench_build_overview(exports, access, manifest_values[["run_id"]]),
      width = 7.09,
      height = 8.4
    ),
    expression_backend_benchmark_observed_scaling = list(
      plot = bench_build_observed_scaling(exports),
      width = 7.09,
      height = 3.5
    ),
    expression_backend_benchmark_repeats = list(
      plot = bench_build_repeat_plot(exports),
      width = 7.09,
      height = 5.4
    ),
    expression_backend_benchmark_query_latency = list(
      plot = bench_build_query_latency(access),
      width = 7.09,
      height = 5.4
    ),
    expression_backend_benchmark_pareto = list(
      plot = bench_build_pareto(exports, access),
      width = 7.09,
      height = 5.4
    ),
    expression_backend_benchmark_correctness = list(
      plot = bench_build_correctness(access),
      width = 7.09,
      height = 3.7
    )
  )
}

bench_save_plot <- function(plot, path, width, height, dpi = 600L) {
  extension <- tolower(tools::file_ext(path))
  if (identical(extension, "png")) {
    args <- list(
      filename = path,
      width = width,
      height = height,
      units = "in",
      res = dpi,
      bg = "white"
    )
    if (capabilities("cairo")) {
      args$type <- "cairo"
    }
    do.call(grDevices::png, args)
  } else if (identical(extension, "pdf")) {
    if (capabilities("cairo")) {
      grDevices::cairo_pdf(path, width = width, height = height, bg = "white")
    } else {
      grDevices::pdf(path, width = width, height = height, bg = "white")
    }
  } else if (identical(extension, "svg")) {
    if (!capabilities("cairo")) {
      stop("SVG output requires Cairo graphics support", call. = FALSE)
    }
    grDevices::svg(path, width = width, height = height, bg = "white")
  } else {
    stop("unsupported figure extension: ", extension, call. = FALSE)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(path)
}

bench_save_publication_figures <- function(figures, out_dir, dpi = 600L) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (stem in names(figures)) {
    figure <- figures[[stem]]
    for (extension in c("png", "pdf", "svg")) {
      bench_save_plot(
        figure$plot,
        file.path(out_dir, paste0(stem, ".", extension)),
        width = figure$width,
        height = figure$height,
        dpi = dpi
      )
    }
  }
  invisible(file.path(out_dir, bench_publication_figure_files()))
}
