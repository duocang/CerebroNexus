builder_eval_figure_files <- function() {
  c(
    "builder_eval_capability.png",
    "builder_eval_plan_identity.png",
    "builder_eval_artifact_fidelity.png",
    "builder_eval_build_timing.png",
    "builder_eval_recovery.png",
    "builder_eval_incremental.png",
    "builder_eval_claim_boundaries.png",
    "builder_evidence_chain.svg"
  )
}

.builder_eval_figure_theme <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    )
}

.builder_eval_save_plot <- function(plot, path, width, height) {
  ggplot2::ggsave(
    path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.builder_eval_short_label <- function(value) {
  value <- gsub("_r[0-9]+$", "", value)
  gsub("_", " ", value, fixed = TRUE)
}

.builder_eval_evidence_chain_svg <- function(path) {
  stages <- c("Inspect", "Freeze", "Build", "Verify", "Publish", "Recover")
  x <- seq(80, 920, length.out = length(stages))
  boxes <- vapply(
    seq_along(stages),
    function(index) {
      paste0(
        '<rect x="',
        x[[index]] - 62,
        '" y="70" width="124" height="58" rx="9" fill="#F7FBFF" ',
        'stroke="#24557A" stroke-width="2"/>',
        '<text x="',
        x[[index]],
        '" y="105" text-anchor="middle" font-family="sans-serif" ',
        'font-size="17" font-weight="bold" fill="#17324D">',
        stages[[index]],
        "</text>"
      )
    },
    character(1)
  )
  arrows <- vapply(
    seq_len(length(stages) - 1L),
    function(index) {
      paste0(
        '<line x1="',
        x[[index]] + 66,
        '" y1="99" x2="',
        x[[index + 1L]] - 70,
        '" y2="99" stroke="#586A7A" stroke-width="2.5" ',
        'marker-end="url(#arrow)"/>'
      )
    },
    character(1)
  )
  writeLines(
    c(
      '<svg xmlns="http://www.w3.org/2000/svg" width="1000" height="230" viewBox="0 0 1000 230" role="img" aria-labelledby="title desc">',
      '<title id="title">Builder evidence chain</title>',
      '<desc id="desc">Inspect, freeze, build, verify, publish, and recover stages connected by arrows. Independent evidence is recorded below the chain.</desc>',
      '<defs><marker id="arrow" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto"><path d="M0,0 L0,6 L9,3 z" fill="#586A7A"/></marker></defs>',
      '<rect width="1000" height="230" fill="white"/>',
      boxes,
      arrows,
      '<path d="M80 151 H920" stroke="#BD6B21" stroke-width="3" stroke-dasharray="7 5"/>',
      '<text x="500" y="183" text-anchor="middle" font-family="sans-serif" font-size="16" fill="#6A3B12">plan digest · artifact contents · release journal · managed fingerprints</text>',
      '<text x="500" y="211" text-anchor="middle" font-family="sans-serif" font-size="13" fill="#44515C">Each claim is tied to a recorded boundary, not inferred from the interface.</text>',
      "</svg>"
    ),
    path,
    useBytes = TRUE
  )
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

builder_eval_generate_figures <- function(tables, output) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Builder evidence figures require ggplot2.")
  }
  if (!is.list(tables) || !length(tables)) {
    stop("Validated Builder evidence tables are required.")
  }
  if (file.exists(output) && !dir.exists(output)) {
    stop("The Builder evidence figure destination is not a directory.")
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  palette <- c(
    PASS = "#18794E",
    FAIL = "#B42318",
    TP = "#18794E",
    TN = "#6B7C8F",
    FP = "#B42318",
    FN = "#D97706",
    Reused = "#2B6CB0",
    Rebuilt = "#BD6B21",
    Supported = "#18794E",
    `Not evaluated` = "#6B7C8F"
  )
  theme <- .builder_eval_figure_theme()
  paths <- file.path(output, builder_eval_figure_files())
  names(paths) <- builder_eval_figure_files()

  capability <- builder_eval_capability_summary(tables$capability_detection)
  capability_long <- rbind(
    data.frame(
      capability = capability$capability,
      metric = "Precision",
      value = capability$precision,
      positive_support = capability$positive_support,
      negative_support = capability$negative_support
    ),
    data.frame(
      capability = capability$capability,
      metric = "Recall",
      value = capability$recall,
      positive_support = capability$positive_support,
      negative_support = capability$negative_support
    )
  )
  capability_long$capability <- factor(
    capability_long$capability,
    levels = rev(capability$capability)
  )
  capability_plot <- ggplot2::ggplot(
    capability_long,
    ggplot2::aes(x = value, y = capability, shape = metric, colour = metric)
  ) +
    ggplot2::geom_point(size = 2.8, na.rm = TRUE) +
    ggplot2::geom_text(
      data = capability_long[capability_long$metric == "Recall", ],
      ggplot2::aes(
        x = 0.02,
        y = capability,
        label = paste0("+", positive_support, " / −", negative_support)
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3,
      colour = "#44515C"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1.04), breaks = c(0, 0.5, 1)) +
    ggplot2::scale_colour_manual(
      values = c(Precision = "#2B6CB0", Recall = "#BD6B21")
    ) +
    ggplot2::labs(
      title = "Capability detection with explicit support",
      subtitle = "+ positive / − negative truth observations",
      x = "Score",
      y = NULL,
      colour = NULL,
      shape = NULL
    ) +
    theme
  .builder_eval_save_plot(
    capability_plot,
    paths[["builder_eval_capability.png"]],
    7.2,
    5.2
  )

  plan <- tables$plan_immutability
  plan$dataset <- .builder_eval_short_label(plan$trial_id)
  plan$status <- ifelse(plan$matches_confirmation, "PASS", "FAIL")
  plan <- stats::aggregate(
    plan$status,
    list(dataset = plan$dataset, boundary = plan$boundary),
    function(value) if (all(value == "PASS")) "PASS" else "FAIL"
  )
  names(plan)[[3L]] <- "status"
  plan_plot <- ggplot2::ggplot(
    plan,
    ggplot2::aes(x = boundary, y = dataset, fill = status)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(status == "PASS", "✓", "×")),
      size = 4
    ) +
    ggplot2::scale_fill_manual(values = palette[c("PASS", "FAIL")]) +
    ggplot2::labs(
      title = "Frozen plan identity at independent boundaries",
      x = NULL,
      y = NULL,
      fill = "Digest match"
    ) +
    theme
  .builder_eval_save_plot(
    plan_plot,
    paths[["builder_eval_plan_identity.png"]],
    7.2,
    max(3.4, 0.28 * length(unique(plan$dataset)) + 1.8)
  )

  fidelity <- as.data.frame(table(
    capability = tables$artifact_fidelity$capability,
    outcome = factor(
      tables$artifact_fidelity$outcome,
      levels = c("TP", "TN", "FP", "FN")
    )
  ))
  fidelity <- fidelity[fidelity$Freq > 0L, ]
  fidelity_plot <- ggplot2::ggplot(
    fidelity,
    ggplot2::aes(x = capability, y = Freq, fill = outcome)
  ) +
    ggplot2::geom_col(colour = "white", linewidth = 0.2) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(outcome, " ", Freq)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 2.8,
      colour = "white"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = palette[c("TP", "TN", "FP", "FN")],
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "Plan-to-artifact fidelity includes negative checks",
      x = NULL,
      y = "Trial-level checks",
      fill = "Outcome"
    ) +
    theme
  .builder_eval_save_plot(
    fidelity_plot,
    paths[["builder_eval_artifact_fidelity.png"]],
    7.2,
    5.2
  )

  builds <- tables$build_runs[tables$build_runs$measured, , drop = FALSE]
  builds$cell <- .builder_eval_short_label(builds$cell_id)
  builds$status <- ifelse(builds$success, "PASS", "FAIL")
  timing_plot <- ggplot2::ggplot(
    builds,
    ggplot2::aes(
      x = cell,
      y = elapsed_seconds,
      colour = status,
      shape = backend
    )
  ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = cell),
      colour = "#8A98A5",
      fill = "#EEF3F7",
      outlier.shape = NA,
      width = 0.55
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.1),
      size = 2
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_colour_manual(values = palette[c("PASS", "FAIL")]) +
    ggplot2::labs(
      title = "Independent-process build timing",
      subtitle = "Box: median and IQR; points: measured processes",
      x = NULL,
      y = "Elapsed seconds",
      colour = "Build",
      shape = "Backend"
    ) +
    theme
  .builder_eval_save_plot(
    timing_plot,
    paths[["builder_eval_build_timing.png"]],
    7.4,
    max(3.8, 0.38 * length(unique(builds$cell)) + 2)
  )

  faults <- tables$fault_injection
  checks <- c(
    old_preserved = "Prior identity",
    old_crb_reopenable = "Prior CRB reopened",
    recovery_success = "Recovery",
    subsequent_publish_success = "Clean republish",
    subsequent_crb_reopenable = "New CRB reopened"
  )
  fault_long <- do.call(
    rbind,
    lapply(names(checks), function(field) {
      data.frame(
        scenario = .builder_eval_short_label(faults$scenario),
        check = checks[[field]],
        status = ifelse(faults[[field]], "PASS", "FAIL"),
        stringsAsFactors = FALSE
      )
    })
  )
  recovery_plot <- ggplot2::ggplot(
    fault_long,
    ggplot2::aes(x = check, y = scenario, fill = status)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(status == "PASS", "✓", "×")),
      size = 3.6
    ) +
    ggplot2::scale_fill_manual(values = palette[c("PASS", "FAIL")]) +
    ggplot2::labs(
      title = "Recovery under injected, handled failures",
      x = NULL,
      y = NULL,
      fill = "Check"
    ) +
    theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
  .builder_eval_save_plot(
    recovery_plot,
    paths[["builder_eval_recovery.png"]],
    8.2,
    max(4.2, 0.32 * length(unique(fault_long$scenario)) + 2)
  )

  incremental <- tables$incremental_rebuild
  incremental$decision <- ifelse(incremental$actual_reused, "Reused", "Rebuilt")
  incremental$dataset <- toupper(sub("project_", "", incremental$dataset))
  incremental_plot <- ggplot2::ggplot(
    incremental,
    ggplot2::aes(x = dataset, y = 1, fill = decision)
  ) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          actual_reused,
          "reused\nhash unchanged",
          "rebuilt\nconfiguration changed"
        )
      ),
      colour = "white",
      fontface = "bold",
      size = 3.2
    ) +
    ggplot2::scale_fill_manual(values = palette[c("Reused", "Rebuilt")]) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL, limits = c(0, 1.22)) +
    ggplot2::labs(
      title = "Project-managed incremental build scope",
      subtitle = paste0(
        "Full ",
        sprintf("%.2f", unique(incremental$full_elapsed_seconds)[[1L]]),
        " s; incremental ",
        sprintf("%.2f", unique(incremental$incremental_elapsed_seconds)[[1L]]),
        " s (descriptive)"
      ),
      x = "Dataset",
      fill = "Observed action"
    ) +
    theme
  .builder_eval_save_plot(
    incremental_plot,
    paths[["builder_eval_incremental.png"]],
    6.8,
    3.8
  )

  claims <- data.frame(
    claim = c(
      "Capability detection and fail-closed parsing",
      "Plan identity across confirmation, worker, report",
      "Selected and unselected artifact content",
      "Declared build matrix on this environment",
      "Recovery under injected, handled failures",
      "Project-managed A/C reuse and B rebuild",
      "Power-loss or SIGKILL atomicity",
      "Concurrent publishers and zero downtime",
      "Universal performance across hardware"
    ),
    boundary = c(rep("Supported", 6L), rep("Not evaluated", 3L)),
    stringsAsFactors = FALSE
  )
  claims$order <- rev(seq_len(nrow(claims)))
  claim_plot <- ggplot2::ggplot(
    claims,
    ggplot2::aes(
      x = boundary,
      y = reorder(claim, order),
      colour = boundary,
      shape = boundary
    )
  ) +
    ggplot2::geom_point(size = 3.2) +
    ggplot2::scale_colour_manual(
      values = palette[c("Supported", "Not evaluated")]
    ) +
    ggplot2::labs(
      title = "Claim boundary",
      subtitle = "The evidence supports bounded statements, not stronger operational guarantees",
      x = NULL,
      y = NULL,
      colour = NULL,
      shape = NULL
    ) +
    theme
  .builder_eval_save_plot(
    claim_plot,
    paths[["builder_eval_claim_boundaries.png"]],
    8.2,
    5.3
  )

  .builder_eval_evidence_chain_svg(paths[["builder_evidence_chain.svg"]])
  unname(paths[builder_eval_figure_files()])
}
