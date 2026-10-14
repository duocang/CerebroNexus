#!/usr/bin/env Rscript

# Historical three-launch Viewer figure; not the general page benchmark report.
# Usage: Rscript tests/benchmarks/viewer/scripts/plot_latest_viewer_performance.R INPUT_TSV OUTPUT_DIR

args <- commandArgs(trailingOnly = TRUE)
options(warn = 1)
if (length(args) != 2L) {
  stop("Usage: plot_latest_viewer_performance.R INPUT_TSV OUTPUT_DIR")
}

# These standalone QA tools may live anywhere; no Codex installation is needed.
qa_tools <- Sys.getenv("FIGURE_QA_TOOLS")
alignment_helper <- file.path(qa_tools, "panel_alignment.R")
alignment_auditor <- file.path(qa_tools, "audit_panel_alignment.py")
if (!nzchar(qa_tools) || !file.exists(alignment_helper) || !file.exists(alignment_auditor)) {
  stop("Set FIGURE_QA_TOOLS to a directory containing panel_alignment.R and audit_panel_alignment.py.")
}
qa_python <- Sys.which("python3")
if (!nzchar(qa_python)) qa_python <- Sys.which("python")
if (!nzchar(qa_python)) stop("Python is required for the panel-alignment audit.")

input <- normalizePath(args[[1L]], mustWork = TRUE)
output_dir <- normalizePath(args[[2L]], mustWork = FALSE)
if (file.exists(output_dir) && (!dir.exists(output_dir) ||
    length(list.files(output_dir, all.files = TRUE, no.. = TRUE)))) {
  stop("Choose a new or empty output directory; previous figures are preserved.")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c(
  "ggplot2",
  "patchwork",
  "svglite",
  "ragg",
  "jsonlite",
  "systemfonts",
  "Cairo"
)
missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1L),
    quietly = TRUE
  )
]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}

library(ggplot2)
library(patchwork)

# ---- Data audit -----------------------------------------------------------
raw <- read.delim(input, check.names = FALSE, stringsAsFactors = FALSE)
required_columns <- c(
  "candidate",
  "round",
  "page",
  "visit",
  "status",
  "error",
  "performance_ms",
  "budget_ms",
  "r_peak_rss_kib",
  "chrome_peak_rss_kib",
  "js_heap_used_bytes",
  "websocket_received_payload_bytes",
  "correctness_pass",
  "pass",
  "candidate_git_sha",
  "viewer_pack_manifest_sha256"
)
missing_columns <- setdiff(required_columns, names(raw))
if (length(missing_columns)) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}
if (!identical(unique(raw$candidate), "latest")) {
  stop("Input must contain exactly the latest candidate.")
}
if (!identical(unique(raw$visit), "first")) {
  stop("Input must contain first visits only.")
}
if (!identical(sort(unique(raw$round)), 1:3)) {
  stop("Input must contain benchmark rounds 1, 2, and 3.")
}

valid <- raw$status == "ok"
skipped <- raw$status == "skipped"
if (any(!valid & !skipped)) {
  stop("Input contains benchmark errors; inspect the raw data before plotting.")
}
metric_columns <- c(
  "performance_ms",
  "budget_ms",
  "r_peak_rss_kib",
  "chrome_peak_rss_kib",
  "js_heap_used_bytes",
  "websocket_received_payload_bytes"
)
if (anyNA(raw[valid, metric_columns])) {
  stop("Valid observations contain missing performance or resource metrics.")
}
if (
  any(raw$performance_ms[valid] < 0) ||
    any(raw$r_peak_rss_kib[valid] <= 0) ||
    any(raw$chrome_peak_rss_kib[valid] <= 0) ||
    any(raw$websocket_received_payload_bytes[valid] <= 0)
) {
  stop("Valid observations contain non-positive performance/resource values.")
}
if (any(!raw$correctness_pass[valid]) || any(!raw$pass[valid])) {
  stop("Valid latest observations contain correctness or budget failures.")
}
page_counts <- table(raw$page[valid])
if (length(page_counts) != 11L || any(page_counts != 3L)) {
  stop("Expected exactly three valid observations for each of 11 pages.")
}

audit <- data.frame(
  item = c(
    "input_rows",
    "valid_rows",
    "skipped_rows",
    "valid_pages",
    "replicates_per_page",
    "missing_key_metrics",
    "correctness_failures",
    "budget_failures",
    "publication_3s_budget_failures",
    "candidate_git_sha",
    "viewer_pack_manifest_sha256"
  ),
  value = c(
    nrow(raw),
    sum(valid),
    sum(skipped),
    length(page_counts),
    paste(sort(unique(as.integer(page_counts))), collapse = ","),
    sum(is.na(raw[valid, metric_columns])),
    sum(!raw$correctness_pass[valid]),
    sum(!raw$pass[valid]),
    sum(raw$performance_ms[valid] > 3000),
    unique(raw$candidate_git_sha),
    unique(raw$viewer_pack_manifest_sha256)
  ),
  stringsAsFactors = FALSE
)
write.csv(audit, file.path(output_dir, "data_audit.csv"), row.names = FALSE)
write.csv(
  raw[skipped, ],
  file.path(output_dir, "skipped_observations.csv"),
  row.names = FALSE
)

# ---- Source data ----------------------------------------------------------
page_labels <- c(
  about = "About",
  color_management = "Color management",
  coordinated_views = "Coordinated views",
  gene_expression = "Gene expression",
  gene_id_conversion = "Gene ID conversion",
  groups = "Groups",
  hla = "HLA",
  immune_repertoire = "Immune repertoire",
  overview = "Overview",
  spatial = "Spatial",
  trajectory = "Trajectory"
)

source_data <- raw[
  valid,
  c(
    "round",
    "page",
    "performance_ms",
    "budget_ms",
    "r_peak_rss_kib",
    "chrome_peak_rss_kib",
    "js_heap_used_bytes",
    "websocket_received_payload_bytes",
    "correctness_pass",
    "pass"
  )
]
source_data$page_label <- unname(page_labels[source_data$page])
source_data$first_frame_s <- source_data$performance_ms / 1000
source_data$budget_s <- source_data$budget_ms / 1000
source_data$budget_ratio <- source_data$performance_ms / source_data$budget_ms
source_data$publication_budget_s <- 3
source_data$r_peak_gib <- source_data$r_peak_rss_kib / 1024^2
source_data$chrome_peak_gib <- source_data$chrome_peak_rss_kib / 1024^2
source_data$js_heap_mib <- source_data$js_heap_used_bytes / 1024^2
source_data$websocket_mib <- source_data$websocket_received_payload_bytes /
  1024^2
write.csv(
  source_data,
  file.path(output_dir, "figure_source_data.csv"),
  row.names = FALSE
)

median_value <- function(value) stats::median(value, na.rm = FALSE)
q1_value <- function(value) unname(stats::quantile(value, 0.25, names = FALSE))
q3_value <- function(value) unname(stats::quantile(value, 0.75, names = FALSE))

summary_rows <- do.call(
  rbind,
  lapply(split(source_data, source_data$page), function(frame) {
    data.frame(
      page = frame$page[[1L]],
      page_label = frame$page_label[[1L]],
      n = nrow(frame),
      first_frame_median_s = median_value(frame$first_frame_s),
      first_frame_q1_s = q1_value(frame$first_frame_s),
      first_frame_q3_s = q3_value(frame$first_frame_s),
      first_frame_max_s = max(frame$first_frame_s),
      budget_s = unique(frame$budget_s),
      budget_ratio_median = median_value(frame$budget_ratio),
      budget_ratio_q1 = q1_value(frame$budget_ratio),
      budget_ratio_q3 = q3_value(frame$budget_ratio),
      r_peak_median_gib = median_value(frame$r_peak_gib),
      chrome_peak_median_gib = median_value(frame$chrome_peak_gib),
      js_heap_median_mib = median_value(frame$js_heap_mib),
      websocket_median_mib = median_value(frame$websocket_mib),
      stringsAsFactors = FALSE
    )
  })
)
rownames(summary_rows) <- NULL
summary_rows$publication_budget_s <- 3
summary_rows <- summary_rows[order(summary_rows$first_frame_median_s), ]
page_order <- summary_rows$page_label
source_data$page_label <- factor(source_data$page_label, levels = page_order)
summary_rows$page_label <- factor(summary_rows$page_label, levels = page_order)
source_data$round_offset <- c(-0.12, 0, 0.12)[match(source_data$round, 1:3)]
write.csv(
  summary_rows,
  file.path(output_dir, "figure_summary_data.csv"),
  row.names = FALSE
)

memory_data <- rbind(
  data.frame(
    source_data[c("round", "page", "page_label")],
    process = "R process tree",
    peak_gib = source_data$r_peak_gib
  ),
  data.frame(
    source_data[c("round", "page", "page_label")],
    process = "Browser process tree",
    peak_gib = source_data$chrome_peak_gib
  )
)
memory_data$process <- factor(
  memory_data$process,
  levels = c("R process tree", "Browser process tree")
)
memory_summary <- aggregate(
  peak_gib ~ page + page_label + process,
  data = memory_data,
  FUN = median_value
)

# ---- Figure contract ------------------------------------------------------
# Claim: The current million-cell Viewer renders every supported page within
# its first-frame budget while maintaining bounded process memory and compact
# browser transport. All three benchmark rounds are shown for every page.

okabe_ito <- c(
  blue = "#0072B2",
  orange = "#E69F00",
  green = "#009E73",
  vermillion = "#D55E00",
  grey = "#777777"
)

base_theme <- theme_bw(base_size = 7, base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line.x.bottom = element_line(colour = "black", linewidth = 0.35),
    axis.line.y.left = element_line(colour = "black", linewidth = 0.35),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 7, colour = "black"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    strip.text = element_text(size = 7, face = "bold"),
    plot.margin = margin(4, 5, 4, 5, unit = "pt")
  )

p_time <- ggplot(source_data, aes(x = first_frame_s, y = page_label)) +
  geom_vline(
    xintercept = 3,
    linewidth = 0.55,
    linetype = "22",
    colour = okabe_ito[["vermillion"]]
  ) +
  geom_errorbar(
    data = summary_rows,
    aes(
      x = first_frame_median_s,
      xmin = first_frame_q1_s,
      xmax = first_frame_q3_s,
      y = page_label
    ),
    orientation = "y",
    width = 0,
    linewidth = 0.7,
    colour = okabe_ito[["blue"]],
    inherit.aes = FALSE
  ) +
  geom_point(
    aes(y = as.numeric(page_label) + round_offset),
    shape = 21,
    size = 1.6,
    stroke = 0.35,
    colour = okabe_ito[["blue"]],
    fill = "white"
  ) +
  geom_point(
    data = summary_rows,
    aes(x = first_frame_median_s, y = page_label),
    shape = 18,
    size = 2.6,
    colour = okabe_ito[["blue"]],
    inherit.aes = FALSE
  ) +
  geom_text(
    data = summary_rows,
    aes(
      x = pmin(first_frame_max_s + 0.10, 2.72),
      y = page_label,
      label = sprintf("%.2f s", first_frame_median_s)
    ),
    hjust = 0,
    size = 2.15,
    family = "Arial",
    colour = okabe_ito[["grey"]],
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = 2.96,
    y = length(page_order) + 0.55,
    label = "3 s budget",
    colour = okabe_ito[["vermillion"]],
    size = 2.15,
    hjust = 1,
    family = "Arial"
  ) +
  scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.45, 0.9))) +
  scale_x_continuous(
    limits = c(0, 3.08),
    breaks = c(0, 1, 2, 3),
    labels = c("0", "1", "2", "3"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "First-frame time (s)", y = NULL) +
  base_theme

p_memory <- ggplot(source_data, aes(y = page_label)) +
  geom_segment(
    data = summary_rows,
    aes(
      x = chrome_peak_median_gib,
      xend = r_peak_median_gib,
      y = page_label,
      yend = page_label
    ),
    linewidth = 0.55,
    colour = "#B5B5B5",
    inherit.aes = FALSE
  ) +
  geom_point(
    aes(
      x = chrome_peak_gib,
      y = as.numeric(page_label) - 0.10 + round_offset * 0.4
    ),
    shape = 24,
    size = 1.15,
    stroke = 0.3,
    colour = okabe_ito[["orange"]],
    fill = "white"
  ) +
  geom_point(
    aes(
      x = r_peak_gib,
      y = as.numeric(page_label) + 0.10 + round_offset * 0.4
    ),
    shape = 21,
    size = 1.15,
    stroke = 0.3,
    colour = okabe_ito[["blue"]],
    fill = "white"
  ) +
  geom_point(
    data = memory_summary,
    aes(x = peak_gib, y = page_label, colour = process, shape = process),
    size = 2.05,
    stroke = 0.45,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "R process tree" = okabe_ito[["blue"]],
      "Browser process tree" = okabe_ito[["orange"]]
    ),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(
      "R process tree" = 16,
      "Browser process tree" = 17
    ),
    guide = "none"
  ) +
  annotate(
    "text",
    x = 0.84,
    y = length(page_order) + 0.55,
    label = "Browser",
    hjust = 0,
    size = 2.15,
    family = "Arial",
    colour = okabe_ito[["orange"]]
  ) +
  annotate(
    "text",
    x = 1.56,
    y = length(page_order) + 0.55,
    label = "R",
    hjust = 0,
    size = 2.15,
    family = "Arial",
    colour = okabe_ito[["blue"]]
  ) +
  scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.45, 0.9))) +
  scale_x_continuous(
    limits = c(0.78, 1.84),
    breaks = c(0.8, 1.2, 1.6),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Peak resident memory\n(GiB)", y = NULL) +
  base_theme +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p_transport <- ggplot(source_data, aes(x = websocket_mib, y = page_label)) +
  geom_point(
    aes(y = as.numeric(page_label) + round_offset),
    shape = 21,
    size = 1.6,
    stroke = 0.35,
    colour = okabe_ito[["green"]],
    fill = "white"
  ) +
  geom_point(
    data = summary_rows,
    aes(x = websocket_median_mib, y = page_label),
    shape = 18,
    size = 2.6,
    colour = okabe_ito[["green"]],
    inherit.aes = FALSE
  ) +
  scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.45, 0.9))) +
  scale_x_log10(
    breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3),
    labels = c("0.003", "0.01", "0.03", "0.1", "0.3", "1", "3")
  ) +
  labs(x = "WebSocket received\n(MiB, log scale)", y = NULL) +
  base_theme +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

panel_design <- "
AABC
"

figure <- p_time +
  p_memory +
  p_transport +
  plot_layout(design = panel_design) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = theme(
      plot.tag = element_text(
        family = "Arial",
        size = 8,
        face = "bold",
        colour = "black",
        hjust = 0,
        vjust = 1
      ),
      plot.tag.position = c(0.015, 0.985)
    )
  )

# ---- Render-time alignment gate and exports -------------------------------
width_mm <- 178
height_mm <- 112
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4
prefix <- file.path(output_dir, "latest_viewer_performance")

source(alignment_helper)

# The bundled helper converts unresolved patchwork `null` units at root level,
# which yields zero-size panels on this Windows R build. Measure each resolved
# panel viewport on the final-size device while retaining the same JSON audit.
write_patchwork_panel_layout <- function(
  plot,
  manifest_path,
  width_in,
  height_in,
  panel_ids = NULL,
  row_groups = NULL,
  column_groups = NULL,
  exemptions = list()
) {
  probe_path <- tempfile(fileext = ".pdf")
  grDevices::cairo_pdf(
    probe_path,
    width = width_in,
    height = height_in,
    family = "Arial",
    onefile = TRUE
  )
  device_open <- TRUE
  on.exit(
    {
      if (device_open) {
        grDevices::dev.off()
      }
      unlink(probe_path)
    },
    add = TRUE
  )
  grob <- patchwork::patchworkGrob(plot)
  panel_rows <- .nature_alignment_panel_rows(grob)
  if (is.null(panel_ids)) {
    panel_ids <- letters[seq_len(nrow(panel_rows))]
  }
  if (length(panel_ids) != nrow(panel_rows)) {
    stop("panel_ids must match the measured patchwork panels.")
  }
  grid::grid.newpage()
  grid::grid.draw(grob)
  grid::grid.force()

  panels <- lapply(seq_len(nrow(panel_rows)), function(index) {
    row <- panel_rows[index, ]
    viewport_name <- sprintf(
      "%s.%d-%d-%d-%d",
      row$name,
      row$t,
      row$r,
      row$b,
      row$l
    )
    grid::seekViewport(viewport_name)
    lower_left <- grid::deviceLoc(
      grid::unit(0, "npc"),
      grid::unit(0, "npc"),
      valueOnly = TRUE
    )
    upper_right <- grid::deviceLoc(
      grid::unit(1, "npc"),
      grid::unit(1, "npc"),
      valueOnly = TRUE
    )
    grid::upViewport(0)
    list(
      id = panel_ids[[index]],
      bbox_pt = unname(c(
        lower_left$x * 72,
        lower_left$y * 72,
        upper_right$x * 72,
        upper_right$y * 72
      )),
      grid_id = "patchwork-grid-1",
      row_start = as.integer(row$t - 1),
      row_stop = as.integer(row$b),
      col_start = as.integer(row$l - 1),
      col_stop = as.integer(row$r)
    )
  })

  manifest <- list(
    schema_version = 1L,
    backend = "r-patchwork",
    figure = list(width_pt = width_in * 72, height_pt = height_in * 72),
    panels = panels,
    exemptions = exemptions
  )
  if (!is.null(row_groups)) {
    manifest$row_groups <- .nature_alignment_groups(row_groups)
  }
  if (!is.null(column_groups)) {
    manifest$column_groups <- .nature_alignment_groups(column_groups)
  }
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA
  )
  grDevices::dev.off()
  device_open <- FALSE
  unlink(probe_path)
  invisible(manifest)
}

require_patchwork_panel_alignment(
  figure,
  manifest_path = paste0(prefix, ".alignment-layout.json"),
  report_path = paste0(prefix, ".alignment.json"),
  overlay_svg = paste0(prefix, ".alignment.svg"),
  width_in = width_in,
  height_in = height_in,
  panel_ids = c("a", "b", "c"),
  row_groups = list(c("a", "b", "c")),
  audit_script = alignment_auditor,
  python = qa_python,
  tolerance_pt = 1.5,
  gutter_tolerance_pt = 1.5,
  strict = TRUE
)

svglite::svglite(
  paste0(prefix, ".svg"),
  width = width_in,
  height = height_in
)
print(figure)
dev.off()

Cairo::CairoPDF(
  paste0(prefix, ".pdf"),
  width = width_in,
  height = height_in,
  family = "Arial",
  onefile = TRUE
)
print(figure)
dev.off()

ragg::agg_tiff(
  paste0(prefix, ".tiff"),
  width = width_in,
  height = height_in,
  units = "in",
  res = 350,
  background = "white"
)
print(figure)
dev.off()

ragg::agg_png(
  paste0(prefix, ".preview.png"),
  width = width_in,
  height = height_in,
  units = "in",
  res = 350,
  background = "white"
)
print(figure)
dev.off()

panel_qa <- data.frame(
  panel = c("(a)", "(b)", "(c)"),
  unique_claim = c(
    "Actual first-frame time relative to a common 3 s budget",
    "Paired R and browser resident-memory envelope",
    "Browser WebSocket transport footprint"
  ),
  center = c("Median", "Median marker", "Median marker"),
  spread = c("IQR", "All three launches shown", "All three launches shown"),
  replicate_unit = rep("Independent benchmark launch (n = 3 per page)", 3),
  labels_and_legend = c(
    "Common 3 s budget labelled directly; visual review pending",
    "Direct process labels; visual review pending",
    "Logarithmic axis declared; visual review pending"
  ),
  alignment = c(
    "Pass; shared row with panels (b) and (c)",
    "Pass; aligned with panels (a) and (c)",
    "Pass; aligned with panels (a) and (b)"
  ),
  visual_collision_and_crop = rep("PENDING: inspect the exported figure at final size", 3),
  pass = rep(NA, 3),
  stringsAsFactors = FALSE
)
write.csv(panel_qa, file.path(output_dir, "panel_qa.csv"), row.names = FALSE)

qa_notes <- c(
  "Target journal: Oxford Bioinformatics",
  sprintf("Final size: %d x %d mm", width_mm, height_mm),
  "Backend: R only (ggplot2 + patchwork)",
  sprintf("First-frame results: %d of %d observations exceed the common 3 s budget; memory and transport measurements are shown alongside timing.", sum(raw$performance_ms[valid] > 3000), sum(valid)),
  "Replicate unit: independent benchmark launches (n = 3 per supported page).",
  "Center/spread: median and IQR in panel (a); all three raw observations are visible in every panel.",
  "No inferential test was performed: three engineering benchmark launches are descriptive replicates, not biological samples.",
  sprintf("Excluded from quantitative panels: %d page-unavailable observations across %d pages; these are preserved in skipped_observations.csv.", sum(skipped), length(unique(raw$page[skipped]))),
  "Panel roles: (a) actual first-frame time versus a common 3 s budget; (b) paired process-memory envelope; (c) browser transport footprint.",
  "Colour: Okabe-Ito categorical palette; point shapes provide grayscale redundancy.",
  "TIFF: colour, 350 dpi. PDF/SVG: vector with Arial requested.",
  "Panel alignment gate: completed; see this export's alignment JSON for counts and findings.",
  "Final-size visual review: PENDING for text collision, clipping, panel labels, and legend clearance. Run a PDF collision audit and inspect the rendered output before claiming publication readiness."
)
writeLines(qa_notes, file.path(output_dir, "qa_notes.txt"))

cat("Wrote figure bundle to", output_dir, "\n")
