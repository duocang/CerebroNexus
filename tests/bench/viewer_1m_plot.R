#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    paste(
      "usage: viewer_1m_plot.R",
      "HOT_PATH_TSV BROWSER_OUTPUT OUTPUT_DIR"
    ),
    call. = FALSE
  )
}

hot_path_file <- normalizePath(args[[1L]], mustWork = TRUE)
browser_file <- normalizePath(args[[2L]], mustWork = TRUE)
output_dir <- args[[3L]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      label,
      " is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

hot <- utils::read.delim(
  hot_path_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
require_columns(
  hot,
  c(
    "metric",
    "before_ms",
    "after_ms",
    "time_change_pct",
    "before_alloc_mib",
    "after_alloc_mib",
    "alloc_change_pct"
  ),
  "hot-path result"
)

browser_lines <- readLines(browser_file, warn = FALSE)
summary_marker <- match("SUMMARY", browser_lines)
if (is.na(summary_marker) || summary_marker == length(browser_lines)) {
  stop("browser result does not contain a SUMMARY table.", call. = FALSE)
}
browser <- utils::read.delim(
  text = paste(
    browser_lines[(summary_marker + 1L):length(browser_lines)],
    collapse = "\n"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
require_columns(
  browser,
  c(
    "version",
    "data_ready_ms",
    "overview_ms",
    "total_ms",
    "shiny_rss_mib"
  ),
  "browser summary"
)
if (!identical(sort(browser$version), c("after", "before"))) {
  stop(
    "browser summary must contain exactly one before and one after row.",
    call. = FALSE
  )
}

result_rows <- function(
  scope,
  metric,
  measure,
  before,
  after,
  unit,
  change_pct = (after / before - 1) * 100
) {
  data.frame(
    scope,
    metric,
    measure,
    before,
    after,
    change_pct,
    unit,
    check.names = FALSE
  )
}

hot_time <- result_rows(
  "hot path",
  hot$metric,
  "elapsed time",
  hot$before_ms,
  hot$after_ms,
  "ms",
  hot$time_change_pct
)
hot_alloc <- result_rows(
  "hot path",
  hot$metric,
  "R allocation",
  hot$before_alloc_mib,
  hot$after_alloc_mib,
  "MiB",
  hot$alloc_change_pct
)
browser_metrics <- c(
  data_ready_ms = "Data ready",
  overview_ms = "Overview Canvas ready",
  total_ms = "Launch through painted Overview"
)
browser_before <- browser[browser$version == "before", , drop = FALSE]
browser_after <- browser[browser$version == "after", , drop = FALSE]
browser_time <- result_rows(
  "browser",
  unname(browser_metrics),
  "elapsed time",
  unlist(browser_before[names(browser_metrics)], use.names = FALSE),
  unlist(browser_after[names(browser_metrics)], use.names = FALSE),
  "ms"
)
browser_rss <- result_rows(
  "browser",
  "Shiny process RSS",
  "resident memory",
  browser_before$shiny_rss_mib,
  browser_after$shiny_rss_mib,
  "MiB"
)

summary <- rbind(hot_time, hot_alloc, browser_time, browser_rss)
numeric_values <- unlist(summary[c("before", "after", "change_pct")])
if (
  any(!is.finite(numeric_values)) ||
    any(summary$before <= 0) ||
    any(summary$after <= 0)
) {
  stop(
    "benchmark results must contain positive finite measurements.",
    call. = FALSE
  )
}

summary_file <- file.path(output_dir, "viewer_1m_benchmark_summary.csv")
utils::write.csv(summary, summary_file, row.names = FALSE)

colours <- c(before = "#777777", after = "#0072B2")
short_metric <- c(
  "full projection selection" = "Full projection selection",
  "filtered projection selection" = "Filtered projection selection",
  "hover preparation" = "Hover preparation",
  "single-gene expression" = "Single-gene expression",
  "RGB expression" = "RGB expression",
  "multi-panel expression" = "Multi-panel expression",
  "mean expression" = "Mean expression"
)

draw_dot_panel <- function(data, title, xlab) {
  labels <- unname(short_metric[data$metric])
  labels[is.na(labels)] <- data$metric[is.na(labels)]
  y <- rev(seq_len(nrow(data)))
  values <- c(data$before, data$after)
  log_range <- range(log10(values))
  padding <- max(diff(log_range) * 0.08, 0.08)
  graphics::par(mar = c(4, 13, 3, 5))
  graphics::plot(
    NA,
    xlim = 10^(log_range + c(-padding, padding)),
    ylim = c(0.5, nrow(data) + 0.5),
    log = "x",
    axes = FALSE,
    xlab = xlab,
    ylab = "",
    main = title
  )
  graphics::axis(1)
  graphics::axis(2, at = y, labels = labels, las = 1, tick = FALSE)
  graphics::axis(
    4,
    at = y,
    labels = sprintf("%+.1f%%", data$change_pct),
    las = 1,
    tick = FALSE,
    cex.axis = 0.8
  )
  graphics::segments(data$before, y, data$after, y, col = "#BBBBBB")
  graphics::points(data$before, y, pch = 16, col = colours[["before"]])
  graphics::points(data$after, y, pch = 16, col = colours[["after"]])
  graphics::box(col = "#CCCCCC")
  graphics::legend(
    "bottomright",
    legend = c("Baseline", "Candidate"),
    col = colours,
    pch = 16,
    bty = "n",
    horiz = TRUE
  )
}

draw_bar_panel <- function(before, after, labels, title, ylab, divisor = 1) {
  values <- rbind(Baseline = before / divisor, Candidate = after / divisor)
  graphics::par(mar = c(5, 5, 3, 1))
  positions <- graphics::barplot(
    values,
    beside = TRUE,
    names.arg = labels,
    las = 1,
    col = colours,
    border = NA,
    ylab = ylab,
    main = title,
    ylim = c(0, max(values) * 1.3)
  )
  graphics::text(
    positions,
    values,
    labels = format(round(values, 1), trim = TRUE),
    pos = 3,
    cex = 0.75
  )
  graphics::legend(
    "topright",
    legend = rownames(values),
    fill = colours,
    border = NA,
    bty = "n",
    horiz = TRUE
  )
}

figure_file <- file.path(output_dir, "viewer_1m_benchmark.png")
grDevices::png(
  figure_file,
  width = 2400,
  height = 1800,
  res = 200,
  bg = "white"
)
graphics::par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
draw_dot_panel(hot_time, "A. Viewer hot-path time", "milliseconds (log scale)")
draw_dot_panel(hot_alloc, "B. Viewer hot-path R allocation", "MiB (log scale)")
draw_bar_panel(
  browser_time$before,
  browser_time$after,
  c("Data ready", "Overview\nCanvas ready", "Launch through\npainted Overview"),
  "C. Real 1M CRB browser loading",
  "seconds",
  divisor = 1000
)
draw_bar_panel(
  browser_rss$before,
  browser_rss$after,
  "Shiny process",
  "D. Browser-run resident memory",
  "GiB",
  divisor = 1024
)
graphics::mtext(
  "CerebroNexus 1M-cell Viewer benchmark — lower is better",
  outer = TRUE,
  cex = 1.35,
  font = 2
)
grDevices::dev.off()

cat("wrote\t", figure_file, "\n", sep = "")
cat("wrote\t", summary_file, "\n", sep = "")
