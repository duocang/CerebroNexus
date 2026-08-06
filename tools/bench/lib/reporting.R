# Aggregation and formatting helpers for benchmark reports.

bench_summarise_metrics <- function(x, group, metrics) {
  missing <- setdiff(c(group, metrics, "status"), names(x))
  if (length(missing)) {
    stop("missing summary columns: ", paste(missing, collapse = ", "))
  }
  ok <- x[x$status == "OK", , drop = FALSE]
  if (!nrow(ok)) {
    return(data.frame())
  }
  key <- interaction(ok[group], drop = TRUE, lex.order = TRUE)
  rows <- split(seq_len(nrow(ok)), key)
  out <- lapply(rows, function(index) {
    group_row <- ok[index[1], group, drop = FALSE]
    group_row$rows_n <- length(index)
    for (metric in metrics) {
      values <- ok[[metric]][index]
      values <- values[is.finite(values)]
      group_row[[paste0(metric, "_median")]] <- if (length(values)) {
        stats::median(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_min")]] <- if (length(values)) {
        min(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_max")]] <- if (length(values)) {
        max(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_n")]] <- length(values)
    }
    group_row
  })
  rownames(out) <- NULL
  do.call(rbind, out)
}

bench_format_interval <- function(median, minimum, maximum, n, digits = 2L) {
  if (
    !is.finite(median) || !is.finite(minimum) || !is.finite(maximum) || n < 1L
  ) {
    return("--")
  }
  sprintf(
    paste0("%.", digits, "f [%.", digits, "f-%.", digits, "f], n=%d"),
    median,
    minimum,
    maximum,
    as.integer(n)
  )
}

bench_evidence_notice <- function(profile) {
  if (isTRUE(profile$article_eligible)) {
    paste0(
      "Publication-profile evidence: backend comparisons use independent ",
      "process repeats and correctness fingerprints."
    )
  } else {
    paste0(
      "Exploratory evidence only: the ",
      profile$name,
      " profile is useful for harness validation but must not support the ",
      "user-facing performance conclusions."
    )
  }
}

bench_normalize_access_metrics <- function(x) {
  required <- c(
    "block_prepare_secs",
    "block_materialize_secs",
    "block_ready_secs"
  )
  has_current <- all(required %in% names(x))
  if (has_current) {
    x$block_timing_contract <- "materialized_v2"
    return(x)
  }

  if ("block_secs" %in% names(x)) {
    x$block_prepare_secs <- x$block_secs
  } else {
    x$block_prepare_secs <- NA_real_
  }
  x$block_materialize_secs <- NA_real_
  x$block_ready_secs <- NA_real_
  x$block_timing_contract <- "legacy_prepare_only"
  x
}

bench_article_comparison_lines <- function(exports, access) {
  required_exports <- c(
    "source",
    "n_cells",
    "backend",
    "status",
    "export_secs",
    "total_mb"
  )
  required_access <- c(
    "source",
    "n_cells",
    "backend",
    "status",
    "load_secs",
    "attach_secs",
    "rss_mb",
    "hot_p50_secs",
    "block_ready_secs"
  )
  missing_exports <- setdiff(required_exports, names(exports))
  missing_access <- setdiff(required_access, names(access))
  if (length(missing_exports) || length(missing_access)) {
    stop(
      "benchmark rows cannot render the article comparison table",
      call. = FALSE
    )
  }

  exports <- exports[exports$status == "OK", , drop = FALSE]
  access <- access[access$status == "OK", , drop = FALSE]
  access$ready_secs <- access$load_secs + access$attach_secs
  key <- c("source", "n_cells", "backend")
  export_summary <- bench_summarise_metrics(
    exports,
    group = key,
    metrics = c("export_secs", "total_mb")
  )
  access_summary <- bench_summarise_metrics(
    access,
    group = key,
    metrics = c("ready_secs", "rss_mb", "hot_p50_secs", "block_ready_secs")
  )
  summary <- merge(export_summary, access_summary, by = key, all = FALSE)
  labels <- c(
    human_pfc_hbcc = "Human PFC · HBCC",
    human_pfc_mssm = "Human PFC · MSSM",
    mouse_brain_e18 = "Mouse E18"
  )
  source_order <- names(labels)
  backend_order <- c("embedded", "bpcells", "h5")
  summary <- summary[
    order(
      match(summary$source, source_order),
      summary$n_cells,
      match(summary$backend, backend_order)
    ),
    ,
    drop = FALSE
  ]
  format_value <- function(value, digits, big.mark = "") {
    format(
      round(value, digits),
      nsmall = digits,
      trim = TRUE,
      big.mark = big.mark
    )
  }
  c(
    "| Data point | Backend | Export s | Stored MB | Ready s | RSS MB | Warmed gene s | 12-gene block s |",
    "|---|---|---:|---:|---:|---:|---:|---:|",
    vapply(
      seq_len(nrow(summary)),
      function(i) {
        row <- summary[i, , drop = FALSE]
        sprintf(
          "| %s · %sk | `%s` | %s | %s | %s | %s | %s | %s |",
          unname(labels[[row$source]]),
          format_value(row$n_cells / 1000, 0),
          row$backend,
          format_value(row$export_secs_median, 1),
          format_value(row$total_mb_median, 1),
          format_value(row$ready_secs_median, 2),
          format_value(row$rss_mb_median, 0, big.mark = ","),
          format_value(row$hot_p50_secs_median, 3),
          format_value(row$block_ready_secs_median, 3)
        )
      },
      character(1)
    )
  )
}

bench_publication_figure_stems <- function() {
  c(
    "expression_backend_benchmark_overview",
    "expression_backend_benchmark_observed_scaling",
    "expression_backend_benchmark_repeats",
    "expression_backend_benchmark_query_latency",
    "expression_backend_benchmark_pareto",
    "expression_backend_benchmark_correctness"
  )
}

bench_publication_figure_files <- function() {
  as.vector(outer(
    bench_publication_figure_stems(),
    c("png", "pdf", "svg"),
    paste,
    sep = "."
  ))
}

bench_current_result_dir <- function(result_root) {
  result_root <- normalizePath(result_root, mustWork = TRUE)
  pointer <- file.path(result_root, "CURRENT")
  if (!file.exists(pointer)) {
    return(result_root)
  }
  run_id <- trimws(readLines(pointer, n = 1L, warn = FALSE))
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", run_id)) {
    stop("unsafe CURRENT benchmark run id", call. = FALSE)
  }
  run_dir <- file.path(result_root, "runs", run_id)
  if (!dir.exists(run_dir)) {
    stop("CURRENT benchmark run directory does not exist", call. = FALSE)
  }
  normalizePath(run_dir)
}
