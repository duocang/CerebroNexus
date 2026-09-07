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

bench_validate_panel_c_baseline <- function(
  result_dir,
  manifest = NULL,
  sources = NULL,
  access = NULL
) {
  expected_run <- "20260907T172844Z-9c6ab101e4a5-publication"
  expected_sha <- "9c6ab101e4a5a2ae79ada5d5fab99bf05952a0f4"
  expected_sources <- c(
    mouse_brain_e18 = "255a36ee92de25cb3568faa2c27d31fe6d0db30f285c5c977be8d6245de14044",
    human_pfc_hbcc = "aeca0480ab8941a7e4cf6b0ff6dc8c5f9d0de376466d65ca8198dc873f1cb16f"
  )
  if (is.null(manifest)) {
    manifest <- utils::read.csv(
      file.path(result_dir, "run_manifest.csv"),
      stringsAsFactors = FALSE
    )
  }
  values <- stats::setNames(as.character(manifest$value), manifest$key)
  if (!identical(values[["run_id"]], expected_run)) {
    stop("Panel C baseline run ID changed", call. = FALSE)
  }
  if (!identical(values[["git_sha"]], expected_sha)) {
    stop("Panel C baseline Git SHA changed", call. = FALSE)
  }
  if (is.null(sources)) {
    sources <- utils::read.csv(
      file.path(result_dir, "source_manifest.csv"),
      stringsAsFactors = FALSE
    )
  }
  observed <- stats::setNames(as.character(sources$sha256), sources$source)
  if (!identical(observed[names(expected_sources)], expected_sources)) {
    stop("Panel C baseline source hash changed", call. = FALSE)
  }
  if (is.null(access)) {
    access <- utils::read.csv(
      file.path(result_dir, "20_access.csv"),
      stringsAsFactors = FALSE
    )
  }
  required <- c(
    "source",
    "n_cells",
    "backend",
    "status",
    "hot_p50_secs",
    "block_secs",
    "n_hot",
    "query_plan_fingerprint"
  )
  if (!all(required %in% names(access))) {
    stop("Panel C baseline metric schema changed", call. = FALSE)
  }
  if (any(access$status != "OK") || any(access$n_hot != 33L)) {
    stop("Panel C baseline query-panel size changed", call. = FALSE)
  }
  groups <- interaction(access$source, access$n_cells, drop = TRUE)
  fingerprints <- tapply(access$query_plan_fingerprint, groups, unique)
  if (any(lengths(fingerprints) != 1L)) {
    stop("Panel C baseline query-plan fingerprint drifted", call. = FALSE)
  }
  TRUE
}
