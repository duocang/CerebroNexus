#!/usr/bin/env Rscript

Sys.setenv(NOT_CRAN = "true")

arguments <- commandArgs(trailingOnly = TRUE)
argument <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  value <- arguments[startsWith(arguments, prefix)]
  if (!length(value)) default else sub(prefix, "", value[[1L]], fixed = TRUE)
}

baseline <- argument("baseline")
candidate <- argument("candidate", normalizePath(".", mustWork = TRUE))
repetitions <- as.integer(argument("reps", "3"))
output <- argument("output")
if (is.null(baseline) || !dir.exists(baseline)) {
  stop("Pass an existing checkout with --baseline=/path.", call. = FALSE)
}
if (is.na(repetitions) || repetitions < 1L) {
  stop("--reps must be a positive integer.", call. = FALSE)
}

app_dir <- function(root) {
  root <- normalizePath(root, mustWork = TRUE)
  directory <- if (file.exists(file.path(root, "inst", "app.R"))) {
    file.path(root, "inst")
  } else {
    root
  }
  if (!file.exists(file.path(directory, "app.R"))) {
    stop("No inst/app.R found below ", root, ".", call. = FALSE)
  }
  directory
}

wait_value <- function(app, output, timeout = 60000) {
  deadline <- Sys.time() + timeout / 1000
  repeat {
    value <- tryCatch(app$get_value(output = output), error = function(e) NULL)
    if (!is.null(value)) {
      return(value)
    }
    if (Sys.time() > deadline) {
      stop("Timed out waiting for output ", output, ".", call. = FALSE)
    }
    Sys.sleep(0.05)
  }
}

click_and_wait <- function(app, tab, ready, timeout = 60000) {
  selector <- sprintf('a[href="#shiny-tab-%s"]', tab)
  app$wait_for_js(
    sprintf("document.querySelector('%s') !== null", selector),
    timeout = timeout
  )
  started <- proc.time()[["elapsed"]]
  app$run_js(sprintf("document.querySelector('%s').click();", selector))
  app$wait_for_js(ready, timeout = timeout)
  1000 * (proc.time()[["elapsed"]] - started)
}

measure <- function(root, build, repetition) {
  started <- proc.time()[["elapsed"]]
  app <- shinytest2::AppDriver$new(
    app_dir(root),
    name = paste0("viewer_latency_", build, "_", repetition),
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  wait_value(app, "load_data_number_of_cells")
  startup <- 1000 * (proc.time()[["elapsed"]] - started)
  logs <- app$get_logs()
  connected <- logs$timestamp[
    grepl("shinytest2; Connected", logs$message, fixed = TRUE)
  ]
  data_info <- logs$timestamp[
    grepl(
      "shiny:value load_data_number_of_cells",
      logs$message,
      fixed = TRUE
    )
  ]
  connected_to_data <- if (length(connected) && length(data_info)) {
    1000 *
      as.numeric(difftime(
        data_info[[1L]],
        connected[[1L]],
        units = "secs"
      ))
  } else {
    NA_real_
  }

  ## Give post-paint work the same quiet window before following a real user
  ## journey through the three high-frequency cell-view pages.
  Sys.sleep(1.5)
  projection <- click_and_wait(
    app,
    "overview",
    paste0(
      "window.cerebroCellViews && ",
      "window.cerebroCellViews.captureState('overview_projection') !== null"
    )
  )
  linked <- click_and_wait(
    app,
    "coordinated_views",
    paste0(
      "window.cerebroLinkedViewsState && ",
      "window.cerebroLinkedViewsState.ready() === true"
    )
  )
  gene_started <- proc.time()[["elapsed"]]
  gene_selector <- 'a[href="#shiny-tab-geneExpression"]'
  app$run_js(sprintf("document.querySelector('%s').click();", gene_selector))
  app$wait_for_js(
    "document.getElementById('expression_genes_input') !== null",
    timeout = 60000
  )
  app$set_inputs(expression_genes_input = "MS4A1", wait_ = FALSE)
  app$wait_for_js(
    paste0(
      "window.cerebroCellViews && ",
      "window.cerebroCellViews.captureState(",
      "'expression_projection') !== null"
    ),
    timeout = 60000
  )
  gene <- 1000 * (proc.time()[["elapsed"]] - gene_started)

  data.frame(
    build = build,
    repetition = repetition,
    metric = c(
      "process_to_data_info",
      "browser_connected_to_data_info",
      "projection_first_open",
      "linked_views_first_open",
      "gene_expression_first_open",
      "projection_plus_linked_journey"
    ),
    milliseconds = c(
      startup,
      connected_to_data,
      projection,
      linked,
      gene,
      projection + linked
    )
  )
}

builds <- list(master = baseline, optimized = candidate)
results <- do.call(
  rbind,
  lapply(
    names(builds),
    function(build) {
      root <- builds[[build]]
      do.call(
        rbind,
        lapply(seq_len(repetitions), function(repetition) {
          message(build, " repetition ", repetition, "/", repetitions)
          measure(root, build, repetition)
        })
      )
    }
  )
)
results$milliseconds <- round(results$milliseconds, 1)
medians <- aggregate(milliseconds ~ build + metric, results, median)
medians <- medians[order(medians$metric, medians$build), ]

print(results, row.names = FALSE)
cat("\nMedians (ms)\n")
print(medians, row.names = FALSE)
if (!is.null(output)) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(results, output, row.names = FALSE)
}
