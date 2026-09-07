#!/usr/bin/env Rscript

Sys.setenv(NOT_CRAN = "true")

arguments <- commandArgs(trailingOnly = TRUE)
argument <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  value <- arguments[startsWith(arguments, prefix)]
  if (!length(value)) default else sub(prefix, "", value[[1L]], fixed = TRUE)
}

baseline <- argument("baseline")
baseline_label <- argument("baseline-label", "master")
candidate <- argument("candidate", normalizePath(".", mustWork = TRUE))
repetitions <- as.integer(argument("reps", "3"))
crb_delay <- as.numeric(argument("crb-delay", "0"))
output <- argument("output")
if (is.null(baseline) || !dir.exists(baseline)) {
  stop("Pass an existing checkout with --baseline=/path.", call. = FALSE)
}
if (is.na(repetitions) || repetitions < 1L) {
  stop("--reps must be a positive integer.", call. = FALSE)
}
if (is.na(crb_delay) || crb_delay < 0) {
  stop("--crb-delay must be a non-negative number.", call. = FALSE)
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

measure_http_ready <- function(root, build, crb_delay = 0, timeout = 60000) {
  port <- httpuv::randomPort()
  started <- proc.time()[["elapsed"]]
  process <- callr::r_bg(
    function(directory, port, delay) {
      Sys.setenv(NOT_CRAN = "true")
      if (delay > 0) {
        original <- base::readRDS
        readRDS <- function(file, ...) {
          if (endsWith(tolower(file), ".crb")) {
            Sys.sleep(delay)
          }
          original(file, ...)
        }
        assign("readRDS", readRDS, envir = .GlobalEnv)
      }
      shiny::runApp(
        directory,
        host = "127.0.0.1",
        port = port,
        launch.browser = FALSE
      )
    },
    args = list(app_dir(root), port, crb_delay),
    stdout = "|",
    stderr = "2>&1",
    supervise = TRUE
  )
  on.exit(if (process$is_alive()) process$kill(), add = TRUE)
  deadline <- Sys.time() + timeout / 1000
  repeat {
    response <- tryCatch(
      curl::curl_fetch_memory(
        sprintf("http://127.0.0.1:%d", port),
        curl::new_handle(timeout_ms = 1000)
      ),
      error = function(error) NULL
    )
    if (!is.null(response) && response$status_code == 200L) {
      if (
        identical(build, "optimized") &&
          !grepl(
            "cerebro-dataset-loading",
            rawToChar(response$content),
            fixed = TRUE
          )
      ) {
        stop(
          "Optimized shell omitted the dataset loading status.",
          call. = FALSE
        )
      }
      return(1000 * (proc.time()[["elapsed"]] - started))
    }
    if (!process$is_alive()) {
      stop(
        paste(process$read_all_output_lines(), collapse = "\n"),
        call. = FALSE
      )
    }
    if (Sys.time() > deadline) {
      stop(
        "Timed out waiting for the app HTTP shell.\n",
        paste(process$read_all_output_lines(), collapse = "\n"),
        call. = FALSE
      )
    }
    Sys.sleep(0.025)
  }
}

measure <- function(root, build, repetition) {
  http_ready <- measure_http_ready(root, build, crb_delay = crb_delay)
  http_metric <- if (crb_delay > 0) {
    sprintf("process_to_http_ready_with_%gs_crb_read", crb_delay)
  } else {
    "process_to_http_ready"
  }
  if (crb_delay > 0) {
    return(data.frame(
      build = build,
      repetition = repetition,
      metric = http_metric,
      milliseconds = http_ready
    ))
  }
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
      http_metric,
      "process_to_data_info",
      "browser_connected_to_data_info",
      "projection_first_open",
      "linked_views_first_open",
      "gene_expression_first_open",
      "projection_plus_linked_journey"
    ),
    milliseconds = c(
      http_ready,
      startup,
      connected_to_data,
      projection,
      linked,
      gene,
      projection + linked
    )
  )
}

builds <- setNames(list(baseline, candidate), c(baseline_label, "optimized"))
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
