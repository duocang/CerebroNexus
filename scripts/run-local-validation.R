#!/usr/bin/env Rscript

local_validation_integer <- function(value, option, minimum, maximum) {
  parsed <- suppressWarnings(as.integer(value))
  if (
    length(parsed) != 1L ||
      is.na(parsed) ||
      parsed < minimum ||
      parsed > maximum ||
      !identical(as.character(parsed), as.character(value))
  ) {
    stop(
      option,
      " must be an integer from ",
      minimum,
      " to ",
      maximum,
      call. = FALSE
    )
  }
  parsed
}

local_validation_parse_args <- function(args) {
  options <- list(
    mode = "full",
    logic_workers = 3L,
    browser_workers = 2L,
    output_dir = tempfile("cerebro-local-validation-"),
    dry_run = FALSE
  )
  valued <- c(
    "--mode",
    "--logic-workers",
    "--browser-workers",
    "--output-dir"
  )
  index <- 1L
  while (index <= length(args)) {
    argument <- args[[index]]
    if (argument %in% valued) {
      if (index == length(args)) {
        stop("Missing value after ", argument, call. = FALSE)
      }
      key <- gsub("-", "_", sub("^--", "", argument))
      options[[key]] <- args[[index + 1L]]
      index <- index + 2L
    } else if (identical(argument, "--dry-run")) {
      options$dry_run <- TRUE
      index <- index + 1L
    } else {
      stop("Unknown argument: ", argument, call. = FALSE)
    }
  }

  if (
    length(options$mode) != 1L ||
      is.na(options$mode) ||
      !options$mode %in% c("tests", "full")
  ) {
    stop("mode must be tests or full", call. = FALSE)
  }
  options$logic_workers <- local_validation_integer(
    options$logic_workers,
    "logic-workers",
    1L,
    4L
  )
  options$browser_workers <- local_validation_integer(
    options$browser_workers,
    "browser-workers",
    1L,
    3L
  )
  if (
    length(options$output_dir) != 1L ||
      is.na(options$output_dir) ||
      !nzchar(options$output_dir)
  ) {
    stop("output-dir must not be empty", call. = FALSE)
  }
  options
}

local_validation_job <- function(
  phase,
  name,
  cap,
  command,
  artifact_dir = NA_character_
) {
  data.frame(
    phase = phase,
    name = name,
    cap = as.integer(cap),
    command = command,
    artifact_dir = artifact_dir,
    predicted_seconds = NA_real_,
    stringsAsFactors = FALSE
  )
}

local_validation_schedule <- function(
  logic_workers = 3L,
  browser_workers = 2L,
  mode = "full",
  output_dir = tempfile("cerebro-local-validation-"),
  has_process_sensitive_tests = TRUE
) {
  logic_workers <- local_validation_integer(
    logic_workers,
    "logic-workers",
    1L,
    4L
  )
  browser_workers <- local_validation_integer(
    browser_workers,
    "browser-workers",
    1L,
    3L
  )
  if (length(mode) != 1L || is.na(mode) || !mode %in% c("tests", "full")) {
    stop("mode must be tests or full", call. = FALSE)
  }
  if (
    !is.logical(has_process_sensitive_tests) ||
      length(has_process_sensitive_tests) != 1L ||
      is.na(has_process_sensitive_tests)
  ) {
    stop("has_process_sensitive_tests must be one logical value", call. = FALSE)
  }

  logic <- do.call(
    rbind,
    lapply(seq_len(4L), function(shard) {
      local_validation_job(
        "logic",
        paste0("logic-", shard, "-of-4"),
        logic_workers,
        paste(
          "Rscript scripts/run-test-shard.R --group logic --shard",
          shard,
          "--shards 4 --strategy weighted"
        )
      )
    })
  )
  process_sensitive <- if (has_process_sensitive_tests) {
    local_validation_job(
      "process-sensitive",
      "process-sensitive",
      1L,
      paste(
        "Rscript scripts/run-test-shard.R",
        "--group process-sensitive"
      )
    )
  }
  browser <- do.call(
    rbind,
    lapply(seq_len(6L), function(shard) {
      artifact_dir <- file.path(
        output_dir,
        "artifacts",
        paste0("browser-", shard, "-of-6")
      )
      local_validation_job(
        "browser",
        paste0("browser-", shard, "-of-6"),
        browser_workers,
        paste(
          "Rscript scripts/run-test-shard.R --group browser --shard",
          shard,
          "--shards 6 --strategy weighted"
        ),
        artifact_dir
      )
    })
  )
  schedule <- do.call(
    rbind,
    Filter(
      Negate(is.null),
      list(logic, process_sensitive, browser)
    )
  )
  if (identical(mode, "full")) {
    check <- local_validation_job(
      "check",
      "R-CMD-check",
      1L,
      paste0(
        "Rscript -e \"devtools::check(",
        "args = c('--timings', '--no-tests'), ",
        "vignettes = TRUE, error_on = 'warning')\""
      )
    )
    pkgdown <- local_validation_job(
      "pkgdown",
      "pkgdown",
      1L,
      paste0(
        "Rscript -e \"pkgdown::build_site_github_pages(",
        "new_process = FALSE, install = TRUE, ",
        "dest_dir = 'pkgdown-site')\""
      )
    )
    schedule <- rbind(schedule, check, pkgdown)
  }
  rownames(schedule) <- NULL
  schedule
}

local_validation_exit_code <- function(statuses) {
  if (length(statuses) && any(is.na(statuses) | statuses != 0L)) 1L else 0L
}

local_validation_job_env <- function(job, repo_root) {
  if (!identical(job$phase[[1L]], "browser")) {
    return(character())
  }
  c(
    CEREBRO_RUN_BROWSER_TESTS = "true",
    CEREBRO_TEST_ARTIFACT_DIR = job$artifact_dir[[1L]],
    CEREBRO_PACKAGE_SOURCE = normalizePath(
      repo_root,
      winslash = "/",
      mustWork = TRUE
    )
  )
}

local_validation_terminate_children <- function(children) {
  for (child in children) {
    if (!is.null(child) && isTRUE(child$is_alive())) {
      try(child$kill_tree(), silent = TRUE)
    }
  }
  for (child in children) {
    if (!is.null(child) && isTRUE(child$is_alive())) {
      try(child$wait(1000), silent = TRUE)
    }
  }
  invisible(NULL)
}

local_validation_result <- function(job, status, started, ended, log) {
  data.frame(
    phase = job$phase[[1L]],
    name = job$name[[1L]],
    status = as.integer(status),
    started = as.numeric(started),
    ended = as.numeric(ended),
    duration = as.numeric(difftime(ended, started, units = "secs")),
    log = log,
    stringsAsFactors = FALSE
  )
}

local_validation_run_phase <- function(
  jobs,
  repo_root,
  output_dir,
  poll_interval = 0.05
) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("The processx package is required.", call. = FALSE)
  }
  if (!nrow(jobs)) {
    return(data.frame())
  }
  if (length(unique(jobs$phase)) != 1L || length(unique(jobs$cap)) != 1L) {
    stop("run_phase requires jobs from one phase and one cap", call. = FALSE)
  }
  cap <- jobs$cap[[1L]]
  log_dir <- file.path(output_dir, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  pending <- seq_len(nrow(jobs))
  running <- list()
  completed <- vector("list", nrow(jobs))
  phase_complete <- FALSE
  on.exit(
    {
      local_validation_terminate_children(lapply(running, `[[`, "process"))
      if (!phase_complete) {
        message("Validation stopped; partial logs remain in ", log_dir)
      }
    },
    add = TRUE
  )

  while (length(pending) || length(running)) {
    while (length(pending) && length(running) < cap) {
      index <- pending[[1L]]
      pending <- pending[-1L]
      job <- jobs[index, , drop = FALSE]
      log <- file.path(log_dir, paste0(job$name[[1L]], ".log"))
      started <- Sys.time()
      additions <- local_validation_job_env(job, repo_root)
      child_env <- Sys.getenv()
      child_env[names(additions)] <- additions
      launched <- tryCatch(
        processx::process$new(
          "/bin/sh",
          c("-c", job$command[[1L]]),
          wd = repo_root,
          env = child_env,
          stdout = log,
          stderr = "2>&1",
          cleanup = TRUE
        ),
        error = identity
      )
      if (inherits(launched, "error")) {
        writeLines(conditionMessage(launched), log)
        ended <- Sys.time()
        completed[[index]] <- local_validation_result(
          job,
          127L,
          started,
          ended,
          log
        )
      } else {
        running[[as.character(index)]] <- list(
          process = launched,
          job = job,
          started = started,
          log = log
        )
      }
    }

    if (length(running)) {
      processx::poll(
        lapply(running, `[[`, "process"),
        as.integer(max(1, poll_interval * 1000))
      )
      finished <- names(running)[vapply(
        running,
        function(item) !item$process$is_alive(),
        logical(1)
      )]
      for (key in finished) {
        item <- running[[key]]
        item$process$wait(1000)
        status <- item$process$get_exit_status()
        if (is.null(status)) {
          status <- 127L
        }
        completed[[as.integer(key)]] <- local_validation_result(
          item$job,
          status,
          item$started,
          Sys.time(),
          item$log
        )
        running[[key]] <- NULL
      }
    }
  }
  result <- do.call(rbind, completed)
  rownames(result) <- NULL
  phase_complete <- TRUE
  result
}

local_validation_stray_processes <- function(process_lines = NULL) {
  if (is.null(process_lines)) {
    process_lines <- tryCatch(
      system2("ps", c("-axo", "pid=,command="), stdout = TRUE),
      error = function(error) character()
    )
  }
  process_lines[grepl(
    "shiny::run(App|Gadget)|cerebroApp",
    process_lines,
    ignore.case = TRUE
  )]
}

local_validation_run_schedule <- function(
  schedule,
  repo_root,
  output_dir,
  poll_interval = 0.05,
  stray_checker = local_validation_stray_processes,
  phase_runner = local_validation_run_phase
) {
  stray <- stray_checker()
  if (length(stray)) {
    stop(
      "Recognizable stray Shiny/Cerebro process(es) found:\n",
      paste(stray, collapse = "\n"),
      "\nStop them manually before running validation.",
      call. = FALSE
    )
  }
  results <- list()
  for (phase in unique(schedule$phase)) {
    jobs <- schedule[schedule$phase == phase, , drop = FALSE]
    results[[phase]] <- phase_runner(
      jobs,
      repo_root = repo_root,
      output_dir = output_dir,
      poll_interval = poll_interval
    )
  }
  result <- do.call(rbind, results)
  rownames(result) <- NULL
  result
}

local_validation_repo_root <- function() {
  file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_argument)) {
    return(normalizePath(".", mustWork = TRUE))
  }
  script <- normalizePath(
    sub("^--file=", "", file_argument[[1L]]),
    mustWork = TRUE
  )
  dirname(dirname(script))
}

local_validation_has_process_sensitive_tests <- function(repo_root) {
  shard_api <- new.env(parent = globalenv())
  sys.source(
    file.path(repo_root, "scripts", "run-test-shard.R"),
    envir = shard_api
  )
  length(shard_api$ci_process_sensitive_test_files()) > 0L
}

local_validation_add_predictions <- function(schedule, repo_root) {
  shard_api <- new.env(parent = globalenv())
  sys.source(
    file.path(repo_root, "scripts", "run-test-shard.R"),
    envir = shard_api
  )
  plan <- shard_api$ci_test_plan(file.path(repo_root, "tests", "testthat"))
  weights <- shard_api$ci_test_runtime_weights(
    plan,
    file.path(repo_root, "scripts", "test-runtime-weights.csv")
  )
  for (group in c("logic", "browser")) {
    shards <- if (identical(group, "logic")) 4L else 6L
    assignments <- shard_api$ci_test_shards(
      plan[[group]],
      shards,
      strategy = "weighted",
      weights = weights
    )
    loads <- shard_api$ci_test_shard_loads(assignments, weights)
    schedule$predicted_seconds[schedule$phase == group] <- loads
  }
  schedule$predicted_seconds[schedule$phase == "process-sensitive"] <-
    unname(weights[plan$process_sensitive])
  attr(schedule, "estimated_files") <- attr(weights, "estimated")
  schedule
}

local_validation_print_schedule <- function(schedule) {
  for (phase in unique(schedule$phase)) {
    jobs <- schedule[schedule$phase == phase, , drop = FALSE]
    message("Phase ", phase, " (max parallel: ", jobs$cap[[1L]], ")")
    for (index in seq_len(nrow(jobs))) {
      prediction <- if (is.finite(jobs$predicted_seconds[[index]])) {
        paste0(" [predicted ", round(jobs$predicted_seconds[[index]], 1), "s]")
      } else {
        ""
      }
      message(
        "  ",
        jobs$name[[index]],
        prediction,
        ": ",
        jobs$command[[index]]
      )
    }
  }
  invisible(schedule)
}

local_validation_print_summary <- function(results, wall_seconds, output_dir) {
  message("\nValidation summary")
  for (index in seq_len(nrow(results))) {
    message(
      sprintf(
        "  %-24s exit=%d duration=%.1fs log=%s",
        results$name[[index]],
        results$status[[index]],
        results$duration[[index]],
        results$log[[index]]
      )
    )
  }
  slowest <- head(results[order(-results$duration), , drop = FALSE], 3L)
  message("Total wall time: ", round(wall_seconds, 1), "s")
  message(
    "Slowest jobs: ",
    paste0(slowest$name, "=", round(slowest$duration, 1), "s", collapse = ", ")
  )
  message("Outputs: ", output_dir)
  invisible(results)
}

local_validation_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- local_validation_parse_args(args)
  repo_root <- local_validation_repo_root()
  schedule <- local_validation_schedule(
    logic_workers = options$logic_workers,
    browser_workers = options$browser_workers,
    mode = options$mode,
    output_dir = options$output_dir,
    has_process_sensitive_tests = local_validation_has_process_sensitive_tests(
      repo_root
    )
  )
  schedule <- local_validation_add_predictions(schedule, repo_root)
  local_validation_print_schedule(schedule)
  estimated <- attr(schedule, "estimated_files")
  if (length(estimated)) {
    message("Estimated runtime weights: ", length(estimated), " file(s)")
  }
  if (isTRUE(options$dry_run)) {
    return(invisible(0L))
  }

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("The processx package is required.", call. = FALSE)
  }
  if (!nzchar(Sys.which("Rscript"))) {
    stop("Rscript is not available on PATH.", call. = FALSE)
  }
  dir.create(options$output_dir, recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  results <- local_validation_run_schedule(
    schedule,
    repo_root = repo_root,
    output_dir = options$output_dir
  )
  wall_seconds <- as.numeric(Sys.time() - started, units = "secs")
  local_validation_print_summary(results, wall_seconds, options$output_dir)
  invisible(local_validation_exit_code(results$status))
}

if (sys.nframe() == 0L) {
  status <- local_validation_main()
  if (!identical(status, 0L)) quit(status = status)
}
