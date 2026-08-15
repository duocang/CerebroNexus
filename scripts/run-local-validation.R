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
    stringsAsFactors = FALSE
  )
}

local_validation_schedule <- function(
  logic_workers = 3L,
  browser_workers = 2L,
  mode = "full",
  output_dir = tempfile("cerebro-local-validation-")
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
  process_sensitive <- local_validation_job(
    "process-sensitive",
    "process-sensitive",
    1L,
    paste(
      "Rscript scripts/run-test-shard.R",
      "--group process-sensitive"
    )
  )
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
  schedule <- rbind(logic, process_sensitive, browser)
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
        "new_process = FALSE, install = FALSE, ",
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

local_validation_print_schedule <- function(schedule) {
  for (phase in unique(schedule$phase)) {
    jobs <- schedule[schedule$phase == phase, , drop = FALSE]
    message("Phase ", phase, " (max parallel: ", jobs$cap[[1L]], ")")
    for (index in seq_len(nrow(jobs))) {
      message("  ", jobs$name[[index]], ": ", jobs$command[[index]])
    }
  }
  invisible(schedule)
}

local_validation_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- local_validation_parse_args(args)
  schedule <- local_validation_schedule(
    logic_workers = options$logic_workers,
    browser_workers = options$browser_workers,
    mode = options$mode,
    output_dir = options$output_dir
  )
  local_validation_print_schedule(schedule)
  if (!isTRUE(options$dry_run)) {
    stop("Local validation execution is not implemented yet.", call. = FALSE)
  }
  invisible(schedule)
}

if (sys.nframe() == 0L) {
  local_validation_main()
}
