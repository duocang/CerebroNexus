#!/usr/bin/env Rscript

.builder_eval_evidence_tables <- function() {
  c(
    "capability_detection",
    "plan_immutability",
    "artifact_fidelity",
    "build_runs",
    "fault_injection",
    "incremental_rebuild"
  )
}

.builder_eval_read_tables <- function(run) {
  stats::setNames(
    lapply(.builder_eval_evidence_tables(), function(name) {
      utils::read.csv(
        file.path(run, paste0(name, ".csv")),
        stringsAsFactors = FALSE,
        na.strings = ""
      )
    }),
    .builder_eval_evidence_tables()
  )
}

.builder_eval_required_run_files <- function() {
  c(
    "environment.json",
    "fixture_manifest.json",
    "source_manifest.json",
    "protocol.json",
    paste0(.builder_eval_evidence_tables(), ".csv"),
    "summary.md"
  )
}

.builder_eval_summary_matches <- function(run, tables) {
  observed <- readLines(file.path(run, "summary.md"), warn = FALSE)
  expected <- paste0("- ", builder_eval_summary_lines(tables))
  identical(observed[observed %in% expected], expected) &&
    "Overall: **PASS**" %in% observed
}

.builder_eval_validate_joins <- function(run, tables) {
  builds <- tables$build_runs
  plan <- tables$plan_immutability
  build_ids <- sort(unique(builds$trial_id), method = "radix")
  plan_ids <- sort(unique(plan$trial_id), method = "radix")
  if (!identical(build_ids, plan_ids)) {
    stop("Build and plan-identity trial joins are incomplete.")
  }
  boundary_count <- table(plan$trial_id)
  if (
    any(boundary_count != 3L) ||
      any(
        !plan$boundary %in% c("confirmation", "worker_result", "build_report")
      )
  ) {
    stop("Plan-identity boundaries are incomplete.")
  }
  reports <- builds$report_path[builds$success]
  if (
    anyNA(reports) ||
      any(!nzchar(reports)) ||
      any(grepl("^(/|~|[A-Za-z]:)", reports)) ||
      any(!file.exists(file.path(run, reports)))
  ) {
    stop("Successful build-report joins are incomplete or non-portable.")
  }
  invisible(TRUE)
}

.builder_eval_validate_publication_units <- function(tables, source_manifest) {
  builds <- tables$build_runs
  expected_cells <- c(
    "public_pbmc_expression_embedded_crb",
    "public_pbmc_expression_h5_crb",
    "public_pbmc_expression_bpcells_crb",
    "public_visium_brain_crb",
    "public_pbmc_vdj_crb",
    "composite_public_app",
    "composite_login_app"
  )
  if (
    nrow(builds) != 42L ||
      !setequal(builds$cell_id, expected_cells) ||
      !all(vapply(
        split(builds$repetition, builds$cell_id),
        function(value) {
          identical(sort(as.integer(value)), 0:5)
        },
        logical(1)
      )) ||
      sum(builds$measured) != 35L
  ) {
    stop("The publication profile experimental units are incomplete.")
  }
  if (
    nrow(tables$capability_detection) != 100L ||
      nrow(tables$plan_immutability) != 126L ||
      nrow(tables$fault_injection) != 9L ||
      nrow(tables$incremental_rebuild) != 3L
  ) {
    stop(
      "The publication profile evidence tables have unexpected trial counts."
    )
  }
  sources <- source_manifest$sources %||% list()
  if (
    !setequal(
      names(sources),
      c("pbmc_expression", "visium_brain", "pbmc_vdj")
    )
  ) {
    stop("The publication profile public-source manifest is incomplete.")
  }
  invisible(TRUE)
}

builder_eval_validate_run <- function(run, require_publication = TRUE) {
  run <- normalizePath(run, winslash = "/", mustWork = TRUE)
  missing <- .builder_eval_required_run_files()[
    !file.exists(file.path(run, .builder_eval_required_run_files()))
  ]
  if (length(missing)) {
    stop(
      "The Builder evidence run is incomplete; missing: ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  environment <- jsonlite::read_json(
    file.path(run, "environment.json"),
    simplifyVector = TRUE
  )
  protocol <- jsonlite::read_json(
    file.path(run, "protocol.json"),
    simplifyVector = TRUE
  )
  source_manifest <- jsonlite::read_json(
    file.path(run, "source_manifest.json"),
    simplifyVector = FALSE
  )
  fixture_manifest <- jsonlite::read_json(
    file.path(run, "fixture_manifest.json"),
    simplifyVector = FALSE
  )
  tables <- .builder_eval_read_tables(run)
  builder_eval_validate_tables(tables, protocol)
  gates <- builder_eval_gates(tables)
  if (!all(gates)) {
    stop(
      "The Builder evidence run failed correctness gates: ",
      paste(names(gates)[!gates], collapse = ", "),
      "."
    )
  }
  if (!.builder_eval_summary_matches(run, tables)) {
    stop("The Builder evidence summary disagrees with its raw rows.")
  }
  .builder_eval_validate_joins(run, tables)
  if (isTRUE(require_publication)) {
    if (
      !identical(protocol$profile, "publication") ||
        !identical(environment$evaluation_profile, "publication")
    ) {
      stop(
        "Only a publication profile can become the current evidence snapshot."
      )
    }
    if (!identical(environment$git_dirty, FALSE)) {
      stop(
        "A dirty publication run cannot become the current evidence snapshot."
      )
    }
    .builder_eval_validate_publication_units(tables, source_manifest)
  }
  list(
    run = run,
    environment = environment,
    protocol = protocol,
    source_manifest = source_manifest,
    fixture_manifest = fixture_manifest,
    tables = tables,
    gates = gates
  )
}

.builder_eval_snapshot_json <- function(value) {
  if (is.list(value)) {
    return(lapply(value, .builder_eval_snapshot_json))
  }
  if (!is.character(value)) {
    return(value)
  }
  absolute <- grepl("^(/|~[/\\]|[A-Za-z]:[/\\])", value)
  value[absolute] <- paste0("<external>/", basename(value[absolute]))
  value
}

.builder_eval_copy_file <- function(source, target) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  isTRUE(file.copy(source, target, overwrite = FALSE, copy.mode = TRUE))
}

.builder_eval_snapshot_has_absolute_path <- function(root) {
  files <- list.files(root, recursive = TRUE, full.names = TRUE)
  files <- files[!dir.exists(files)]
  text <- unlist(
    lapply(files, function(path) {
      if (grepl("[.](json|csv|md|svg)$", path, ignore.case = TRUE)) {
        readLines(path, warn = FALSE)
      } else {
        character()
      }
    }),
    use.names = FALSE
  )
  any(grepl("(^|[\"', ])(/Users/|/private/|/home/|[A-Za-z]:[/\\])", text))
}

.builder_eval_stage_snapshot <- function(validated, stage) {
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  json_names <- c(
    "environment.json",
    "fixture_manifest.json",
    "source_manifest.json",
    "protocol.json"
  )
  json_values <- list(
    validated$environment,
    validated$fixture_manifest,
    validated$source_manifest,
    validated$protocol
  )
  for (index in seq_along(json_names)) {
    value <- .builder_eval_snapshot_json(json_values[[index]])
    if (identical(json_names[[index]], "environment.json")) {
      value$invocation <- NULL
      value$git_status <- NULL
    }
    jsonlite::write_json(
      value,
      file.path(stage, json_names[[index]]),
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
  }
  for (name in .builder_eval_evidence_tables()) {
    utils::write.csv(
      validated$tables[[name]],
      file.path(stage, paste0(name, ".csv")),
      row.names = FALSE,
      na = ""
    )
  }
  if (
    !.builder_eval_copy_file(
      file.path(validated$run, "summary.md"),
      file.path(stage, "summary.md")
    )
  ) {
    stop("The Builder evidence summary could not be staged.")
  }
  reports <- file.path(validated$run, "reports")
  if (dir.exists(reports)) {
    dir.create(file.path(stage, "reports"))
    report_files <- list.files(reports, full.names = TRUE)
    if (
      length(report_files) &&
        !all(file.copy(
          report_files,
          file.path(stage, "reports", basename(report_files)),
          overwrite = FALSE,
          copy.mode = TRUE
        ))
    ) {
      stop("Portable Builder reports could not be staged.")
    }
  }
  figure_paths <- builder_eval_generate_figures(
    validated$tables,
    file.path(stage, "figures")
  )
  validation <- list(
    schema_version = 1L,
    status = "PASS",
    evidence_schema_version = validated$protocol$schema_version,
    profile = validated$protocol$profile,
    git_sha = validated$environment$git_sha,
    validated_at_utc = validated$environment$timestamp_utc,
    gates = as.list(validated$gates),
    table_rows = as.list(vapply(validated$tables, nrow, integer(1))),
    figure_md5 = as.list(stats::setNames(
      unname(as.character(tools::md5sum(figure_paths))),
      basename(figure_paths)
    ))
  )
  jsonlite::write_json(
    validation,
    file.path(stage, "validation.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  if (.builder_eval_snapshot_has_absolute_path(stage)) {
    stop(
      "The staged Builder evidence snapshot contains an absolute local path."
    )
  }
  invisible(stage)
}

builder_eval_publish_run <- function(
  run,
  package_root = getwd(),
  require_publication = TRUE
) {
  package_root <- normalizePath(package_root, winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(package_root, "DESCRIPTION"))) {
    stop("The Builder evidence publisher requires the package root.")
  }
  validated <- builder_eval_validate_run(run, require_publication)
  parent <- file.path(package_root, "inst", "extdata", "builder-evaluation")
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  current <- file.path(parent, "current")
  if (dir.exists(current)) {
    existing <- tryCatch(
      jsonlite::read_json(file.path(current, "validation.json")),
      error = function(error) NULL
    )
    if (
      is.list(existing) &&
        identical(existing$git_sha, validated$environment$git_sha)
    ) {
      stop("This immutable Builder evidence run is already current.")
    }
  }
  stage <- tempfile(".builder-evidence-", tmpdir = parent)
  prior <- tempfile(".builder-evidence-prior-", tmpdir = parent)
  on.exit(
    {
      unlink(stage, recursive = TRUE, force = TRUE)
      if (dir.exists(prior) && !dir.exists(current)) {
        file.rename(prior, current)
      }
    },
    add = TRUE
  )
  .builder_eval_stage_snapshot(validated, stage)
  if (dir.exists(current) && !file.rename(current, prior)) {
    stop("The prior Builder evidence snapshot could not be isolated.")
  }
  if (!file.rename(stage, current)) {
    stop("The validated Builder evidence snapshot could not be promoted.")
  }
  unlink(prior, recursive = TRUE, force = TRUE)
  image_dir <- file.path(package_root, "vignettes", "img")
  dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)
  figure_sources <- file.path(
    current,
    "figures",
    builder_eval_figure_files()
  )
  for (source in figure_sources) {
    target <- file.path(image_dir, basename(source))
    temporary <- tempfile(
      paste0(".", basename(source), "-"),
      tmpdir = image_dir
    )
    if (!file.copy(source, temporary, overwrite = FALSE, copy.mode = TRUE)) {
      stop("A vignette evidence figure could not be staged.")
    }
    if (file.exists(target)) {
      unlink(target, force = TRUE)
    }
    if (!file.rename(temporary, target)) {
      stop("A vignette evidence figure could not be promoted.")
    }
  }
  normalizePath(current, winslash = "/", mustWork = TRUE)
}

builder_eval_publish_args <- function(args) {
  index <- match("--run", args)
  if (is.na(index) || index == length(args)) {
    stop("Usage: Rscript validation/builder/publish.R --run <evidence-run>")
  }
  list(run = args[[index + 1L]])
}

if (!identical(Sys.getenv("BUILDER_EVAL_PUBLISH_LIBRARY_ONLY"), "true")) {
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Run validation/builder/publish.R from the repository root.")
  }
  sys.source(file.path(root, "validation", "builder", "lib.R"), globalenv())
  sys.source(file.path(root, "validation", "builder", "figures.R"), globalenv())
  options <- builder_eval_publish_args(commandArgs(trailingOnly = TRUE))
  published <- builder_eval_publish_run(options$run, root, TRUE)
  cat("Published Builder evidence:", published, "\n")
}
