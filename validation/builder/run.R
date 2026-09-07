#!/usr/bin/env Rscript

builder_eval_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(builder_eval_root, "DESCRIPTION"))) {
  stop("Run validation/builder/run.R from the repository root.")
}
devtools::load_all(builder_eval_root, quiet = TRUE, export_all = FALSE)

sys.source(
  file.path(builder_eval_root, "validation", "builder", "fixtures.R"),
  envir = globalenv()
)
sys.source(
  file.path(builder_eval_root, "validation", "builder", "lib.R"),
  envir = globalenv()
)
sys.source(
  file.path(builder_eval_root, "validation", "builder", "public_data.R"),
  envir = globalenv()
)
sys.source(
  file.path(builder_eval_root, "validation", "builder", "trials.R"),
  envir = globalenv()
)
sys.source(
  file.path(
    builder_eval_root,
    "tests",
    "testthat",
    "helper-builder-end-to-end.R"
  ),
  envir = globalenv()
)
builder_e2e_source_runtime(globalenv())
sys.source(
  file.path(
    builder_eval_root,
    "inst",
    "builder",
    "core",
    "bundle_path_contract.R"
  ),
  envir = globalenv()
)
sys.source(
  file.path(builder_eval_root, "inst", "builder", "review.R"),
  envir = globalenv()
)
sys.source(
  file.path(builder_eval_root, "inst", "builder", "workflow.R"),
  envir = globalenv()
)

builder_eval_args <- function(args) {
  value_after <- function(flag) {
    index <- match(flag, args)
    if (is.na(index)) {
      return(NULL)
    }
    if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
      stop(flag, " requires a value.")
    }
    args[[index + 1L]]
  }
  profile <- value_after("--profile")
  if (is.null(profile)) {
    profile <- if ("--smoke" %in% args) "smoke" else "standard"
  }
  if (!profile %in% c("smoke", "standard", "publication")) {
    stop("--profile must be smoke, standard, or publication.")
  }
  output_index <- match("--output", args)
  output <- NULL
  if (!is.na(output_index)) {
    if (
      output_index == length(args) ||
        startsWith(args[[output_index + 1L]], "--")
    ) {
      stop("--output requires a directory path.")
    }
    output <- args[[output_index + 1L]]
  }
  cache <- value_after("--cache")
  if (identical(profile, "publication") && is.null(cache)) {
    cache <- Sys.getenv("BUILDER_EVAL_PUBLIC_CACHE", unset = "")
    if (!nzchar(cache)) {
      stop(
        "The publication profile requires --cache or ",
        "BUILDER_EVAL_PUBLIC_CACHE."
      )
    }
  }
  list(profile = profile, output = output, cache = cache)
}

builder_eval_git <- function(...) {
  output <- system2("git", c(...), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("Git metadata could not be read.")
  }
  output
}

builder_eval_package_versions <- function(packages) {
  stats::setNames(
    lapply(packages, function(package) {
      tryCatch(
        as.character(utils::packageVersion(package)),
        error = function(error) NA_character_
      )
    }),
    packages
  )
}

builder_eval_incremental_trial <- function() {
  builder_eval_project_reuse_trial()
}

builder_eval_records_for_schedule <- function(row, fixtures, public_objects) {
  ids <- strsplit(row$dataset_id[[1L]], "+", fixed = TRUE)[[1L]]
  lapply(ids, function(id) {
    if (identical(row$source_kind[[1L]], "synthetic")) {
      fixture <- fixtures[[id]]
      if (is.null(fixture)) {
        stop("Unknown synthetic Builder fixture: ", id, ".")
      }
      return(list(id = id, label = id, object = fixture$make()))
    }
    prepared <- public_objects[[id]]
    source <- builder_eval_public_sources()[[id]]
    if (is.null(prepared) || is.null(source)) {
      stop("A scheduled public Builder source is unavailable: ", id, ".")
    }
    list(id = id, label = source$label, object = prepared)
  })
}

builder_eval_failed_build_row <- function(row, error) {
  data.frame(
    trial_id = row$trial_id,
    dataset_id = row$dataset_id,
    output_type = row$output_type,
    backend = row$backend,
    success = FALSE,
    failure_stage = "trial_setup",
    error = error,
    elapsed_seconds = NA_real_,
    process_elapsed_seconds = NA_real_,
    input_bytes = NA_real_,
    output_bytes = 0,
    artifact_hash = NA_character_,
    report_identity = NA_character_,
    report_path = NA_character_,
    stringsAsFactors = FALSE
  )
}

builder_eval_run_schedule <- function(
  schedule,
  fixtures,
  public_objects,
  trial_root
) {
  plan_rows <- list()
  artifact_rows <- list()
  build_rows <- list()
  for (index in seq_len(nrow(schedule))) {
    row <- schedule[index, , drop = FALSE]
    trial <- if (!isTRUE(row$available)) {
      structure(
        list(message = row$unavailability_reason),
        class = c("builder_eval_unavailable", "condition")
      )
    } else {
      tryCatch(
        builder_eval_build_trial(
          records = builder_eval_records_for_schedule(
            row,
            fixtures,
            public_objects
          ),
          trial_dir = file.path(trial_root, row$trial_id),
          trial_id = row$trial_id,
          output_type = row$output_type,
          backend = row$backend,
          repository_root = builder_eval_root
        ),
        error = function(error) error
      )
    }
    if (inherits(trial, "condition")) {
      build <- builder_eval_failed_build_row(row, conditionMessage(trial))
    } else {
      plan_rows[[row$trial_id]] <- trial$plans
      artifact_rows[[row$trial_id]] <- trial$artifacts
      build <- trial$builds
    }
    build$profile <- row$profile
    build$cell_id <- row$cell_id
    build$source_kind <- row$source_kind
    build$dataset_id <- row$dataset_id
    build$fixture <- row$dataset_id
    build$repetition <- row$repetition
    build$measured <- row$measured
    build$available <- row$available
    build_rows[[row$trial_id]] <- build
  }
  bind <- function(rows, empty) {
    if (!length(rows)) {
      return(empty)
    }
    value <- do.call(rbind, rows)
    rownames(value) <- NULL
    value
  }
  list(
    plans = bind(plan_rows, data.frame()),
    artifacts = bind(artifact_rows, .builder_eval_trial_empty_artifacts()),
    builds = bind(build_rows, data.frame())
  )
}

options <- builder_eval_args(commandArgs(trailingOnly = TRUE))
git_sha <- builder_eval_git("rev-parse", "HEAD")[[1L]]
dirty <- builder_eval_git("status", "--porcelain")
timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
output <- options$output
if (is.null(output)) {
  output <- file.path(
    builder_eval_root,
    "results",
    "builder",
    paste0(substr(git_sha, 1L, 12L), "_", timestamp)
  )
}
output <- normalizePath(output, winslash = "/", mustWork = FALSE)
if (identical(options$profile, "publication") && length(dirty)) {
  stop("The publication profile requires a clean Git worktree.")
}

fixtures <- builder_eval_fixtures()
capability_rows <- builder_eval_capability_rows(
  fixtures,
  profiler = builder_dataset_profile
)
schedule <- builder_eval_trial_schedule(options$profile)
public_objects <- list()
source_records <- list()
if (identical(options$profile, "publication")) {
  public_ids <- names(builder_eval_public_sources())
  public_cell_limit <- as.integer(Sys.getenv(
    "BUILDER_EVAL_PUBLIC_CELLS",
    unset = "200"
  ))
  for (id in public_ids) {
    prepared <- builder_eval_prepare_public_source(
      id,
      options$cache,
      max_cells = public_cell_limit
    )
    public_objects[[id]] <- prepared$object
    source_records[[id]] <- prepared$manifest
  }
}
trial_root <- tempfile("builder-evaluation-trials-")
dir.create(trial_root)
build_evidence <- builder_eval_run_schedule(
  schedule,
  fixtures,
  public_objects,
  trial_root
)

tables <- list(
  capability_detection = capability_rows,
  plan_immutability = build_evidence$plans,
  artifact_fidelity = build_evidence$artifacts,
  build_runs = build_evidence$builds,
  fault_injection = builder_eval_fault_trials(),
  incremental_rebuild = builder_eval_incremental_trial()
)
protocol <- builder_eval_protocol(options$profile, schedule)
protocol_validation <- tryCatch(
  {
    builder_eval_validate_tables(tables, protocol)
    NULL
  },
  error = function(error) error
)
gates <- c(
  protocol_schema = is.null(protocol_validation),
  builder_eval_gates(tables)
)
fixture_manifest <- list(
  schema_version = 2L,
  support = builder_eval_fixture_support(fixtures),
  fixtures = lapply(fixtures, function(fixture) {
    list(
      id = fixture$id,
      truth = as.list(fixture$truth),
      object_hash = builder_eval_hash(fixture$make())
    )
  }),
  challenges = lapply(builder_eval_challenges(), function(challenge) {
    list(
      id = challenge$id,
      capability = challenge$capability,
      expected_state = challenge$expected_state,
      object_hash = builder_eval_hash(challenge$make())
    )
  })
)
environment <- list(
  schema_version = 2L,
  git_sha = git_sha,
  git_dirty = length(dirty) > 0L,
  git_status = unname(dirty),
  invocation = commandArgs(),
  timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  r_version = R.version.string,
  platform = R.version$platform,
  operating_system = Sys.info()[c("sysname", "release", "machine")],
  package_version = as.character(utils::packageVersion("CerebroNexus")),
  package_versions = builder_eval_package_versions(c(
    "Seurat",
    "SeuratObject",
    "jsonlite",
    "callr",
    "testthat"
  )),
  evaluation_profile = options$profile,
  protocol_validation_error = if (is.null(protocol_validation)) {
    NULL
  } else {
    conditionMessage(protocol_validation)
  }
)

builder_eval_write_results(
  output,
  environment = environment,
  fixture_manifest = fixture_manifest,
  tables = tables,
  gates = gates,
  source_manifest = list(
    schema_version = 1L,
    sources = source_records
  ),
  protocol = protocol
)
unlink(trial_root, recursive = TRUE, force = TRUE)
cat("Builder evaluation:", if (all(gates)) "PASS" else "FAIL", "\n")
cat("Results:", output, "\n")
if (!all(gates)) {
  quit(save = "no", status = 1L)
}
