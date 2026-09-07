builder_eval_available_backends <- function() {
  backends <- "embedded"
  if (
    requireNamespace("rhdf5", quietly = TRUE) &&
      requireNamespace("HDF5Array", quietly = TRUE)
  ) {
    backends <- c(backends, "h5")
  }
  if (requireNamespace("BPCells", quietly = TRUE)) {
    backends <- c(backends, "bpcells")
  }
  backends
}

.builder_eval_trial_cells <- function(profile, available_backends) {
  cell <- function(
    cell_id,
    source_kind,
    dataset_id,
    output_type = "CRB",
    backend = "embedded"
  ) {
    data.frame(
      cell_id = cell_id,
      source_kind = source_kind,
      dataset_id = dataset_id,
      output_type = output_type,
      backend = backend,
      available = backend %in% available_backends,
      unavailability_reason = if (backend %in% available_backends) {
        ""
      } else {
        paste0("Backend `", backend, "` is unavailable.")
      },
      stringsAsFactors = FALSE
    )
  }
  if (identical(profile, "smoke")) {
    return(cell("synthetic_minimal_crb", "synthetic", "minimal"))
  }
  if (identical(profile, "standard")) {
    ids <- c(
      "minimal",
      "multi_assay",
      "spatial",
      "immune_hla",
      "trajectory",
      "extra_content"
    )
    return(do.call(
      rbind,
      lapply(ids, function(id) {
        cell(paste0("synthetic_", id, "_crb"), "synthetic", id)
      })
    ))
  }
  cells <- list(
    cell(
      "public_pbmc_expression_embedded_crb",
      "public",
      "pbmc_expression"
    ),
    cell(
      "public_pbmc_expression_h5_crb",
      "public",
      "pbmc_expression",
      backend = "h5"
    ),
    cell(
      "public_pbmc_expression_bpcells_crb",
      "public",
      "pbmc_expression",
      backend = "bpcells"
    ),
    cell("public_visium_brain_crb", "public", "visium_brain"),
    cell("public_pbmc_vdj_crb", "public", "pbmc_vdj"),
    cell(
      "composite_public_app",
      "composite",
      "pbmc_expression+visium_brain",
      output_type = "public_app"
    ),
    cell(
      "composite_login_app",
      "composite",
      "pbmc_expression+pbmc_vdj",
      output_type = "login_app"
    )
  )
  do.call(rbind, cells)
}

builder_eval_trial_schedule <- function(
  profile = c("smoke", "standard", "publication"),
  available_backends = builder_eval_available_backends()
) {
  profile <- match.arg(profile)
  if (
    !is.character(available_backends) ||
      anyNA(available_backends) ||
      any(!available_backends %in% c("embedded", "h5", "bpcells"))
  ) {
    stop("The Builder evaluation backend registry is invalid.")
  }
  cells <- .builder_eval_trial_cells(profile, unique(available_backends))
  repetitions <- if (identical(profile, "publication")) 0:5 else 1L
  scheduled <- lapply(repetitions, function(repetition) {
    offset <- repetition %% nrow(cells)
    order <- c(
      seq.int(offset + 1L, nrow(cells)),
      if (offset > 0L) seq_len(offset) else integer()
    )
    rows <- cells[order, , drop = FALSE]
    rows$profile <- profile
    rows$repetition <- as.integer(repetition)
    rows$measured <- if (identical(profile, "publication")) {
      repetition > 0L
    } else {
      TRUE
    }
    rows$trial_id <- paste0(rows$cell_id, "_r", repetition)
    rows[, c(
      "trial_id",
      "profile",
      "cell_id",
      "source_kind",
      "dataset_id",
      "output_type",
      "backend",
      "repetition",
      "measured",
      "available",
      "unavailability_reason"
    )]
  })
  schedule <- do.call(rbind, scheduled)
  rownames(schedule) <- NULL
  schedule
}

.builder_eval_trial_repository_root <- function() {
  candidates <- c(
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  )
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[file.exists(file.path(candidates, "DESCRIPTION"))]
  if (!length(matches)) {
    stop("The Builder evaluation repository root is unavailable.")
  }
  matches[[1L]]
}

.builder_eval_trial_record <- function(record) {
  if (
    !is.list(record) ||
      !is.character(record$id) ||
      length(record$id) != 1L ||
      is.na(record$id) ||
      !nzchar(record$id) ||
      !is.character(record$label) ||
      length(record$label) != 1L ||
      is.na(record$label) ||
      !nzchar(record$label) ||
      !inherits(record$object, "Seurat")
  ) {
    stop("A Builder evaluation trial record is invalid.")
  }
  object <- record$object
  list(
    id = record$id,
    label = record$label,
    make = function() list(object = object, format = "Evaluation source")
  )
}

.builder_eval_trial_child <- function(
  repository_root,
  plan,
  stage,
  snapshots,
  login
) {
  setwd(repository_root)
  devtools::load_all(repository_root, quiet = TRUE, export_all = FALSE)
  assign(
    ".viewerAuthValidateDatabase",
    getFromNamespace(".viewerAuthValidateDatabase", "CerebroNexus"),
    envir = globalenv()
  )
  for (file in c(
    "helper-builder-00-paths.R",
    "helper-builder-profile-fixtures.R",
    "helper-builder-end-to-end.R"
  )) {
    sys.source(
      file.path(repository_root, "tests", "testthat", file),
      envir = globalenv()
    )
  }
  builder_e2e_source_runtime(globalenv())
  sys.source(
    file.path(
      repository_root,
      "inst",
      "builder",
      "core",
      "bundle_path_contract.R"
    ),
    envir = globalenv()
  )
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  auth_material <- NULL
  if (isTRUE(login)) {
    accounts <- list(
      list(
        id = "auth-account-1",
        username = "evaluation-reader-1",
        password = "evaluation-password-1"
      ),
      list(
        id = "auth-account-2",
        username = "evaluation-reader-2",
        password = "evaluation-password-2"
      )
    )
    auth_material <- builder_auth_create_material(accounts, stage)
    accounts <- NULL
  }
  started <- proc.time()[["elapsed"]]
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots,
    auth_material = auth_material
  )
  elapsed <- proc.time()[["elapsed"]] - started
  auth_material <- NULL
  if (!identical(result$state, "success") || !isTRUE(result$publishable)) {
    return(list(
      success = FALSE,
      state = result$state,
      error = result$error %||% "Builder worker failed.",
      elapsed_seconds = as.double(elapsed),
      plan_digest = result$plan_digest %||% NA_character_,
      report_plan_digest = NA_character_,
      report_identity = NA_character_,
      report_path = NA_character_,
      built = result$built %||% character(),
      verifications = result$verifications %||% list()
    ))
  }
  report <- builder_build_report(plan, result)
  report_path <- builder_write_build_report(stage, report)
  reread <- builder_read_build_report(report_path)
  list(
    success = TRUE,
    state = result$state,
    error = "",
    elapsed_seconds = as.double(elapsed),
    plan_digest = result$plan_digest,
    report_plan_digest = reread$plan_digest,
    report_identity = reread$identity,
    report_path = report_path,
    built = result$built,
    verifications = result$verifications
  )
}

.builder_eval_trial_empty_artifacts <- function() {
  data.frame(
    trial_id = character(),
    fixture = character(),
    dataset = character(),
    capability = character(),
    selected = logical(),
    observed = logical(),
    outcome = character(),
    artifact_hash = character(),
    source_identity_hash = character(),
    stringsAsFactors = FALSE
  )
}

builder_eval_build_trial <- function(
  records,
  trial_dir,
  trial_id,
  output_type = c("CRB", "public_app", "login_app"),
  backend = c("embedded", "h5", "bpcells"),
  repository_root = .builder_eval_trial_repository_root()
) {
  output_type <- match.arg(output_type)
  backend <- match.arg(backend)
  caller <- parent.frame()
  if (
    !is.list(records) ||
      !length(records) ||
      !.builder_eval_public_scalar(trial_dir) ||
      !.builder_eval_public_scalar(trial_id) ||
      file.exists(trial_dir) ||
      dir.exists(trial_dir)
  ) {
    stop("A new Builder evaluation trial directory and trial ID are required.")
  }
  dir.create(trial_dir, recursive = TRUE, showWarnings = FALSE)
  profile_records <- lapply(records, .builder_eval_trial_record)
  make_entry <- get("builder_e2e_entry", envir = caller, inherits = TRUE)
  entries <- lapply(profile_records, function(record) {
    make_entry(record, caller = caller)
  })
  for (index in seq_along(entries)) {
    entries[[index]]$settings$expression_backend <- backend
    trajectory <- entries[[index]]$dataset_profile$content$trajectory
    if (
      is.list(trajectory) &&
        isTRUE(trajectory$detected) &&
        isTRUE(trajectory$valid)
    ) {
      entries[[index]]$settings$included_trajectories <- list(
        monocle2 = "lineage"
      )
      entries[[index]]$settings$default_trajectory <- list(
        method = "monocle2",
        name = "lineage"
      )
    }
  }
  snapshots <- vector("list", length(entries))
  names(snapshots) <- vapply(records, `[[`, "", "id")
  for (index in seq_along(entries)) {
    snapshot <- get(
      "builder_snapshot_seurat",
      envir = caller,
      inherits = TRUE
    )(
      records[[index]]$object,
      file.path(trial_dir, paste0("snapshot-", records[[index]]$id)),
      available_bytes = 2^40
    )
    entries[[index]]$snapshot <- snapshot
    snapshots[[index]] <- snapshot
  }
  release <- file.path(trial_dir, "release")
  make_app <- !identical(output_type, "CRB")
  login <- identical(output_type, "login_app")
  plan <- get("builder_freeze_plan", envir = caller, inherits = TRUE)(
    entries,
    release,
    make_app = make_app,
    app_auth = list(
      enabled = login,
      account_count = if (login) 2L else 0L,
      timeout_minutes = 15L
    )
  )
  if (inherits(plan, "builder_plan_failure")) {
    stop("The Builder evaluation plan failed: ", plan$error)
  }
  plan_digest <- get(
    "builder_publication_plan_digest",
    envir = caller,
    inherits = TRUE
  )
  confirmation_digest <- plan_digest(plan)
  records[[1L]]$label <- paste(records[[1L]]$label, "mutated")
  records[[1L]]$object@misc$evaluation_mutation <- trial_id
  mutation_not_propagated <- identical(
    confirmation_digest,
    plan_digest(plan)
  )
  stage <- file.path(trial_dir, "stage")
  process_started <- proc.time()[["elapsed"]]
  child <- tryCatch(
    callr::r(
      .builder_eval_trial_child,
      args = list(
        repository_root = repository_root,
        plan = plan,
        stage = stage,
        snapshots = snapshots,
        login = login
      ),
      show = FALSE
    ),
    error = function(error) error
  )
  process_elapsed <- proc.time()[["elapsed"]] - process_started
  if (inherits(child, "condition")) {
    child <- list(
      success = FALSE,
      state = "failure",
      error = conditionMessage(child),
      elapsed_seconds = NA_real_,
      plan_digest = NA_character_,
      report_plan_digest = NA_character_,
      report_identity = NA_character_,
      report_path = NA_character_,
      built = character(),
      verifications = list()
    )
  }
  digests <- c(
    confirmation = confirmation_digest,
    worker_result = child$plan_digest,
    build_report = child$report_plan_digest
  )
  plans <- data.frame(
    trial_id = trial_id,
    boundary = names(digests),
    plan_digest = unname(digests),
    matches_confirmation = unname(digests == confirmation_digest),
    mutation_not_propagated = mutation_not_propagated,
    stringsAsFactors = FALSE
  )
  artifacts <- .builder_eval_trial_empty_artifacts()
  artifact_hashes <- character()
  if (isTRUE(child$success)) {
    artifact_reader <- get(
      ".builder_build_field",
      envir = caller,
      inherits = TRUE
    )
    artifact_tables <- lapply(seq_along(plan$items), function(index) {
      item <- plan$items[[index]]
      path <- unname(child$built[[item$name]])
      artifact_hash <- unname(as.character(tools::md5sum(path)))
      artifact_hashes <<- c(artifact_hashes, artifact_hash)
      rows <- builder_eval_artifact_rows(
        item,
        readRDS(path),
        reader = artifact_reader
      )
      rows$trial_id <- trial_id
      rows$fixture <- item$id
      rows$artifact_hash <- artifact_hash
      rows$source_identity_hash <- builder_eval_hash(
        item$source_snapshot_identity
      )
      rows[, names(.builder_eval_trial_empty_artifacts())]
    })
    artifacts <- do.call(rbind, artifact_tables)
    rownames(artifacts) <- NULL
  }
  output_paths <- c(child$built, child$report_path)
  output_paths <- output_paths[
    is.character(output_paths) &
      !is.na(output_paths) &
      file.exists(output_paths)
  ]
  builds <- data.frame(
    trial_id = trial_id,
    dataset_id = paste(vapply(records, `[[`, "", "id"), collapse = "+"),
    output_type = output_type,
    backend = backend,
    success = isTRUE(child$success),
    failure_stage = if (isTRUE(child$success)) "" else "worker_or_report",
    error = child$error %||% "",
    elapsed_seconds = as.double(child$elapsed_seconds),
    process_elapsed_seconds = as.double(process_elapsed),
    input_bytes = sum(vapply(
      records,
      function(record) as.double(object.size(record$object)),
      numeric(1)
    )),
    output_bytes = sum(as.double(file.info(output_paths)$size)),
    artifact_hash = if (length(artifact_hashes)) {
      builder_eval_hash(sort(artifact_hashes, method = "radix"))
    } else {
      NA_character_
    },
    report_identity = child$report_identity,
    report_path = child$report_path,
    stringsAsFactors = FALSE
  )
  list(plans = plans, artifacts = artifacts, builds = builds, plan = plan)
}

.builder_eval_fault_child <- function(
  repository_root,
  target,
  replacement_crb,
  scenario
) {
  sys.source(
    file.path(
      repository_root,
      "inst",
      "builder",
      "core",
      "bundle_path_contract.R"
    ),
    envir = globalenv()
  )
  sys.source(
    file.path(repository_root, "inst", "builder", "publish.R"),
    envir = globalenv()
  )
  handle <- builder_prepare_release(
    target,
    paste0("fault-", scenario),
    builder_release_identity(target)
  )
  if (!file.copy(replacement_crb, file.path(handle$stage, "dataset.crb"))) {
    stop("The replacement CRB could not be staged.")
  }
  exit_at <- function(kind, value) {
    matched <- switch(
      scenario,
      process_exit_before_old_move = identical(kind, "phase") &&
        identical(value, "old_moving"),
      process_exit_after_old_move = identical(kind, "move") &&
        identical(value, "old_to_backup"),
      process_exit_after_old_moved_phase = identical(kind, "phase") &&
        identical(value, "old_moved"),
      process_exit_after_new_move = identical(kind, "move") &&
        identical(value, "new_to_target"),
      process_exit_after_new_published_phase = identical(kind, "phase") &&
        identical(value, "new_published"),
      FALSE
    )
    if (isTRUE(matched)) {
      quit(save = "no", status = 86L, runLast = FALSE)
    }
  }
  builder_publish_release(
    handle,
    .after_phase = function(phase) exit_at("phase", phase),
    .after_move = function(move) exit_at("move", move)
  )
  stop("The process-exit injection point was not reached.")
}

.builder_eval_reopen_crb <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  artifact <- tryCatch(readRDS(path), error = identity)
  !inherits(artifact, "condition") && !is.null(artifact)
}

.builder_eval_release_residue <- function(control) {
  stages <- file.path(control, "stages")
  diagnostics <- file.path(control, "diagnostics")
  data.frame(
    stage_residue_count = if (dir.exists(stages)) {
      length(list.files(stages, all.files = TRUE, no.. = TRUE))
    } else {
      0L
    },
    lock_residue_count = as.integer(dir.exists(file.path(control, "lock"))),
    backup_residue_count = as.integer(dir.exists(file.path(control, "backup"))),
    retired_backup_residue_count = if (dir.exists(diagnostics)) {
      length(list.files(
        diagnostics,
        pattern = "^retired-backup-",
        all.files = TRUE,
        no.. = TRUE
      ))
    } else {
      0L
    }
  )
}

# This replaces the earlier pilot in lib.R after trials.R is sourced. It builds
# two genuine CRBs once, then exercises handled errors and actual child-process
# exits against copies of those artifacts.
builder_eval_fault_trials <- function(
  repository_root = .builder_eval_trial_repository_root()
) {
  caller <- parent.frame()
  trial_runtime <- environment()
  get(
    "builder_e2e_source_runtime",
    envir = caller,
    inherits = TRUE
  )(trial_runtime)
  assign(
    "builder_e2e_entry",
    get("builder_e2e_entry", envir = caller, inherits = TRUE),
    envir = trial_runtime
  )
  bundle_contract <- file.path(
    repository_root,
    "inst",
    "builder",
    "core",
    "bundle_path_contract.R"
  )
  if (!exists(".canonicalTargetPath", envir = trial_runtime, inherits = TRUE)) {
    sys.source(bundle_contract, envir = trial_runtime)
  }
  runtime <- mget(
    c(
      "builder_release_identity",
      "builder_prepare_release",
      "builder_publish_release",
      "builder_abort_release",
      "builder_discover_recovery",
      "builder_recover_release",
      "builder_release_control_path"
    ),
    envir = trial_runtime,
    inherits = TRUE
  )
  root <- tempfile("builder-eval-faults-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  fixtures <- builder_eval_fixtures()
  records <- lapply(c("minimal", "multi_assay"), function(id) {
    list(id = id, label = id, object = fixtures[[id]]$make())
  })
  artifact_trial <- builder_eval_build_trial(
    records,
    file.path(root, "artifact-build"),
    "fault-artifact-build",
    repository_root = repository_root
  )
  if (!all(artifact_trial$builds$success)) {
    stop(
      "The real CRB prerequisites for fault trials failed: ",
      paste(artifact_trial$builds$error, collapse = "; ")
    )
  }
  built_paths <- file.path(
    root,
    "artifact-build",
    "stage",
    vapply(artifact_trial$plan$items, `[[`, "", "filename")
  )
  names(built_paths) <- vapply(artifact_trial$plan$items, `[[`, "", "id")
  old_crb <- unname(built_paths[["minimal"]])
  replacement_crb <- unname(built_paths[["multi_assay"]])
  if (!all(file.exists(c(old_crb, replacement_crb)))) {
    stop("The real CRB prerequisites for fault trials are missing.")
  }

  handled <- list(
    prior_protection_failure = function(handle) {
      runtime$builder_publish_release(handle, .move = function(from, to) FALSE)
    },
    pre_rename_verification_failure = function(handle) {
      runtime$builder_publish_release(
        handle,
        .verify_payload = function(root, phase) {
          !identical(phase, "before_rename")
        }
      )
    },
    promotion_failure = function(handle) {
      moves <- 0L
      runtime$builder_publish_release(handle, .move = function(from, to) {
        moves <<- moves + 1L
        if (identical(moves, 2L)) FALSE else file.rename(from, to)
      })
    },
    post_rename_verification_failure = function(handle) {
      runtime$builder_publish_release(
        handle,
        .verify_payload = function(root, phase) {
          !identical(phase, "after_rename")
        }
      )
    }
  )
  crashed <- c(
    "process_exit_before_old_move",
    "process_exit_after_old_move",
    "process_exit_after_old_moved_phase",
    "process_exit_after_new_move",
    "process_exit_after_new_published_phase"
  )
  scenarios <- c(names(handled), crashed)
  rows <- lapply(scenarios, function(scenario) {
    trial_root <- file.path(root, scenario)
    dir.create(trial_root)
    target <- file.path(trial_root, "release")
    dir.create(target)
    file.copy(old_crb, file.path(target, "dataset.crb"))
    prior <- runtime$builder_release_identity(target)
    prior_release_hash <- builder_eval_hash(prior)
    prior_crb_hash <- unname(tools::md5sum(file.path(target, "dataset.crb")))
    recovery_invoked <- scenario %in% crashed

    if (scenario %in% names(handled)) {
      handle <- runtime$builder_prepare_release(
        target,
        paste0("handled-", scenario),
        expected_prior = prior
      )
      file.copy(replacement_crb, file.path(handle$stage, "dataset.crb"))
      failure <- tryCatch(handled[[scenario]](handle), error = identity)
      cleanup <- tryCatch(
        runtime$builder_abort_release(handle),
        error = identity
      )
      recovery_success <- !inherits(cleanup, "condition")
      failure_message <- if (inherits(failure, "condition")) {
        conditionMessage(failure)
      } else {
        "The handled fault did not raise an error."
      }
    } else {
      child <- callr::r_bg(
        .builder_eval_fault_child,
        args = list(
          repository_root = repository_root,
          target = target,
          replacement_crb = replacement_crb,
          scenario = scenario
        ),
        supervise = TRUE
      )
      child$wait(timeout = 30000)
      status <- child$get_exit_status()
      failure_message <- paste0(
        "Independent R process exited with status ",
        status,
        "."
      )
      recovery <- tryCatch(
        runtime$builder_recover_release(target, "restore"),
        error = identity
      )
      recovery_success <- !inherits(recovery, "condition") &&
        isTRUE(recovery$recovered)
    }

    recovered <- runtime$builder_release_identity(target)
    recovered_crb <- file.path(target, "dataset.crb")
    old_preserved <- identical(recovered, prior) &&
      identical(unname(tools::md5sum(recovered_crb)), prior_crb_hash)
    old_crb_reopenable <- .builder_eval_reopen_crb(recovered_crb)

    clean <- runtime$builder_prepare_release(
      target,
      paste0("clean-after-", scenario),
      expected_prior = recovered
    )
    file.copy(replacement_crb, file.path(clean$stage, "dataset.crb"))
    published <- tryCatch(
      runtime$builder_publish_release(clean),
      error = identity
    )
    published_crb <- file.path(target, "dataset.crb")
    subsequent_publish_success <- !inherits(published, "condition") &&
      isTRUE(published$published) &&
      identical(
        unname(tools::md5sum(published_crb)),
        unname(tools::md5sum(replacement_crb))
      )
    control <- runtime$builder_release_control_path(target)
    residue <- .builder_eval_release_residue(control)
    journal <- readRDS(file.path(control, "journal.rds"))
    formal_targets <- list.files(
      trial_root,
      pattern = "^release$",
      full.names = TRUE
    )
    data.frame(
      trial_id = paste0("fault-", scenario),
      scenario = scenario,
      fault_class = if (scenario %in% crashed) {
        "process_exit"
      } else {
        "handled_error"
      },
      injection_point = sub("^(process_exit_|.*_)", "", scenario),
      error = failure_message,
      prior_release_hash = prior_release_hash,
      prior_crb_hash = prior_crb_hash,
      recovered_release_hash = builder_eval_hash(recovered),
      published_crb_hash = unname(tools::md5sum(published_crb)),
      old_preserved = old_preserved,
      old_crb_reopenable = old_crb_reopenable,
      recovery_invoked = recovery_invoked,
      recovery_success = recovery_success && old_preserved,
      subsequent_publish_success = subsequent_publish_success,
      subsequent_crb_reopenable = .builder_eval_reopen_crb(published_crb),
      partial_target_count = as.integer(length(formal_targets) != 1L),
      stage_residue_count = residue$stage_residue_count,
      lock_residue_count = residue$lock_residue_count,
      backup_residue_count = residue$backup_residue_count,
      retired_backup_residue_count = residue$retired_backup_residue_count,
      journal_state = as.character(journal$phase),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL
  rows
}

builder_eval_project_reuse_trial <- function(
  repository_root = .builder_eval_trial_repository_root()
) {
  caller <- parent.frame()
  trial_runtime <- environment()
  get(
    "builder_e2e_source_runtime",
    envir = caller,
    inherits = TRUE
  )(trial_runtime)
  assign(
    "builder_e2e_entry",
    get("builder_e2e_entry", envir = caller, inherits = TRUE),
    envir = trial_runtime
  )
  sys.source(
    file.path(
      repository_root,
      "inst",
      "builder",
      "core",
      "bundle_path_contract.R"
    ),
    envir = trial_runtime
  )
  sys.source(
    file.path(repository_root, "inst", "builder", "project.R"),
    envir = trial_runtime
  )
  runtime <- mget(
    c(
      "builder_e2e_entry",
      "builder_snapshot_seurat",
      "builder_freeze_plan",
      "builder_execute_plan",
      "builder_project_store_artifact_bundle",
      "builder_project_artifact_available",
      "builder_project_artifact_entry",
      "builder_project_resolve_path",
      "builder_project_configuration_digest"
    ),
    envir = trial_runtime,
    inherits = TRUE
  )
  root <- tempfile("builder-eval-project-reuse-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  project_root <- file.path(root, "project")
  dir.create(project_root)
  fixtures <- builder_eval_fixtures()
  ids <- c("project_a", "project_b", "project_c")
  objects <- list(
    project_a = fixtures$minimal$make(),
    project_b = fixtures$minimal$make(),
    project_c = fixtures$multi_assay$make()
  )
  labels <- c(
    project_a = "Project A",
    project_b = "Project B v1",
    project_c = "Project C"
  )
  make_entry <- function(id, object, label, snapshot_root) {
    record <- .builder_eval_trial_record(list(
      id = id,
      label = label,
      object = object
    ))
    entry <- runtime$builder_e2e_entry(record, caller = trial_runtime)
    dir.create(dirname(snapshot_root), recursive = TRUE, showWarnings = FALSE)
    snapshot <- runtime$builder_snapshot_seurat(
      object,
      snapshot_root,
      available_bytes = 2^40
    )
    entry$snapshot <- snapshot
    list(entry = entry, snapshot = snapshot)
  }
  initial <- lapply(ids, function(id) {
    make_entry(
      id,
      objects[[id]],
      labels[[id]],
      file.path(root, "snapshots-initial", id)
    )
  })
  names(initial) <- ids
  initial_entries <- unname(lapply(initial, `[[`, "entry"))
  initial_snapshots <- lapply(initial, `[[`, "snapshot")
  initial_plan <- runtime$builder_freeze_plan(
    initial_entries,
    file.path(root, "full-release"),
    make_app = FALSE
  )
  if (inherits(initial_plan, "builder_plan_failure")) {
    stop("The full project-reuse plan failed: ", initial_plan$error)
  }
  full_stage <- file.path(root, "full-stage")
  dir.create(full_stage)
  full_elapsed <- system.time({
    full <- runtime$builder_execute_plan(
      initial_plan,
      full_stage,
      initial_snapshots
    )
  })[["elapsed"]]
  if (!identical(full$state, "success")) {
    stop("The full project build failed: ", full$error)
  }

  initial_digests <- stats::setNames(
    vapply(
      initial_entries,
      runtime$builder_project_configuration_digest,
      character(1)
    ),
    ids
  )
  artifacts <- lapply(seq_along(ids), function(index) {
    id <- ids[[index]]
    item <- initial_plan$items[[index]]
    built <- unname(full$built[[item$name]])
    bundle <- runtime$builder_project_store_artifact_bundle(
      built,
      sidecars = item$sidecars %||% character(),
      dataset_id = id,
      root = project_root
    )
    artifact <- list(
      status = "ready",
      reusable = TRUE,
      path = bundle$path,
      fingerprint = bundle$fingerprint,
      built_from_configuration = initial_digests[[id]],
      plan_payload = jsonlite::serializeJSON(
        item,
        digits = NA,
        pretty = FALSE
      ),
      members = bundle$members
    )
    artifact$available <- runtime$builder_project_artifact_available(
      artifact,
      project_root
    )
    artifact$resolved_path <- runtime$builder_project_resolve_path(
      artifact$path,
      project_root,
      "managed"
    )
    if (length(artifact$members)) {
      artifact$members <- lapply(artifact$members, function(member) {
        member$resolved_path <- runtime$builder_project_resolve_path(
          member$path,
          project_root,
          "managed"
        )
        member
      })
    }
    artifact
  })
  names(artifacts) <- ids
  if (!all(vapply(artifacts, `[[`, logical(1), "available"))) {
    stop("A project-managed CRB failed its availability contract.")
  }

  incremental_entries <- initial_entries
  for (id in c("project_a", "project_c")) {
    index <- match(
      id,
      vapply(incremental_entries, `[[`, character(1), "id")
    )
    incremental_entries[[index]] <- runtime$builder_project_artifact_entry(
      incremental_entries[[index]],
      artifacts[[id]],
      project_root
    )
  }
  changed <- make_entry(
    "project_b",
    fixtures$multi_assay$make(),
    "Project B v2",
    file.path(root, "snapshots-incremental", "project_b")
  )
  incremental_entries[[match(
    "project_b",
    vapply(incremental_entries, `[[`, character(1), "id")
  )]] <- changed$entry
  incremental_snapshots <- list(project_b = changed$snapshot)
  incremental_digests <- stats::setNames(
    vapply(
      incremental_entries,
      runtime$builder_project_configuration_digest,
      character(1)
    ),
    ids
  )
  incremental_plan <- runtime$builder_freeze_plan(
    incremental_entries,
    file.path(root, "incremental-release"),
    make_app = FALSE
  )
  if (inherits(incremental_plan, "builder_plan_failure")) {
    stop(
      "The incremental project-reuse plan failed: ",
      incremental_plan$error
    )
  }
  incremental_stage <- file.path(root, "incremental-stage")
  dir.create(incremental_stage)
  incremental_elapsed <- system.time({
    incremental <- runtime$builder_execute_plan(
      incremental_plan,
      incremental_stage,
      incremental_snapshots
    )
  })[["elapsed"]]
  if (!identical(incremental$state, "success")) {
    stop("The incremental project build failed: ", incremental$error)
  }
  rows <- builder_eval_reuse_rows(
    incremental_plan,
    incremental,
    expected_rebuilt = "project_b",
    trial_id = "project-managed-incremental-1"
  )
  initial_hashes <- stats::setNames(
    vapply(
      ids,
      function(id) artifacts[[id]]$fingerprint$md5,
      character(1)
    ),
    ids
  )
  rows$baseline_hash <- unname(initial_hashes[rows$dataset])
  rows$configuration_before <- unname(initial_digests[rows$dataset])
  rows$configuration_after <- unname(incremental_digests[rows$dataset])
  rows$configuration_changed <- rows$configuration_before !=
    rows$configuration_after
  rows$project_managed <- unname(vapply(
    artifacts[rows$dataset],
    `[[`,
    logical(1),
    "available"
  ))
  rows$real_builder_hooks <- TRUE
  rows$full_elapsed_seconds <- as.double(full_elapsed)
  rows$incremental_elapsed_seconds <- as.double(incremental_elapsed)
  rows$time_saved_ratio <- if (full_elapsed > 0) {
    1 - incremental_elapsed / full_elapsed
  } else {
    NA_real_
  }
  rows
}
