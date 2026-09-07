evaluation_library <- testthat::test_path(
  "..",
  "..",
  "validation",
  "builder",
  "lib.R"
)
if (file.exists(evaluation_library)) {
  sys.source(evaluation_library, envir = environment())
}
evaluation_fixtures <- testthat::test_path(
  "..",
  "..",
  "validation",
  "builder",
  "fixtures.R"
)
if (file.exists(evaluation_fixtures)) {
  sys.source(evaluation_fixtures, envir = environment())
}
evaluation_public_data <- testthat::test_path(
  "..",
  "..",
  "validation",
  "builder",
  "public_data.R"
)
if (file.exists(evaluation_public_data)) {
  sys.source(evaluation_public_data, envir = environment())
}
evaluation_trials <- testthat::test_path(
  "..",
  "..",
  "validation",
  "builder",
  "trials.R"
)
if (file.exists(evaluation_trials)) {
  sys.source(evaluation_trials, envir = environment())
}
evaluation_figures <- testthat::test_path(
  "..",
  "..",
  "validation",
  "builder",
  "figures.R"
)
if (file.exists(evaluation_figures)) {
  sys.source(evaluation_figures, envir = environment())
}
evaluation_publisher <- testthat::test_path(
  "..",
  "..",
  "validation",
  "builder",
  "publish.R"
)
if (file.exists(evaluation_publisher)) {
  Sys.setenv(BUILDER_EVAL_PUBLISH_LIBRARY_ONLY = "true")
  sys.source(evaluation_publisher, envir = environment())
  Sys.unsetenv("BUILDER_EVAL_PUBLISH_LIBRARY_ONLY")
}

builder_eval_test_run <- function(output) {
  capabilities <- builder_eval_capability_families()
  capability <- data.frame(
    trial_id = paste0("profile-minimal-", capabilities),
    fixture = "minimal",
    capability = capabilities,
    truth = FALSE,
    detected = FALSE,
    valid = TRUE,
    observed = FALSE,
    outcome = "TN",
    stringsAsFactors = FALSE
  )
  artifact <- data.frame(
    trial_id = "synthetic_minimal_crb_r1",
    fixture = "minimal",
    dataset = "minimal",
    capability = capabilities,
    selected = FALSE,
    observed = FALSE,
    outcome = "TN",
    artifact_hash = "artifact-hash",
    source_identity_hash = "source-hash",
    stringsAsFactors = FALSE
  )
  plan <- data.frame(
    trial_id = "synthetic_minimal_crb_r1",
    boundary = c("confirmation", "worker_result", "build_report"),
    plan_digest = "0123456789abcdef0123456789abcdef",
    matches_confirmation = TRUE,
    mutation_not_propagated = TRUE,
    stringsAsFactors = FALSE
  )
  report_source <- tempfile(fileext = ".json")
  writeLines('{"schema_version":2}', report_source)
  build <- data.frame(
    trial_id = "synthetic_minimal_crb_r1",
    dataset_id = "minimal",
    output_type = "CRB",
    backend = "embedded",
    success = TRUE,
    failure_stage = "",
    error = "",
    elapsed_seconds = 1,
    process_elapsed_seconds = 2,
    input_bytes = 100,
    output_bytes = 200,
    artifact_hash = "artifact-hash",
    report_identity = "report-hash",
    report_path = report_source,
    profile = "smoke",
    cell_id = "synthetic_minimal_crb",
    source_kind = "synthetic",
    fixture = "minimal",
    repetition = 1L,
    measured = TRUE,
    available = TRUE,
    stringsAsFactors = FALSE
  )
  fault <- data.frame(
    trial_id = "fault-handled",
    scenario = "pre_rename_verification_failure",
    fault_class = "handled_error",
    injection_point = "verification",
    error = "injected",
    prior_release_hash = "prior-release",
    prior_crb_hash = "prior-crb",
    recovered_release_hash = "prior-release",
    published_crb_hash = "published-crb",
    old_preserved = TRUE,
    old_crb_reopenable = TRUE,
    recovery_invoked = FALSE,
    recovery_success = TRUE,
    subsequent_publish_success = TRUE,
    subsequent_crb_reopenable = TRUE,
    partial_target_count = 0L,
    stage_residue_count = 0L,
    lock_residue_count = 0L,
    backup_residue_count = 0L,
    retired_backup_residue_count = 0L,
    journal_state = "complete",
    stringsAsFactors = FALSE
  )
  incremental <- data.frame(
    trial_id = "project-managed-incremental-1",
    dataset = c("project_a", "project_b", "project_c"),
    eligible = c(TRUE, FALSE, TRUE),
    expected_reused = c(TRUE, FALSE, TRUE),
    expected_rebuilt = c(FALSE, TRUE, FALSE),
    actual_reused = c(TRUE, FALSE, TRUE),
    actual_rebuilt = c(FALSE, TRUE, FALSE),
    source_hash = c("a", NA, "c"),
    output_hash = c("a", "b2", "c"),
    hash_preserved = c(TRUE, NA, TRUE),
    scope_correct = TRUE,
    baseline_hash = c("a", "b1", "c"),
    configuration_before = c("ca", "cb1", "cc"),
    configuration_after = c("ca", "cb2", "cc"),
    configuration_changed = c(FALSE, TRUE, FALSE),
    project_managed = TRUE,
    real_builder_hooks = TRUE,
    full_elapsed_seconds = 3,
    incremental_elapsed_seconds = 1,
    time_saved_ratio = 2 / 3,
    stringsAsFactors = FALSE
  )
  tables <- list(
    capability_detection = capability,
    plan_immutability = plan,
    artifact_fidelity = artifact,
    build_runs = build,
    fault_injection = fault,
    incremental_rebuild = incremental
  )
  schedule <- builder_eval_trial_schedule("smoke")
  protocol <- builder_eval_protocol("smoke", schedule)
  gates <- c(protocol_schema = TRUE, builder_eval_gates(tables))
  builder_eval_write_results(
    output,
    environment = list(
      schema_version = 2L,
      git_sha = paste(rep("a", 40L), collapse = ""),
      git_dirty = FALSE,
      evaluation_profile = "smoke",
      timestamp_utc = "2026-08-24 00:00:00 UTC"
    ),
    fixture_manifest = list(schema_version = 2L, fixtures = list()),
    tables = tables,
    gates = gates,
    source_manifest = list(schema_version = 1L, sources = list()),
    protocol = protocol
  )
}

test_that("evaluation confusion counts retain all four outcomes", {
  truth <- c(TRUE, FALSE, TRUE, FALSE)
  observed <- c(TRUE, TRUE, FALSE, FALSE)

  counts <- builder_eval_confusion(truth, observed)
  metrics <- builder_eval_metrics(counts)

  expect_identical(counts, c(TP = 1L, FP = 1L, FN = 1L, TN = 1L))
  expect_identical(metrics$precision, 0.5)
  expect_identical(metrics$recall, 0.5)
})

test_that("evaluation hashes canonicalize named list order", {
  left <- list(b = list(y = 2L, x = 1L), a = TRUE)
  reordered <- list(a = TRUE, b = list(x = 1L, y = 2L))
  changed <- list(a = TRUE, b = list(x = 1L, y = 3L))

  expect_identical(builder_eval_hash(left), builder_eval_hash(reordered))
  expect_false(identical(builder_eval_hash(left), builder_eval_hash(changed)))
})

test_that("evaluation metrics expose undefined denominators", {
  metrics <- builder_eval_metrics(c(TP = 0L, FP = 0L, FN = 0L, TN = 2L))

  expect_true(is.na(metrics$precision))
  expect_true(is.na(metrics$recall))
})

test_that("capability summaries retain per-family support and macro denominators", {
  rows <- data.frame(
    capability = rep(c("markers", "spatial", "hla"), each = 4L),
    outcome = c(
      "TP",
      "TP",
      "TN",
      "FP",
      "TP",
      "FN",
      "TN",
      "TN",
      "TN",
      "TN",
      "TN",
      "TN"
    ),
    stringsAsFactors = FALSE
  )

  by_capability <- builder_eval_capability_summary(rows)
  totals <- builder_eval_capability_totals(rows)

  expect_named(
    by_capability,
    c(
      "capability",
      "TP",
      "FP",
      "FN",
      "TN",
      "precision",
      "recall",
      "positive_support",
      "negative_support"
    )
  )
  expect_identical(
    by_capability[
      by_capability$capability == "markers",
      c("TP", "FP", "FN", "TN")
    ],
    data.frame(TP = 2L, FP = 1L, FN = 0L, TN = 1L)
  )
  expect_true(is.na(by_capability$recall[by_capability$capability == "hla"]))
  expect_identical(totals$scope, c("micro", "macro"))
  expect_identical(totals$precision_denominator, c(4L, 2L))
  expect_identical(totals$recall_denominator, c(4L, 2L))
  expect_equal(totals$precision[[1L]], 3 / 4)
  expect_equal(totals$recall[[1L]], 3 / 4)
  expect_equal(totals$precision[[2L]], mean(c(2 / 3, 1)))
  expect_equal(totals$recall[[2L]], mean(c(1, 1 / 2)))
})

test_that("build summaries use measured process repetitions", {
  rows <- data.frame(
    fixture = rep("pbmc", 6L),
    output_type = rep("CRB", 6L),
    backend = rep("embedded", 6L),
    repetition = 0:5,
    measured = c(FALSE, rep(TRUE, 5L)),
    success = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    elapsed_seconds = c(99, 1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  summary <- builder_eval_build_summary(rows)

  expect_identical(summary$n, 5L)
  expect_identical(summary$successes, 4L)
  expect_identical(summary$failures, 1L)
  expect_identical(summary$median_seconds, 3)
  expect_identical(summary$iqr_seconds, 2)
  expect_identical(summary$min_seconds, 1)
  expect_identical(summary$max_seconds, 5)
})

test_that("scientific protocol rejects empty, duplicate, and incomplete trials", {
  protocol <- list(
    tables = list(
      capability_detection = list(
        required_columns = c("trial_id", "capability", "outcome"),
        key = c("trial_id", "capability"),
        non_missing = c("trial_id", "capability", "outcome"),
        levels = list(outcome = c("TP", "FP", "FN", "TN"))
      ),
      build_runs = list(
        required_columns = c(
          "trial_id",
          "fixture",
          "backend",
          "repetition",
          "measured",
          "success"
        ),
        key = "trial_id",
        non_missing = c("trial_id", "fixture", "backend", "repetition"),
        repetitions = list(
          group_by = c("fixture", "backend"),
          repetition_column = "repetition",
          expected = 1:2,
          where = list(measured = TRUE)
        )
      )
    )
  )
  tables <- list(
    capability_detection = data.frame(
      trial_id = c("profile-a", "profile-a"),
      capability = c("markers", "spatial"),
      outcome = c("TP", "TN"),
      stringsAsFactors = FALSE
    ),
    build_runs = data.frame(
      trial_id = c("build-a-1", "build-a-2"),
      fixture = c("a", "a"),
      backend = c("embedded", "embedded"),
      repetition = 1:2,
      measured = TRUE,
      success = TRUE,
      stringsAsFactors = FALSE
    )
  )

  expect_true(builder_eval_validate_tables(tables, protocol))
  empty <- tables
  empty$capability_detection <- empty$capability_detection[FALSE, ]
  expect_error(builder_eval_validate_tables(empty, protocol), "non-empty")
  duplicated <- tables
  duplicated$build_runs$trial_id[[2L]] <- "build-a-1"
  expect_error(builder_eval_validate_tables(duplicated, protocol), "unique key")
  incomplete <- tables
  incomplete$build_runs <- incomplete$build_runs[1L, ]
  expect_error(
    builder_eval_validate_tables(incomplete, protocol),
    "repetitions"
  )
  invalid_level <- tables
  invalid_level$capability_detection$outcome[[1L]] <- "UNKNOWN"
  expect_error(
    builder_eval_validate_tables(invalid_level, protocol),
    "allowed levels"
  )
})

test_that("evaluation fixtures carry independent complete truth", {
  fixtures <- builder_eval_fixtures()
  families <- builder_eval_capability_families()

  expect_false(anyDuplicated(names(fixtures)) > 0L)
  expect_setequal(
    names(fixtures),
    c(
      "minimal",
      "multi_assay",
      "spatial",
      "spatial_b",
      "immune_hla",
      "immune_hla_b",
      "trajectory",
      "extra_content",
      "table_content_b",
      "malformed"
    )
  )
  for (fixture in fixtures) {
    expect_named(fixture$truth, families, ignore.order = FALSE)
    expect_type(fixture$truth, "logical")
    expect_false(anyNA(fixture$truth))
    expect_true(is.function(fixture$make))
    expect_identical(
      builder_eval_hash(fixture$make()),
      builder_eval_hash(fixture$make()),
      info = fixture$id
    )
  }
  expect_false(any(fixtures$minimal$truth))
})

test_that("every scored capability has balanced declared support", {
  support <- builder_eval_fixture_support(builder_eval_fixtures())

  expect_identical(support$capability, builder_eval_capability_families())
  expect_true(all(support$positive_support >= 2L))
  expect_true(all(support$negative_support >= 2L))
})

test_that("malformed challenges fail closed for their declared family", {
  builder_profile_source_runtime(environment())
  challenges <- builder_eval_challenges()

  expect_setequal(
    names(challenges),
    c(
      "invalid_marker",
      "invalid_trajectory",
      "invalid_extra_material",
      "invalid_spatial",
      "invalid_trekker",
      "invalid_immune_repertoire",
      "invalid_hla"
    )
  )
  expect_setequal(
    unique(vapply(challenges, `[[`, "", "expected_state")),
    "invalid"
  )
  for (challenge in challenges) {
    profile <- builder_dataset_profile(
      challenge$make(),
      list(type = "evaluation", location = challenge$id)
    )
    fact <- profile$content[[challenge$capability]]
    expect_true(fact$detected, info = challenge$id)
    expect_false(fact$valid, info = challenge$id)
  }
})

test_that("public Builder sources have complete citable acquisition records", {
  sources <- builder_eval_public_sources()

  expect_setequal(
    names(sources),
    c("pbmc_expression", "visium_brain", "pbmc_vdj")
  )
  expect_true(all(vapply(sources, builder_eval_source_complete, logical(1))))
  expect_false(anyDuplicated(vapply(sources, `[[`, "", "source_url")) > 0L)
  expect_true(all(vapply(
    sources,
    function(source) identical(source$license, "CC BY 4.0"),
    logical(1)
  )))
})

test_that("public source cache paths stay below the caller-owned cache", {
  cache <- file.path(withr::local_tempdir(), "public-cache")
  paths <- builder_eval_public_cache_paths(cache)
  root <- paste0(normalizePath(cache, winslash = "/", mustWork = TRUE), "/")
  flattened <- unlist(paths, use.names = FALSE)

  expect_setequal(names(paths), names(builder_eval_public_sources()))
  expect_true(all(startsWith(flattened, root)))
  expect_false(anyDuplicated(flattened) > 0L)
})

test_that("public source manifests record bytes, SHA-256, and object shape", {
  source <- builder_eval_public_sources()$pbmc_expression
  object <- .builder_eval_seeded(
    301L,
    function() .builder_fixture_object(12L)
  )()
  file <- withr::local_tempfile()
  writeBin(charToRaw("public-source-fixture"), file)

  manifest <- builder_eval_public_object_manifest(
    source,
    object,
    files = stats::setNames(file, "matrix_archive")
  )

  expect_identical(manifest$source_id, "pbmc_expression")
  expect_identical(manifest$cells, 12L)
  expect_identical(manifest$features, 40L)
  expect_match(manifest$object_sha256, "^[0-9a-f]{64}$")
  expect_identical(manifest$files[[1L]]$bytes, as.double(file.info(file)$size))
  expect_match(manifest$files[[1L]]$sha256, "^[0-9a-f]{64}$")
  expect_false("path" %in% names(manifest$files[[1L]]))
})

test_that("public preparation adds declared technical Builder controls", {
  counts <- Matrix::Matrix(
    matrix(
      seq_len(24L),
      nrow = 6L,
      dimnames = list(paste0("g", 1:6), paste0("c", 1:4))
    ),
    sparse = TRUE
  )
  object <- SeuratObject::CreateSeuratObject(counts)

  prepared <- .builder_eval_public_builder_controls(object)

  expect_setequal(levels(prepared$builder_evaluation_partition), c("A", "B"))
  expect_identical(
    SeuratObject::Reductions(prepared),
    "builder_evaluation"
  )
  expect_identical(
    rownames(SeuratObject::Embeddings(prepared[["builder_evaluation"]])),
    colnames(prepared)
  )
})

test_that("public 10x matrices use stable feature identifiers", {
  matrix_dir <- withr::local_tempdir()
  counts <- Matrix::Matrix(
    matrix(
      seq_len(6L),
      nrow = 2L,
      dimnames = list(NULL, NULL)
    ),
    sparse = TRUE
  )
  Matrix::writeMM(counts, file.path(matrix_dir, "matrix.mtx"))
  utils::write.table(
    data.frame(
      id = c("ENSG000001", "ENSG000002"),
      symbol = c("GeneA", "genea")
    ),
    file.path(matrix_dir, "genes.tsv"),
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE
  )
  utils::write.table(
    paste0("cell", seq_len(3L)),
    file.path(matrix_dir, "barcodes.tsv"),
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )

  prepared <- .builder_eval_public_counts(matrix_dir)

  expect_identical(rownames(prepared), c("ENSG000001", "ENSG000002"))
  expect_identical(colnames(prepared), paste0("cell", seq_len(3L)))
})

test_that("an independent worker exposes confirmation, result, and report identity", {
  builder_e2e_source_runtime(environment())
  fixture <- builder_eval_fixtures()$minimal
  object <- fixture$make()
  record <- list(
    id = "independent-minimal",
    label = "Independent minimal",
    object = object
  )

  trial <- builder_eval_build_trial(
    records = list(record),
    trial_dir = file.path(withr::local_tempdir(), "trial"),
    trial_id = "independent-minimal-crb-1",
    output_type = "CRB",
    backend = "embedded"
  )

  expect_true(trial$builds$success, info = trial$builds$error)
  expect_identical(
    trial$plans$boundary,
    c("confirmation", "worker_result", "build_report")
  )
  expect_identical(length(unique(trial$plans$plan_digest)), 1L)
  expect_true(all(trial$plans$matches_confirmation))
  expect_true(all(trial$plans$mutation_not_propagated))
  expect_true(file.exists(trial$builds$report_path))
  expect_false(any(trial$artifacts$outcome %in% c("FP", "FN")))
})

test_that("publication schedules preregister warm-ups and five processes", {
  publication <- builder_eval_trial_schedule(
    "publication",
    available_backends = c("embedded", "h5", "bpcells")
  )
  standard <- builder_eval_trial_schedule("standard")
  smoke <- builder_eval_trial_schedule("smoke")

  expect_false(anyDuplicated(publication$trial_id) > 0L)
  expect_setequal(
    unique(publication$dataset_id[publication$source_kind == "public"]),
    c("pbmc_expression", "visium_brain", "pbmc_vdj")
  )
  expect_setequal(
    unique(publication$output_type),
    c("CRB", "public_app", "login_app")
  )
  expect_setequal(
    unique(publication$backend[publication$dataset_id == "pbmc_expression"]),
    c("embedded", "h5", "bpcells")
  )
  for (cell in unique(publication$cell_id)) {
    rows <- publication[publication$cell_id == cell, ]
    expect_identical(sort(rows$repetition), 0:5, info = cell)
    expect_identical(rows$repetition[!rows$measured], 0L, info = cell)
    expect_identical(sort(rows$repetition[rows$measured]), 1:5, info = cell)
  }
  expect_true(nrow(standard) > nrow(smoke))
  expect_true(all(standard$measured))
  expect_true(all(smoke$measured))
})

test_that("malformed optional content fails closed", {
  builder_profile_source_runtime(environment())
  fixture <- builder_eval_fixtures()$malformed
  profile <- builder_dataset_profile(
    fixture$make(),
    list(type = "evaluation", location = fixture$id)
  )
  observed <- builder_eval_observe_capabilities(profile)

  expect_true(profile$content$marker_genes$detected)
  expect_false(profile$content$marker_genes$valid)
  expect_false(any(observed))
})

test_that("fixture profiles match the independent capability truth", {
  builder_profile_source_runtime(environment())
  for (fixture in builder_eval_fixtures()) {
    profile <- builder_dataset_profile(
      fixture$make(),
      list(type = "evaluation", location = fixture$id)
    )
    expect_identical(
      builder_eval_observe_capabilities(profile),
      fixture$truth,
      info = fixture$id
    )
  }
})

test_that("capability evidence retains one scored row per fixture family", {
  builder_profile_source_runtime(environment())
  fixtures <- builder_eval_fixtures()
  rows <- builder_eval_capability_rows(fixtures)

  expect_named(
    rows,
    c(
      "trial_id",
      "fixture",
      "capability",
      "truth",
      "detected",
      "valid",
      "observed",
      "outcome"
    )
  )
  expect_identical(
    nrow(rows),
    length(fixtures) * length(builder_eval_capability_families())
  )
  expect_setequal(unique(rows$outcome), c("TP", "TN"))
})

test_that("plan evidence keeps one identity across frozen boundaries", {
  builder_repo_source(file.path("core", "plan_identity.R"))
  builder_repo_source("review.R")
  builder_repo_source("workflow.R")
  root <- withr::local_tempdir()
  plan <- structure(
    list(
      readiness = "ready",
      review_identity = list(
        schema_version = 1L,
        dataset_order = "dataset-a",
        checks = c(`dataset-a` = "configuration-a:revision:1")
      ),
      dataset_order = "dataset-a",
      items = list(list(id = "dataset-a", filename = "dataset-a.crb")),
      manifest = list(),
      acknowledgements = character(),
      out_dir = root,
      overwrite = FALSE,
      targets = file.path(root, "dataset-a.crb"),
      make_app = FALSE,
      app_contract_version = NULL,
      app_options = list(),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )

  rows <- builder_eval_plan_rows(plan, run_root = root)

  expect_identical(rows$boundary, c("confirmation", "worker", "post_build"))
  expect_identical(length(unique(rows$plan_hash)), 1L)
  expect_true(all(rows$matches_confirmation))
})

test_that("artifact fidelity scores selected and unselected content", {
  item <- list(
    id = "dataset-a",
    manifest = list(spatial = list(disposition = "preserved"))
  )
  artifact <- list(spatial = list(section_a = list()), marker_genes = list())
  rows <- builder_eval_artifact_rows(
    item,
    artifact,
    reader = function(object, field) object[[field]]
  )

  expect_identical(nrow(rows), length(builder_eval_capability_families()))
  expect_identical(rows$outcome[rows$capability == "spatial"], "TP")
  expect_identical(rows$outcome[rows$capability == "marker_genes"], "TN")
  expect_false(any(rows$outcome %in% c("FP", "FN")))
})

test_that("artifact fidelity inspects a real staged CRB", {
  skip_if_not_installed("SeuratObject")
  builder_e2e_source_runtime(environment())
  root <- withr::local_tempdir()
  object <- .builder_fixture_spatial()
  record <- list(
    id = "evaluation-spatial",
    label = "Evaluation spatial",
    make = function() list(object = object, format = "Evaluation fixture")
  )
  entry <- builder_e2e_entry(record)
  snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  entry$snapshot <- snapshot
  plan <- builder_freeze_plan(
    list(entry),
    file.path(root, "release"),
    make_app = FALSE
  )
  stage <- file.path(root, "stage")
  dir.create(stage)
  result <- builder_execute_plan(
    plan,
    stage,
    list(`evaluation-spatial` = snapshot)
  )

  expect_identical(result$state, "success", info = result$error)
  rows <- builder_eval_artifact_rows(
    plan$items[[1L]],
    readRDS(result$built[[1L]])
  )
  expect_identical(rows$outcome[rows$capability == "spatial"], "TP")
  mismatches <- rows[rows$outcome %in% c("FP", "FN"), ]
  expect_false(
    nrow(mismatches) > 0L,
    info = paste(mismatches$capability, mismatches$outcome, collapse = ", ")
  )
})

test_that("handled publication failures preserve the prior release", {
  builder_e2e_source_runtime(environment())
  rows <- builder_eval_fault_trials()

  expect_named(
    rows,
    c(
      "trial_id",
      "scenario",
      "fault_class",
      "injection_point",
      "error",
      "prior_release_hash",
      "prior_crb_hash",
      "recovered_release_hash",
      "published_crb_hash",
      "old_preserved",
      "old_crb_reopenable",
      "recovery_invoked",
      "recovery_success",
      "subsequent_publish_success",
      "subsequent_crb_reopenable",
      "partial_target_count",
      "stage_residue_count",
      "lock_residue_count",
      "backup_residue_count",
      "retired_backup_residue_count",
      "journal_state"
    )
  )
  expect_setequal(
    rows$scenario,
    c(
      "prior_protection_failure",
      "pre_rename_verification_failure",
      "promotion_failure",
      "post_rename_verification_failure",
      "process_exit_before_old_move",
      "process_exit_after_old_move",
      "process_exit_after_old_moved_phase",
      "process_exit_after_new_move",
      "process_exit_after_new_published_phase"
    )
  )
  expect_setequal(rows$fault_class, c("handled_error", "process_exit"))
  expect_true(all(nzchar(rows$error)))
  expect_true(all(rows$old_preserved))
  expect_true(all(rows$old_crb_reopenable))
  expect_true(all(rows$recovery_success))
  expect_true(all(rows$subsequent_publish_success))
  expect_true(all(rows$subsequent_crb_reopenable))
  expect_true(all(rows$partial_target_count == 0L))
  expect_true(all(rows$stage_residue_count == 0L))
  expect_true(all(rows$lock_residue_count == 0L))
  expect_true(all(rows$backup_residue_count == 0L))
  expect_true(all(rows$retired_backup_residue_count == 0L))
  expect_true(all(rows$journal_state == "complete"))
})

test_that("incremental evidence records exact reuse and rebuild scope", {
  builder_e2e_source_runtime(environment())
  rows <- builder_eval_project_reuse_trial()

  expect_setequal(rows$dataset[rows$actual_reused], c("project_a", "project_c"))
  expect_identical(rows$dataset[rows$actual_rebuilt], "project_b")
  expect_true(all(rows$project_managed))
  expect_true(all(rows$real_builder_hooks))
  expect_true(all(rows$hash_preserved[rows$actual_reused]))
  expect_true(rows$configuration_changed[rows$dataset == "project_b"])
  expect_false(any(rows$configuration_changed[rows$dataset != "project_b"]))
  expect_true(all(rows$scope_correct))
})

test_that("result writer creates one immutable evidence directory", {
  output <- file.path(withr::local_tempdir(), "evidence")
  tables <- stats::setNames(
    rep(list(data.frame()), 6L),
    c(
      "capability_detection",
      "plan_immutability",
      "artifact_fidelity",
      "build_runs",
      "fault_injection",
      "incremental_rebuild"
    )
  )
  builder_eval_write_results(
    output,
    environment = list(git_sha = "abc123"),
    fixture_manifest = list(fixtures = list()),
    tables = tables,
    gates = c(correctness = TRUE)
  )

  expect_true(dir.exists(file.path(output, "failures")))
  expect_true(dir.exists(file.path(output, "reports")))
  expect_true(file.exists(file.path(output, "environment.json")))
  expect_true(file.exists(file.path(output, "fixture_manifest.json")))
  expect_true(file.exists(file.path(output, "source_manifest.json")))
  expect_true(file.exists(file.path(output, "protocol.json")))
  expect_true(file.exists(file.path(output, "summary.md")))
  expect_match(
    paste(readLines(file.path(output, "summary.md")), collapse = "\n"),
    "Capability detection: no trials.",
    fixed = TRUE
  )
  expect_true(all(file.exists(file.path(
    output,
    paste0(names(tables), ".csv")
  ))))
  expect_identical(
    jsonlite::read_json(file.path(output, "environment.json"))$git_sha,
    "abc123"
  )
  expect_error(
    builder_eval_write_results(
      output,
      list(),
      list(),
      tables,
      c(correctness = TRUE)
    ),
    "already exists"
  )
})

test_that("paper figure registry covers every evidence panel", {
  expect_setequal(
    builder_eval_figure_files(),
    c(
      "builder_eval_capability.png",
      "builder_eval_plan_identity.png",
      "builder_eval_artifact_fidelity.png",
      "builder_eval_build_timing.png",
      "builder_eval_recovery.png",
      "builder_eval_incremental.png",
      "builder_eval_claim_boundaries.png",
      "builder_evidence_chain.svg"
    )
  )
})

test_that("evidence validation and figures are derived from raw rows", {
  run <- file.path(withr::local_tempdir(), "run")
  builder_eval_test_run(run)

  validated <- builder_eval_validate_run(run, require_publication = FALSE)
  package_root <- file.path(withr::local_tempdir(), "package")
  dir.create(package_root)
  writeLines("Package: evaluationtest", file.path(package_root, "DESCRIPTION"))
  current <- builder_eval_publish_run(
    run,
    package_root,
    require_publication = FALSE
  )
  figures <- file.path(current, "figures", builder_eval_figure_files())

  expect_true(all(file.exists(figures)))
  expect_true(all(file.info(figures)$size > 100L))
  expect_true(file.exists(file.path(current, "validation.json")))
  expect_true(all(file.exists(file.path(
    package_root,
    "vignettes",
    "img",
    builder_eval_figure_files()
  ))))
  expect_true(all(builder_eval_gates(validated$tables)))
  expect_error(
    builder_eval_publish_run(
      run,
      package_root,
      require_publication = FALSE
    ),
    "already current"
  )
  expect_error(
    builder_eval_validate_run(run, require_publication = TRUE),
    "publication profile"
  )

  altered <- validated$tables$artifact_fidelity
  altered$outcome[[1L]] <- "FP"
  utils::write.csv(
    altered,
    file.path(run, "artifact_fidelity.csv"),
    row.names = FALSE,
    na = ""
  )
  expect_error(
    builder_eval_validate_run(run, require_publication = FALSE),
    "summary|gate",
    ignore.case = TRUE
  )
})

test_that("paper vignette states its workflow, evidence, and claim limits", {
  vignette <- testthat::test_path(
    "..",
    "..",
    "vignettes",
    "builder_publication_evaluation.Rmd"
  )
  expect_true(file.exists(vignette))
  text <- paste(readLines(vignette, warn = FALSE), collapse = "\n")
  headings <- c(
    "Scientific question and bounded claims",
    "Using Builder from import to safe publication",
    "Why frozen plans and negative checks matter",
    "Evaluation methods",
    "Capability-detection results",
    "Plan identity and artifact fidelity",
    "Build and publication results",
    "Recovery under injected, handled failures",
    "Project-managed incremental rebuilding",
    "Limitations and unsupported stronger claims",
    "Reproduction, data and code availability"
  )
  expect_true(all(vapply(headings, grepl, logical(1), x = text, fixed = TRUE)))
  expect_true(all(vapply(
    builder_eval_figure_files(),
    grepl,
    logical(1),
    x = text,
    fixed = TRUE
  )))
  expect_match(text, "VignetteIndexEntry\\{Builder publication evaluation\\}")
  expect_match(text, "inst.*extdata.*builder-evaluation.*current")
  expect_match(text, "system[.]file.*builder-evaluation")
  expect_match(
    text,
    "recovery under injected, handled failures",
    ignore.case = TRUE
  )
  expect_match(text, "SIGKILL")
  expect_match(text, "power loss", ignore.case = TRUE)
  expect_match(text, "concurrent publish", ignore.case = TRUE)
})

test_that("empty evidence cannot pass a correctness gate", {
  tables <- list(
    capability_detection = data.frame(),
    plan_immutability = data.frame(),
    artifact_fidelity = data.frame(),
    build_runs = data.frame(),
    fault_injection = data.frame(),
    incremental_rebuild = data.frame()
  )

  expect_false(any(builder_eval_gates(tables)))
})
