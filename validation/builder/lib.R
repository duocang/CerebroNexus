builder_eval_canonicalize <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  value <- lapply(value, builder_eval_canonicalize)
  value_names <- names(value)
  if (!is.null(value_names)) {
    value <- value[order(value_names, method = "radix")]
  }
  value
}

builder_eval_hash <- function(value) {
  path <- tempfile("builder-eval-", fileext = ".rds")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  saveRDS(builder_eval_canonicalize(value), path, version = 3L)
  unname(as.character(tools::md5sum(path)))
}

builder_eval_confusion <- function(truth, observed) {
  if (
    !is.logical(truth) ||
      !is.logical(observed) ||
      length(truth) != length(observed) ||
      anyNA(truth) ||
      anyNA(observed)
  ) {
    stop("Truth and observations must be equal-length logical vectors.")
  }
  c(
    TP = as.integer(sum(truth & observed)),
    FP = as.integer(sum(!truth & observed)),
    FN = as.integer(sum(truth & !observed)),
    TN = as.integer(sum(!truth & !observed))
  )
}

builder_eval_metrics <- function(counts) {
  if (!identical(names(counts), c("TP", "FP", "FN", "TN"))) {
    stop("Confusion counts must be named TP, FP, FN, and TN.")
  }
  ratio <- function(numerator, denominator) {
    if (denominator == 0) NA_real_ else numerator / denominator
  }
  list(
    precision = ratio(counts[["TP"]], counts[["TP"]] + counts[["FP"]]),
    recall = ratio(counts[["TP"]], counts[["TP"]] + counts[["FN"]])
  )
}

builder_eval_capability_summary <- function(rows) {
  required <- c("capability", "outcome")
  if (
    !is.data.frame(rows) ||
      !nrow(rows) ||
      !all(required %in% names(rows)) ||
      anyNA(rows$capability) ||
      any(!nzchar(rows$capability)) ||
      anyNA(rows$outcome) ||
      any(!rows$outcome %in% c("TP", "FP", "FN", "TN"))
  ) {
    stop("Capability evidence must contain scored non-empty rows.")
  }
  capabilities <- unique(as.character(rows$capability))
  summaries <- lapply(capabilities, function(capability) {
    counts <- as.integer(base::table(factor(
      rows$outcome[rows$capability == capability],
      levels = c("TP", "FP", "FN", "TN")
    )))
    names(counts) <- c("TP", "FP", "FN", "TN")
    metrics <- builder_eval_metrics(counts)
    data.frame(
      capability = capability,
      TP = counts[["TP"]],
      FP = counts[["FP"]],
      FN = counts[["FN"]],
      TN = counts[["TN"]],
      precision = metrics$precision,
      recall = metrics$recall,
      positive_support = counts[["TP"]] + counts[["FN"]],
      negative_support = counts[["FP"]] + counts[["TN"]],
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, summaries)
  rownames(summary) <- NULL
  summary
}

builder_eval_capability_totals <- function(rows) {
  summary <- builder_eval_capability_summary(rows)
  counts <- colSums(summary[c("TP", "FP", "FN", "TN")])
  precision_defined <- !is.na(summary$precision)
  recall_defined <- !is.na(summary$recall)
  data.frame(
    scope = c("micro", "macro"),
    TP = c(as.integer(counts[["TP"]]), NA_integer_),
    FP = c(as.integer(counts[["FP"]]), NA_integer_),
    FN = c(as.integer(counts[["FN"]]), NA_integer_),
    TN = c(as.integer(counts[["TN"]]), NA_integer_),
    precision = c(
      .builder_eval_rate(counts[["TP"]], counts[["TP"]] + counts[["FP"]]),
      if (any(precision_defined)) {
        mean(summary$precision[precision_defined])
      } else {
        NA_real_
      }
    ),
    recall = c(
      .builder_eval_rate(counts[["TP"]], counts[["TP"]] + counts[["FN"]]),
      if (any(recall_defined)) {
        mean(summary$recall[recall_defined])
      } else {
        NA_real_
      }
    ),
    precision_denominator = c(
      as.integer(counts[["TP"]] + counts[["FP"]]),
      as.integer(sum(precision_defined))
    ),
    recall_denominator = c(
      as.integer(counts[["TP"]] + counts[["FN"]]),
      as.integer(sum(recall_defined))
    ),
    positive_support = c(
      as.integer(sum(summary$positive_support)),
      as.integer(sum(summary$positive_support > 0L))
    ),
    negative_support = c(
      as.integer(sum(summary$negative_support)),
      as.integer(sum(summary$negative_support > 0L))
    ),
    stringsAsFactors = FALSE
  )
}

builder_eval_build_summary <- function(
  rows,
  group_by = c("fixture", "output_type", "backend")
) {
  required <- c("success", "elapsed_seconds")
  if (
    !is.data.frame(rows) ||
      !nrow(rows) ||
      !all(required %in% names(rows)) ||
      !is.logical(rows$success) ||
      anyNA(rows$success) ||
      !is.numeric(rows$elapsed_seconds) ||
      anyNA(rows$elapsed_seconds) ||
      any(rows$elapsed_seconds < 0)
  ) {
    stop("Build evidence must contain valid non-empty process rows.")
  }
  if ("measured" %in% names(rows)) {
    if (!is.logical(rows$measured) || anyNA(rows$measured)) {
      stop("Build measurement flags are invalid.")
    }
    rows <- rows[rows$measured, , drop = FALSE]
  }
  if (!nrow(rows)) {
    stop("Build evidence has no measured process rows.")
  }
  group_by <- intersect(group_by, names(rows))
  group_key <- if (length(group_by)) {
    do.call(paste, c(rows[group_by], sep = "\r"))
  } else {
    rep("all", nrow(rows))
  }
  groups <- split(seq_len(nrow(rows)), group_key)
  summaries <- lapply(groups, function(index) {
    elapsed <- rows$elapsed_seconds[index]
    values <- if (length(group_by)) {
      rows[index[[1L]], group_by, drop = FALSE]
    } else {
      data.frame()
    }
    cbind(
      values,
      data.frame(
        n = as.integer(length(index)),
        successes = as.integer(sum(rows$success[index])),
        failures = as.integer(sum(!rows$success[index])),
        success_rate = mean(rows$success[index]),
        median_seconds = stats::median(elapsed),
        iqr_seconds = stats::IQR(elapsed),
        min_seconds = min(elapsed),
        max_seconds = max(elapsed),
        stringsAsFactors = FALSE
      )
    )
  })
  summary <- do.call(rbind, summaries)
  rownames(summary) <- NULL
  summary
}

.builder_eval_protocol_key <- function(table, columns) {
  do.call(paste, c(lapply(table[columns], as.character), sep = "\r"))
}

.builder_eval_protocol_where <- function(table, where) {
  keep <- rep(TRUE, nrow(table))
  for (column in names(where)) {
    if (!column %in% names(table)) {
      stop("A protocol filter column is missing: ", column, ".")
    }
    keep <- keep & table[[column]] %in% where[[column]]
  }
  table[keep, , drop = FALSE]
}

.builder_eval_validate_repetitions <- function(table, repetitions, name) {
  required <- c(
    repetitions$group_by,
    repetitions$repetition_column,
    names(repetitions$where)
  )
  if (
    !is.character(repetitions$group_by) ||
      !is.character(repetitions$repetition_column) ||
      length(repetitions$repetition_column) != 1L ||
      !length(repetitions$expected) ||
      !all(required %in% names(table))
  ) {
    stop("The repetition protocol for `", name, "` is invalid.")
  }
  measured <- .builder_eval_protocol_where(
    table,
    repetitions$where %||% list()
  )
  if (!nrow(measured)) {
    stop("The `", name, "` table has no protocol repetitions.")
  }
  group_key <- if (length(repetitions$group_by)) {
    .builder_eval_protocol_key(measured, repetitions$group_by)
  } else {
    rep("all", nrow(measured))
  }
  groups <- split(seq_len(nrow(measured)), group_key)
  expected <- sort(as.character(repetitions$expected), method = "radix")
  valid <- vapply(
    groups,
    function(index) {
      observed <- sort(
        as.character(measured[[repetitions$repetition_column]][index]),
        method = "radix"
      )
      identical(observed, expected)
    },
    logical(1)
  )
  if (!all(valid)) {
    stop("The `", name, "` table has incomplete protocol repetitions.")
  }
  invisible(TRUE)
}

builder_eval_validate_tables <- function(tables, protocol) {
  if (
    !is.list(tables) ||
      !is.list(protocol) ||
      !is.list(protocol$tables) ||
      !length(protocol$tables) ||
      is.null(names(protocol$tables)) ||
      any(!nzchar(names(protocol$tables)))
  ) {
    stop("A named scientific evidence protocol is required.")
  }
  missing_tables <- setdiff(names(protocol$tables), names(tables))
  if (length(missing_tables)) {
    stop(
      "Evidence tables are missing: ",
      paste(missing_tables, collapse = ", "),
      "."
    )
  }
  for (name in names(protocol$tables)) {
    table <- tables[[name]]
    spec <- protocol$tables[[name]]
    if (!is.data.frame(table) || !nrow(table)) {
      stop("The `", name, "` evidence table must be non-empty.")
    }
    required <- unique(c(
      spec$required_columns %||% character(),
      spec$key %||% character(),
      spec$non_missing %||% character(),
      names(spec$levels %||% list())
    ))
    missing_columns <- setdiff(required, names(table))
    if (length(missing_columns)) {
      stop(
        "The `",
        name,
        "` evidence table is incomplete; missing: ",
        paste(missing_columns, collapse = ", "),
        "."
      )
    }
    for (column in spec$non_missing %||% character()) {
      value <- table[[column]]
      missing <- is.na(value)
      if (is.character(value)) {
        missing <- missing | !nzchar(value)
      }
      if (any(missing)) {
        stop("The `", name, "` table has missing `", column, "` values.")
      }
    }
    key <- spec$key %||% character()
    if (length(key) && anyDuplicated(.builder_eval_protocol_key(table, key))) {
      stop("The `", name, "` evidence table violates its unique key.")
    }
    for (column in names(spec$levels %||% list())) {
      if (any(!table[[column]] %in% spec$levels[[column]])) {
        stop(
          "The `",
          name,
          "` table contains values outside the allowed levels for `",
          column,
          "`."
        )
      }
    }
    if (!is.null(spec$repetitions)) {
      .builder_eval_validate_repetitions(table, spec$repetitions, name)
    }
  }
  TRUE
}

builder_eval_protocol <- function(profile, schedule) {
  if (
    !profile %in% c("smoke", "standard", "publication") ||
      !is.data.frame(schedule) ||
      !nrow(schedule) ||
      !all(c("cell_id", "repetition", "measured") %in% names(schedule))
  ) {
    stop("A preregistered Builder evaluation schedule is required.")
  }
  repetitions <- list(
    group_by = "cell_id",
    repetition_column = "repetition",
    expected = if (identical(profile, "publication")) 1:5 else 1L,
    where = list(measured = TRUE)
  )
  list(
    schema_version = 2L,
    profile = profile,
    experimental_unit = "independent R process",
    tables = list(
      capability_detection = list(
        required_columns = c(
          "trial_id",
          "fixture",
          "capability",
          "truth",
          "observed",
          "outcome"
        ),
        key = c("trial_id", "capability"),
        non_missing = c("trial_id", "fixture", "capability", "outcome"),
        levels = list(outcome = c("TP", "FP", "FN", "TN"))
      ),
      plan_immutability = list(
        required_columns = c(
          "trial_id",
          "boundary",
          "plan_digest",
          "matches_confirmation",
          "mutation_not_propagated"
        ),
        key = c("trial_id", "boundary"),
        non_missing = c("trial_id", "boundary", "plan_digest"),
        levels = list(
          boundary = c("confirmation", "worker_result", "build_report")
        )
      ),
      artifact_fidelity = list(
        required_columns = c(
          "trial_id",
          "dataset",
          "capability",
          "selected",
          "observed",
          "outcome"
        ),
        key = c("trial_id", "dataset", "capability"),
        non_missing = c("trial_id", "dataset", "capability", "outcome"),
        levels = list(outcome = c("TP", "FP", "FN", "TN"))
      ),
      build_runs = list(
        required_columns = c(
          "trial_id",
          "cell_id",
          "output_type",
          "backend",
          "repetition",
          "measured",
          "success",
          "elapsed_seconds"
        ),
        key = "trial_id",
        non_missing = c(
          "trial_id",
          "cell_id",
          "output_type",
          "backend",
          "repetition",
          "measured",
          "success"
        ),
        levels = list(
          output_type = c("CRB", "public_app", "login_app"),
          backend = c("embedded", "h5", "bpcells")
        ),
        repetitions = repetitions
      ),
      fault_injection = list(
        required_columns = c(
          "trial_id",
          "scenario",
          "fault_class",
          "old_preserved",
          "old_crb_reopenable",
          "recovery_success",
          "subsequent_publish_success",
          "subsequent_crb_reopenable",
          "partial_target_count",
          "stage_residue_count",
          "lock_residue_count",
          "backup_residue_count",
          "retired_backup_residue_count",
          "journal_state"
        ),
        key = "trial_id",
        non_missing = c("trial_id", "scenario", "fault_class"),
        levels = list(fault_class = c("handled_error", "process_exit"))
      ),
      incremental_rebuild = list(
        required_columns = c(
          "trial_id",
          "dataset",
          "expected_reused",
          "expected_rebuilt",
          "project_managed",
          "real_builder_hooks",
          "configuration_changed",
          "hash_preserved",
          "scope_correct"
        ),
        key = c("trial_id", "dataset"),
        non_missing = c("trial_id", "dataset")
      )
    )
  )
}

builder_eval_observe_capabilities <- function(profile) {
  families <- builder_eval_capability_families()
  stats::setNames(
    vapply(
      families,
      function(family) {
        fact <- profile$content[[family]]
        is.list(fact) && isTRUE(fact$detected) && isTRUE(fact$valid)
      },
      logical(1)
    ),
    families
  )
}

builder_eval_capability_rows <- function(fixtures, profiler = NULL) {
  if (is.null(profiler)) {
    profiler <- get(
      "builder_dataset_profile",
      envir = parent.frame(),
      inherits = TRUE
    )
  }
  families <- builder_eval_capability_families()
  rows <- lapply(fixtures, function(fixture) {
    profile <- profiler(
      fixture$make(),
      list(type = "evaluation", location = fixture$id)
    )
    detected <- stats::setNames(
      vapply(
        families,
        function(family) isTRUE(profile$content[[family]]$detected),
        logical(1)
      ),
      families
    )
    valid <- stats::setNames(
      vapply(
        families,
        function(family) isTRUE(profile$content[[family]]$valid),
        logical(1)
      ),
      families
    )
    observed <- detected & valid
    truth <- fixture$truth
    outcome <- ifelse(
      truth & observed,
      "TP",
      ifelse(!truth & observed, "FP", ifelse(truth, "FN", "TN"))
    )
    data.frame(
      trial_id = rep(paste0("profile-", fixture$id), length(families)),
      fixture = rep(fixture$id, length(families)),
      capability = families,
      truth = unname(truth),
      detected = unname(detected),
      valid = unname(valid),
      observed = unname(observed),
      outcome = unname(outcome),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL
  rows
}

.builder_eval_replace_root <- function(value, root) {
  if (is.list(value)) {
    return(lapply(value, .builder_eval_replace_root, root = root))
  }
  if (!is.character(value) || !length(value)) {
    return(value)
  }
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  normalized <- gsub("\\\\", "/", value)
  inside <- !is.na(normalized) &
    (normalized == root | startsWith(normalized, paste0(root, "/")))
  normalized[inside] <- paste0(
    "<run-root>",
    substring(normalized[inside], nchar(root) + 1L)
  )
  normalized
}

builder_eval_plan_rows <- function(plan, run_root, identity = NULL) {
  if (is.null(identity)) {
    identity <- get(
      "builder_final_build_identity",
      envir = parent.frame(),
      inherits = TRUE
    )
  }
  copies <- list(
    confirmation = plan,
    worker = unserialize(serialize(plan, NULL, version = 3L)),
    post_build = unserialize(serialize(plan, NULL, version = 3L))
  )
  hashes <- vapply(
    copies,
    function(copy) {
      builder_eval_hash(.builder_eval_replace_root(identity(copy), run_root))
    },
    character(1)
  )
  data.frame(
    boundary = names(copies),
    plan_hash = unname(hashes),
    matches_confirmation = unname(hashes == hashes[[1L]]),
    stringsAsFactors = FALSE
  )
}

.builder_eval_artifact_fields <- c(
  marker_genes = "marker_genes",
  most_expressed_genes = "most_expressed_genes",
  mean_expression = "mean_expression",
  enriched_pathways = "enriched_pathways",
  trajectory = "trajectories",
  extra_material = "extra_material",
  immune_repertoire = "immune_repertoire",
  hla = "hla_typing",
  spatial = "spatial",
  trekker = "trekker"
)

builder_eval_artifact_rows <- function(item, artifact, reader = NULL) {
  if (is.null(reader)) {
    reader <- get(
      ".builder_build_field",
      envir = parent.frame(),
      inherits = TRUE
    )
  }
  families <- builder_eval_capability_families()
  manifest <- if (is.null(item$manifest)) list() else item$manifest
  selected <- vapply(
    families,
    function(family) {
      entry <- manifest[[family]]
      is.list(entry) &&
        is.character(entry$disposition) &&
        length(entry$disposition) == 1L &&
        entry$disposition %in% c("preserved", "converted", "attached")
    },
    logical(1)
  )
  observed <- vapply(
    families,
    function(family) {
      value <- reader(artifact, unname(.builder_eval_artifact_fields[[family]]))
      !is.null(value) && length(value) > 0L
    },
    logical(1)
  )
  outcome <- ifelse(
    selected & observed,
    "TP",
    ifelse(!selected & observed, "FP", ifelse(selected, "FN", "TN"))
  )
  data.frame(
    dataset = rep(item$id, length(families)),
    capability = families,
    selected = unname(selected),
    observed = unname(observed),
    outcome = unname(outcome),
    stringsAsFactors = FALSE
  )
}

builder_eval_reuse_rows <- function(
  plan,
  result,
  expected_rebuilt,
  trial_id = "incremental-rebuild-1"
) {
  rows <- lapply(plan$items, function(item) {
    reused <- item$reused_artifact
    eligible <- is.list(reused)
    output <- unname(result$built[item$name])
    output_exists <- length(output) == 1L && file.exists(output)
    source_hash <- if (eligible && file.exists(reused$path)) {
      unname(as.character(tools::md5sum(reused$path)))
    } else {
      NA_character_
    }
    output_hash <- if (output_exists) {
      unname(as.character(tools::md5sum(output)))
    } else {
      NA_character_
    }
    actual_reused <- eligible &&
      output_exists &&
      identical(source_hash, output_hash)
    actual_rebuilt <- !eligible && output_exists
    should_rebuild <- item$id %in% expected_rebuilt
    data.frame(
      trial_id = trial_id,
      dataset = item$id,
      eligible = eligible,
      expected_reused = !should_rebuild,
      expected_rebuilt = should_rebuild,
      actual_reused = actual_reused,
      actual_rebuilt = actual_rebuilt,
      source_hash = source_hash,
      output_hash = output_hash,
      hash_preserved = if (eligible) actual_reused else NA,
      scope_correct = identical(actual_reused, !should_rebuild) &&
        identical(actual_rebuilt, should_rebuild),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL
  rows
}

builder_eval_gates <- function(tables) {
  has_rows <- function(table, columns) {
    is.data.frame(table) && nrow(table) > 0L && all(columns %in% names(table))
  }
  c(
    capability_detection = has_rows(
      tables$capability_detection,
      "outcome"
    ) &&
      !any(tables$capability_detection$outcome %in% c("FP", "FN")),
    plan_immutability = has_rows(
      tables$plan_immutability,
      c("matches_confirmation", "mutation_not_propagated")
    ) &&
      all(
        tables$plan_immutability$matches_confirmation &
          tables$plan_immutability$mutation_not_propagated
      ),
    artifact_fidelity = has_rows(tables$artifact_fidelity, "outcome") &&
      !any(tables$artifact_fidelity$outcome %in% c("FP", "FN")),
    build_success = has_rows(tables$build_runs, "success") &&
      all(tables$build_runs$success),
    handled_recovery = has_rows(
      tables$fault_injection,
      c(
        "old_preserved",
        "old_crb_reopenable",
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
    ) &&
      all(
        tables$fault_injection$old_preserved &
          tables$fault_injection$old_crb_reopenable &
          tables$fault_injection$recovery_success &
          tables$fault_injection$subsequent_publish_success &
          tables$fault_injection$subsequent_crb_reopenable &
          tables$fault_injection$partial_target_count == 0L &
          tables$fault_injection$stage_residue_count == 0L &
          tables$fault_injection$lock_residue_count == 0L &
          tables$fault_injection$backup_residue_count == 0L &
          tables$fault_injection$retired_backup_residue_count == 0L &
          tables$fault_injection$journal_state == "complete"
      ),
    incremental_scope = has_rows(
      tables$incremental_rebuild,
      c(
        "scope_correct",
        "project_managed",
        "real_builder_hooks",
        "configuration_changed",
        "expected_rebuilt",
        "hash_preserved",
        "expected_reused"
      )
    ) &&
      all(
        tables$incremental_rebuild$scope_correct &
          tables$incremental_rebuild$project_managed &
          tables$incremental_rebuild$real_builder_hooks &
          tables$incremental_rebuild$configuration_changed ==
            tables$incremental_rebuild$expected_rebuilt &
          (!tables$incremental_rebuild$expected_reused |
            tables$incremental_rebuild$hash_preserved)
      )
  )
}

.builder_eval_rate <- function(numerator, denominator) {
  if (denominator == 0L) NA_real_ else numerator / denominator
}

.builder_eval_rate_text <- function(value) {
  if (is.na(value)) "NA" else sprintf("%.3f", value)
}

.builder_eval_outcomes <- function(table) {
  as.integer(base::table(factor(
    table$outcome,
    levels = c("TP", "FP", "FN", "TN")
  )))
}

builder_eval_summary_lines <- function(tables) {
  no_trials <- function(label) paste0(label, ": no trials.")
  capability <- tables$capability_detection
  capability_line <- if (!is.data.frame(capability) || !nrow(capability)) {
    no_trials("Capability detection")
  } else {
    counts <- .builder_eval_outcomes(capability)
    precision <- .builder_eval_rate(counts[[1L]], counts[[1L]] + counts[[2L]])
    recall <- .builder_eval_rate(counts[[1L]], counts[[1L]] + counts[[3L]])
    sprintf(
      paste0(
        "Capability detection: %d/%d correct; TP=%d, FP=%d, FN=%d, TN=%d; ",
        "precision=%s, recall=%s."
      ),
      counts[[1L]] + counts[[4L]],
      sum(counts),
      counts[[1L]],
      counts[[2L]],
      counts[[3L]],
      counts[[4L]],
      .builder_eval_rate_text(precision),
      .builder_eval_rate_text(recall)
    )
  }
  plans <- tables$plan_immutability
  plan_line <- if (!is.data.frame(plans) || !nrow(plans)) {
    no_trials("Plan immutability")
  } else {
    passed <- plans$matches_confirmation & plans$mutation_not_propagated
    sprintf(
      "Plan immutability: %d/%d boundary checks passed.",
      sum(passed),
      length(passed)
    )
  }
  artifacts <- tables$artifact_fidelity
  artifact_line <- if (!is.data.frame(artifacts) || !nrow(artifacts)) {
    no_trials("Artifact fidelity")
  } else {
    counts <- .builder_eval_outcomes(artifacts)
    fidelity <- .builder_eval_rate(counts[[1L]] + counts[[4L]], sum(counts))
    omission <- .builder_eval_rate(counts[[3L]], counts[[1L]] + counts[[3L]])
    unintended <- .builder_eval_rate(counts[[2L]], counts[[2L]] + counts[[4L]])
    sprintf(
      "Artifact fidelity: %s; omission=%s; unintended inclusion=%s.",
      .builder_eval_rate_text(fidelity),
      .builder_eval_rate_text(omission),
      .builder_eval_rate_text(unintended)
    )
  }
  builds <- tables$build_runs
  build_line <- if (!is.data.frame(builds) || !nrow(builds)) {
    no_trials("End-to-end builds")
  } else {
    sprintf(
      "End-to-end builds: %d/%d succeeded; median elapsed=%.3f s.",
      sum(builds$success),
      nrow(builds),
      stats::median(builds$elapsed_seconds)
    )
  }
  faults <- tables$fault_injection
  fault_line <- if (!is.data.frame(faults) || !nrow(faults)) {
    no_trials("Handled recovery")
  } else {
    recovered <- faults$old_preserved &
      faults$old_crb_reopenable &
      faults$recovery_success &
      faults$subsequent_publish_success &
      faults$subsequent_crb_reopenable &
      faults$partial_target_count == 0L &
      faults$stage_residue_count == 0L &
      faults$lock_residue_count == 0L &
      faults$backup_residue_count == 0L &
      faults$retired_backup_residue_count == 0L &
      faults$journal_state == "complete"
    sprintf(
      paste0(
        "Release recovery: %d/%d trials reopened the prior CRB and then ",
        "published cleanly (%d independent process exits)."
      ),
      sum(recovered),
      length(recovered),
      sum(faults$fault_class == "process_exit")
    )
  }
  incremental <- tables$incremental_rebuild
  incremental_line <- if (!is.data.frame(incremental) || !nrow(incremental)) {
    no_trials("Incremental rebuild")
  } else {
    expected_reuse <- sum(incremental$expected_reused)
    expected_rebuild <- sum(incremental$expected_rebuilt)
    sprintf(
      paste0(
        "Incremental rebuild: %d/%d eligible datasets reused and %d/%d ",
        "changed datasets rebuilt; measured time saved=%s."
      ),
      sum(incremental$actual_reused & incremental$expected_reused),
      expected_reuse,
      sum(incremental$actual_rebuilt & incremental$expected_rebuilt),
      expected_rebuild,
      .builder_eval_rate_text(unique(incremental$time_saved_ratio)[[1L]])
    )
  }
  c(
    capability_line,
    plan_line,
    artifact_line,
    build_line,
    fault_line,
    incremental_line
  )
}

builder_eval_write_results <- function(
  output,
  environment,
  fixture_manifest,
  tables,
  gates,
  source_manifest = list(),
  protocol = list(schema_version = 1L, profile = "legacy")
) {
  if (file.exists(output) || dir.exists(output)) {
    stop("The Builder evaluation result directory already exists.")
  }
  required <- c(
    "capability_detection",
    "plan_immutability",
    "artifact_fidelity",
    "build_runs",
    "fault_injection",
    "incremental_rebuild"
  )
  if (!identical(names(tables), required)) {
    stop("The Builder evaluation tables are incomplete or out of order.")
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output, "failures"), showWarnings = FALSE)
  dir.create(file.path(output, "reports"), showWarnings = FALSE)
  builds <- tables$build_runs
  if (
    is.data.frame(builds) &&
      nrow(builds) &&
      all(c("trial_id", "success", "report_path") %in% names(builds))
  ) {
    report_rows <- which(
      builds$success &
        !is.na(builds$report_path) &
        nzchar(builds$report_path)
    )
    for (index in report_rows) {
      trial_id <- builds$trial_id[[index]]
      source <- builds$report_path[[index]]
      if (
        !grepl("^[A-Za-z0-9_.-]+$", trial_id) ||
          !file.exists(source) ||
          dir.exists(source)
      ) {
        stop("A successful build report is unavailable for publication.")
      }
      relative <- file.path("reports", paste0(trial_id, ".json"))
      target <- file.path(output, relative)
      if (!isTRUE(file.copy(source, target, overwrite = FALSE))) {
        stop("A successful build report could not enter the evidence snapshot.")
      }
      tables$build_runs$report_path[[index]] <- relative
    }
  }
  jsonlite::write_json(
    environment,
    file.path(output, "environment.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  jsonlite::write_json(
    fixture_manifest,
    file.path(output, "fixture_manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  jsonlite::write_json(
    source_manifest,
    file.path(output, "source_manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  jsonlite::write_json(
    protocol,
    file.path(output, "protocol.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  for (name in names(tables)) {
    utils::write.csv(
      tables[[name]],
      file.path(output, paste0(name, ".csv")),
      row.names = FALSE,
      na = ""
    )
  }
  written_tables <- stats::setNames(
    lapply(names(tables), function(name) {
      if (!nrow(tables[[name]])) {
        return(tables[[name]])
      }
      utils::read.csv(
        file.path(output, paste0(name, ".csv")),
        stringsAsFactors = FALSE,
        na.strings = ""
      )
    }),
    names(tables)
  )
  status <- if (all(gates)) "PASS" else "FAIL"
  lines <- c(
    "# Builder Evaluation Summary",
    "",
    paste0("Overall: **", status, "**"),
    "",
    "## Correctness gates",
    "",
    paste0(
      "- ",
      ifelse(gates, "PASS", "FAIL"),
      " — `",
      names(gates),
      "`"
    ),
    "",
    "## Quantitative evidence",
    "",
    paste0("- ", builder_eval_summary_lines(written_tables)),
    "",
    "Raw trial-level evidence is stored in the CSV files in this directory."
  )
  writeLines(lines, file.path(output, "summary.md"), useBytes = TRUE)
  invisible(normalizePath(output, winslash = "/", mustWork = TRUE))
}
