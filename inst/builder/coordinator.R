##----------------------------------------------------------------------------##
## Parent-side ownership of Builder stages and release publication.
##----------------------------------------------------------------------------##

.builder_coordinator_freeze <- function(value) {
  if (.builder_app_has_reference(value)) {
    stop("The App publication expectation is not inert.", call. = FALSE)
  }
  tryCatch(
    unserialize(serialize(value, NULL, version = 3L)),
    error = function(error) {
      stop(
        "The App publication expectation could not be frozen.",
        call. = FALSE
      )
    }
  )
}

.builder_coordinator_report_plan <- function(plan) {
  items <- lapply(plan$items, function(item) {
    list(
      id = item$id,
      name = item$name,
      filename = item$filename,
      organism = item$organism %||% NULL,
      analyses = item$analyses %||% character(),
      included_groups = item$included_groups %||% character(),
      included_projections = item$included_projections %||% character(),
      metadata_policy = list(
        included = item$metadata_policy$included %||% character()
      ),
      expression_backend = item$expression_backend,
      sidecars = item$sidecars %||% character(),
      viewer_page_expectations = list(
        visible_conditional = item$viewer_page_expectations$visible_conditional %||%
          character()
      )
    )
  })
  manifest <- lapply(plan$manifest %||% list(), function(entry) {
    list(
      status = entry$status %||% NULL,
      disposition = entry$disposition %||% NULL,
      pages = entry$pages %||% character()
    )
  })
  projected <- structure(
    list(
      revision = plan$revision,
      readiness = plan$readiness,
      out_dir = plan$out_dir,
      make_app = plan$make_app,
      dataset_order = plan$dataset_order,
      items = items,
      manifest = manifest,
      acknowledgements = as.character(unique(.builder_report_strings(
        plan$acknowledgements %||% list()
      ))),
      viewer_bundle_assets = plan$viewer_bundle_assets %||% character(),
      private_assets = plan$private_assets %||% character(),
      output_release = list(
        targets = plan$output_release$targets %||% plan$targets %||% character()
      )
    ),
    class = c("builder_build_plan", "list")
  )
  .builder_coordinator_freeze(projected)
}

.builder_coordinator_app_contract <- function(plan) {
  plan_class <- attr(plan, "class", exact = TRUE)
  if (
    !identical(typeof(plan), "list") ||
      (!is.null(plan_class) &&
        !identical(plan_class, c("builder_build_plan", "list")))
  ) {
    stop(
      "App publication requires an inert contract-v1 BuildPlan.",
      call. = FALSE
    )
  }
  if (!isTRUE(.subset2(plan, "make_app"))) {
    return(list(
      plan = NULL,
      expectation = list(expected = FALSE)
    ))
  }
  if (!identical(plan_class, c("builder_build_plan", "list"))) {
    stop(
      "App publication requires an inert contract-v1 BuildPlan.",
      call. = FALSE
    )
  }
  plan <- tryCatch(
    .builder_app_plan_contract(plan, context = "App publication"),
    error = function(error) error
  )
  if (
    inherits(plan, "condition") ||
      !identical(plan$app_contract_version, 1L)
  ) {
    stop(
      "App publication requires an inert contract-v1 BuildPlan.",
      call. = FALSE
    )
  }
  dataset_ids <- plan$dataset_order
  items <- plan$items
  if (
    !is.character(dataset_ids) ||
      !length(dataset_ids) ||
      anyNA(dataset_ids) ||
      any(!nzchar(dataset_ids)) ||
      anyDuplicated(dataset_ids) ||
      !is.list(items) ||
      length(items) != length(dataset_ids)
  ) {
    stop("The App publication dataset order is invalid.", call. = FALSE)
  }
  item_ids <- vapply(items, `[[`, character(1), "id")
  labels <- vapply(items, `[[`, character(1), "name")
  filenames <- vapply(items, `[[`, character(1), "filename")
  colors <- lapply(items, `[[`, "colors")
  names(colors) <- labels
  options <- plan$app_options
  if (
    !identical(item_ids, dataset_ids) ||
      anyNA(labels) ||
      any(!nzchar(labels)) ||
      anyDuplicated(labels) ||
      anyNA(filenames) ||
      any(!nzchar(filenames)) ||
      anyDuplicated(filenames) ||
      !.builder_app_colors_valid(colors, labels) ||
      .builder_app_has_reference(options) ||
      !.builder_app_options_valid(options, dataset_ids)
  ) {
    stop("The App publication expectation is invalid.", call. = FALSE)
  }
  backend_entries <- lapply(items, .builder_app_backend_entry)
  names(backend_entries) <- file.path("private-data", filenames)
  initial_index <- match(options$initial_dataset, dataset_ids)
  request_items <- lapply(items, function(item) {
    item[c(
      "id",
      "name",
      "filename",
      "colors",
      "expression_backend",
      "sidecars"
    )]
  })
  request_plan <- structure(
    list(
      app_contract_version = 1L,
      dataset_order = dataset_ids,
      make_app = TRUE,
      items = request_items,
      app_options = options
    ),
    class = c("builder_build_plan", "list")
  )
  frozen_plan <- .builder_coordinator_freeze(request_plan)
  expectation <- .builder_coordinator_freeze(list(
    expected = TRUE,
    contract_version = 1L,
    dataset_ids = dataset_ids,
    labels = labels,
    filenames = filenames,
    initial_dataset = labels[[initial_index]],
    initial_dataset_mode = options$initial_dataset_mode,
    show_upload_ui = options$show_upload_ui,
    welcome_message = options$welcome_message,
    point_size = options$point_size,
    variable_to_compare = options$variable_to_compare,
    host = options$host,
    port = as.integer(options$port),
    max_request_size = options$max_request_size,
    display_mode = options$display_mode,
    launch_browser = options$launch_browser,
    colors = colors,
    backend_plan = list(schema_version = 1L, entries = backend_entries),
    app_dir = NULL
  ))
  list(plan = frozen_plan, expectation = expectation)
}

.builder_coordinator_app_verification <- function(value, expectation) {
  if (
    !identical(typeof(value), "list") ||
      !identical(
        attr(value, "class", exact = TRUE),
        c("builder_app_verification", "list")
      ) ||
      .builder_app_has_reference(value)
  ) {
    stop("App verification evidence is missing or forged.", call. = FALSE)
  }
  value <- .builder_app_plain_value(value)
  required <- c(
    "valid",
    "contract_version",
    "app_dir",
    "selector_order",
    "initial_dataset",
    "show_upload_ui",
    "colors",
    "backend_plan",
    "private_files",
    "legacy_data_absent",
    "diagnostic_tree_identity"
  )
  diagnostic <- value$diagnostic_tree_identity
  diagnostic_valid <- is.list(diagnostic) &&
    !is.object(diagnostic) &&
    identical(
      names(diagnostic),
      c(
        "schema_version",
        "entry_count",
        "file_count",
        "directory_count",
        "aggregate_md5"
      )
    ) &&
    identical(diagnostic$schema_version, 1L) &&
    all(vapply(
      diagnostic[c("entry_count", "file_count", "directory_count")],
      function(count) {
        is.numeric(count) &&
          length(count) == 1L &&
          is.finite(count) &&
          count >= 0 &&
          count == floor(count)
      },
      logical(1)
    )) &&
    is.character(diagnostic$aggregate_md5) &&
    length(diagnostic$aggregate_md5) == 1L &&
    !is.na(diagnostic$aggregate_md5) &&
    grepl("^[0-9a-f]{32}$", diagnostic$aggregate_md5)
  private_root <- file.path(expectation$app_dir, "private-data")
  private_files_valid <- is.character(value$private_files) &&
    !anyNA(value$private_files) &&
    all(vapply(
      value$private_files,
      .pathWithin,
      logical(1),
      parent = private_root
    ))
  if (
    !identical(names(value), required) ||
      !isTRUE(value$valid) ||
      !identical(value$contract_version, expectation$contract_version) ||
      !identical(value$app_dir, expectation$app_dir) ||
      !identical(value$selector_order, expectation$labels) ||
      !identical(value$initial_dataset, expectation$initial_dataset) ||
      !identical(value$show_upload_ui, expectation$show_upload_ui) ||
      !identical(value$colors, expectation$colors) ||
      !identical(value$backend_plan, expectation$backend_plan) ||
      !isTRUE(value$legacy_data_absent) ||
      !private_files_valid ||
      !diagnostic_valid
  ) {
    stop(
      "App verification evidence differs from the frozen plan.",
      call. = FALSE
    )
  }
  value
}

.builder_coordinator_request_matches <- function(request, expectation) {
  identical(request$contract_version, expectation$contract_version) &&
    identical(request$selector_order, expectation$labels) &&
    identical(request$initial_dataset, expectation$initial_dataset) &&
    identical(
      request$initial_dataset_mode,
      expectation$initial_dataset_mode
    ) &&
    identical(request$show_upload_ui, expectation$show_upload_ui) &&
    identical(request$welcome_message, expectation$welcome_message) &&
    identical(request$point_size, expectation$point_size) &&
    identical(request$variable_to_compare, expectation$variable_to_compare) &&
    identical(request$host, expectation$host) &&
    identical(request$port, expectation$port) &&
    identical(request$max_request_size, expectation$max_request_size) &&
    identical(request$display_mode, expectation$display_mode) &&
    identical(request$launch_browser, expectation$launch_browser) &&
    identical(request$colors, expectation$colors) &&
    identical(request$backend_plan, expectation$backend_plan)
}

.builder_coordinator_assert_input_closure <- function(
  handle,
  built,
  parent_request,
  phase = "after parent verification"
) {
  current_request <- tryCatch(
    builder_app_bundle_request(
      handle$app_plan,
      built,
      handle$app_expectation$labels
    ),
    error = function(error) NULL
  )
  if (is.null(current_request) || !identical(current_request, parent_request)) {
    stop("The App input closure changed ", phase, ".", call. = FALSE)
  }
  invisible(TRUE)
}

.builder_coordinator_app_payload_summary <- function(identity) {
  paths <- vapply(identity$entries, `[[`, character(1), "path")
  prefix <- "cerebro_app/"
  selected <- startsWith(paths, prefix)
  entries <- identity$entries[selected]
  relative <- substring(paths[selected], nchar(prefix) + 1L)
  entries <- lapply(seq_along(entries), function(index) {
    entry <- entries[[index]]
    if (identical(entry$type, "directory")) {
      return(list(path = relative[[index]], type = "directory"))
    }
    list(
      path = relative[[index]],
      type = "file",
      size = entry$size,
      md5 = entry$md5
    )
  })
  names(entries) <- relative
  .builder_app_tree_summary(list(entries = entries))
}

builder_coordinator_prepare <- function(plan, build_id) {
  if (
    !is.list(plan) ||
      !.builder_release_text(.builder_release_or(plan$out_dir, NULL))
  ) {
    stop("A frozen plan with an output release is required.", call. = FALSE)
  }
  app_contract <- .builder_coordinator_app_contract(plan)
  output_release <- .builder_release_or(plan$output_release, list())
  expected <- .builder_release_or(
    output_release$targets,
    .builder_release_or(plan$targets, character())
  )
  relative <- vapply(
    expected,
    function(path) {
      .builder_release_relative(path, plan$out_dir)
    },
    ""
  )
  expected <- sort(unique(relative), method = "radix")
  prior <- .builder_release_or(
    plan$expected_prior_identity,
    builder_release_identity(plan$out_dir)
  )
  if (!.builder_release_identity_valid(prior)) {
    stop("The expected prior release identity is invalid.", call. = FALSE)
  }
  prior_state <- builder_release_state(plan$out_dir, exact_record = FALSE)
  if (!identical(prior_state$identity, prior)) {
    stop(
      "The release changed after Review; nothing was published.",
      call. = FALSE
    )
  }
  prior_paths <- vapply(prior$entries, `[[`, character(1), "path")
  record <- prior_state$record
  expected_roots <- unique(sub("/.*$", "", expected))
  if (is.null(record)) {
    prior_roots <- unique(sub("/.*$", "", prior_paths))
    foreign <- setdiff(prior_roots, expected_roots)
  } else {
    foreign <- record$foreign
  }
  if (length(foreign)) {
    stop(
      "The output directory contains foreign release entries: ",
      paste(foreign, collapse = ", "),
      ". They were preserved.",
      call. = FALSE
    )
  }
  if (length(prior_paths) && !isTRUE(plan$overwrite)) {
    stop(
      "Known outputs already exist. Enable Replace existing outputs.",
      call. = FALSE
    )
  }
  handle <- builder_prepare_release(
    plan$out_dir,
    build_id,
    expected_prior = prior,
    expected_prior_state = prior_state
  )
  handle$expected_payload_targets <- expected
  handle$expected_final_targets <- NULL
  handle$legacy_prior_members <- if (is.null(record)) {
    .builder_release_identity_members(prior)
  } else {
    NULL
  }
  app_contract$expectation$app_dir <- if (
    isTRUE(app_contract$expectation$expected)
  ) {
    file.path(handle$stage, "cerebro_app")
  } else {
    NULL
  }
  handle$app_plan <- app_contract$plan
  handle$report_plan <- .builder_coordinator_report_plan(plan)
  handle$app_expectation <- .builder_coordinator_freeze(
    app_contract$expectation
  )
  class(handle) <- c("builder_release_coordinator", class(handle))
  handle
}

.builder_coordinator_handle <- function(handle) {
  if (!inherits(handle, "builder_release_coordinator")) {
    stop("A Builder release coordinator is required.", call. = FALSE)
  }
  .builder_release_handle(handle)
}

.builder_coordinator_stage_identity <- function(
  handle,
  expected = handle$expected_payload_targets,
  exact = FALSE
) {
  identity <- builder_release_identity(handle$stage)
  if (!isTRUE(identity$exists)) {
    stop("The coordinator-assigned stage is missing.", call. = FALSE)
  }
  present <- vapply(identity$entries, `[[`, character(1), "path")
  if (length(expected)) {
    missing <- setdiff(expected, present)
    if (length(missing)) {
      stop(
        "The verified stage is missing planned release entries: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    if (isTRUE(exact)) {
      unplanned <- setdiff(present, expected)
    } else {
      expected_roots <- unique(sub("/.*$", "", expected))
      present_roots <- unique(sub("/.*$", "", present))
      unplanned <- setdiff(present_roots, expected_roots)
    }
    if (length(unplanned)) {
      stop(
        "The verified stage contains unplanned release entries: ",
        paste(unplanned, collapse = ", "),
        call. = FALSE
      )
    }
  }
  identity
}

builder_coordinator_publish <- function(
  handle,
  build_result,
  .record_move = file.rename,
  .verify_app = builder_verify_app,
  .write_report = builder_write_build_report
) {
  handle <- .builder_coordinator_handle(handle)
  if (
    !is.list(build_result) ||
      !identical(build_result$state, "success") ||
      !isTRUE(build_result$publishable)
  ) {
    stop("Only a verified successful build can be published.", call. = FALSE)
  }
  result_stage <- tryCatch(
    .canonicalTargetPath(build_result$stage),
    error = function(error) ""
  )
  if (!identical(result_stage, .canonicalTargetPath(handle$stage))) {
    stop(
      "The build result did not come from the assigned stage.",
      call. = FALSE
    )
  }
  built <- .builder_release_or(build_result$built, character())
  if (
    !length(built) ||
      !all(vapply(
        built,
        .pathWithin,
        logical(1),
        parent = handle$stage
      ))
  ) {
    stop("Verified build artifacts escaped the assigned stage.", call. = FALSE)
  }
  app_expected <- isTRUE(handle$app_expectation$expected)
  parent_verification <- NULL
  staged_app <- file.path(handle$stage, "cerebro_app")
  if (!app_expected) {
    if (
      !is.null(build_result$app_dir) ||
        !is.null(build_result$app_verification) ||
        .builder_release_exists(staged_app)
    ) {
      stop(
        "A CRB-only plan returned an unexpected App.",
        call. = FALSE
      )
    }
  } else {
    if (!identical(build_result$build_id, handle$build_id)) {
      stop(
        "The App build identity does not match its coordinator.",
        call. = FALSE
      )
    }
    if (
      !identical(build_result$app_dir, handle$app_expectation$app_dir) ||
        !dir.exists(build_result$app_dir) ||
        .builder_release_link(build_result$app_dir)
    ) {
      stop(
        "The build result did not return the assigned App directory.",
        call. = FALSE
      )
    }
    worker_verification <- .builder_coordinator_app_verification(
      build_result$app_verification,
      handle$app_expectation
    )
    if (!identical(build_result$labels, handle$app_expectation$labels)) {
      stop("The App labels differ from the frozen plan.", call. = FALSE)
    }
    parent_request <- tryCatch(
      builder_app_bundle_request(
        handle$app_plan,
        built,
        handle$app_expectation$labels
      ),
      error = function(error) error
    )
    if (
      inherits(parent_request, "condition") ||
        !.builder_coordinator_request_matches(
          parent_request,
          handle$app_expectation
        )
    ) {
      stop(
        "The staged App inputs differ from the frozen plan.",
        call. = FALSE
      )
    }
    parent_verification <- tryCatch(
      .verify_app(
        build_result$app_dir,
        parent_request,
        .retain_tree_identity = TRUE
      ),
      error = function(error) error
    )
    if (inherits(parent_verification, "condition")) {
      stop(
        "Parent App verification failed: ",
        conditionMessage(parent_verification),
        call. = FALSE
      )
    }
    parent_tree_identity <- attr(
      parent_verification,
      "parent_tree_identity",
      exact = TRUE
    )
    attr(parent_verification, "parent_tree_identity") <- NULL
    parent_verification <- .builder_coordinator_app_verification(
      parent_verification,
      handle$app_expectation
    )
    if (
      !identical(
        worker_verification$diagnostic_tree_identity,
        parent_verification$diagnostic_tree_identity
      )
    ) {
      stop(
        "The staged App changed after worker verification.",
        call. = FALSE
      )
    }
    if (!identical(worker_verification, parent_verification)) {
      stop(
        "App verification evidence differs from parent read-back.",
        call. = FALSE
      )
    }
  }
  payload_identity <- .builder_coordinator_stage_identity(handle)
  if (app_expected) {
    current_app_identity <- tryCatch(
      .builder_app_tree_identity(handle$app_expectation$app_dir),
      error = function(error) NULL
    )
    if (
      is.null(parent_tree_identity) ||
        !identical(current_app_identity, parent_tree_identity)
    ) {
      stop(
        "The staged App changed after parent verification.",
        call. = FALSE
      )
    }
  }
  if (
    app_expected &&
      !identical(
        .builder_coordinator_app_payload_summary(payload_identity),
        parent_verification$diagnostic_tree_identity
      )
  ) {
    stop(
      "The staged App changed after parent verification.",
      call. = FALSE
    )
  }
  if (app_expected) {
    .builder_coordinator_assert_input_closure(
      handle,
      built,
      parent_request
    )
  }
  if (length(handle$legacy_prior_members)) {
    payload_members <- .builder_release_identity_members(payload_identity)
    payload_keys <- vapply(
      payload_members,
      function(member) {
        paste(member$type, member$path, sep = "\t")
      },
      ""
    )
    legacy_keys <- vapply(
      handle$legacy_prior_members,
      function(member) {
        paste(member$type, member$path, sep = "\t")
      },
      ""
    )
    missing_legacy <- setdiff(legacy_keys, payload_keys)
    if (length(missing_legacy)) {
      stop(
        "The verified stage would shrink the legacy release topology: ",
        paste(sub("^[FD]\t", "", missing_legacy), collapse = ", "),
        call. = FALSE
      )
    }
  }
  report_result <- build_result
  if (app_expected) {
    report_result$app_verification <- parent_verification
  }
  report <- builder_build_report(handle$report_plan, report_result)
  report_path <- .write_report(handle$stage, report)
  expected_report_path <- file.path(handle$stage, "build-report.json")
  if (
    !identical(
      normalizePath(report_path, winslash = "/", mustWork = TRUE),
      normalizePath(expected_report_path, winslash = "/", mustWork = TRUE)
    )
  ) {
    stop(
      "The build report was not written to the assigned stage.",
      call. = FALSE
    )
  }
  handle$expected_payload_targets <- sort(
    unique(c(
      handle$expected_payload_targets,
      "build-report.json"
    )),
    method = "radix"
  )
  payload_identity <- .builder_coordinator_stage_identity(handle)
  handle$expected_payload_targets <- vapply(
    payload_identity$entries,
    `[[`,
    character(1),
    "path"
  )
  ownership <- .builder_release_write_record(
    handle$stage,
    payload_identity,
    handle$token,
    .move = .record_move
  )
  if (app_expected) {
    ownership_app_identity <- tryCatch(
      .builder_app_tree_identity(handle$app_expectation$app_dir),
      error = function(error) NULL
    )
    if (!identical(ownership_app_identity, parent_tree_identity)) {
      stop(
        "The staged App changed during ownership record commit.",
        call. = FALSE
      )
    }
    .builder_coordinator_assert_input_closure(
      handle,
      built,
      parent_request,
      phase = "during ownership record commit"
    )
  }
  final_payload_identity <- ownership$identity
  final_paths <- vapply(
    final_payload_identity$entries,
    `[[`,
    character(1),
    "path"
  )
  final_payload_identity$entries <- final_payload_identity$entries[
    !final_paths %in% .builder_release_record_name
  ]
  if (!identical(final_payload_identity, payload_identity)) {
    stop(
      "The verified payload changed during ownership record commit.",
      call. = FALSE
    )
  }
  handle$expected_final_targets <- c(
    handle$expected_payload_targets,
    .builder_release_record_name
  )
  final_identity <- .builder_coordinator_stage_identity(
    handle,
    expected = handle$expected_final_targets,
    exact = TRUE
  )
  if (!identical(final_identity, ownership$identity)) {
    stop("The ownership record read-back identity changed.", call. = FALSE)
  }
  handle$expected_stage_identity <- final_identity
  published <- builder_publish_release(handle)
  relative_built <- vapply(
    built,
    .builder_release_relative,
    "",
    root = handle$stage
  )
  build_result$built <- file.path(published$target, relative_built)
  if (app_expected) {
    relative_app <- .builder_release_relative(
      build_result$app_dir,
      handle$stage
    )
    build_result$app_dir <- file.path(published$target, relative_app)
  } else {
    build_result$app_dir <- NULL
  }
  if (length(build_result$verifications)) {
    build_result$verifications <- lapply(
      build_result$verifications,
      function(verification) {
        if (
          is.list(verification) &&
            .builder_release_text(verification$path) &&
            .pathWithin(verification$path, handle$stage)
        ) {
          relative <- .builder_release_relative(
            verification$path,
            handle$stage
          )
          verification$path <- file.path(published$target, relative)
        }
        verification
      }
    )
  }
  build_result$app_verification <- NULL
  build_result$app_verified <- app_expected
  build_result$report_path <- file.path(published$target, "build-report.json")
  build_result$stage <- NULL
  build_result$published <- TRUE
  build_result$publishable <- FALSE
  build_result$release <- published
  build_result
}

builder_coordinator_abort <- function(handle) {
  handle <- .builder_coordinator_handle(handle)
  builder_abort_release(handle)
}

builder_coordinator_recovery <- function(target) {
  builder_discover_recovery(target)
}
