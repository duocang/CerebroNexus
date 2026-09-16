##----------------------------------------------------------------------------##
## Parent-side ownership of Builder stages and release publication.

builder_release_runtime_files <- function() {
  roots <- unique(c(
    getwd(),
    file.path(getwd(), "inst", "builder"),
    system.file("builder", package = "CerebroNexus")
  ))
  roots <- roots[nzchar(roots)]
  required <- c(
    contract = file.path("core", "bundle_path_contract.R"),
    publish = "publish.R",
    app_bundle = "app_bundle.R",
    report = "report.R",
    coordinator = "coordinator.R"
  )
  usable <- vapply(
    roots,
    function(root) all(file.exists(file.path(root, required))),
    logical(1)
  )
  if (!any(usable)) {
    stop("The release runtime is unavailable.", call. = FALSE)
  }
  root <- normalizePath(roots[usable][[1L]], winslash = "/", mustWork = TRUE)
  c(
    list(root = root),
    as.list(stats::setNames(file.path(root, required), names(required)))
  )
}
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
  app_auth <- .subset2(plan, "app_auth")
  items <- lapply(.subset2(plan, "items"), function(item) {
    list(
      id = .subset2(item, "id"),
      name = .subset2(item, "name"),
      filename = .subset2(item, "filename"),
      organism = .subset2(item, "organism") %||% NULL,
      analyses = .subset2(item, "analyses") %||% character(),
      included_groups = .subset2(item, "included_groups") %||% character(),
      included_projections = .subset2(item, "included_projections") %||%
        character(),
      metadata_policy = {
        policy <- .subset2(item, "metadata_policy") %||% list()
        retained <- .subset2(policy, "retained") %||%
          .subset2(policy, "included") %||%
          character()
        list(
          retained = retained,
          included = retained,
          excluded = .subset2(policy, "excluded") %||% character(),
          forced = .subset2(policy, "forced") %||% character()
        )
      },
      expression_backend = .subset2(item, "expression_backend"),
      spatial_image_storage = "external",
      spatial_alignment = {
        alignment <- .subset2(item, "spatial_alignment") %||% list()
        list(
          section_count = as.integer(
            .subset2(alignment, "section_count") %||% 0L
          ),
          image_count = as.integer(
            .subset2(alignment, "image_count") %||% 0L
          )
        )
      },
      sidecars = .subset2(item, "sidecars") %||% character(),
      viewer_page_expectations = list(
        visible_conditional = .subset2(
          .subset2(item, "viewer_page_expectations") %||% list(),
          "visible_conditional"
        ) %||%
          character()
      )
    )
  })
  manifest <- lapply(.subset2(plan, "manifest") %||% list(), function(entry) {
    list(
      status = .subset2(entry, "status") %||% NULL,
      disposition = .subset2(entry, "disposition") %||% NULL,
      pages = .subset2(entry, "pages") %||% character()
    )
  })
  projected <- structure(
    list(
      revision = .subset2(plan, "revision"),
      readiness = .subset2(plan, "readiness"),
      out_dir = .subset2(plan, "out_dir"),
      make_app = .subset2(plan, "make_app"),
      dataset_order = .subset2(plan, "dataset_order"),
      app_auth = list(
        enabled = isTRUE(.subset2(app_auth, "enabled")),
        account_count = as.integer(.subset2(app_auth, "account_count")),
        timeout_minutes = 15L
      ),
      items = items,
      manifest = manifest,
      acknowledgements = as.character(unique(.builder_report_strings(
        .subset2(plan, "acknowledgements") %||% list()
      ))),
      viewer_bundle_assets = .subset2(plan, "viewer_bundle_assets") %||%
        character(),
      private_assets = .subset2(plan, "private_assets") %||% character(),
      output_release = list(
        targets = .subset2(
          .subset2(plan, "output_release") %||% list(),
          "targets"
        ) %||%
          .subset2(plan, "targets") %||%
          character()
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
  app_auth <- .subset2(plan, "app_auth")
  if (
    !.builder_app_auth_summary_valid(app_auth) ||
      (!isTRUE(.subset2(plan, "make_app")) &&
        isTRUE(.subset2(app_auth, "enabled")))
  ) {
    stop("The App publication expectation is invalid.", call. = FALSE)
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
      !.builder_app_options_valid(options, dataset_ids) ||
      !.builder_app_auth_summary_valid(plan$app_auth)
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
      "default_projection",
      "initial_projections",
      "default_trajectory",
      "overview_point_size",
      "overview_point_opacity",
      "overview_percentage_cells_to_show",
      "spatial_point_appearance",
      "spatial_roi_settings",
      "expression_backend",
      "sidecars",
      "spatial_image_storage"
    )]
  })
  request_plan <- structure(
    list(
      app_contract_version = 1L,
      dataset_order = dataset_ids,
      make_app = TRUE,
      items = request_items,
      app_options = options,
      app_auth = plan$app_auth
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
    initial_page = options$initial_page,
    show_upload_ui = options$show_upload_ui,
    welcome_message = options$welcome_message,
    point_size = options$point_size,
    variable_to_compare = options$variable_to_compare,
    host = options$host,
    port = as.integer(options$port),
    max_request_size = options$max_request_size,
    display_mode = options$display_mode,
    launch_browser = options$launch_browser,
    auth = .builder_app_auth_request(plan$app_auth),
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
    "auth_enabled",
    "auth_database",
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
  auth_valid <- identical(
    value$auth_enabled,
    isTRUE(expectation$auth$enabled)
  ) &&
    if (isTRUE(expectation$auth$enabled)) {
      identical(
        value$auth_database,
        file.path(
          expectation$app_dir,
          "private-data",
          "auth",
          "credentials.sqlite"
        )
      )
    } else {
      is.null(value$auth_database)
    }
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
      !auth_valid ||
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
    identical(request$initial_page, expectation$initial_page) &&
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
    identical(request$auth, expectation$auth) &&
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

.builder_coordinator_utf16_length <- function(path) {
  vapply(path, function(value) {
    encoded <- tryCatch(
      iconv(value, from = "", to = "UTF-16LE", toRaw = TRUE)[[1L]],
      error = function(error) NULL
    )
    if (is.null(encoded)) {
      return(as.double(nchar(value, type = "chars")))
    }
    as.double(length(encoded) / 2L)
  }, numeric(1), USE.NAMES = FALSE)
}

.builder_coordinator_windows_path_candidates <- function(
  plan,
  stage,
  app_expected,
  .tempfile = tempfile
) {
  items <- .subset2(plan, "items") %||% list()
  artifacts <- unique(unlist(lapply(items, function(item) {
    c(
      .subset2(item, "filename") %||% character(),
      .subset2(item, "sidecars") %||% character()
    )
  }), use.names = FALSE))
  artifacts <- artifacts[
    !is.na(artifacts) & nzchar(artifacts) & !grepl("[/\\\\]", artifacts)
  ]
  export_stage <- .tempfile(pattern = ".crb-stage-", tmpdir = stage)
  report_temporary <- .tempfile(
    pattern = ".build-report-",
    tmpdir = stage,
    fileext = ".json"
  )
  stages <- dirname(stage)
  control <- dirname(stages)
  release_token_reserve <- strrep("t", 64L)
  atomic_token_reserve <- strrep("a", 64L)
  candidates <- c(
    stage,
    file.path(stage, artifacts),
    file.path(export_stage, artifacts),
    file.path(stage, "build-report.json"),
    report_temporary,
    file.path(
      stage,
      paste0(
        ".",
        .builder_release_record_name,
        ".",
        release_token_reserve,
        ".tmp"
      )
    ),
    file.path(
      control,
      "diagnostics",
      paste0(
        ".owner.rds.",
        release_token_reserve,
        ".",
        atomic_token_reserve,
        ".tmp"
      )
    ),
    file.path(
      control,
      "diagnostics",
      paste0(
        "released-lock-",
        release_token_reserve,
        "-",
        atomic_token_reserve
      )
    )
  )
  safe_spatial_component <- function(value, fallback) {
    value <- tolower(iconv(
      as.character(value %||% ""),
      to = "ASCII//TRANSLIT",
      sub = ""
    ))
    value <- gsub("[^a-z0-9]+", "-", value)
    value <- gsub("(^-+|-+$)", "", value)
    if (!nzchar(value)) fallback else substr(value, 1L, 48L)
  }
  for (item in items) {
    images <- .subset2(item, "images") %||% list()
    for (section_id in names(images)) {
      records <- images[[section_id]] %||% list()
      for (label in names(records)) {
        record <- records[[label]] %||% list()
        source <- .subset2(record, "source") %||% list()
        filename <- .subset2(source, "name") %||% label
        filename <- basename(gsub("\\", "/", filename, fixed = TRUE))
        stem <- tools::file_path_sans_ext(filename)
        if (!nzchar(stem)) {
          stem <- "image"
        }
        # Materialization normalizes to png/jpg and may append a make.unique()
        # suffix when labels share a file name. Reserve that bounded suffix.
        filename <- paste0(stem, ".duplicate-0000000000.jpeg")
        candidates <- c(candidates, file.path(
          stage,
          ".builder-spatial-assets",
          safe_spatial_component(.subset2(item, "id"), "dataset"),
          safe_spatial_component(section_id, "section"),
          filename
        ))
      }
    }
  }
  sidecars <- unique(unlist(lapply(items, function(item) {
    if (!identical(.subset2(item, "expression_backend"), "bpcells")) {
      return(character())
    }
    .subset2(item, "sidecars") %||% character()
  }), use.names = FALSE))
  sidecars <- sidecars[
    !is.na(sidecars) & nzchar(sidecars) & !grepl("[/\\\\]", sidecars)
  ]
  if (length(sidecars)) {
    # BPCells controls the names below its sidecar directory. Reserve room for
    # those implementation-owned entries without guessing their exact names.
    candidates <- c(
      candidates,
      file.path(export_stage, sidecars, strrep("x", 96L))
    )
  }
  if (isTRUE(app_expected)) {
    app_dir <- file.path(stage, "cerebro_app")
    app_stage <- .tempfile(
      pattern = ".cerebro_app-stage-",
      tmpdir = stage
    )
    app_relative <- c(
      "app.R",
      "config.yml",
      "build-manifest.rds",
      file.path("private-data", artifacts),
      file.path("spatial-assets", paste0("u", strrep("f", 32L), ".tiff")),
      # Current Viewer paths are shorter; this reserve makes future packaged
      # resource growth fail during preflight instead of halfway through copy.
      file.path("viewer-resource", strrep("x", 96L))
    )
    candidates <- c(
      candidates,
      file.path(app_dir, app_relative),
      file.path(app_stage, app_relative)
    )
  }
  unique(gsub("\\", "/", candidates, fixed = TRUE))
}

.builder_coordinator_assert_windows_path_budget <- function(
  plan,
  stage,
  app_expected,
  os_type = .Platform$OS.type,
  limit = 259L,
  .tempfile = tempfile
) {
  if (!identical(os_type, "windows")) {
    return(invisible(TRUE))
  }
  candidates <- .builder_coordinator_windows_path_candidates(
    plan,
    stage,
    app_expected,
    .tempfile = .tempfile
  )
  lengths <- .builder_coordinator_utf16_length(candidates)
  longest <- which.max(lengths)
  if (lengths[[longest]] > as.double(limit)) {
    stop(
      "The selected output folder is too deep for this Windows R runtime. ",
      "The longest planned Builder path is ", lengths[[longest]],
      " UTF-16 characters (supported limit: ", limit, "). ",
      "Choose a shorter output folder, for example C:/CerebroBuild, or ",
      "shorten the dataset/spatial image name shown below. ",
      "Planned path: ", candidates[[longest]],
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_coordinator_publication_tree_matches <- function(
  current,
  parent,
  portable,
  phase
) {
  if (
    is.null(current) ||
      !identical(.builder_app_tree_summary(current), portable)
  ) {
    return(FALSE)
  }
  if (identical(phase, "after_in_place")) {
    current_relocated <- current
    parent_relocated <- parent
    current_relocated$root_fingerprint[c(
      "modification_time",
      "change_time"
    )] <- NULL
    parent_relocated$root_fingerprint[c(
      "modification_time",
      "change_time"
    )] <- NULL
    return(identical(current_relocated, parent_relocated))
  }
  identical(current, parent)
}

builder_coordinator_output_preflight <- function(plan, prior_state = NULL) {
  plan_class <- attr(plan, "class", exact = TRUE)
  if (
    !identical(typeof(plan), "list") ||
      (!is.null(plan_class) &&
        !identical(plan_class, c("builder_build_plan", "list")))
  ) {
    stop("A frozen plan with an output release is required.", call. = FALSE)
  }
  out_dir <- .subset2(plan, "out_dir")
  if (!.builder_release_text(.builder_release_or(out_dir, NULL))) {
    stop("A frozen plan with an output release is required.", call. = FALSE)
  }
  output_release <- .builder_release_or(
    .subset2(plan, "output_release"),
    list()
  )
  expected <- .builder_release_or(
    .subset2(output_release, "targets"),
    .builder_release_or(.subset2(plan, "targets"), character())
  )
  relative <- vapply(
    expected,
    function(path) {
      .builder_release_relative(path, out_dir)
    },
    ""
  )
  expected <- sort(unique(relative), method = "radix")
  declared_prior <- .subset2(plan, "expected_prior_identity")
  if (
    !is.null(declared_prior) && !.builder_release_identity_valid(declared_prior)
  ) {
    stop("The expected prior release identity is invalid.", call. = FALSE)
  }
  if (is.null(prior_state)) {
    prior_state <- builder_release_state(
      out_dir,
      exact_record = FALSE,
      allow_abandoned = TRUE
    )
  }
  valid_prior_state <- is.list(prior_state) &&
    identical(prior_state$schema_version, 1L) &&
    .builder_release_identity_valid(prior_state$identity)
  if (!valid_prior_state) {
    stop("The verified prior release state is invalid.", call. = FALSE)
  }
  prior <- declared_prior %||% prior_state$identity
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
  list(
    out_dir = out_dir,
    expected = expected,
    prior = prior,
    prior_state = prior_state,
    prior_paths = prior_paths,
    record = record,
    foreign = foreign
  )
}

builder_coordinator_prepare <- function(plan, build_id, prior_state = NULL) {
  app_contract <- .builder_coordinator_app_contract(plan)
  preflight <- builder_coordinator_output_preflight(plan, prior_state)
  out_dir <- preflight$out_dir
  expected <- preflight$expected
  prior <- preflight$prior
  prior_state <- preflight$prior_state
  prior_paths <- preflight$prior_paths
  record <- preflight$record
  foreign <- preflight$foreign
  if (length(foreign)) {
    stop(
      "The output directory contains foreign release entries: ",
      paste(foreign, collapse = ", "),
      ". They were preserved.",
      call. = FALSE
    )
  }
  if (
    length(prior_paths) &&
      !isTRUE(record$abandoned) &&
      !isTRUE(.subset2(plan, "overwrite"))
  ) {
    stop(
      "Known outputs already exist. Enable Replace existing outputs.",
      call. = FALSE
    )
  }
  handle <- builder_prepare_release(
    out_dir,
    build_id,
    expected_prior = prior,
    expected_prior_state = prior_state,
    .stage_guard = function(stage) {
      .builder_coordinator_assert_windows_path_budget(
        plan,
        stage,
        app_expected = isTRUE(app_contract$expectation$expected)
      )
    }
  )
  handle$expected_payload_targets <- expected
  handle$transient_app_inputs <- if (
    isTRUE(app_contract$expectation$expected)
  ) {
    sort(
      unique(unlist(
        lapply(plan$items, function(item) {
          c(item$filename, item$sidecars %||% character())
        }),
        use.names = FALSE
      )),
      method = "radix"
    )
  } else {
    character()
  }
  handle$expected_build_targets <- sort(
    unique(c(
      handle$expected_payload_targets,
      handle$transient_app_inputs
    )),
    method = "radix"
  )
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
  exact = FALSE,
  .digest_cache = NULL
) {
  identity <- builder_release_identity(
    handle$stage,
    .digest_cache = .digest_cache
  )
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
      planned <- vapply(
        present,
        function(path) {
          any(
            path %in% expected,
            startsWith(path, paste0(expected, "/"))
          )
        },
        logical(1)
      )
      unplanned <- present[!planned]
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

.builder_coordinator_validate_build_auth <- function(handle, build_result) {
  expected_enabled <- isTRUE(handle$app_expectation$expected) &&
    isTRUE(handle$app_expectation$auth$enabled)
  expected_env_file <- if (expected_enabled) {
    file.path(handle$stage, "cerebro_app", "viewer-auth.env")
  } else {
    NULL
  }
  if (
    !is.list(build_result) ||
      !identical(build_result$auth_enabled, expected_enabled) ||
      !identical(build_result$auth_env_file, expected_env_file)
  ) {
    stop(
      "The build result authentication evidence differs from the frozen plan.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_coordinator_auth_env_identity <- function(
  handle,
  release_root = handle$stage
) {
  if (
    !isTRUE(handle$app_expectation$expected) ||
      !isTRUE(handle$app_expectation$auth$enabled)
  ) {
    return(NULL)
  }
  env_file <- file.path(
    release_root,
    "cerebro_app",
    "viewer-auth.env"
  )
  secret <- builder_auth_read_env_file(env_file)
  secret <- NULL
  database <- file.path(
    release_root,
    "cerebro_app",
    "private-data",
    "auth",
    "credentials.sqlite"
  )
  if (!isTRUE(builder_auth_verify_database_pair(database, env_file))) {
    stop("The authentication database could not be verified.", call. = FALSE)
  }
  first <- .builder_release_payload_snapshot(env_file)
  md5 <- .builder_release_payload_md5(env_file)
  second <- .builder_release_payload_snapshot(env_file)
  if (!identical(first, second)) {
    stop(
      "The authentication environment changed during inspection.",
      call. = FALSE
    )
  }
  c(first, list(md5 = md5))
}

.builder_coordinator_remove_app_inputs <- function(
  handle,
  built,
  parent_request,
  .unlink = unlink
) {
  expected <- handle$transient_app_inputs
  built_relative <- vapply(
    unname(built),
    .builder_release_relative,
    character(1),
    root = handle$stage
  )
  frozen <- vapply(handle$app_plan$items, `[[`, character(1), "filename")
  if (!setequal(built_relative, frozen)) {
    stop("The staged App inputs differ from the frozen plan.", call. = FALSE)
  }
  .builder_coordinator_assert_input_closure(
    handle,
    built,
    parent_request,
    phase = "before temporary input removal"
  )
  for (relative in expected) {
    path <- file.path(handle$stage, relative)
    if (
      !.pathWithin(path, handle$stage) ||
        .builder_release_link(path) ||
        !.builder_release_exists(path)
    ) {
      stop("A temporary App input is missing or unsafe.", call. = FALSE)
    }
    status <- .unlink(path, recursive = dir.exists(path), force = TRUE)
    if (
      !identical(as.integer(status), 0L) ||
        .builder_release_exists(path) ||
        .builder_release_link(path)
    ) {
      stop("A temporary App input could not be removed.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.builder_coordinator_register_spatial_molecule_inputs <- function(
  handle,
  built,
  app_expected
) {
  identities <- lapply(
    unname(built),
    .builder_app_capture_spatial_molecule_identity
  )
  locations <- vapply(identities, function(identity) {
    if (is.null(identity)) "" else basename(identity$root)
  }, character(1))
  locations <- sort(unique(locations[nzchar(locations)]), method = "radix")
  if (!length(locations)) {
    return(handle)
  }
  if (
    anyDuplicated(tolower(locations)) ||
      any(grepl("[/\\\\]", locations)) ||
      any(locations %in% c(".", ".."))
  ) {
    stop(
      "The staged spatial molecule sidecar closure is invalid.",
      call. = FALSE
    )
  }
  if (isTRUE(app_expected)) {
    handle$transient_app_inputs <- sort(
      unique(c(handle$transient_app_inputs, locations)),
      method = "radix"
    )
  } else {
    handle$expected_payload_targets <- sort(
      unique(c(handle$expected_payload_targets, locations)),
      method = "radix"
    )
  }
  handle$expected_build_targets <- sort(
    unique(c(
      handle$expected_payload_targets,
      handle$transient_app_inputs
    )),
    method = "radix"
  )
  handle
}

builder_coordinator_publish <- function(
  handle,
  build_result,
  .record_move = file.rename,
  .verify_app = builder_verify_app,
  .write_report = builder_write_build_report,
  .publish = builder_publish_release
) {
  handle <- .builder_coordinator_handle(handle)
  digest_cache <- new.env(parent = emptyenv())
  if (
    !is.list(build_result) ||
      !identical(build_result$state, "success") ||
      !isTRUE(build_result$publishable)
  ) {
    stop("Only a verified successful build can be published.", call. = FALSE)
  }
  .builder_coordinator_validate_build_auth(handle, build_result)
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
  handle <- .builder_coordinator_register_spatial_molecule_inputs(
    handle,
    built,
    app_expected
  )
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
    returned_app_dir <- tryCatch(
      .canonicalTargetPath(build_result$app_dir),
      error = function(error) ""
    )
    expected_app_dir <- .canonicalTargetPath(
      handle$app_expectation$app_dir
    )
    if (
      !identical(returned_app_dir, expected_app_dir) ||
        !dir.exists(returned_app_dir) ||
        .builder_release_link(returned_app_dir)
    ) {
      stop(
        "The build result did not return the assigned App directory.",
        call. = FALSE
      )
    }
    build_result$app_dir <- returned_app_dir
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
        auth_env_file = if (isTRUE(handle$app_expectation$auth$enabled)) {
          expected_env <- file.path(
            handle$stage,
            "cerebro_app",
            "viewer-auth.env"
          )
          if (!identical(build_result$auth_env_file, expected_env)) {
            stop(
              "The build result returned an invalid authentication environment."
            )
          }
          expected_env
        } else {
          if (
            !is.null(build_result$auth_env_file) ||
              isTRUE(build_result$auth_enabled)
          ) {
            stop("A public build returned authentication material.")
          }
          NULL
        },
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
    # Cache hits bind device, inode, and change time; mutations are rehashed.
    for (entry in parent_tree_identity$entries) {
      if (identical(entry$type, "file")) {
        assign(
          .builder_release_digest_cache_key(entry),
          entry$md5,
          envir = digest_cache
        )
      }
    }
  }
  payload_identity <- .builder_coordinator_stage_identity(
    handle,
    expected = handle$expected_build_targets,
    .digest_cache = digest_cache
  )
  parent_env_identity <- .builder_coordinator_auth_env_identity(handle)
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
  if (app_expected) {
    if (
      !identical(
        .builder_coordinator_auth_env_identity(handle),
        parent_env_identity
      )
    ) {
      stop("The staged App changed after parent verification.", call. = FALSE)
    }
    .builder_coordinator_remove_app_inputs(
      handle,
      built,
      parent_request
    )
    if (
      !identical(
        .builder_coordinator_auth_env_identity(handle),
        parent_env_identity
      )
    ) {
      stop(
        "The authentication environment changed during temporary input removal.",
        call. = FALSE
      )
    }
  }
  final_payload_targets <- sort(
    unique(c(
      handle$expected_payload_targets,
      "build-report.json"
    )),
    method = "radix"
  )
  payload_identity <- .builder_coordinator_stage_identity(
    handle,
    expected = final_payload_targets,
    exact = TRUE,
    .digest_cache = digest_cache
  )
  if (
    app_expected &&
      !identical(
        .builder_coordinator_app_payload_summary(payload_identity),
        parent_verification$diagnostic_tree_identity
      )
  ) {
    stop(
      "The staged App changed during temporary input removal.",
      call. = FALSE
    )
  }
  handle$expected_payload_targets <- final_payload_targets
  ownership <- .builder_release_write_record(
    handle$stage,
    payload_identity,
    handle$token,
    .move = .record_move,
    .digest_cache = digest_cache
  )
  if (app_expected) {
    if (
      !identical(
        .builder_coordinator_app_payload_summary(ownership$identity),
        parent_verification$diagnostic_tree_identity
      )
    ) {
      stop(
        "The staged App changed during ownership record commit.",
        call. = FALSE
      )
    }
    if (
      !identical(
        .builder_coordinator_auth_env_identity(handle),
        parent_env_identity
      )
    ) {
      stop(
        "The authentication environment changed during ownership record commit.",
        call. = FALSE
      )
    }
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
  handle$expected_stage_identity <- ownership$identity
  publication_guard <- function(root, phase) {
    if (!app_expected) {
      return(TRUE)
    }
    app_dir <- file.path(root, "cerebro_app")
    current <- tryCatch(
      .builder_app_tree_identity(
        app_dir,
        .previous = parent_tree_identity
      ),
      error = function(error) NULL
    )
    current_env <- tryCatch(
      .builder_coordinator_auth_env_identity(handle, release_root = root),
      error = function(error) NULL
    )
    paired <- if (isTRUE(handle$app_expectation$auth$enabled)) {
      database <- file.path(
        app_dir,
        "private-data",
        "auth",
        "credentials.sqlite"
      )
      tryCatch(
        builder_auth_verify_database_pair(
          database,
          file.path(app_dir, "viewer-auth.env")
        ),
        error = function(error) FALSE
      )
    } else {
      is.null(current_env)
    }
    artifact_root <- if (app_expected) {
      file.path(root, "cerebro_app", "private-data")
    } else {
      root
    }
    verification_relative <- vapply(
      build_result$verifications,
      function(value) {
        if (
          !is.list(value) ||
            !.builder_release_text(value$path) ||
            !.pathWithin(value$path, handle$stage)
        ) {
          return(NA_character_)
        }
        basename(.builder_release_relative(value$path, handle$stage))
      },
      character(1)
    )
    artifact_paths <- c(
      file.path(artifact_root, basename(unname(built))),
      file.path(artifact_root, verification_relative)
    )
    isTRUE(paired) &&
      .builder_coordinator_publication_tree_matches(
        current,
        parent_tree_identity,
        parent_verification$diagnostic_tree_identity,
        phase
      ) &&
      identical(current_env, parent_env_identity) &&
      !anyNA(artifact_paths) &&
      all(file.exists(artifact_paths))
  }
  published <- .publish(
    handle,
    .verify_payload = publication_guard,
    .digest_cache = digest_cache
  )
  relative_built <- vapply(
    built,
    .builder_release_relative,
    "",
    root = handle$stage
  )
  mapped_built <- if (app_expected) {
    file.path(
      published$target,
      "cerebro_app",
      "private-data",
      basename(unname(built))
    )
  } else {
    file.path(published$target, relative_built)
  }
  build_result$built <- stats::setNames(mapped_built, names(built))
  if (app_expected) {
    relative_app <- .builder_release_relative(
      build_result$app_dir,
      handle$stage
    )
    build_result$app_dir <- file.path(published$target, relative_app)
    if (isTRUE(handle$app_expectation$auth$enabled)) {
      build_result$auth_enabled <- TRUE
      build_result$auth_env_file <- file.path(
        published$target,
        "cerebro_app",
        "viewer-auth.env"
      )
    } else {
      build_result$auth_enabled <- FALSE
      build_result$auth_env_file <- NULL
    }
  } else {
    build_result$app_dir <- NULL
    build_result$auth_enabled <- FALSE
    build_result$auth_env_file <- NULL
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
          relative <- .builder_release_relative(verification$path, handle$stage)
          verification$path <- if (app_expected) {
            file.path(
              published$target,
              "cerebro_app",
              "private-data",
              basename(relative)
            )
          } else {
            file.path(published$target, relative)
          }
        }
        if (
          is.list(verification) &&
            .builder_release_text(verification$spatial_molecule_path)
        ) {
          location <- basename(verification$spatial_molecule_path)
          verification$spatial_molecule_path <- if (app_expected) {
            file.path(
              published$target,
              "cerebro_app",
              "private-data",
              location
            )
          } else {
            file.path(published$target, location)
          }
        }
        verification
      }
    )
  }
  verification_paths <- if (length(build_result$verifications)) {
    vapply(
      build_result$verifications,
      function(verification) {
        if (
          !is.list(verification) || !.builder_release_text(verification$path)
        ) {
          return(NA_character_)
        }
        verification$path
      },
      character(1)
    )
  } else {
    character()
  }
  mapped_paths <- c(unname(build_result$built), verification_paths)
  spatial_molecule_paths <- unlist(lapply(
    build_result$verifications,
    function(verification) {
      verification$spatial_molecule_path %||% character()
    }
  ), use.names = FALSE)
  mapped_paths <- c(mapped_paths, spatial_molecule_paths)
  if (
    !length(mapped_paths) ||
      anyNA(mapped_paths) ||
      !all(file.exists(mapped_paths))
  ) {
    stop("Published build artifacts are missing.", call. = FALSE)
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

builder_coordinator_claim <- function(handle) {
  handle <- .builder_coordinator_handle(handle)
  pid <- as.integer(Sys.getpid())
  .builder_release_transfer_lock(
    handle$lock,
    handle$token,
    from_pid = as.integer(handle$pid),
    to_pid = pid
  )
  handle$pid <- pid
  handle$host <- .builder_release_host()
  handle$record$pid <- pid
  handle$record$host <- handle$host
  handle
}

builder_coordinator_settle <- function(
  handle,
  build_result,
  .publish = builder_coordinator_publish,
  .abort = builder_coordinator_abort,
  .recovery = builder_coordinator_recovery
) {
  target <- if (
    is.list(handle) && .builder_release_text(handle$target %||% "")
  ) {
    handle$target
  } else {
    NULL
  }
  failed <- function(message) {
    recovery <- if (!is.null(target)) {
      tryCatch(.recovery(target), error = function(error) NULL)
    } else {
      NULL
    }
    list(
      ok = FALSE,
      value = NULL,
      error = message,
      target = target,
      recovery = recovery
    )
  }
  if (is.null(target)) {
    return(failed("The parent release coordinator identity was lost."))
  }
  if (
    is.list(build_result) &&
      identical(build_result$state, "success") &&
      isTRUE(build_result$publishable)
  ) {
    published <- tryCatch(
      .publish(handle, build_result),
      error = function(error) error
    )
    if (!inherits(published, "condition")) {
      return(list(
        ok = TRUE,
        value = published,
        error = NULL,
        target = target,
        recovery = NULL
      ))
    }
    publication_error <- conditionMessage(published)
    try(.abort(handle), silent = TRUE)
    return(failed(publication_error))
  }
  cleaned <- tryCatch(.abort(handle), error = function(error) error)
  if (inherits(cleaned, "condition") || !isTRUE(cleaned$aborted)) {
    return(failed(
      if (inherits(cleaned, "condition")) {
        conditionMessage(cleaned)
      } else {
        "The assigned build stage could not be cleaned."
      }
    ))
  }
  list(
    ok = TRUE,
    value = build_result,
    error = NULL,
    target = target,
    recovery = NULL
  )
}

builder_coordinator_recovery <- function(target) {
  builder_discover_recovery(target)
}
