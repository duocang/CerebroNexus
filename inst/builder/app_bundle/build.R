builder_build_app <- function(
  request,
  stage,
  create_app = CerebroNexus::createShinyApp
) {
  request <- .builder_app_validate_request(request)
  stage <- normalizePath(stage, winslash = "/", mustWork = TRUE)
  if (!identical(stage, request$stage) || .builder_app_is_link(stage)) {
    stop("App assembly requires the request's assigned stage.", call. = FALSE)
  }
  app_dir <- file.path(stage, "cerebro_app")
  if (.builder_app_path_exists(app_dir)) {
    stop("The staged App directory already exists.", call. = FALSE)
  }
  .builder_app_assert_input_identities(request)
  create_app(
    cerebro_data = request$cerebro_data,
    result_dir = app_dir,
    colors = request$colors,
    cerebro_options = list(
      exclude_trivial_metadata = TRUE,
      viewer_content = request$viewer_content
    ),
    overwrite = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    crb_pick_smallest_file = FALSE,
    show_upload_ui = request$show_upload_ui,
    initial_dataset = request$initial_dataset,
    initial_page = request$initial_page,
    welcome_message = request$welcome_message,
    point_size = request$point_size,
    variable_to_compare = request$variable_to_compare,
    host = request$host,
    port = request$port,
    max_request_size = request$max_request_size,
    display_mode = request$display_mode,
    launch_browser = request$launch_browser
  )
  .builder_app_assert_input_identities(request)
  if (
    !dir.exists(app_dir) ||
      .builder_app_is_link(app_dir) ||
      !.builder_app_path_within(app_dir, stage)
  ) {
    stop(
      "App assembly did not create the assigned private directory.",
      call. = FALSE
    )
  }
  app_dir
}

builder_verify_app <- function(
  app_dir,
  request,
  .tree_identity = .builder_app_tree_identity,
  .retain_tree_identity = FALSE
) {
  request <- .builder_app_validate_request(request)
  if (!dir.exists(app_dir) || .builder_app_is_link(app_dir)) {
    stop("The staged App directory is missing or symbolic.", call. = FALSE)
  }
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  expected_app_dir <- file.path(request$stage, "cerebro_app")
  if (!identical(app_dir, expected_app_dir)) {
    stop("The staged App is outside its assigned stage.", call. = FALSE)
  }
  tree_before <- .tree_identity(app_dir)
  legacy <- file.path(app_dir, "data")
  if (.builder_app_path_exists(legacy)) {
    stop(
      "The staged App contains the forbidden legacy data directory.",
      call. = FALSE
    )
  }
  app_file <- file.path(app_dir, "app.R")
  config_file <- file.path(app_dir, "cerebro_config.rds")
  if (
    !file.exists(app_file) ||
      dir.exists(app_file) ||
      .builder_app_is_link(app_file)
  ) {
    stop("The staged app.R is missing or symbolic.", call. = FALSE)
  }
  tryCatch(
    parse(file = app_file, keep.source = FALSE),
    error = function(error) {
      stop("The staged app.R cannot be parsed.", call. = FALSE)
    }
  )
  if (
    !file.exists(config_file) ||
      dir.exists(config_file) ||
      .builder_app_is_link(config_file)
  ) {
    stop("The staged App config is missing or symbolic.", call. = FALSE)
  }
  config_info <- tryCatch(
    fs::file_info(config_file, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(config_info) ||
      nrow(config_info) != 1L ||
      !identical(as.character(config_info$type), "file") ||
      !is.finite(as.double(config_info$size)) ||
      as.double(config_info$size) > .builder_app_config_max_bytes
  ) {
    stop("The staged App config is too large to read safely.", call. = FALSE)
  }
  config <- tryCatch(readRDS(config_file), error = function(error) error)
  if (
    inherits(config, "condition") ||
      !is.list(config) ||
      .builder_app_has_reference(config)
  ) {
    stop("The staged App config is not an inert readable list.", call. = FALSE)
  }
  config <- .builder_app_plain_value(config)
  crbs <- config[["crb_file_to_load"]]
  expected_crbs <- stats::setNames(
    file.path("private-data", basename(request$cerebro_data)),
    request$selector_order
  )
  if (!identical(crbs, expected_crbs)) {
    stop(
      "The staged App selector labels or order differ from request.",
      call. = FALSE
    )
  }
  if (!identical(config[["initial_dataset"]], request$initial_dataset)) {
    stop("The staged App initial dataset differs from request.", call. = FALSE)
  }
  if (!identical(config[["initial_page"]], request$initial_page)) {
    stop("The staged App starting page differs from request.", call. = FALSE)
  }
  if (!identical(config[["show_upload_ui"]], request$show_upload_ui)) {
    stop("The staged App upload policy differs from request.", call. = FALSE)
  }
  if (!identical(config[["welcome_message"]], request$welcome_message)) {
    stop("The staged App welcome message differs from request.", call. = FALSE)
  }
  if (!identical(config[["point_size"]], request$point_size)) {
    stop("The staged App point sizes differ from request.", call. = FALSE)
  }
  if (!identical(config[["viewer_content"]], request$viewer_content)) {
    stop("The staged App Viewer defaults differ from request.", call. = FALSE)
  }
  if (
    !identical(config[["variable_to_compare"]], request$variable_to_compare)
  ) {
    stop(
      "The staged App comparison option differs from request.",
      call. = FALSE
    )
  }
  expected_run_options <- list(
    schema_version = 1L,
    max_request_size_bytes = as.double(request$max_request_size * 1024^2),
    shiny_app_options = list(
      port = as.integer(request$port),
      host = request$host,
      launch.browser = request$launch_browser,
      quiet = TRUE,
      display.mode = request$display_mode
    )
  )
  if (!identical(config[[".bundle_run_options"]], expected_run_options)) {
    stop("The staged App launch options differ from request.", call. = FALSE)
  }
  if (!identical(config[["colors"]], request$colors)) {
    stop("The staged App palettes differ from request.", call. = FALSE)
  }
  if (
    !identical(
      config[["crb_pick_smallest_file"]],
      request$crb_pick_smallest_file
    )
  ) {
    stop(
      "The staged App smallest-file policy differs from request.",
      call. = FALSE
    )
  }
  backend_plan <- config[[".bundle_backend_plan"]]
  if (!identical(backend_plan, request$backend_plan)) {
    stop("The staged App backend plan differs from request.", call. = FALSE)
  }

  private_root <- file.path(app_dir, "private-data")
  if (!dir.exists(private_root) || .builder_app_is_link(private_root)) {
    stop(
      "The staged App private-data directory is missing or symbolic.",
      call. = FALSE
    )
  }
  private_root <- normalizePath(private_root, winslash = "/", mustWork = TRUE)
  .builder_app_validate_private_locations(tree_before, app_dir)
  .builder_app_assert_trusted_templates(tree_before)
  .builder_app_assert_root_topology(tree_before)
  configured_files <- character()
  for (index in seq_along(crbs)) {
    relative_crb <- unname(crbs[[index]])
    if (
      !.builder_app_safe_relative(relative_crb) ||
        !startsWith(relative_crb, "private-data/")
    ) {
      stop("A configured CRB path escapes private-data.", call. = FALSE)
    }
    crb <- file.path(app_dir, relative_crb)
    if (
      !file.exists(crb) ||
        dir.exists(crb) ||
        .builder_app_is_link(crb) ||
        !.builder_app_path_within(crb, private_root)
    ) {
      stop(
        "A configured CRB is missing, symbolic, or outside private-data.",
        call. = FALSE
      )
    }
    configured_files <- c(configured_files, normalizePath(crb, winslash = "/"))
    entry <- backend_plan$entries[[relative_crb]]
    if (identical(entry$mode, "bundled")) {
      if (!.builder_app_safe_relative(entry$location)) {
        stop(
          "A configured backend sidecar path escapes private-data.",
          call. = FALSE
        )
      }
      sidecar <- file.path(dirname(crb), entry$location)
      expected_type <- switch(
        entry$type,
        h5 = "file",
        bpcells = "directory",
        NULL
      )
      sidecar_info <- tryCatch(
        fs::file_info(sidecar, fail = TRUE, follow = FALSE),
        error = function(error) NULL
      )
      if (
        is.null(expected_type) ||
          !.builder_app_path_exists(sidecar) ||
          .builder_app_is_link(sidecar) ||
          is.null(sidecar_info) ||
          !identical(as.character(sidecar_info$type), expected_type) ||
          !.builder_app_path_within(sidecar, private_root)
      ) {
        stop(
          paste0(
            "A configured backend sidecar must be the expected ",
            if (identical(expected_type, "file")) {
              "regular file"
            } else {
              "directory"
            },
            " inside private-data."
          ),
          call. = FALSE
        )
      }
      configured_files <- c(
        configured_files,
        normalizePath(sidecar, winslash = "/", mustWork = TRUE)
      )
    }
  }

  .builder_app_assert_private_topology(tree_before, request)
  copied_content <- .builder_app_output_content_identities(
    tree_before,
    request
  )
  if (!identical(copied_content, request$content_identities)) {
    stop(
      "The staged App copied content differs from frozen input.",
      call. = FALSE
    )
  }

  tree_after <- .tree_identity(app_dir)
  if (!identical(tree_before, tree_after)) {
    stop(
      "The staged App tree changed during verification.",
      call. = FALSE
    )
  }

  verification <- structure(
    list(
      valid = TRUE,
      contract_version = 1L,
      app_dir = app_dir,
      selector_order = request$selector_order,
      initial_dataset = request$initial_dataset,
      show_upload_ui = request$show_upload_ui,
      colors = request$colors,
      backend_plan = request$backend_plan,
      private_files = configured_files,
      legacy_data_absent = TRUE,
      diagnostic_tree_identity = .builder_app_tree_summary(tree_after)
    ),
    class = c("builder_app_verification", "list")
  )
  if (isTRUE(.retain_tree_identity)) {
    attr(verification, "parent_tree_identity") <- tree_after
  }
  verification
}
