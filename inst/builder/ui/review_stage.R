## Frozen-plan Review stage. This file never reads mutable draft inputs.

.builder_review_copy <- function(value) {
  unserialize(serialize(value, NULL, version = 3L))
}

builder_review_options <- function(
  welcome_message = "Welcome to CerebroNexus!",
  point_size = 5,
  variable_to_compare = FALSE,
  host = "127.0.0.1",
  port = 8080L,
  max_request_size = 8000,
  display_mode = "normal",
  launch_browser = TRUE,
  show_upload_ui = FALSE
) {
  valid_number <- function(value, lower, upper = Inf, whole = FALSE) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value >= lower &&
      value <= upper &&
      (!whole || value == floor(value))
  }
  valid_flag <- function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }
  if (
    !builder_stage_has_text(welcome_message) ||
      !valid_number(point_size, 0, 20) ||
      !valid_flag(variable_to_compare) ||
      !builder_stage_has_text(host) ||
      !valid_number(port, 1, 65535, whole = TRUE) ||
      !valid_number(max_request_size, .Machine$double.eps) ||
      !is.character(display_mode) ||
      length(display_mode) != 1L ||
      is.na(display_mode) ||
      !display_mode %in% c("auto", "normal", "showcase") ||
      !valid_flag(launch_browser) ||
      !valid_flag(show_upload_ui)
  ) {
    stop("Review options are invalid.", call. = FALSE)
  }
  structure(
    list(
      welcome_message = welcome_message,
      point_size = as.double(point_size),
      variable_to_compare = variable_to_compare,
      host = host,
      port = as.integer(port),
      max_request_size = as.double(max_request_size),
      display_mode = display_mode,
      launch_browser = launch_browser,
      show_upload_ui = show_upload_ui
    ),
    class = c("builder_review_options", "list")
  )
}

builder_review_options_for_plan <- function(options, initial_dataset = NULL) {
  if (!inherits(options, "builder_review_options")) {
    stop("Typed Review options are required.", call. = FALSE)
  }
  initial <- if (is.null(initial_dataset)) {
    list()
  } else {
    list(initial_dataset = initial_dataset)
  }
  c(
    list(show_upload_ui = options$show_upload_ui),
    initial,
    list(
      welcome_message = options$welcome_message,
      point_size = list(
        overview_projection_point_size = options$point_size
      ),
      variable_to_compare = options$variable_to_compare,
      host = options$host,
      port = options$port,
      max_request_size = options$max_request_size,
      display_mode = options$display_mode,
      launch_browser = options$launch_browser
    )
  )
}

builder_review_controls_ui <- function(id, options) {
  stopifnot(inherits(options, "builder_review_options"))
  ns <- NS(id)
  tags$details(
    class = "review-app-options",
    tags$summary("App options"),
    textInput(
      ns("welcome_message"),
      "Welcome message",
      options$welcome_message
    ),
    numericInput(
      ns("point_size"),
      "Point size",
      options$point_size,
      min = 0,
      max = 20
    ),
    checkboxInput(
      ns("variable_to_compare"),
      "Variable to compare",
      options$variable_to_compare
    ),
    textInput(ns("host"), "Host", options$host),
    numericInput(ns("port"), "Port", options$port, min = 1, max = 65535),
    numericInput(
      ns("max_request_size"),
      "Request size (MB)",
      options$max_request_size,
      min = 1
    ),
    selectInput(
      ns("display_mode"),
      "Display mode",
      choices = c("auto", "normal", "showcase"),
      selected = options$display_mode
    ),
    checkboxInput(
      ns("launch_browser"),
      "Launch browser",
      options$launch_browser
    ),
    checkboxInput(ns("show_upload_ui"), "Allow uploads", options$show_upload_ui)
  )
}

builder_review_can_build <- function(plan) {
  inherits(plan, "builder_build_plan") &&
    is.list(plan) &&
    identical(plan$readiness, "ready") &&
    is.null(plan$error) &&
    (!length(plan$existing_targets) || isTRUE(plan$overwrite))
}

builder_review_model <- function(plan, verification = NULL) {
  if (
    !inherits(plan, "builder_build_plan") ||
      !is.list(plan) ||
      !identical(plan$readiness, "ready")
  ) {
    stop("Review requires a ready frozen BuildPlan.", call. = FALSE)
  }
  items <- plan$items
  release_targets <- plan$output_release$targets %||%
    plan$targets %||%
    character()
  app_options <- plan$app_options %||% list(enabled = FALSE)
  review_dataset <- function(item) {
    fields <- c(
      "id",
      "name",
      "filename",
      "organism",
      "assay",
      "layer",
      "groups",
      "included_groups",
      "reductions",
      "included_projections",
      "analyses",
      "analysis_dependency_graph",
      "artifact_identity",
      "cell_count",
      "gene_count",
      "histology_coverage",
      "estimated_runtime",
      "estimated_disk_bytes",
      "colors",
      "nUMI",
      "nGene",
      "default_group",
      "default_projection",
      "metadata_policy",
      "nomenclature",
      "expression_backend",
      "sidecars",
      "readiness",
      "manifest",
      "viewer_page_expectations",
      "acknowledgements",
      "viewer_bundle_assets",
      "private_assets",
      "recommendations"
    )
    dataset <- item[intersect(fields, names(item))]
    if (length(item$tables)) {
      dataset$table_members <- names(item$tables) %||% character()
    }
    if (length(item$images)) {
      dataset$image_sections <- names(item$images) %||% character()
    }
    dataset
  }
  verified_pages <- lapply(
    verification$verifications %||% list(),
    function(item) {
      item$page_contract %||% list()
    }
  )
  model <- list(
    revision = plan$revision,
    artifact_mode = if (isTRUE(plan$make_app)) {
      "crbs_and_private_app"
    } else {
      "crbs_only"
    },
    app_contract_version = plan$app_contract_version,
    dataset_order = plan$dataset_order,
    datasets = lapply(items, review_dataset),
    manifest = plan$manifest,
    viewer_page_expectations = plan$viewer_page_expectations,
    viewer_bundle_assets = plan$viewer_bundle_assets,
    private_assets = plan$private_assets,
    acknowledgements = plan$acknowledgements,
    output_release = plan$output_release,
    app_options = app_options,
    release_members = basename(release_targets),
    duplicated_storage = isTRUE(plan$make_app),
    verified_page_expectations = verified_pages
  )
  .builder_review_copy(model)
}

builder_review_value_lines <- function(value, prefix = NULL) {
  if (is.null(value)) {
    return(character())
  }
  if (!is.list(value)) {
    text <- paste(as.character(value), collapse = ", ")
    return(
      if (builder_stage_has_text(prefix %||% "")) {
        paste0(prefix, ": ", text)
      } else {
        text
      }
    )
  }
  value_names <- names(value)
  unlist(
    lapply(seq_along(value), function(index) {
      label <- if (is.null(value_names) || !nzchar(value_names[[index]])) {
        as.character(index)
      } else {
        value_names[[index]]
      }
      next_prefix <- if (builder_stage_has_text(prefix %||% "")) {
        paste(prefix, label, sep = " / ")
      } else {
        label
      }
      builder_review_value_lines(value[[index]], next_prefix)
    }),
    use.names = FALSE
  )
}

builder_review_bounded_lines <- function(
  value,
  value_limit = 8L,
  prefix = NULL
) {
  if (is.null(value)) {
    return(character())
  }
  if (!is.list(value)) {
    shown <- utils::head(as.character(value), value_limit)
    text <- paste(shown, collapse = ", ")
    if (length(value) > value_limit) {
      text <- paste0(
        text,
        " … (",
        length(value),
        " values; ",
        length(value) - value_limit,
        " more values not shown)"
      )
    }
    return(
      if (builder_stage_has_text(prefix %||% "")) {
        paste0(prefix, ": ", text)
      } else {
        text
      }
    )
  }
  value_names <- names(value)
  lines <- unlist(
    lapply(seq_along(value), function(index) {
      label <- if (is.null(value_names) || !nzchar(value_names[[index]])) {
        as.character(index)
      } else {
        value_names[[index]]
      }
      next_prefix <- if (builder_stage_has_text(prefix %||% "")) {
        paste(prefix, label, sep = " / ")
      } else {
        label
      }
      builder_review_bounded_lines(
        value[[index]],
        value_limit = value_limit,
        prefix = next_prefix
      )
    }),
    use.names = FALSE
  )
  lines
}

builder_review_stage_ui <- function(id, model) {
  ns <- NS(id)
  options <- model$app_options %||% list()
  compact_dataset_fields <- function(dataset) {
    dataset[c(
      "analysis_dependency_graph",
      "artifact_identity",
      "histology_coverage"
    )] <- NULL
    dataset
  }
  names_by_id <- stats::setNames(
    vapply(model$datasets, `[[`, character(1), "name"),
    model$dataset_order
  )
  initial <- names_by_id[[options$initial_dataset]] %||% options$initial_dataset
  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-review",
    h2("Review"),
    span(
      class = "visually-hidden",
      paste0("Artifact mode: ", model$artifact_mode)
    ),
    p(
      class = "stage-intro",
      "Confirm the datasets and output below. Technical details stay available when you need them."
    ),
    div(
      class = "review-summary-strip",
      span(tags$b(length(model$datasets)), " dataset(s)"),
      span("Plan revision ", tags$b(model$revision)),
      span(
        "Output: ",
        tags$b(
          if (identical(model$artifact_mode, "crbs_only")) {
            "CRB files"
          } else {
            "CRB files + private App"
          }
        )
      )
    ),
    div(
      class = "review-dataset-grid",
      lapply(model$datasets, function(dataset) {
        div(
          class = "review-dataset-card",
          tags$b(dataset$name),
          p(class = "review-dataset-file", dataset$filename),
          div(
            class = "review-counts",
            span(format(dataset$cell_count %||% 0L, big.mark = ","), " cells"),
            span(format(dataset$gene_count %||% 0L, big.mark = ","), " genes")
          ),
          p(
            paste(length(dataset$groups %||% character()), "group fields ·"),
            paste(length(dataset$reductions %||% character()), "projections ·"),
            paste("backend", dataset$expression_backend %||% "embedded")
          ),
          tags$details(
            tags$summary("Technical dataset details"),
            h4("Analysis dependency graph"),
            builder_stage_text_items(builder_review_bounded_lines(
              dataset$analysis_dependency_graph
            )),
            h4("Artifact identity"),
            builder_stage_text_items(builder_review_bounded_lines(list(
              schema_version = dataset$artifact_identity$schema_version,
              frozen_cell_ids = length(
                dataset$artifact_identity$cells %||% character()
              ),
              frozen_feature_ids = length(
                dataset$artifact_identity$features %||% character()
              ),
              group_levels = dataset$artifact_identity$group_levels,
              projections = dataset$artifact_identity$projections,
              source_metadata = dataset$artifact_identity$source_metadata,
              metadata = dataset$artifact_identity$metadata,
              spatial_sections = dataset$artifact_identity$spatial_sections
            ))),
            h4("Histology coverage"),
            builder_stage_text_items(builder_review_bounded_lines(
              dataset$histology_coverage
            )),
            h4("Frozen dataset fields"),
            builder_stage_text_items(
              builder_review_bounded_lines(compact_dataset_fields(dataset))
            )
          )
        )
      })
    ),
    if (identical(model$artifact_mode, "crbs_and_private_app")) {
      tagList(
        h3(paste0("App contract ", model$app_contract_version)),
        p(
          "Selector order: ",
          paste(unname(names_by_id[model$dataset_order]), collapse = " → ")
        ),
        p(
          "Initial dataset: ",
          initial,
          " (",
          options$initial_dataset_mode,
          ")"
        ),
        p(
          if (isTRUE(options$show_upload_ui)) {
            "Uploads enabled"
          } else {
            "Uploads disabled"
          }
        ),
        p(paste0("Welcome message: ", options$welcome_message)),
        p(paste0(
          "Point size: ",
          options$point_size$overview_projection_point_size
        )),
        p(paste0(
          "Variable comparison: ",
          if (isTRUE(options$variable_to_compare)) "enabled" else "disabled"
        )),
        p(paste0("Host: ", options$host)),
        p(paste0("Port: ", options$port)),
        p(paste0("Request limit: ", options$max_request_size, " MB")),
        p(paste0("Display mode: ", options$display_mode)),
        p(paste0(
          "Launch browser: ",
          if (isTRUE(options$launch_browser)) "enabled" else "disabled"
        )),
        p("Palettes are frozen per dataset and metadata group."),
        p(
          "The CRBs and sidecars are duplicated into private App storage."
        ),
        p(
          class = "privacy",
          "Private data is not directly downloadable and belongs to no HTTP-public asset class."
        )
      )
    },
    h3("Planned payload members"),
    builder_stage_text_items(model$release_members),
    h3("Output release"),
    p(paste0("Output directory: ", model$output_release$directory)),
    p(paste0(
      "Overwrite: ",
      if (isTRUE(model$output_release$overwrite)) "enabled" else "disabled"
    )),
    p(paste0(
      "Replacement policy: ",
      model$output_release$replacement_policy
    )),
    p(paste0(
      "Estimated runtime: ",
      model$output_release$estimated_runtime
    )),
    p(paste0(
      "Estimated disk: ",
      model$output_release$estimated_disk_bytes,
      " bytes"
    )),
    h3("Acknowledged warnings"),
    builder_stage_text_items(builder_review_value_lines(
      model$acknowledgements
    )),
    tags$details(
      class = "review-technical-details",
      tags$summary("Technical plan details"),
      h3("Frozen content manifest"),
      p(
        class = "hint",
        paste(length(model$manifest %||% list()), "manifest sections")
      ),
      tags$details(
        tags$summary("Detailed manifest (bounded preview)"),
        builder_stage_text_items(builder_review_bounded_lines(model$manifest))
      )
    ),
    h3("Viewer page expectations"),
    div(
      class = "expected-versus-verified",
      div(
        class = "page-checklist expected-pages",
        h4("Expected after build"),
        builder_stage_text_items(builder_review_value_lines(
          model$viewer_page_expectations
        ))
      ),
      div(
        class = "page-checklist verified-pages",
        h4("Verified after build"),
        if (length(model$verified_page_expectations %||% list())) {
          builder_stage_text_items(builder_review_value_lines(
            model$verified_page_expectations
          ))
        } else {
          p("Available after a successful build.")
        }
      )
    ),
    if (length(model$viewer_bundle_assets)) {
      tagList(
        h3("Viewer bundle assets"),
        p(
          class = "privacy",
          "Viewer-bundle assets are private runtime inputs and are not directly downloadable."
        ),
        builder_stage_text_items(model$viewer_bundle_assets)
      )
    },
    if (length(model$private_assets)) {
      tagList(
        h3("Private assets"),
        builder_stage_text_items(model$private_assets)
      )
    }
  )
}
