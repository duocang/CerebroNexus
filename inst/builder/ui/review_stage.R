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
    class = "review-app-options builder-card builder-section",
    tags$summary("App options"),
    textInput(
      ns("welcome_message"),
      "Welcome message",
      options$welcome_message
    ),
    checkboxInput(
      ns("variable_to_compare"),
      "Variable to compare",
      options$variable_to_compare
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

builder_review_human_size <- function(bytes) {
  builder_file_human_size(bytes)
}

builder_review_existing_files <- function(policy, overwrite = FALSE) {
  policy <- as.character(policy %||% "")
  switch(
    policy,
    preserve_existing = "Keep existing files",
    overwrite = "Replace existing files",
    error_if_exists = "Stop if files already exist",
    if (isTRUE(overwrite)) "Replace existing files" else "Keep existing files"
  )
}

builder_review_page_labels <- function(items, plan_contract = list()) {
  catalog <- builder_viewer_page_catalog()
  visible_ids <- unique(unlist(
    lapply(items, function(item) {
      item$viewer_page_expectations$visible_conditional %||% character()
    }),
    use.names = FALSE
  ))
  if (!length(visible_ids)) {
    visible_ids <- unique(unlist(
      lapply(plan_contract, function(contract) {
        contract$visible_conditional %||% character()
      }),
      use.names = FALSE
    ))
  }
  labels <- c(
    catalog$always$label,
    catalog$conditional$label[match(
      intersect(catalog$conditional$id, visible_ids),
      catalog$conditional$id
    )]
  )
  unique(labels[!is.na(labels) & nzchar(labels)])
}

builder_review_group_label <- function(value) {
  value <- gsub("[_.]+", " ", as.character(value %||% ""))
  value <- trimws(value)
  if (!nzchar(value)) {
    return("Not selected")
  }
  paste0(toupper(substr(value, 1L, 1L)), substr(value, 2L, nchar(value)))
}

builder_review_projection_label <- function(value) {
  value <- as.character(value %||% "")
  known <- c(
    umap = "UMAP",
    tsne = "t-SNE",
    `t-sne` = "t-SNE",
    pca = "PCA"
  )
  key <- tolower(value)
  labels <- unname(known[key])
  labels[is.na(labels)] <- value[is.na(labels)]
  labels
}

builder_review_trajectory_model <- function(included, default = NULL) {
  if (!is.list(included) || !length(included)) {
    return(NULL)
  }
  trajectory_names <- unname(unlist(included, use.names = FALSE))
  trajectory_names <- trajectory_names[
    !is.na(trajectory_names) &
      nzchar(trajectory_names)
  ]
  if (!length(trajectory_names)) {
    return(NULL)
  }
  default_name <- if (
    is.list(default) && builder_stage_has_text(default$name %||% "")
  ) {
    default$name
  } else if (builder_stage_has_text(default %||% "")) {
    default
  } else {
    trajectory_names[[1L]]
  }
  list(
    included_count = as.integer(length(trajectory_names)),
    included = trajectory_names,
    default = as.character(default_name)
  )
}

builder_review_metadata_model <- function(
  policy,
  manifest = list(),
  acknowledgements = character()
) {
  if (!is.list(policy)) {
    return(list(
      total_count = 0L,
      kept_count = 0L,
      excluded_count = 0L,
      attention_count = 0L
    ))
  }
  columns <- policy$columns
  if (is.list(columns) && length(columns)) {
    ids <- setdiff(names(columns) %||% character(), "cell_barcode")
    records <- columns[ids]
    effective_included <- vapply(
      records,
      function(record) {
        value <- if (is.list(record)) record$effective_included else NULL
        if (is.logical(value) && length(value) == 1L && !is.na(value)) {
          value
        } else {
          NA
        }
      },
      logical(1),
      USE.NAMES = FALSE
    )
    retained <- !is.na(effective_included) & effective_included
    dispositions <- vapply(
      records,
      function(record) {
        if (is.list(record)) record$disposition %||% "unknown" else "unknown"
      },
      character(1)
    )
    attention_count <- as.integer(sum(
      dispositions %in% c("attention", "blocking")
    ))
    metadata_entry <- manifest$metadata_policy %||% list()
    action <- metadata_entry$required_action %||% list()
    acknowledged <- identical(metadata_entry$status %||% "", "attention") &&
      identical(action$type %||% "", "acknowledge") &&
      builder_stage_has_text(action$token %||% "") &&
      (action$token %||% "") %in% acknowledgements
    if (identical(metadata_entry$status %||% "", "valid") || acknowledged) {
      attention_count <- 0L
    }
    return(list(
      total_count = as.integer(length(records)),
      kept_count = as.integer(sum(retained)),
      excluded_count = as.integer(sum(
        dispositions == "excluded" |
          (!is.na(effective_included) & !effective_included)
      )),
      attention_count = attention_count
    ))
  }
  included <- setdiff(policy$included %||% character(), "cell_barcode")
  excluded <- setdiff(policy$excluded %||% character(), "cell_barcode")
  attention <- setdiff(policy$attention %||% character(), "cell_barcode")
  list(
    total_count = as.integer(length(unique(c(included, excluded, attention)))),
    kept_count = as.integer(length(unique(included))),
    excluded_count = as.integer(length(unique(excluded))),
    attention_count = as.integer(length(unique(attention)))
  )
}

builder_review_model <- function(plan, verification = NULL) {
  if (
    !inherits(plan, "builder_build_plan") ||
      !is.list(plan) ||
      !identical(plan$readiness, "ready")
  ) {
    stop("Review requires a ready frozen BuildPlan.", call. = FALSE)
  }
  items <- plan$items %||% list()
  app_options <- plan$app_options %||% list(enabled = FALSE)
  names_by_id <- stats::setNames(
    vapply(items, function(item) item$name %||% "Dataset", character(1)),
    vapply(items, function(item) item$id %||% "", character(1))
  )
  order_ids <- plan$dataset_order %||% names(names_by_id)
  ordered_names <- unname(names_by_id[order_ids])
  ordered_names <- ordered_names[!is.na(ordered_names)]
  initial_id <- app_options$initial_dataset %||% ""
  initial_name <- if (
    nzchar(initial_id) && initial_id %in% names(names_by_id)
  ) {
    names_by_id[[initial_id]]
  } else if (length(ordered_names)) {
    ordered_names[[1L]]
  } else {
    "Not selected"
  }
  review_dataset <- function(item) {
    group_values <- item$included_groups %||% item$groups %||% character()
    group_levels <- item$artifact_identity$group_levels %||% list()
    default_group <- item$default_group %||% ""
    selected_group_levels <- if (
      is.list(group_levels) &&
        nzchar(default_group) &&
        default_group %in% names(group_levels)
    ) {
      group_levels[[default_group]] %||% character()
    } else {
      character()
    }
    group_colors <- item$colors[[default_group]] %||% character()
    color_preview_limit <- 5L
    color_preview <- head(unname(group_colors), color_preview_limit)
    color_custom_count <- as.integer(item$color_custom_count %||% 0L)
    projection_values <- item$included_projections %||%
      item$reductions %||%
      character()
    color_overrides <- item$group_color_overrides %||% list()
    custom_color_count <- if (
      is.list(color_overrides) && length(color_overrides)
    ) {
      as.integer(sum(vapply(color_overrides, length, integer(1))))
    } else {
      color_custom_count
    }
    point_size <- item$overview_point_size %||%
      app_options$point_size$overview_projection_point_size %||%
      5
    trajectory_model <- builder_review_trajectory_model(
      item$included_trajectories %||% list(),
      item$default_trajectory %||% NULL
    )
    analysis_results <- builder_analysis_results_model(list(
      analysis_manifest = item$manifest %||% list(),
      analysis_acknowledgements = item$acknowledgements %||% character()
    ))
    specialized_content <- builder_specialized_content_model(list(
      content_manifest = item$manifest %||% list(),
      content_acknowledgements = item$acknowledgements %||% character()
    ))
    list(
      name = item$name %||% "Dataset",
      cells = as.integer(item$cell_count %||% 0L),
      genes = as.integer(item$gene_count %||% 0L),
      group_count = as.integer(
        if (length(selected_group_levels)) {
          length(selected_group_levels)
        } else {
          length(group_values)
        }
      ),
      projection_count = as.integer(length(projection_values)),
      default_group = item$default_group %||% "Not selected",
      viewer_content = list(
        metadata = builder_review_metadata_model(
          item$metadata_policy,
          item$manifest %||% list(),
          item$acknowledgements %||% character()
        ),
        groups = list(
          included_count = as.integer(length(group_values)),
          included = unname(group_values),
          default = builder_review_group_label(item$default_group %||% ""),
          custom_color_count = custom_color_count
        ),
        cell_cycle = if (length(item$cell_cycle %||% character())) {
          list(included = unname(item$cell_cycle))
        } else {
          NULL
        },
        projections = list(
          included_count = as.integer(length(projection_values)),
          included = builder_review_projection_label(projection_values),
          default = if (
            builder_stage_has_text(item$default_projection %||% "")
          ) {
            builder_review_projection_label(item$default_projection)
          } else {
            "Not selected"
          },
          point_size = as.numeric(point_size)
        ),
        trajectories = trajectory_model,
        analysis_results = analysis_results,
        specialized = specialized_content
      ),
      group_colors = list(
        group = default_group,
        count = as.integer(length(group_colors)),
        custom_count = custom_color_count,
        preview = color_preview,
        remaining = as.integer(max(
          0L,
          length(group_colors) - color_preview_limit
        ))
      ),
      default_projection = if (
        builder_stage_has_text(item$default_projection %||% "")
      ) {
        toupper(item$default_projection)
      } else {
        "Not selected"
      },
      organism = switch(
        item$organism %||% "",
        hg = "Human",
        mm = "Mouse",
        item$organism %||% "Not specified"
      ),
      expression_storage = switch(
        item$expression_backend %||% "embedded",
        embedded = "Embedded",
        h5 = "HDF5",
        bpcells = "BPCells",
        item$expression_backend %||% "Embedded"
      ),
      spatial_alignment = item$spatial_alignment %||% NULL,
      output_file = basename(item$filename %||% "dataset.crb")
    )
  }
  runtime_values <- tolower(as.character(c(
    plan$output_release$estimated_runtime %||% character(),
    unlist(
      lapply(items, function(item) {
        item$estimated_runtime %||% character()
      }),
      use.names = FALSE
    )
  )))
  runtime <- if (any(grepl("network", runtime_values, fixed = TRUE))) {
    "Depends on network response"
  } else if (any(grepl("minute", runtime_values, fixed = TRUE))) {
    "A few minutes"
  } else {
    "Less than a minute"
  }
  warning_values <- plan$required_settings %||%
    plan$user_warnings %||%
    character()
  warning_values <- as.character(unlist(warning_values, use.names = FALSE))
  warning_values <- warning_values[nzchar(trimws(warning_values))]
  if (
    length(plan$existing_targets %||% character()) &&
      !isTRUE(plan$overwrite)
  ) {
    warning_values <- unique(c(
      warning_values,
      paste(
        "Files already exist in the output folder.",
        "Choose another folder or replace the matching files."
      )
    ))
  }
  if (length(warning_values)) {
    for (dataset_id in names(names_by_id)) {
      warning_values <- gsub(
        dataset_id,
        names_by_id[[dataset_id]],
        warning_values,
        fixed = TRUE
      )
    }
  }
  model <- list(
    dataset_count = as.integer(length(items)),
    output_label = if (isTRUE(plan$make_app)) {
      "CRB files + private App"
    } else {
      "CRB files"
    },
    datasets = lapply(items, review_dataset),
    app = list(
      enabled = isTRUE(plan$make_app),
      initial_dataset = initial_name,
      dataset_order = ordered_names,
      uploads_enabled = isTRUE(app_options$show_upload_ui),
      welcome_message = app_options$welcome_message %||%
        "Welcome to CerebroNexus!",
      point_size = app_options$point_size$overview_projection_point_size %||% 5,
      variable_comparison = isTRUE(app_options$variable_to_compare)
    ),
    pages = builder_review_page_labels(items, plan$viewer_page_expectations),
    output = list(
      directory = if (isTRUE(plan$output_pending)) {
        "Choose when you build"
      } else {
        plan$output_release$directory %||% ""
      },
      crb_count = as.integer(length(items)),
      private_app = isTRUE(plan$make_app),
      existing_files = builder_review_existing_files(
        plan$output_release$replacement_policy,
        plan$output_release$overwrite
      ),
      estimated_size = builder_review_human_size(
        (plan$output_release$estimated_disk_bytes %||% 0) *
          if (isTRUE(plan$make_app)) 2 else 1
      ),
      estimated_time = runtime
    ),
    warnings = warning_values,
    can_build = builder_review_can_build(plan)
  )
  .builder_review_copy(model)
}

builder_review_blocked_ui <- function(id, message = NULL) {
  ns <- NS(id)
  issue <- if (builder_stage_has_text(message %||% "")) {
    message
  } else {
    "Review the highlighted settings before building."
  }
  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-review builder-card builder-section",
    h2("Review"),
    p(class = "stage-intro", "Check your datasets and output before building."),
    tags$section(
      class = "review-section review-needs-attention",
      h3("Needs attention"),
      p(issue),
      p("Correct the highlighted settings, then return here to build.")
    )
  )
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
    class = "builder-stage builder-stage-review builder-card builder-section",
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

## The Review surface consumes only the user-facing projection above. The
## frozen BuildPlan remains intact for execution and reporting.
builder_review_stage_ui <- function(id, model) {
  ns <- NS(id)
  plural <- function(value, singular, plural = paste0(singular, "s")) {
    paste(value, if (identical(as.integer(value), 1L)) singular else plural)
  }
  field <- function(label, value, class = NULL) {
    div(
      class = paste("review-field", class),
      tags$dt(label),
      tags$dd(value)
    )
  }
  page_limit <- 8L
  shown_pages <- utils::head(model$pages %||% character(), page_limit)
  more_pages <- utils::tail(
    model$pages %||% character(),
    max(0L, length(model$pages %||% character()) - page_limit)
  )
  page_tags <- function(pages) {
    div(
      class = "review-page-tags",
      lapply(seq_along(pages), function(index) {
        span(
          class = paste0("review-page-tag tone-", ((index - 1L) %% 5L) + 1L),
          pages[[index]]
        )
      })
    )
  }

  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-review builder-card builder-section",
    h2("Review"),
    p(
      class = "stage-intro",
      "Check your datasets and output before building."
    ),
    div(
      class = "review-summary-strip",
      span(plural(model$dataset_count, "dataset")),
      span(paste("Creates", model$output_label))
    ),
    tags$section(
      class = "review-section review-datasets",
      h3("Datasets"),
      div(
        class = "review-dataset-grid",
        lapply(model$datasets, function(dataset) {
          viewer_content <- dataset$viewer_content
          div(
            class = "review-dataset-card",
            h4(dataset$name),
            p(
              class = "review-dataset-counts",
              paste0(
                format(dataset$cells, big.mark = ","),
                " cells · ",
                format(dataset$genes, big.mark = ","),
                " genes"
              )
            ),
            div(
              class = "review-viewer-content",
              if (viewer_content$metadata$total_count > 0L) {
                div(
                  class = "review-viewer-content-item review-viewer-metadata",
                  h5("Metadata"),
                  p(paste0(
                    viewer_content$metadata$kept_count,
                    " kept · ",
                    viewer_content$metadata$excluded_count,
                    " excluded"
                  )),
                  if (viewer_content$metadata$attention_count > 0L) {
                    p(
                      class = "hint",
                      paste(
                        viewer_content$metadata$attention_count,
                        "needs attention"
                      )
                    )
                  }
                )
              },
              div(
                class = "review-viewer-content-item review-viewer-groups",
                h5("Groups"),
                p(paste0(
                  viewer_content$groups$included_count,
                  " included · Default: ",
                  viewer_content$groups$default
                )),
                p(
                  class = "hint",
                  paste(
                    viewer_content$groups$custom_color_count,
                    "colors customized"
                  )
                )
              ),
              if (!is.null(viewer_content$cell_cycle)) {
                div(
                  class = "review-viewer-content-item review-viewer-cell-cycle",
                  h5("Cell cycle"),
                  p(paste(viewer_content$cell_cycle$included, collapse = ", "))
                )
              },
              div(
                class = "review-viewer-content-item review-viewer-projections",
                h5("Projections"),
                p(paste(
                  viewer_content$projections$included,
                  collapse = ", "
                )),
                p(paste0(
                  "Default: ",
                  viewer_content$projections$default
                )),
                p(
                  class = "hint",
                  paste("Point size", viewer_content$projections$point_size)
                )
              ),
              if (!is.null(viewer_content$trajectories)) {
                div(
                  class = "review-viewer-content-item review-viewer-trajectories",
                  h5("Trajectories"),
                  p(paste0(
                    viewer_content$trajectories$included_count,
                    " included · Default: ",
                    viewer_content$trajectories$default
                  ))
                )
              },
              if (viewer_content$analysis_results$total_count > 0L) {
                div(
                  class = paste(
                    "review-viewer-content-item",
                    "review-viewer-analysis-results"
                  ),
                  h5("Analysis results"),
                  p(paste(
                    c(
                      if (viewer_content$analysis_results$existing_count > 0L) {
                        paste(
                          viewer_content$analysis_results$existing_count,
                          "existing"
                        )
                      },
                      if (
                        viewer_content$analysis_results$generated_count > 0L
                      ) {
                        paste(
                          viewer_content$analysis_results$generated_count,
                          "will be generated"
                        )
                      },
                      if (
                        viewer_content$analysis_results$attention_count > 0L
                      ) {
                        paste(
                          viewer_content$analysis_results$attention_count,
                          "needs attention"
                        )
                      },
                      if (viewer_content$analysis_results$excluded_count > 0L) {
                        paste(
                          viewer_content$analysis_results$excluded_count,
                          "not included"
                        )
                      }
                    ),
                    collapse = " · "
                  )),
                  p(
                    class = "hint",
                    paste(
                      vapply(
                        viewer_content$analysis_results$items,
                        `[[`,
                        character(1),
                        "label"
                      ),
                      collapse = ", "
                    )
                  )
                )
              },
              if (viewer_content$specialized$total_count > 0L) {
                div(
                  class = paste(
                    "review-viewer-content-item",
                    "review-viewer-specialized-content"
                  ),
                  h5("Specialized content"),
                  p(viewer_content$specialized$summary),
                  p(
                    class = "hint",
                    paste(
                      vapply(
                        viewer_content$specialized$items,
                        `[[`,
                        character(1),
                        "label"
                      ),
                      collapse = ", "
                    )
                  )
                )
              }
            ),
            if (
              !is.null(dataset$spatial_alignment) &&
                dataset$spatial_alignment$section_count > 0L
            ) {
              div(
                class = "review-spatial-alignment",
                tags$b("Spatial alignment"),
                p(paste0(
                  dataset$spatial_alignment$saved_count,
                  " of ",
                  dataset$spatial_alignment$section_count,
                  if (identical(dataset$spatial_alignment$section_count, 1L)) {
                    " section has a saved tissue image."
                  } else if (
                    identical(dataset$spatial_alignment$saved_count, 1L)
                  ) {
                    " sections has a saved tissue image."
                  } else {
                    " sections have saved tissue images."
                  }
                )),
                if (length(dataset$spatial_alignment$points_only)) {
                  p(
                    class = "hint",
                    paste0(
                      length(dataset$spatial_alignment$points_only),
                      if (length(dataset$spatial_alignment$points_only) == 1L) {
                        " section remains points-only."
                      } else {
                        " sections remain points-only."
                      }
                    )
                  )
                }
              )
            },
            p(
              class = "review-dataset-file",
              span("Output file: "),
              tags$code(dataset$output_file)
            )
          )
        })
      )
    ),
    if (isTRUE(model$app$enabled)) {
      tags$section(
        class = "review-section review-app-experience",
        h3("App experience"),
        div(
          class = "review-app-grid",
          tags$dl(
            class = "review-fields",
            field("Opens with", model$app$initial_dataset),
            field(
              "Visitor uploads",
              if (isTRUE(model$app$uploads_enabled)) "On" else "Off"
            ),
            field("Welcome message", model$app$welcome_message),
            field(
              "Variable comparison",
              if (isTRUE(model$app$variable_comparison)) "On" else "Off"
            )
          ),
          div(
            class = "review-order",
            h4("Dataset order"),
            tags$ol(lapply(model$app$dataset_order, tags$li))
          )
        )
      )
    },
    tags$section(
      class = "review-section review-pages",
      h3("Pages in the App"),
      page_tags(shown_pages),
      if (length(more_pages)) {
        tags$details(
          class = "review-pages-more",
          tags$summary(paste("Show", length(more_pages), "more")),
          page_tags(more_pages)
        )
      }
    ),
    tags$section(
      class = "review-section review-output",
      h3("Output"),
      tags$dl(
        class = "review-fields review-output-fields",
        field("Folder", model$output$directory, "is-path"),
        field(
          "Creates",
          paste(
            plural(model$output$crb_count, "CRB file"),
            if (isTRUE(model$output$private_app)) "+ 1 private App" else NULL
          )
        ),
        field("Estimated size", model$output$estimated_size),
        field("Estimated build time", model$output$estimated_time)
      )
    ),
    if (isTRUE(model$output$private_app)) {
      tags$section(
        class = "review-section review-privacy",
        h3("Private App"),
        p(
          "Dataset files are bundled privately with the App and are not offered as public downloads."
        )
      )
    },
    if (length(model$warnings %||% character())) {
      tags$section(
        class = "review-section review-needs-attention",
        h3("Needs attention"),
        tags$ul(lapply(model$warnings, tags$li))
      )
    }
  )
}
