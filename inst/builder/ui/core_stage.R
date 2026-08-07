## Guided Core stage.

builder_core_assay_controls <- function(profile, settings, assay) {
  assay_profile <- profile$assay_profiles[[assay]]
  if (!is.list(assay_profile)) {
    stop("The selected assay profile is unavailable.", call. = FALSE)
  }
  select_value <- function(current, choices, default = NULL) {
    choices <- unname(as.character(choices %||% character()))
    selected <- if (
      builder_stage_has_text(current %||% "") &&
        current %in% choices
    ) {
      current
    } else if (
      builder_stage_has_text(default %||% "") &&
        default %in% choices
    ) {
      default
    } else if (length(choices)) {
      choices[[1L]]
    } else {
      character()
    }
    list(choices = choices, selected = unname(selected))
  }
  list(
    layer = select_value(
      settings$layer,
      assay_profile$layers,
      assay_profile$default_layer
    ),
    nUMI = select_value(
      settings$nUMI,
      assay_profile$nUMI_choices,
      assay_profile$nUMI
    ),
    nGene = select_value(
      settings$nGene,
      assay_profile$nGene_choices,
      assay_profile$nGene
    )
  )
}

builder_group_colors_model <- function(
  group,
  levels,
  palette = "cerebro",
  overrides = list()
) {
  valid_group <- builder_stage_has_text(group %||% "")
  levels <- unname(as.character(levels %||% character()))
  levels <- levels[!is.na(levels) & nzchar(levels)]
  if (!valid_group || !length(levels)) {
    return(list(group = "", items = list(), total = 0L, custom_count = 0L))
  }
  group_overrides <- overrides[[group]] %||% character()
  colors <- builder_level_colors(levels, palette, group_overrides)
  custom_levels <- intersect(levels, names(group_overrides))
  items <- lapply(seq_along(levels), function(index) {
    level <- levels[[index]]
    list(
      index = index,
      key = level,
      label = builder_group_level_label(level),
      color = unname(colors[[level]]),
      custom = level %in% custom_levels
    )
  })
  list(
    group = group,
    items = items,
    total = length(items),
    custom_count = length(custom_levels)
  )
}

builder_group_colors_ui <- function(id, model) {
  ns <- NS(id)
  if (!length(model$items %||% list())) {
    return(div(
      class = "builder-group-colors is-empty",
      h3("Group colors"),
      p(
        class = "group-color-empty",
        "Select a Viewer Group to set its initial colors."
      )
    ))
  }
  searchable <- model$total > 30L
  expandable <- model$total > 12L
  div(
    class = "builder-group-colors",
    `data-group` = model$group,
    h3("Group colors"),
    p(
      class = "group-color-intro",
      paste(
        "Choose the initial colors used when plots are colored by this group.",
        "You can change them later in Color management."
      )
    ),
    p(
      class = "group-color-context",
      "Coloring by: ",
      strong(model$group)
    ),
    if (searchable) {
      tagList(
        tags$label(
          class = "group-color-search-label",
          `for` = ns("group_color_search"),
          "Find a group value"
        ),
        tags$input(
          id = ns("group_color_search"),
          type = "search",
          class = "group-color-search",
          autocomplete = "off"
        )
      )
    },
    div(
      class = paste("group-color-grid", if (expandable) "is-collapsed" else ""),
      `data-visible-limit` = "12",
      lapply(model$items, function(item) {
        input_id <- ns(paste0("group_color_", item$index))
        div(
          class = "group-color-item",
          `data-search` = tolower(item$label),
          title = item$label,
          tags$input(
            id = input_id,
            type = "color",
            class = "group-color-input",
            value = item$color,
            `data-input-id` = ns("group_color"),
            `data-group` = model$group,
            `data-level` = item$key,
            `aria-label` = paste("Color for", model$group, item$label)
          ),
          tags$label(
            class = "group-color-name",
            `for` = input_id,
            title = item$label,
            item$label
          ),
          span(class = "group-color-hex", item$color)
        )
      })
    ),
    if (expandable) {
      div(
        class = "group-color-disclosure",
        tags$button(
          type = "button",
          class = "btn group-color-toggle",
          `data-action` = "show-all",
          paste("Show all", model$total, "colors")
        ),
        tags$button(
          type = "button",
          class = "btn group-color-toggle",
          `data-action` = "show-fewer",
          hidden = "hidden",
          "Show fewer"
        )
      )
    },
    div(
      class = "group-color-reset-row",
      actionButton(
        ns("reset_colors"),
        "Reset colors",
        class = "btn group-color-reset"
      ),
      span("Restore the default palette for this group.")
    ),
    div(
      class = "visually-hidden group-color-status",
      `aria-live` = "polite",
      `aria-atomic` = "true"
    )
  )
}

builder_metadata_policy_status <- function(policy, id) {
  record <- if (
    is.list(policy) &&
      is.list(policy$columns) &&
      is.list(policy$columns[[id]])
  ) {
    policy$columns[[id]]
  } else {
    NULL
  }
  disposition <- if (
    is.list(record) &&
      builder_stage_has_text(record$disposition %||% "")
  ) {
    record$disposition
  } else {
    "unknown"
  }
  retained <- if (
    is.list(record) &&
      is.logical(record$effective_included) &&
      length(record$effective_included) == 1L &&
      !is.na(record$effective_included)
  ) {
    isTRUE(record$effective_included)
  } else {
    NA
  }
  list(
    retained = retained,
    disposition = disposition
  )
}

builder_group_catalog_model <- function(model) {
  metadata <- model$metadata_catalog %||% list()
  if (!is.list(metadata) || !length(metadata)) {
    choices <- unname(as.character(model$group_choices %||% character()))
    metadata <- lapply(choices, function(name) {
      list(
        name = name,
        classification = "categorical",
        group_eligible = TRUE,
        group_reason = NULL,
        count = NA_integer_,
        distinct_count = length(model$levels[[name]] %||% character()),
        missing_count = 0L,
        missing_percentage = 0,
        sample_values = character(),
        level_counts = list(
          items = list(),
          total = length(model$levels[[name]] %||% character()),
          truncated = FALSE,
          remaining_count = 0L
        )
      )
    })
    names(metadata) <- choices
  }
  ids <- names(metadata)
  if (is.null(ids)) {
    ids <- character()
  }
  included <- unname(as.character(
    model$included_groups %||%
      model$groups %||%
      model$default_group %||%
      character()
  ))
  included <- unique(included[!is.na(included) & nzchar(included)])
  suggested <- unname(as.character(
    model$suggested_groups %||% included
  ))
  default <- model$default_group %||% NULL

  scalar_number <- function(value, fallback = 0) {
    if (
      is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value)
    ) {
      as.numeric(value)
    } else {
      fallback
    }
  }
  type_label <- function(value) {
    switch(
      value %||% "metadata",
      categorical = "Categorical",
      continuous = "Continuous",
      logical = "Logical",
      integer = "Integer",
      character = "Text",
      factor = "Categorical",
      "Metadata"
    )
  }
  items <- lapply(seq_along(ids), function(index) {
    id <- ids[[index]]
    column <- metadata[[index]]
    metadata_status <- builder_metadata_policy_status(
      model$metadata_policy %||% list(),
      id
    )
    eligible <- isTRUE(column$group_eligible)
    category_count <- scalar_number(
      column$level_counts$total,
      scalar_number(column$distinct_count, 0)
    )
    missing <- scalar_number(column$missing_percentage, 0)
    count <- as.integer(scalar_number(column$count, 0))
    missing_count <- as.integer(scalar_number(column$missing_count, 0))
    reason <- column$group_reason %||% ""
    classification <- column$classification %||% "metadata"
    selected <- eligible && id %in% included
    list(
      index = index,
      id = id,
      label = column$name %||% id,
      classification = classification,
      type_label = type_label(classification),
      eligible = eligible,
      reason = reason,
      metadata_retained = metadata_status$retained,
      metadata_disposition = metadata_status$disposition,
      included = selected,
      default = selected && identical(id, default),
      suggested = eligible && id %in% suggested,
      category_count = as.integer(category_count),
      count = count,
      non_missing_count = as.integer(max(0L, count - missing_count)),
      missing_percentage = missing,
      sample_values = utils::head(
        as.character(column$sample_values %||% character()),
        5L
      ),
      distribution = utils::head(
        column$level_counts$items %||% list(),
        12L
      ),
      distribution_truncated = isTRUE(column$level_counts$truncated),
      remaining_count = as.integer(scalar_number(
        column$level_counts$remaining_count,
        0
      )),
      search = tolower(paste(id, classification, reason))
    )
  })
  included <- vapply(
    Filter(function(item) isTRUE(item$included), items),
    `[[`,
    character(1),
    "id"
  )
  default_valid <- builder_stage_has_text(default %||% "") &&
    default %in% included
  focus_valid <- builder_stage_has_text(default %||% "") && default %in% ids
  preview_items <- utils::head(items, 6L)
  preview_row_count <- min(
    5L,
    max(c(
      0L,
      vapply(
        preview_items,
        function(item) length(item$sample_values %||% character()),
        integer(1)
      )
    ))
  )
  preview_rows <- lapply(seq_len(preview_row_count), function(row_index) {
    vapply(
      preview_items,
      function(item) {
        values <- item$sample_values %||% character()
        value <- if (row_index <= length(values)) values[[row_index]] else ""
        if (is.na(value)) "N/A" else as.character(value)
      },
      character(1)
    )
  })
  list(
    items = items,
    included = included,
    default = if (default_valid) {
      default
    } else if (length(included)) {
      included[[1L]]
    } else {
      NULL
    },
    included_count = length(included),
    eligible_count = sum(vapply(items, `[[`, logical(1), "eligible")),
    metadata_preview = list(
      columns = vapply(preview_items, `[[`, character(1), "label"),
      rows = preview_rows,
      shown_columns = length(preview_items),
      total_columns = length(items)
    ),
    focus = if (focus_valid) {
      default
    } else if (length(ids)) {
      ids[[1L]]
    } else {
      NULL
    }
  )
}

builder_cell_cycle_catalog_model <- function(model) {
  metadata <- model$metadata_catalog %||% list()
  ids <- builder_cell_cycle_candidate_ids(metadata)
  policy <- model$metadata_policy %||% list()
  ids <- ids[vapply(
    ids,
    function(id) {
      status <- builder_metadata_policy_status(policy, id)
      !identical(status$retained, FALSE)
    },
    logical(1)
  )]
  selected <- unname(as.character(
    model$cell_cycle_columns %||% character()
  ))
  selected <- ids[ids %in% selected]
  items <- lapply(ids, function(id) {
    column <- metadata[[id]] %||% list()
    count <- column$level_counts$total %||% column$distinct_count %||% 0L
    count <- if (is.numeric(count) && length(count) == 1L && !is.na(count)) {
      as.integer(count)
    } else {
      0L
    }
    list(
      id = id,
      label = column$name %||% id,
      phase_count = count,
      included = id %in% selected
    )
  })
  list(
    items = items,
    included = selected,
    included_count = length(selected)
  )
}

builder_cell_cycle_catalog_ui <- function(id, catalog) {
  ns <- NS(id)
  choices <- vapply(
    catalog$items,
    function(item) {
      paste0(
        item$label,
        if (item$phase_count > 0L) {
          paste0(
            " · ",
            item$phase_count,
            if (item$phase_count == 1L) " phase" else " phases"
          )
        } else {
          ""
        }
      )
    },
    character(1)
  )
  values <- vapply(catalog$items, `[[`, character(1), "id")
  names(values) <- choices
  div(
    class = "viewer-cell-cycle-workspace",
    p(
      class = "viewer-cell-cycle-intro",
      paste(
        "Use categorical phase annotations in Color management and",
        "composition views."
      )
    ),
    checkboxGroupInput(
      ns("cell_cycle"),
      label = NULL,
      choices = values,
      selected = catalog$included
    )
  )
}

builder_group_detail_model <- function(catalog, group = NULL) {
  items <- catalog$items %||% list()
  ids <- vapply(items, `[[`, character(1), "id")
  if (!builder_stage_has_text(group %||% "") || !group %in% ids) {
    group <- catalog$focus %||% NULL
  }
  if (!builder_stage_has_text(group %||% "") || !group %in% ids) {
    return(list(item = NULL, metadata_preview = catalog$metadata_preview))
  }
  list(
    item = items[[match(group, ids)]],
    metadata_preview = catalog$metadata_preview
  )
}

builder_metadata_preview_ui <- function(preview, disclosure_key = NULL) {
  columns <- preview$columns %||% character()
  rows <- preview$rows %||% list()
  if (!length(columns) || !length(rows)) {
    return(NULL)
  }
  tags$details(
    class = "viewer-metadata-preview-disclosure",
    `data-disclosure-key` = disclosure_key,
    tags$summary("Preview metadata"),
    p(
      class = "viewer-metadata-preview-note",
      paste0(
        "Showing the first ",
        length(rows),
        " rows and ",
        length(columns),
        if (length(columns) == 1L) " column" else " columns",
        if ((preview$total_columns %||% length(columns)) > length(columns)) {
          paste0(" of ", preview$total_columns)
        } else {
          ""
        },
        "."
      )
    ),
    div(
      class = "viewer-metadata-preview-scroll",
      tags$table(
        class = "viewer-metadata-preview-table",
        tags$thead(tags$tr(
          tags$th(scope = "col", "Row"),
          lapply(columns, function(column) tags$th(scope = "col", column))
        )),
        tags$tbody(lapply(seq_along(rows), function(row_index) {
          tags$tr(
            class = "viewer-metadata-preview-row",
            tags$th(scope = "row", row_index),
            lapply(rows[[row_index]], function(value) {
              tags$td(title = value, value)
            })
          )
        }))
      )
    )
  )
}

builder_group_detail_ui <- function(id, model) {
  ns <- NS(id)
  item <- model$item
  if (!is.list(item)) {
    return(div(
      class = "viewer-group-detail is-empty",
      p("Choose a metadata column to see its details.")
    ))
  }
  percent <- format(
    round(item$missing_percentage, 2L),
    trim = TRUE,
    scientific = FALSE
  )
  distribution <- item$distribution %||% list()
  largest <- if (length(distribution)) {
    max(vapply(distribution, function(value) value$count, numeric(1)))
  } else {
    1
  }
  div(
    class = "viewer-group-detail",
    `data-group` = item$id,
    div(
      class = "viewer-group-detail-head",
      div(
        h4(item$label),
        p(
          item$type_label,
          span(`aria-hidden` = "true", " · "),
          paste0(
            format(item$non_missing_count, big.mark = ","),
            " non-missing"
          ),
          span(`aria-hidden` = "true", " · "),
          paste0(item$category_count, " categories"),
          span(`aria-hidden` = "true", " · "),
          paste0(percent, "% missing")
        )
      ),
      span(
        class = paste(
          "viewer-group-eligibility",
          if (item$eligible) "is-eligible" else "is-metadata-only"
        ),
        if (item$eligible) "Can be a Group" else "Not a Group"
      )
    ),
    if (
      !item$eligible &&
        (nzchar(item$reason) ||
          !is.na(item$metadata_retained))
    ) {
      div(
        class = "viewer-group-reason",
        if (nzchar(item$reason)) item$reason,
        if (isTRUE(item$metadata_retained)) {
          span(
            class = "viewer-metadata-policy is-retained",
            "Kept as ordinary metadata."
          )
        } else if (identical(item$metadata_retained, FALSE)) {
          span(
            class = "viewer-metadata-policy is-excluded",
            "Not included in the generated app."
          )
        }
      )
    },
    div(
      class = "viewer-group-detail-grid",
      tags$section(
        class = "viewer-group-distribution",
        h5("Distribution"),
        if (length(distribution)) {
          tagList(lapply(distribution, function(value) {
            count <- as.numeric(value$count)
            width <- max(3, 100 * count / largest)
            div(
              class = "viewer-group-distribution-row",
              span(class = "viewer-group-value", value$value),
              span(
                class = "viewer-group-bar-track",
                span(
                  class = "viewer-group-bar",
                  style = paste0("width:", round(width, 2L), "%")
                )
              ),
              span(class = "viewer-group-count", format(count, big.mark = ","))
            )
          }))
        } else {
          p(
            class = "viewer-group-detail-empty",
            "A categorical distribution is not shown for this column."
          )
        }
      ),
      NULL
    ),
    builder_metadata_preview_ui(
      model$metadata_preview,
      paste0("metadata-preview:", item$id)
    ),
    if (item$eligible && item$included) {
      tags$details(
        class = "viewer-group-colors-disclosure",
        `data-disclosure-key` = paste0("group-colors:", item$id),
        tags$summary("Edit colors"),
        div(
          class = "builder-group-colors-slot",
          uiOutput(ns("group_colors"))
        )
      )
    }
  )
}

builder_group_catalog_ui <- function(id, catalog) {
  ns <- NS(id)
  div(
    class = "viewer-group-workspace",
    `data-input-id` = ns("group_action"),
    `data-focus-input-id` = ns("group_focus"),
    div(
      class = "viewer-group-directory",
      div(
        class = "viewer-group-tools",
        tags$label(
          class = "visually-hidden",
          `for` = ns("group_search"),
          "Find metadata"
        ),
        tags$input(
          id = ns("group_search"),
          type = "search",
          class = "viewer-group-search",
          placeholder = "Find metadata",
          autocomplete = "off"
        ),
        div(
          class = "viewer-group-actions",
          tags$button(
            type = "button",
            class = "btn viewer-group-select",
            `data-action` = "suggested",
            "Select suggested"
          ),
          tags$button(
            type = "button",
            class = "btn viewer-group-select",
            `data-action` = "all",
            "Select all eligible"
          )
        )
      ),
      div(
        class = "viewer-group-list",
        lapply(catalog$items, function(item) {
          checkbox_id <- ns(paste0("group_include_", item$index))
          radio_id <- ns(paste0("group_default_", item$index))
          div(
            class = paste(
              "viewer-group-row",
              if (item$default) "is-focused" else "",
              if (!item$eligible) "is-ineligible" else ""
            ),
            `data-group` = item$id,
            `data-search` = item$search,
            `data-eligible` = tolower(as.character(item$eligible)),
            `data-suggested` = tolower(as.character(item$suggested)),
            if (item$eligible) {
              tags$label(
                class = "viewer-group-check",
                `for` = checkbox_id,
                tags$input(
                  id = checkbox_id,
                  type = "checkbox",
                  class = "viewer-group-include",
                  `data-group` = item$id,
                  checked = if (item$included) "checked" else NULL
                ),
                span(class = "visually-hidden", paste("Include", item$label))
              )
            } else {
              span(class = "viewer-group-check viewer-group-check-spacer")
            },
            tags$button(
              type = "button",
              class = "viewer-group-focus",
              `data-group` = item$id,
              `aria-pressed` = if (item$default) "true" else "false",
              span(class = "viewer-group-name", item$label),
              span(
                class = "viewer-group-meta",
                if (item$eligible) {
                  paste0(
                    item$category_count,
                    " categories · ",
                    format(round(item$missing_percentage, 2L), trim = TRUE),
                    "% missing"
                  )
                } else {
                  item$reason
                }
              )
            ),
            if (item$eligible) {
              tags$label(
                class = "viewer-group-default-label",
                `for` = radio_id,
                tags$input(
                  id = radio_id,
                  type = "radio",
                  name = ns("group_default_choice"),
                  class = "viewer-group-default",
                  value = item$id,
                  `data-group` = item$id,
                  checked = if (item$default) "checked" else NULL,
                  disabled = if (!item$included) "disabled" else NULL
                ),
                span(
                  class = "viewer-default-copy",
                  if (item$default) "Default" else "Set default"
                )
              )
            } else {
              span(class = "viewer-group-not-eligible", "Not a Group")
            }
          )
        })
      ),
      div(
        class = "visually-hidden viewer-group-status",
        role = "status",
        `aria-live` = "polite"
      )
    ),
    uiOutput(ns("group_detail"))
  )
}

.builder_viewer_scalar_number <- function(value, fallback = 0) {
  if (
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value)
  ) {
    as.numeric(value)
  } else {
    fallback
  }
}

.builder_projection_label <- function(item) {
  switch(
    item$kind %||% "other",
    umap = "UMAP",
    tsne = "t-SNE",
    pca = "PCA",
    item$name %||% item$id
  )
}

.builder_scatter_coordinates <- function(points, edges = NULL) {
  if (!is.data.frame(points) || !nrow(points)) {
    return(NULL)
  }
  valid <- is.finite(points$x) & is.finite(points$y)
  points <- points[valid, , drop = FALSE]
  if (!nrow(points)) {
    return(NULL)
  }
  x_values <- points$x
  y_values <- points$y
  if (is.data.frame(edges) && nrow(edges)) {
    x_values <- c(x_values, edges$x, edges$xend)
    y_values <- c(y_values, edges$y, edges$yend)
  }
  x_range <- range(x_values, finite = TRUE)
  y_range <- range(y_values, finite = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  if (!is.finite(x_span) || x_span == 0) {
    x_span <- 1
  }
  if (!is.finite(y_span) || y_span == 0) {
    y_span <- 1
  }
  project_x <- function(value) 12 + 216 * (value - x_range[[1L]]) / x_span
  project_y <- function(value) 138 - 126 * (value - y_range[[1L]]) / y_span
  list(
    points = data.frame(
      x = project_x(points$x),
      y = project_y(points$y),
      group = as.character(points$group %||% rep("cells", nrow(points))),
      stringsAsFactors = FALSE
    ),
    edges = if (is.data.frame(edges) && nrow(edges)) {
      data.frame(
        x = project_x(edges$x),
        y = project_y(edges$y),
        xend = project_x(edges$xend),
        yend = project_y(edges$yend)
      )
    } else {
      data.frame(
        x = numeric(),
        y = numeric(),
        xend = numeric(),
        yend = numeric()
      )
    }
  )
}

builder_scatter_thumbnail_ui <- function(
  points,
  colors = NULL,
  point_size = 5,
  label = "Coordinate preview",
  class = "viewer-scatter-preview",
  edges = NULL
) {
  coordinates <- .builder_scatter_coordinates(points, edges)
  if (is.null(coordinates)) {
    return(div(
      class = paste(class, "is-loading"),
      span("Loading preview…")
    ))
  }
  groups <- coordinates$points$group
  groups[is.na(groups) | !nzchar(groups)] <- "N/A"
  levels_present <- unique(groups)
  palette <- builder_preview_palette(length(levels_present))
  names(palette) <- levels_present
  if (length(colors)) {
    shared <- intersect(names(colors), levels_present)
    palette[shared] <- as.character(colors[shared])
  }
  if ("N/A" %in% levels_present) {
    palette[["N/A"]] <- "#898989"
  }
  radius <- max(
    0,
    min(
      4.5,
      .builder_viewer_scalar_number(
        point_size,
        5
      ) *
        0.34
    )
  )
  tags$svg(
    class = class,
    viewBox = "0 0 240 150",
    preserveAspectRatio = "xMidYMid meet",
    role = "img",
    `aria-label` = label,
    `data-point-size` = format(point_size, trim = TRUE),
    if (nrow(coordinates$edges)) {
      lapply(seq_len(nrow(coordinates$edges)), function(index) {
        edge <- coordinates$edges[index, ]
        tags$line(
          x1 = round(edge$x, 2L),
          y1 = round(edge$y, 2L),
          x2 = round(edge$xend, 2L),
          y2 = round(edge$yend, 2L),
          class = "viewer-trajectory-edge"
        )
      })
    },
    lapply(seq_len(nrow(coordinates$points)), function(index) {
      point <- coordinates$points[index, ]
      tags$circle(
        cx = round(point$x, 2L),
        cy = round(point$y, 2L),
        r = round(radius, 2L),
        fill = unname(palette[[groups[[index]]]]),
        class = "viewer-scatter-point"
      )
    })
  )
}

builder_projection_catalog_model <- function(model) {
  catalog <- model$projection_catalog %||% list()
  if (!is.list(catalog) || !length(catalog)) {
    ids <- unname(as.character(model$projection_choices %||% character()))
    catalog <- lapply(ids, function(id) {
      list(
        id = id,
        name = id,
        kind = if (grepl("pca", id, ignore.case = TRUE)) "pca" else "other",
        dimensions = 2L,
        cell_count = NA_integer_,
        available = TRUE
      )
    })
    names(catalog) <- ids
  }
  ids <- names(catalog)
  included <- unique(as.character(
    model$included_projections %||% model$default_projection %||% character()
  ))
  included <- included[!is.na(included) & nzchar(included)]
  default <- model$default_projection %||% NULL
  previews <- model$projection_previews %||% list()
  items <- lapply(seq_along(catalog), function(index) {
    projection <- catalog[[index]]
    id <- projection$id %||% ids[[index]]
    available <- isTRUE(projection$available)
    selected <- available && id %in% included
    item <- list(
      index = index,
      id = id,
      name = projection$name %||% id,
      kind = projection$kind %||% "other",
      dimensions = as.integer(.builder_viewer_scalar_number(
        projection$dimensions,
        0
      )),
      cell_count = as.integer(.builder_viewer_scalar_number(
        projection$cell_count,
        0
      )),
      available = available,
      reason = projection$reason %||% NULL,
      included = selected,
      default = selected && identical(id, default),
      preview = previews[[id]] %||% NULL
    )
    item$label <- .builder_projection_label(item)
    item
  })
  selected_ids <- vapply(
    Filter(function(item) isTRUE(item$included), items),
    `[[`,
    character(1),
    "id"
  )
  if (!default %in% selected_ids) {
    default <- if (length(selected_ids)) selected_ids[[1L]] else NULL
  }
  list(
    items = items,
    included = selected_ids,
    included_count = length(selected_ids),
    default = default,
    point_size = .builder_viewer_scalar_number(
      model$overview_point_size,
      5
    ),
    colors = model$preview_colors %||% character()
  )
}

builder_projection_catalog_ui <- function(id, model) {
  ns <- NS(id)
  div(
    class = "viewer-projection-workspace",
    `data-input-id` = ns("projection_action"),
    div(
      class = "viewer-projection-toolbar",
      tags$label(
        `for` = ns("overview_point_size"),
        span("Initial point size"),
        span(
          class = "viewer-point-size-value",
          format(model$point_size, trim = TRUE)
        )
      ),
      tags$input(
        id = ns("overview_point_size"),
        class = "viewer-point-size-input",
        type = "range",
        min = "0",
        max = "20",
        step = "1",
        value = format(model$point_size, trim = TRUE),
        `data-input-id` = ns("point_size")
      )
    ),
    div(
      class = "viewer-projection-gallery",
      lapply(model$items, function(item) {
        checkbox_id <- ns(paste0("projection_include_", item$index))
        radio_id <- ns(paste0("projection_default_", item$index))
        tags$article(
          class = paste(
            "viewer-projection-card",
            if (item$included) "is-included" else "",
            if (!item$available) "is-unavailable" else ""
          ),
          `data-projection` = item$id,
          div(
            class = "viewer-card-choice-row",
            tags$label(
              class = "viewer-card-include",
              `for` = checkbox_id,
              tags$input(
                id = checkbox_id,
                type = "checkbox",
                class = "viewer-projection-include",
                `data-projection` = item$id,
                checked = if (item$included) "checked" else NULL,
                disabled = if (!item$available) "disabled" else NULL
              ),
              span("Include")
            ),
            tags$label(
              class = "viewer-card-default",
              `for` = radio_id,
              tags$input(
                id = radio_id,
                type = "radio",
                name = ns("projection_default_choice"),
                class = "viewer-projection-default",
                `data-projection` = item$id,
                value = item$id,
                checked = if (item$default) "checked" else NULL,
                disabled = if (!item$included) "disabled" else NULL
              ),
              span(
                class = "viewer-default-copy",
                if (item$default) "Default" else "Set default"
              )
            )
          ),
          h4(item$label),
          p(
            class = "viewer-card-meta",
            paste0(format(item$cell_count, big.mark = ","), " points"),
            span(`aria-hidden` = "true", " · "),
            paste0(item$dimensions, " dimensions")
          ),
          builder_scatter_thumbnail_ui(
            item$preview,
            colors = model$colors,
            point_size = model$point_size,
            label = paste(item$label, "projection preview"),
            class = "viewer-projection-preview"
          ),
          if (!item$available && nzchar(item$reason %||% "")) {
            p(class = "viewer-card-reason", item$reason)
          }
        )
      })
    ),
    div(
      class = "visually-hidden viewer-projection-status",
      role = "status",
      `aria-live` = "polite"
    )
  )
}

.builder_trajectory_selected <- function(included, method, name) {
  is.list(included) &&
    method %in% names(included) &&
    name %in% as.character(included[[method]])
}

builder_trajectory_catalog_model <- function(model) {
  catalog <- model$trajectory_catalog %||% list()
  included <- model$included_trajectories %||% list()
  default <- model$default_trajectory %||% NULL
  previews <- model$trajectory_previews %||% list()
  items <- lapply(seq_along(catalog), function(index) {
    trajectory <- catalog[[index]]
    method <- trajectory$method %||% ""
    name <- trajectory$name %||% ""
    key <- .builder_trajectory_preview_key(method, name)
    selectable <- isTRUE(trajectory$selectable)
    selected <- selectable &&
      .builder_trajectory_selected(
        included,
        method,
        name
      )
    list(
      index = index,
      key = key,
      method = method,
      name = name,
      selectable = selectable,
      included = selected,
      default = selected &&
        is.list(default) &&
        identical(default$method, method) &&
        identical(default$name, name),
      cell_count = as.integer(.builder_viewer_scalar_number(
        trajectory$cell_count,
        0
      )),
      coverage = .builder_viewer_scalar_number(trajectory$coverage, 0),
      state_count = as.integer(.builder_viewer_scalar_number(
        trajectory$state_count,
        0
      )),
      edge_count = as.integer(.builder_viewer_scalar_number(
        trajectory$edge_count,
        0
      )),
      reason = trajectory$reason %||% NULL,
      preview = previews[[key]] %||% NULL
    )
  })
  selected <- Filter(function(item) isTRUE(item$included), items)
  default_item <- Filter(function(item) isTRUE(item$default), selected)
  if (!length(default_item) && length(selected)) {
    default <- list(
      method = selected[[1L]]$method,
      name = selected[[1L]]$name
    )
    items[[selected[[1L]]$index]]$default <- TRUE
  }
  list(
    items = items,
    included_count = length(selected),
    selectable_count = sum(vapply(items, `[[`, logical(1), "selectable")),
    default = default
  )
}

builder_trajectory_catalog_ui <- function(id, model) {
  ns <- NS(id)
  div(
    class = "viewer-trajectory-workspace",
    `data-input-id` = ns("trajectory_action"),
    div(
      class = "viewer-trajectory-gallery",
      lapply(model$items, function(item) {
        checkbox_id <- ns(paste0("trajectory_include_", item$index))
        radio_id <- ns(paste0("trajectory_default_", item$index))
        points <- if (is.list(item$preview)) item$preview$points else NULL
        edges <- if (is.list(item$preview)) item$preview$edges else NULL
        tags$article(
          class = paste(
            "viewer-trajectory-card",
            if (item$included) "is-included" else "",
            if (!item$selectable) "is-unavailable" else ""
          ),
          `data-trajectory-key` = item$key,
          `data-method` = item$method,
          `data-trajectory` = item$name,
          div(
            class = "viewer-card-choice-row",
            tags$label(
              class = "viewer-card-include",
              `for` = checkbox_id,
              tags$input(
                id = checkbox_id,
                type = "checkbox",
                class = "viewer-trajectory-include",
                `data-method` = item$method,
                `data-trajectory` = item$name,
                checked = if (item$included) "checked" else NULL,
                disabled = if (!item$selectable) "disabled" else NULL
              ),
              span("Include")
            ),
            tags$label(
              class = "viewer-card-default",
              `for` = radio_id,
              tags$input(
                id = radio_id,
                type = "radio",
                name = ns("trajectory_default_choice"),
                class = "viewer-trajectory-default",
                `data-method` = item$method,
                `data-trajectory` = item$name,
                value = item$key,
                checked = if (item$default) "checked" else NULL,
                disabled = if (!item$included) "disabled" else NULL
              ),
              span(
                class = "viewer-default-copy",
                if (item$default) "Default" else "Set default"
              )
            )
          ),
          h4(item$name),
          p(class = "viewer-card-method", item$method),
          p(
            class = "viewer-card-meta",
            paste0(format(item$cell_count, big.mark = ","), " cells"),
            span(`aria-hidden` = "true", " · "),
            paste0(item$state_count, " states"),
            span(`aria-hidden` = "true", " · "),
            paste0(round(100 * item$coverage), "% coverage")
          ),
          if (item$selectable) {
            builder_scatter_thumbnail_ui(
              points,
              point_size = 4,
              label = paste(item$name, "trajectory preview"),
              class = "viewer-trajectory-preview",
              edges = edges
            )
          },
          if (!item$selectable && nzchar(item$reason %||% "")) {
            p(class = "viewer-card-reason", item$reason)
          }
        )
      })
    ),
    div(
      class = "visually-hidden viewer-trajectory-status",
      role = "status",
      `aria-live` = "polite"
    )
  )
}

builder_specialized_content_model <- function(model) {
  manifest <- model$content_manifest %||%
    model$analysis_manifest %||%
    model$manifest %||%
    list()
  acknowledgements <- model$content_acknowledgements %||%
    model$analysis_acknowledgements %||%
    model$acknowledgements %||%
    character()
  count_value <- function(value) {
    if (
      is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value) &&
        value >= 0
    ) {
      as.integer(value)
    } else {
      0L
    }
  }
  plural <- function(value, singular, plural_form = paste0(singular, "s")) {
    paste0(
      value,
      " ",
      if (identical(value, 1L)) singular else plural_form
    )
  }
  record_state <- function(record) {
    action <- record$required_action %||% list()
    disposition <- record$disposition %||% ""
    acknowledged <- identical(record$status %||% "", "attention") &&
      identical(action$type %||% "", "acknowledge") &&
      (action$token %||% "") %in% acknowledgements
    if (
      disposition %in%
        c("filtered", "stored_only") ||
        identical(record$status %||% "", "not_applicable")
    ) {
      return("not_included")
    }
    if (
      !acknowledged &&
        (record$status %in%
          c("attention", "blocking") ||
          identical(disposition, "rejected"))
    ) {
      return("needs_attention")
    }
    if (
      (identical(record$status %||% "", "valid") || acknowledged) &&
        disposition %in% c("preserved", "generated", "converted", "attached")
    ) {
      return("included")
    }
    "not_included"
  }
  content_item <- function(
    id,
    label,
    metrics,
    message,
    directory = character(),
    attention_message = NULL
  ) {
    record <- manifest[[id]]
    if (!is.list(record)) {
      return(NULL)
    }
    evidence <- record$evidence %||% list()
    if (!isTRUE(evidence$detected)) {
      return(NULL)
    }
    state <- record_state(record)
    if (identical(state, "needs_attention")) {
      message <- attention_message %||%
        paste(
          "This content cannot be used yet.",
          "Review the source data before building."
        )
    } else if (identical(state, "not_included")) {
      message <- "This content will not be included in the generated app."
    }
    list(
      id = id,
      label = label,
      state = state,
      metrics = metrics,
      message = message,
      directory = directory
    )
  }
  spatial_record <- manifest$spatial %||% list()
  spatial <- spatial_record$evidence$normalized %||% list()
  spatial_sections <- spatial$sections %||% list()
  tissue_image_count <- as.integer(sum(vapply(
    spatial_sections,
    function(section) {
      is.list(section) &&
        is.list(section$raster) &&
        isTRUE(section$raster$present) &&
        isTRUE(section$raster$valid)
    },
    logical(1)
  )))
  spatial_metrics <- c(
    plural(count_value(spatial$section_count), "section"),
    paste(count_value(spatial$valid_section_count), "ready"),
    if (isTRUE(spatial$sections_truncated)) {
      paste0(">=", tissue_image_count, " tissue images in preview")
    } else {
      plural(tissue_image_count, "tissue image")
    }
  )

  trekker_record <- manifest$trekker %||% list()
  trekker <- trekker_record$evidence$normalized %||% list()
  coverage <- trekker$barcode_coverage %||% 0
  if (
    !is.numeric(coverage) ||
      length(coverage) != 1L ||
      is.na(coverage) ||
      !is.finite(coverage)
  ) {
    coverage <- 0
  }
  coverage <- min(1, max(0, coverage))
  trekker_metrics <- c(
    plural(count_value(trekker$cell_count), "cell"),
    plural(count_value(trekker$cluster_count), "cluster"),
    plural(count_value(trekker$field_count), "field"),
    paste0(round(100 * coverage), "% coverage"),
    if (count_value(trekker$moran_count) > 0L) {
      plural(count_value(trekker$moran_count), "Moran result")
    },
    if (count_value(trekker$evidence_count) > 0L) {
      plural(count_value(trekker$evidence_count), "evidence image")
    }
  )

  immune_record <- manifest$immune_repertoire %||% list()
  immune_evidence <- immune_record$evidence %||% list()
  immune <- immune_evidence$normalized %||% list()
  selected_immune <- immune_evidence$selected_candidate$normalized %||%
    list()
  chains <- selected_immune$chains %||% immune$chains %||% character()
  immune_diagnostics <- unique(c(
    immune_evidence$diagnostics %||% character(),
    immune_evidence$selected_candidate$diagnostics %||% character()
  ))
  immune_attention_message <- if (
    "no_dataset_barcode_overlap" %in% immune_diagnostics
  ) {
    paste(
      "Immune cell barcodes do not match this dataset.",
      "Check barcode prefixes or choose the matching dataset."
    )
  } else if ("barcodes_outside_dataset" %in% immune_diagnostics) {
    paste(
      "Some immune cell barcodes do not match this dataset.",
      "Check barcode prefixes before building."
    )
  } else if ("duplicate_barcodes" %in% immune_diagnostics) {
    "Immune cell barcodes are duplicated. Make them unique before building."
  } else if (
    any(
      c(
        "divergent_source_overlap",
        "incomplete_source_equivalence",
        "unverified_source_equivalence"
      ) %in%
        immune_diagnostics
    )
  ) {
    paste(
      "Multiple immune-data sources disagree.",
      "Review the source data before building."
    )
  } else {
    NULL
  }
  immune_metrics <- c(
    plural(count_value(selected_immune$n_rows %||% immune$n_rows), "record"),
    plural(
      count_value(selected_immune$n_samples %||% immune$n_samples),
      "sample"
    ),
    plural(length(unique(as.character(chains))), "chain")
  )

  hla_record <- manifest$hla %||% list()
  hla <- hla_record$evidence$normalized %||% list()
  motif_record <- manifest$hla_tcr_motifs %||% list()
  motif_evidence <- motif_record$evidence %||% list()
  motif_ready <- isTRUE(motif_evidence$detected) &&
    identical(record_state(motif_record), "included") &&
    "hla_tcr_motifs" %in% (motif_record$pages %||% character())
  hla_metrics <- c(
    plural(count_value(hla$n_samples), "sample"),
    plural(count_value(hla$n_loci), "locus", "loci"),
    plural(count_value(hla$n_alleles), "allele")
  )

  extra_record <- manifest$extra_material %||% list()
  extra <- extra_record$evidence$normalized %||% list()
  table_names <- names(extra$tables %||% list()) %||% character()
  plot_names <- names(extra$plots %||% list()) %||% character()
  friendly_name <- function(value) {
    value <- gsub("[_.]+", " ", value)
    if (tolower(value) %in% c("qc", "hla", "tcr", "bcr", "pca", "umap")) {
      return(toupper(value))
    }
    paste0(toupper(substr(value, 1L, 1L)), substr(value, 2L, nchar(value)))
  }
  directory_line <- function(label, values) {
    if (!length(values)) {
      return(character())
    }
    shown <- utils::head(vapply(values, friendly_name, character(1)), 3L)
    suffix <- if (length(values) > length(shown)) {
      paste0(" +", length(values) - length(shown), " more")
    } else {
      ""
    }
    paste0(label, ": ", paste(shown, collapse = ", "), suffix)
  }

  items <- Filter(
    Negate(is.null),
    list(
      content_item(
        "spatial",
        "Spatial",
        spatial_metrics,
        "Available on the Spatial page."
      ),
      content_item(
        "trekker",
        "Trekker",
        trekker_metrics,
        paste(
          "Paired transcriptome and physical coordinates are available",
          "on the Trekker page."
        )
      ),
      content_item(
        "immune_repertoire",
        "Immune repertoire",
        immune_metrics,
        "Available on the Immune repertoire page.",
        attention_message = immune_attention_message
      ),
      content_item(
        "hla",
        "HLA",
        hla_metrics,
        if (motif_ready) {
          "Supports the HLA & TCR motifs page."
        } else {
          "Supporting HLA typing will be preserved with this dataset."
        }
      ),
      content_item(
        "extra_material",
        "Extra material",
        c(
          plural(length(table_names), "table"),
          plural(length(plot_names), "plot")
        ),
        "Available on the Extra material page.",
        c(
          directory_line("Tables", table_names),
          directory_line("Plots", plot_names)
        )
      )
    )
  )
  states <- if (length(items)) {
    vapply(items, `[[`, character(1), "state")
  } else {
    character()
  }
  included_count <- as.integer(sum(states == "included"))
  attention_count <- as.integer(sum(states == "needs_attention"))
  excluded_count <- as.integer(sum(states == "not_included"))
  summary <- c(
    if (included_count > 0L) paste(included_count, "included"),
    if (attention_count > 0L) {
      paste(
        attention_count,
        if (identical(attention_count, 1L)) {
          "needs attention"
        } else {
          "need attention"
        }
      )
    },
    if (excluded_count > 0L) paste(excluded_count, "not included")
  )
  list(
    items = unname(items),
    total_count = as.integer(length(items)),
    included_count = included_count,
    attention_count = attention_count,
    excluded_count = excluded_count,
    summary = paste(summary, collapse = " · ")
  )
}

builder_specialized_content_ui <- function(model) {
  badge <- function(state) {
    switch(
      state,
      included = "Included",
      needs_attention = "Needs attention",
      not_included = "Not included",
      "Available"
    )
  }
  div(
    class = "viewer-specialized-content",
    lapply(model$items %||% list(), function(item) {
      tags$article(
        class = paste(
          "viewer-specialized-item",
          paste0("is-", gsub("_", "-", item$state))
        ),
        div(
          class = "viewer-specialized-head",
          h4(item$label),
          span(class = "viewer-specialized-badge", badge(item$state))
        ),
        p(
          class = "viewer-specialized-metrics",
          paste(item$metrics, collapse = " · ")
        ),
        lapply(item$directory, function(line) {
          p(class = "viewer-specialized-directory", line)
        }),
        p(class = "viewer-specialized-page", item$message)
      )
    })
  )
}

builder_analysis_results_model <- function(model) {
  manifest <- model$analysis_manifest %||% model$manifest %||% list()
  acknowledgements <- model$analysis_acknowledgements %||%
    model$acknowledgements %||%
    character()
  specs <- list(
    marker_genes = list(
      label = "Marker genes",
      page = "Marker genes",
      nested = TRUE
    ),
    most_expressed_genes = list(
      label = "Most expressed genes",
      page = "Most expressed genes",
      nested = FALSE
    ),
    mean_expression = list(
      label = "Mean expression",
      page = "Most expressed genes",
      nested = FALSE
    ),
    enriched_pathways = list(
      label = "Enriched pathways",
      page = "Enriched pathways",
      nested = TRUE
    )
  )
  named_values <- function(value) {
    if (!is.list(value) || !length(value)) {
      return(character())
    }
    ids <- names(value)
    if (is.null(ids)) character() else ids[nzchar(ids)]
  }
  count_tables <- function(values) {
    if (!is.list(values) || !length(values)) {
      return(0L)
    }
    as.integer(sum(vapply(
      values,
      function(value) is.list(value) && identical(value$kind, "table"),
      logical(1)
    )))
  }
  summarize <- function(id, spec) {
    record <- manifest[[id]]
    if (!is.list(record)) {
      return(NULL)
    }
    evidence <- record$evidence %||% list()
    detected <- isTRUE(evidence$detected)
    disposition <- record$disposition %||% NA_character_
    action <- record$required_action
    acknowledged <- identical(record$status, "attention") &&
      is.list(action) &&
      identical(action$type, "acknowledge") &&
      action$token %in% acknowledgements
    needs_attention <- !acknowledged &&
      (identical(record$status, "attention") ||
        identical(record$status, "blocking") ||
        identical(disposition, "rejected"))
    generated <- identical(disposition, "generated")
    excluded <- disposition %in% c("filtered", "stored_only")
    if (!detected && !generated && !needs_attention && !excluded) {
      return(NULL)
    }
    normalized <- evidence$normalized %||% list()
    if (!is.list(normalized)) {
      normalized <- list()
    }
    if (isTRUE(spec$nested)) {
      method_count <- length(named_values(normalized))
      group_names <- unique(unlist(
        lapply(normalized, named_values),
        use.names = FALSE
      ))
      leaves <- unlist(
        lapply(normalized, function(value) {
          if (is.list(value)) unname(value) else list()
        }),
        recursive = FALSE,
        use.names = FALSE
      )
    } else {
      method_count <- 0L
      group_names <- named_values(normalized)
      leaves <- unname(normalized)
    }
    status <- if (needs_attention) {
      "needs_attention"
    } else if (generated) {
      "will_be_generated"
    } else if (excluded) {
      "not_included"
    } else {
      "existing"
    }
    list(
      id = id,
      label = spec$label,
      page = spec$page,
      page_message = if (needs_attention) {
        paste0(
          "The ",
          spec$page,
          " page stays unavailable until this is fixed."
        )
      } else if (excluded) {
        "This result will not be included in the generated app."
      } else if (identical(id, "mean_expression")) {
        paste(
          "Used by the Most expressed genes page",
          "when that page is available."
        )
      } else {
        paste0("Shown on the ", spec$page, " page.")
      },
      status = status,
      method_count = as.integer(method_count),
      group_count = as.integer(length(group_names)),
      table_count = count_tables(leaves)
    )
  }
  items <- Filter(
    Negate(is.null),
    Map(summarize, names(specs), specs)
  )
  statuses <- if (length(items)) {
    vapply(items, `[[`, character(1), "status")
  } else {
    character()
  }
  list(
    items = unname(items),
    total_count = as.integer(length(items)),
    existing_count = as.integer(sum(statuses == "existing")),
    generated_count = as.integer(sum(statuses == "will_be_generated")),
    attention_count = as.integer(sum(statuses == "needs_attention")),
    excluded_count = as.integer(sum(statuses == "not_included"))
  )
}

builder_analysis_results_ui <- function(model) {
  plural <- function(value, singular) {
    paste0(value, " ", singular, if (identical(value, 1L)) "" else "s")
  }
  status_label <- function(status) {
    switch(
      status,
      existing = "Existing",
      will_be_generated = "Will be generated",
      needs_attention = "Needs attention",
      not_included = "Not included",
      "Available"
    )
  }
  div(
    class = "viewer-analysis-results",
    lapply(model$items %||% list(), function(item) {
      metrics <- c(
        if (item$method_count > 0L) plural(item$method_count, "method"),
        plural(item$group_count, "group"),
        plural(item$table_count, "table")
      )
      tags$article(
        class = paste("viewer-analysis-result", paste0("is-", item$status)),
        div(
          class = "viewer-analysis-result-head",
          h4(item$label),
          span(
            class = "viewer-analysis-result-status",
            status_label(item$status)
          )
        ),
        if (item$status == "needs_attention") {
          p(
            class = "viewer-analysis-result-action",
            "This result cannot be used yet. Recompute it or review its source."
          )
        } else if (item$status == "will_be_generated") {
          p(
            class = "viewer-analysis-result-metrics",
            "Created during build."
          )
        } else if (item$status == "not_included") {
          p(
            class = "viewer-analysis-result-metrics",
            "Excluded from the generated app."
          )
        } else {
          p(
            class = "viewer-analysis-result-metrics",
            paste(metrics, collapse = " · ")
          )
        },
        p(
          class = "viewer-analysis-result-page",
          item$page_message
        )
      )
    })
  )
}

builder_core_stage_ui <- function(id, model) {
  ns <- NS(id)
  organism_choices <- model$organism_choices %||% character()
  organism <- model$organism %||% ""
  if (
    builder_stage_has_text(organism) &&
      !organism %in% unname(organism_choices)
  ) {
    organism_choices <- c(organism_choices, organism)
  }
  group_catalog <- builder_group_catalog_model(model)
  cell_cycle_catalog <- builder_cell_cycle_catalog_model(model)
  projection_catalog <- builder_projection_catalog_model(model)
  trajectory_catalog <- builder_trajectory_catalog_model(model)
  analysis_results <- builder_analysis_results_model(model)
  specialized_content <- builder_specialized_content_model(model)
  projection_default <- Filter(
    function(item) identical(item$id, projection_catalog$default),
    projection_catalog$items
  )
  projection_default_label <- if (length(projection_default)) {
    projection_default[[1L]]$label
  } else {
    "None"
  }
  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-core builder-card builder-section",
    h2("Core"),
    tags$input(
      id = ns("rendered_for"),
      type = "text",
      class = "builder-rendered-for-input",
      value = model$id,
      hidden = "hidden",
      tabindex = "-1",
      `aria-hidden` = "true"
    ),
    div(
      class = "builder-form-grid",
      div(
        class = "builder-field builder-field--name",
        textInput(ns("name"), "Dataset name", value = model$name %||% "")
      ),
      div(
        class = "builder-field builder-field--organism",
        selectizeInput(
          ns("organism"),
          "Organism",
          choices = organism_choices,
          selected = organism,
          options = list(
            create = TRUE,
            persist = TRUE,
            maxItems = 1L
          )
        )
      )
    ),
    tags$section(
      class = "builder-viewer-content",
      div(
        class = "builder-viewer-content-head",
        h3("Viewer content"),
        p("Choose what the generated app includes and how it opens.")
      ),
      tags$details(
        class = "builder-viewer-card builder-viewer-groups",
        `data-disclosure-key` = "viewer-groups",
        tags$summary(
          span(class = "builder-viewer-card-title", "Groups"),
          span(
            class = "builder-viewer-card-count",
            `data-viewer-group-count` = "true",
            paste0(
              group_catalog$included_count,
              " included · ",
              "Default: ",
              group_catalog$default %||% "None"
            )
          )
        ),
        div(
          class = "builder-viewer-card-body",
          builder_group_catalog_ui(id, group_catalog)
        )
      ),
      if (length(cell_cycle_catalog$items)) {
        tags$details(
          class = "builder-viewer-card builder-viewer-cell-cycle",
          `data-disclosure-key` = "viewer-cell-cycle",
          tags$summary(
            span(class = "builder-viewer-card-title", "Cell cycle"),
            span(
              class = "builder-viewer-card-count",
              paste(cell_cycle_catalog$included_count, "included")
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_cell_cycle_catalog_ui(id, cell_cycle_catalog)
          )
        )
      },
      tags$details(
        class = "builder-viewer-card builder-viewer-projections",
        `data-disclosure-key` = "viewer-projections",
        tags$summary(
          span(class = "builder-viewer-card-title", "Projections"),
          span(
            class = "builder-viewer-card-count",
            `data-viewer-projection-count` = "true",
            paste0(
              projection_catalog$included_count,
              " included · ",
              "Default: ",
              projection_default_label
            )
          )
        ),
        div(
          class = "builder-viewer-card-body",
          uiOutput(ns("projection_gallery"))
        )
      ),
      if (length(trajectory_catalog$items)) {
        tags$details(
          class = "builder-viewer-card builder-viewer-trajectories",
          `data-disclosure-key` = "viewer-trajectories",
          tags$summary(
            span(class = "builder-viewer-card-title", "Trajectories"),
            span(
              class = "builder-viewer-card-count",
              `data-viewer-trajectory-count` = "true",
              paste0(
                trajectory_catalog$included_count,
                " included",
                if (is.list(trajectory_catalog$default)) {
                  paste0(
                    " · ",
                    "Default: ",
                    trajectory_catalog$default$name
                  )
                } else {
                  ""
                }
              )
            )
          ),
          div(
            class = "builder-viewer-card-body",
            uiOutput(ns("trajectory_gallery"))
          )
        )
      },
      if (analysis_results$total_count > 0L) {
        tags$details(
          class = "builder-viewer-card builder-viewer-analysis-results",
          `data-disclosure-key` = "viewer-analysis-results",
          tags$summary(
            span(class = "builder-viewer-card-title", "Analysis results"),
            span(
              class = "builder-viewer-card-count",
              paste(analysis_results$total_count, "detected or planned")
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_analysis_results_ui(analysis_results)
          )
        )
      },
      if (specialized_content$total_count > 0L) {
        tags$details(
          class = "builder-viewer-card builder-viewer-specialized-content",
          `data-disclosure-key` = "viewer-specialized-content",
          tags$summary(
            span(class = "builder-viewer-card-title", "Specialized content"),
            span(
              class = "builder-viewer-card-count",
              specialized_content$summary
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_specialized_content_ui(specialized_content)
          )
        )
      }
    ),
    if (builder_stage_has_text(model$metadata_attention %||% "")) {
      div(class = "notice warn", model$metadata_attention)
    },
    tags$details(
      class = "builder-disclosure",
      tags$summary("Advanced settings"),
      div(
        class = "builder-advanced-grid",
        selectInput(
          ns("assay"),
          "Assay",
          choices = model$assay_choices,
          selected = model$assay
        ),
        selectInput(
          ns("layer"),
          "Layer",
          choices = model$layer_choices,
          selected = model$layer
        ),
        selectInput(
          ns("nUMI"),
          "UMI QC column",
          choices = model$nUMI_choices,
          selected = model$nUMI
        ),
        selectInput(
          ns("nGene"),
          "Gene QC column",
          choices = model$nGene_choices,
          selected = model$nGene
        ),
        selectInput(
          ns("backend"),
          "Expression backend",
          choices = model$backend_choices,
          selected = model$backend
        )
      )
    )
  )
}
