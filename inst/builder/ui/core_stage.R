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
        "Choose a categorical default group to set initial colors."
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
      class = "sr-only group-color-status",
      `aria-live` = "polite",
      `aria-atomic` = "true"
    )
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
  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-core builder-card builder-section",
    h2("Core"),
    tags$input(
      id = ns("rendered_for"),
      type = "hidden",
      value = model$id
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
      ),
      div(
        class = "builder-field builder-field--default-group",
        selectInput(
          ns("default_group"),
          "Default group",
          choices = model$group_choices,
          selected = model$default_group
        )
      ),
      div(
        class = "builder-group-colors-slot",
        uiOutput(ns("group_colors"))
      ),
      div(
        class = "builder-field builder-field--default-projection",
        selectInput(
          ns("default_projection"),
          "Default projection",
          choices = model$projection_choices,
          selected = model$default_projection
        )
      )
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
