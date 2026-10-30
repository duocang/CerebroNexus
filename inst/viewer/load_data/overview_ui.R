viewerOverviewNumber <- function(value) {
  formatC(value, format = "f", digits = 0, big.mark = ",")
}

# The fill of each bar is the colour its category has on the other pages.
# `colors` is a vector named by category; a category without a usable entry
# keeps the stylesheet's accent colour. Only a hex colour, an R colour name or
# rgb[a]()/hsl[a]() of plain numbers is accepted, because the value is written
# into a style attribute: url() and expression() never get through.
viewerOverviewFills <- function(labels, colors) {
  if (is.null(colors) || is.null(names(colors))) {
    return(rep("", length(labels)))
  }
  picked <- unname(as.character(colors[labels]))
  usable <- !is.na(picked) &
    (grepl("^#([0-9A-Fa-f]{3,4}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", picked) |
      grepl("^(rgb|hsl)a?\\([0-9., %]+\\)$", picked) |
      picked %in% grDevices::colors())
  ifelse(usable, paste0(";background:", picked), "")
}

viewerOverviewBars <- function(summary, colors = NULL) {
  if (is.null(summary) || !length(summary$counts)) {
    return(NULL)
  }
  counts <- unname(summary$counts)
  labels <- names(summary$counts)
  percent <- 100 * counts / summary$total
  fills <- viewerOverviewFills(labels, colors)
  # A sample column can have hundreds of categories. One vectorised string per
  # row renders them several times faster than one tag tree per row.
  rows <- paste0(
    '<div class="dataset-bar-row">\n',
    '  <span class="dataset-bar-label" title="',
    htmltools::htmlEscape(labels, attribute = TRUE),
    '">',
    htmltools::htmlEscape(labels),
    '</span>\n',
    '  <span class="dataset-bar-track" aria-hidden="true">\n',
    '    <span style="width:',
    vapply(percent, format, character(1), scientific = FALSE, trim = TRUE),
    '%',
    fills,
    '"></span>\n',
    '  </span>\n',
    '  <span class="dataset-bar-value">\n',
    '    ',
    viewerOverviewNumber(counts),
    '\n',
    '    <small>',
    sprintf("%.1f%%", percent),
    '</small>\n',
    '  </span>\n',
    '</div>'
  )
  shiny::tags$div(
    class = "dataset-bars",
    htmltools::HTML(paste(rows, collapse = "\n")),
    if (summary$missing > 0) {
      shiny::tags$p(
        class = "dataset-note",
        paste(
          viewerOverviewNumber(summary$missing),
          "cells without an annotation are excluded."
        )
      )
    }
  )
}

viewerOverviewUI <- function(info) {
  tags <- shiny::tags
  overview <- info$overview
  number <- function(value, label) {
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
      return(NULL)
    }
    tags$div(
      class = "dataset-stat",
      id = if (label == "Cells") "load_data_number_of_cells" else NULL,
      tags$strong(viewerOverviewNumber(value)),
      tags$span(label)
    )
  }
  organism <- viewerOverviewText(info$organism)
  if (!is.null(organism)) {
    organism <- switch(organism, hg = "Human", mm = "Mouse", organism)
  }
  date <- viewerOverviewText(info$date)
  annotations <- overview$annotations
  links <- list()
  add_link <- function(target, title, detail) {
    links[[length(links) + 1L]] <<- shiny::actionLink(
      paste0("dataset_open_", target),
      label = tags$span(
        tags$strong(title),
        tags$small(detail),
        tags$span(class = "dataset-link-arrow", `aria-hidden` = "true", "→")
      ),
      class = "dataset-explore-link"
    )
  }
  projections <- if (length(overview$projections)) {
    overview$projections
  } else {
    info$projection_names
  }
  if (length(projections)) {
    add_link("overview", "Projection", paste(projections, collapse = " · "))
  }
  if (!is.null(overview$genes)) {
    add_link(
      "geneExpression",
      "Gene expression",
      "Explore expression across cells"
    )
  }
  if (length(annotations) || !is.null(overview$sample)) {
    add_link("groups", "Groups", "Compare cell populations")
  }
  if (length(overview$spatial)) {
    add_link("spatial", "Spatial", "Explore tissue and spatial coordinates")
  }
  capabilities <- info$capabilities
  if (isTRUE(capabilities$trajectory)) {
    add_link("trajectory", "Trajectory", "Explore cell trajectories")
  }
  if (isTRUE(info$immune_repertoire)) {
    add_link(
      "immune_repertoire",
      "Immune repertoire",
      "Explore receptor repertoires"
    )
  }
  if (isTRUE(capabilities$marker_genes)) {
    add_link("markerGenes", "Marker genes", "Inspect exported marker results")
  }
  if (isTRUE(capabilities$enriched_pathways)) {
    add_link(
      "enrichedPathways",
      "Enriched pathways",
      "Explore exported enrichment results"
    )
  }
  if (isTRUE(capabilities$extra_material)) {
    add_link("extra_material", "Extra material", "Browse supporting material")
  }
  shiny::tagList(
    tags$section(
      class = "dataset-overview",
      `aria-label` = "Dataset overview",
      tags$header(
        class = "dataset-summary-heading",
        tags$h2(info$label),
        if (!is.null(overview$description)) {
          tags$p(class = "dataset-description", overview$description)
        },
        if (!is.null(organism) || !is.null(date)) {
          tags$div(
            class = "dataset-meta",
            if (!is.null(organism)) {
              tags$span(id = "load_data_organism", organism)
            },
            if (!is.null(date)) {
              tags$span(
                id = "load_data_date_of_export",
                paste("Exported", date)
              )
            }
          )
        }
      ),
      tags$div(
        class = "cerebro-dataset-stats dataset-metrics",
        number(info$cells, "Cells"),
        number(overview$genes, "Genes"),
        number(overview$samples, "Samples"),
        if (length(overview$spatial)) {
          number(length(overview$spatial), "Spatial views")
        }
      ),
      if (!is.null(overview$sample) || length(annotations)) {
        tags$div(
          class = "dataset-compositions",
          if (!is.null(overview$sample)) {
            tags$section(
              class = "dataset-panel",
              tags$h3("Sample composition"),
              tags$p(class = "dataset-note", "Cells per sample"),
              htmltools::tagAppendChild(
                shiny::uiOutput("load_data_sample_plot"),
                viewerOverviewBars(overview$sample)
              )
            )
          },
          if (length(annotations)) {
            tags$section(
              class = "dataset-panel",
              tags$h3("Cell annotations"),
              shiny::selectInput(
                "load_data_annotation",
                "Group by",
                choices = names(annotations),
                width = "100%"
              ),
              htmltools::tagAppendChild(
                shiny::uiOutput("load_data_annotation_plot"),
                viewerOverviewBars(annotations[[1L]])
              )
            )
          }
        )
      },
      if (length(links)) {
        tags$section(
          class = "dataset-explore",
          tags$h3("Explore this dataset"),
          tags$div(class = "dataset-explore-grid", links)
        )
      },
      if (!is.null(overview$source)) {
        tags$section(
          class = "dataset-source",
          tags$h3("Data source"),
          tags$p(overview$source)
        )
      }
    )
  )
}
