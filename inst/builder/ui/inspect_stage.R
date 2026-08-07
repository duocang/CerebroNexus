## Guided Import / Inspect stage.

builder_stage_has_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

builder_stage_text_items <- function(values, class = NULL) {
  values <- as.character(values %||% character())
  if (!length(values)) {
    return(NULL)
  }
  tags$ul(
    class = class,
    lapply(values, function(value) tags$li(value))
  )
}

builder_inspect_model <- function(profile, state, format, dataset_id) {
  manifest <- state$manifest %||% list()
  detected <- unlist(
    lapply(manifest, function(entry) {
      status <- switch(
        entry$status %||% "unknown",
        valid = "ready",
        not_applicable = "not applicable",
        checking = "checking",
        attention = "needs attention",
        blocking = "blocked",
        entry$status %||% "unknown"
      )
      c(
        paste0(entry$id, " — ", status),
        if (builder_stage_has_text(entry$summary %||% "")) entry$summary
      )
    }),
    use.names = FALSE
  )
  list(
    summary = c(
      paste(format(profile$n_cells, big.mark = ","), "cells"),
      paste(format(profile$n_genes, big.mark = ","), "genes")
    ),
    attention = state$attention_ids %||% character(),
    blockers = state$blocking_ids %||% character(),
    detected = detected,
    diagnostics = c(paste("Format", format), paste("Dataset id", dataset_id))
  )
}

builder_inspect_stage_ui <- function(id, model) {
  ns <- NS(id)
  attention <- as.character(model$attention %||% character())
  blockers <- as.character(model$blockers %||% character())
  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-inspect",
    h2("Import & Inspect"),
    div(
      class = "facts",
      lapply(as.character(model$summary %||% character()), function(value) {
        div(class = "fact", value)
      })
    ),
    if (length(c(attention, blockers))) {
      div(
        class = "notice warn",
        h3("Needs attention"),
        builder_stage_text_items(c(attention, blockers))
      )
    },
    tags$details(
      tags$summary("View all detected content"),
      builder_stage_text_items(model$detected)
    ),
    tags$details(
      tags$summary("Technical diagnostics"),
      builder_stage_text_items(model$diagnostics, "technical-diagnostics")
    )
  )
}
