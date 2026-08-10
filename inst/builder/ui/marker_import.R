builder_marker_import_choice_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "marker-import-choice",
    shiny::p(
      class = "marker-import-choice-intro",
      "Choose how this app should provide marker genes."
    ),
    shiny::div(
      class = "marker-import-choice-actions",
      shiny::actionButton(
        ns("marker_genes_calculate"),
        "Calculate for all Groups",
        class = "btn btn-action"
      ),
      shiny::actionButton(
        ns("marker_genes_upload"),
        "Upload precomputed results",
        class = "btn"
      )
    ),
    shiny::p(
      class = "hint marker-import-choice-note",
      "Calculation uses the selected analysis settings. Uploaded results are added as a separate Viewer method."
    )
  )
}

builder_marker_import_ui <- function(id, groups = character()) {
  ns <- shiny::NS(id)
  choices <- stats::setNames(groups, groups)
  shiny::div(
    class = "marker-import builder-subcard",
    shiny::div(
      class = "marker-import-hero",
      shiny::span(class = "marker-import-kicker", "Marker genes"),
      shiny::h4("Import precomputed Marker genes"),
      shiny::p(
        "Add results from another workflow as a selectable Viewer method."
      )
    ),
    shiny::div(
      class = "marker-import-controls",
      shiny::textInput(ns("marker_import_method"), "Method name"),
      shiny::selectInput(
        ns("marker_import_group"),
        "Groups",
        choices = choices
      ),
      shiny::div(
        class = "marker-import-file-control builder-file-picker builder-file-picker--content",
        shiny::tags$input(
          id = ns("marker_import_files"),
          name = ns("marker_import_files"),
          class = "shiny-input-file marker-import-file-input builder-file-input",
          type = "file",
          multiple = "multiple",
          accept = ".xlsx,.csv,.tsv",
          tabindex = "-1"
        ),
        shiny::tags$label(
          `for` = ns("marker_import_files"),
          class = "marker-import-file-button builder-file-trigger",
          tabindex = "0",
          role = "button",
          "+ Add result files…"
        )
      )
    ),
    shiny::div(
      id = ns("marker_import_status"),
      class = "marker-import-status",
      `aria-live` = "polite",
      "Choose a method, a grouping variable, and one or more result files."
    ),
    shiny::uiOutput(ns("marker_import_list"))
  )
}
