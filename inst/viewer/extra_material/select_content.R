##----------------------------------------------------------------------------##
## Select category and content.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of category and specific content,
## which are split because the content depends on which category is selected.
##----------------------------------------------------------------------------##
extra_material_category_needed <- function(categories) {
  categories <- as.character(categories)
  categories <- categories[!is.na(categories) & nzchar(categories)]
  length(unique(categories)) > 1L
}

output[["extra_material_select_category_and_content_UI"]] <- renderUI({
  categories <- getExtraMaterialCategories()
  has_category_choice <- extra_material_category_needed(categories)
  tagList(
    fluidRow(
      if (has_category_choice) {
        column(6, uiOutput("extra_material_selected_category_UI"))
      } else {
        uiOutput("extra_material_selected_category_UI")
      },
      column(
        if (has_category_choice) 6 else 12,
        uiOutput("extra_material_selected_content_UI")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select from which category the content should be shown.
##----------------------------------------------------------------------------##
output[["extra_material_selected_category_UI"]] <- renderUI({
  categories <- getExtraMaterialCategories()
  has_category_choice <- extra_material_category_needed(categories)
  category_input <- selectInput(
    "extra_material_selected_category",
    label = NULL,
    choices = categories,
    width = "100%"
  )
  if (!has_category_choice) {
    return(tags$div(class = "visually-hidden", category_input))
  }
  tagList(
    div(
      HTML(
        '<h3 style="text-align: center; margin-top: 0"><strong>Choose a category:</strong></h3>'
      )
    ),
    fluidRow(
      column(2),
      column(
        8,
        category_input
      ),
      column(2)
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select which content should be shown.
##----------------------------------------------------------------------------##
output[["extra_material_selected_content_UI"]] <- renderUI({
  req(input[["extra_material_selected_category"]])
  ## if selected category is `tables`
  if (
    input[["extra_material_selected_category"]] == 'tables' &&
      checkForExtraTables() == TRUE
  ) {
    groups <- extra_material_table_groups()
    file_choices <- extra_material_table_file_choices(groups)
    selected_file <- input[["extra_material_selected_file"]]
    if (
      length(selected_file) != 1L || !selected_file %in% unname(file_choices)
    ) {
      selected_file <- unname(file_choices)[[1L]]
    }
    selection <- extra_material_table_selection(
      groups,
      file_key = selected_file
    )
    req(!is.null(selection))
    sheet_choices <- stats::setNames(
      vapply(selection$group$sheets, `[[`, character(1), "key"),
      vapply(selection$group$sheets, `[[`, character(1), "label")
    )
    selected_sheet <- input[["extra_material_selected_content"]]
    if (
      length(selected_sheet) != 1L || !selected_sheet %in% unname(sheet_choices)
    ) {
      selected_sheet <- unname(sheet_choices)[[1L]]
    }

    tagList(
      if (length(file_choices) > 1L) {
        div(
          HTML(
            '<h3 style="text-align: center; margin-top: 0"><strong>Choose a file:</strong></h3>'
          ),
          fluidRow(
            column(2),
            column(
              8,
              selectInput(
                "extra_material_selected_file",
                label = NULL,
                choices = file_choices,
                selected = selected_file,
                width = "100%"
              )
            ),
            column(2)
          )
        )
      },
      if (length(sheet_choices) > 1L) {
        div(
          HTML(
            '<h3 style="text-align: center"><strong>Choose a sheet:</strong></h3>'
          ),
          fluidRow(
            column(2),
            column(
              8,
              selectInput(
                "extra_material_selected_content",
                label = NULL,
                choices = sheet_choices,
                selected = selected_sheet,
                width = "100%"
              )
            ),
            column(2)
          )
        )
      }
    )
    ## if selected category is `plots`
  } else if (
    input[["extra_material_selected_category"]] == 'plots' &&
      checkForExtraPlots() == TRUE
  ) {
    ##
    tagList(
      div(
        HTML(
          '<h3 style="text-align: center; margin-top: 0"><strong>Choose a plot:</strong></h3>'
        )
      ),
      fluidRow(
        column(2),
        column(
          8,
          selectInput(
            "extra_material_selected_content",
            label = NULL,
            choices = getNamesOfExtraPlots(),
            width = "100%"
          )
        ),
        column(2)
      )
    )
  }
})
