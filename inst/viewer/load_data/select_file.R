##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Cerebro.
##----------------------------------------------------------------------------##

output[["load_data_select_file_UI"]] <- renderUI({
  if (
    exists('Cerebro.options') &&
      !is.null(Cerebro.options[['mode']]) &&
      Cerebro.options[["mode"]] != "closed"
  ) {
    tagList(
      div(
        class = "cerebro-welcome",
        HTML(Cerebro.options[["welcome_message"]] %||%
          "<p>Please load your data set or explore the pre-loaded data.</p>")
      ),
      fluidRow(
        column(
          12,
          titlePanel("Load data"),
          fileInput(
            inputId = "input_file",
            label = "Select input data (.crb or .rds file)",
            multiple = FALSE,
            accept = c(".rds", ".crb", ".cerebro"),
            width = '350px',
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          )
        )
      )
    )
  } else {
    tagList(
      div(
        class = "cerebro-welcome",
        HTML(Cerebro.options[["welcome_message"]] %||%
          "<p>Explore the loaded dataset using the pages in the sidebar.</p>")
      )
    )
  }
})
