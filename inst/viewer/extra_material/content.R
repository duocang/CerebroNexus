##----------------------------------------------------------------------------##
## Show content or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------#
output[["extra_material_content_UI"]] <- renderUI({
  req(input[["extra_material_selected_category"]])
  ## if selected category is `tables`
  if (input[["extra_material_selected_category"]] == 'tables') {
    if (!checkForExtraTables()) {
      return(fluidRow(cerebroBox(
        title = boxTitle("Extra material"),
        "No non-empty tables available."
      )))
    }
    ##
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Extra material"),
          cerebroInfoButton("extra_material_info")
        ),
        fluidRow(
          column(
            12,
            shinyWidgets::materialSwitch(
              inputId = "extra_material_table_number_formatting",
              label = "Automatically format numbers:",
              value = TRUE,
              status = "primary",
              inline = TRUE
            ),
            shinyWidgets::materialSwitch(
              inputId = "extra_material_table_color_highlighting",
              label = "Highlight values with colours:",
              value = TRUE,
              status = "primary",
              inline = TRUE
            )
          ),
          column(12, DT::dataTableOutput("extra_material_table"))
        )
      )
    )
    ## if selected category is `plots`
  } else if (input[["extra_material_selected_category"]] == 'plots') {
    ##
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Extra material"),
          cerebroInfoButton("extra_material_info")
        ),
        fluidRow(
          column(
            12,
            shinyWidgets::materialSwitch(
              inputId = "extra_material_plot_interactive_switch",
              label = "Make plot interactive:",
              value = TRUE,
              status = "primary",
              inline = TRUE
            )
          ),
          column(12, uiOutput("extra_material_plot_UI"))
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##
extra_material_table_job <- cerebro_async_latest_value(
  session,
  cerebro_async_source_call
)

extra_material_async_selection <- reactive({
  selection <- extra_material_table_selection(
    extra_material_table_groups(),
    file_key = input[["extra_material_selected_file"]],
    sheet_key = input[["extra_material_selected_content"]],
    load = FALSE
  )
  req(!is.null(selection))
  if (!is.null(selection$sheet$table)) {
    return(selection)
  }
  key <- selection$sheet$key
  if (
    exists(
      key,
      envir = extra_material_external_table_cache,
      inherits = FALSE
    )
  ) {
    selection$sheet$table <- get(
      key,
      envir = extra_material_external_table_cache,
      inherits = FALSE
    )
    return(selection)
  }
  extra_material_table_job$invoke(
    key,
    list(
      root = file.path(
        Cerebro.options[["cerebro_root"]],
        "viewer",
        "extra_material"
      ),
      files = "async_workers.R",
      function_name = "extra_material_read_table",
      args = list(
        root = Cerebro.options[["cerebro_root"]],
        path = selection$sheet$path,
        group_label = selection$group$label,
        sheet_label = selection$sheet$label
      )
    )
  )
  selection$sheet$table <- extra_material_table_job$result()
  assign(
    key,
    selection$sheet$table,
    envir = extra_material_external_table_cache
  )
  selection
})

output[["extra_material_table"]] <- DT::renderDataTable({
  req(input[["extra_material_selected_category"]] == "tables")
  selection <- extra_material_async_selection()
  results_df <- selection$sheet$table
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  ## if the table is empty, skip the processing and show and empty table
  ## (otherwise the procedure would result in an error)
  if (nrow(results_df) == 0) {
    results_df %>%
      as.data.frame() %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()
    ## if there is at least 1 row, create proper table
  } else {
    prettifyTable(
      results_df,
      filter = extra_material_table_filter(nrow(results_df), ncol(results_df)),
      dom = "Bfrtlip",
      escape = !identical(selection$group$key, "embedded"),
      show_buttons = TRUE,
      number_formatting = input[["extra_material_table_number_formatting"]],
      color_highlighting = input[["extra_material_table_color_highlighting"]],
      hide_long_columns = TRUE,
      download_file_name = paste0(
        "extra_material_tables_",
        gsub(
          "[^[:alnum:]_-]+",
          "_",
          paste(selection$group$label, selection$sheet$label, sep = "_")
        )
      ),
      page_length_default = 20,
      page_length_menu = c(20, 50, 100)
    )
  }
})

##----------------------------------------------------------------------------##
## Plot.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element that contains interactive or plain version of plot, depending on
## switch status
##----------------------------------------------------------------------------##
output[["extra_material_plot_UI"]] <- renderUI({
  req(!is.null(input[["extra_material_plot_interactive_switch"]]))
  if (input[["extra_material_plot_interactive_switch"]] == TRUE) {
    plotly::plotlyOutput(
      "extra_material_plot_interactive",
      width = "auto",
      height = "70vh"
    )
  } else {
    plotOutput(
      "extra_material_plot_plain",
      width = "auto",
      height = "70vh"
    )
  }
})

##----------------------------------------------------------------------------##
## UI element that contains interactive version of plot
##----------------------------------------------------------------------------##
output[["extra_material_plot_interactive"]] <- plotly::renderPlotly({
  req(
    input[["extra_material_selected_category"]],
    input[["extra_material_selected_content"]]
  )
  ## fetch results
  plot <- getExtraPlot(input[["extra_material_selected_content"]])
  ## don't proceed if input is not of class "ggplot"
  req("ggplot" %in% class(plot))
  ## convert to plotly
  plot <- plotly::ggplotly(plot)
  ## return plot either with WebGL or without, depending on setting
  if (preferences$use_webgl == TRUE) {
    plot %>% plotly::toWebGL()
  } else {
    plot
  }
})

##----------------------------------------------------------------------------##
## UI element that contains plain version of plot
##----------------------------------------------------------------------------##
output[["extra_material_plot_plain"]] <- renderPlot({
  req(
    input[["extra_material_selected_category"]],
    input[["extra_material_selected_content"]]
  )
  ## fetch results
  plot <- getExtraPlot(input[["extra_material_selected_content"]])
  ## don't proceed if input is not of class "ggplot"
  req("ggplot" %in% class(plot))
  return(plot)
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["extra_material_info"]], {
  showModal(
    modalDialog(
      extra_material_info[["text"]],
      title = extra_material_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
extra_material_info <- list(
  title = "Extra material",
  text = HTML(
    "
    Here, additional material related to the data set can be stored. At the moment, only tables and plots made with ggplot2 are supported, but depending on user requests, support for others types of content can be added in the future.<br>
    <br>
    For an explanation of the specific content, please refer to the person who exported this data set."
  )
)
