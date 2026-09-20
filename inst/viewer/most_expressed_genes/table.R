##----------------------------------------------------------------------------##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

most_expressed_genes_available_groups <- reactive({
  unique(c(
    getGroupsWithMostExpressedGenes(),
    getGroupsWithMeanExpression()
  ))
})

most_expressed_genes_selected_tables <- reactive({
  selected_group <- input[["most_expressed_genes_selected_group"]]
  req(
    selected_group,
    selected_group %in% most_expressed_genes_available_groups()
  )
  list(
    pct = if (selected_group %in% getGroupsWithMostExpressedGenes()) {
      tryCatch(
        viewerGetMostExpressedGenes(selected_group),
        error = function(e) NULL
      )
    },
    mean_expr = if (selected_group %in% getGroupsWithMeanExpression()) {
      tryCatch(getMeanExpression(selected_group), error = function(e) NULL)
    }
  )
})

most_expressed_genes_selected_table <- reactive({
  metric_type <- input[["most_expressed_genes_metric_type"]]
  req(metric_type)
  most_expressed_genes_selected_tables()[[metric_type]]
})

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_table_UI"]] <- renderUI({
  selected_group <- input[['most_expressed_genes_selected_group']]
  if (
    is.null(selected_group) ||
      selected_group %in% most_expressed_genes_available_groups() == FALSE
  ) {
    fluidRow(
      cerebroBox(
        title = boxTitle("Gene counts"),
        textOutput("most_expressed_genes_message_no_data_found")
      )
    )
  } else {
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Gene counts"),
          cerebroInfoButton("most_expressed_genes_info")
        ),
        uiOutput("most_expressed_genes_table_or_text_UI")
      )
    )
  }
})

##----------------------------------------------------------------------------##
## UI element that shows either a table with a switch to toggle sub-filtering
## of results and the corresponding selector, or a text message if data is
## missing.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_table_or_text_UI"]] <- renderUI({
  ## Build available metric choices based on data availability
  metric_choices <- c()
  tables <- most_expressed_genes_selected_tables()
  pct_data <- tables$pct

  if (!is.null(pct_data) && is.data.frame(pct_data) && nrow(pct_data) > 0) {
    metric_choices <- c(metric_choices, "Percent expressed" = "pct")
  }

  mean_data <- tables$mean_expr

  if (!is.null(mean_data) && is.data.frame(mean_data) && nrow(mean_data) > 0) {
    metric_choices <- c(metric_choices, "Mean expression" = "mean_expr")
  }

  ## If no data available, show message
  if (length(metric_choices) == 0) {
    return(fluidRow(
      column(12, tags$p("No expression data available for the selected group."))
    ))
  }

  fluidRow(
    column(
      12,
      selectInput(
        inputId = "most_expressed_genes_metric_type",
        label = "Expression metric:",
        choices = metric_choices,
        selected = metric_choices[1]
      ),
      uiOutput("most_expressed_genes_metric_description_UI")
    ),
    column(
      12,
      shinyWidgets::materialSwitch(
        inputId = "most_expressed_genes_table_filter_switch",
        label = "Show results for all subgroups (no pre-filtering):",
        value = FALSE,
        status = "primary",
        inline = TRUE
      )
    ),
    column(12, uiOutput("most_expressed_genes_filter_subgroups_UI")),
    column(12, DT::dataTableOutput("most_expressed_genes_table"))
  )
})

##----------------------------------------------------------------------------##
## UI element for metric description.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_metric_description_UI"]] <- renderUI({
  req(input[["most_expressed_genes_metric_type"]])
  metric_type <- input[["most_expressed_genes_metric_type"]]

  if (metric_type == "pct") {
    tagList(
      tags$p(
        style = "color: #888; font-size: 12px; margin-top: -10px; margin-bottom: 5px;",
        "Percentage of cells expressing each gene. Based on normalized read counts, shows what proportion of cells in each group have detectable expression (count > 0)."
      ),
      tags$p(
        style = "color: #888; font-size: 12px; text-align: center; font-style: italic; margin-bottom: 15px;",
        "Percent = (Number of cells with count > 0) / (Total cells in group) × 100%"
      )
    )
  } else {
    tagList(
      tags$p(
        style = "color: #888; font-size: 12px; margin-top: -10px; margin-bottom: 5px;",
        "Average expression level per gene. Calculated from normalized read counts across all cells in each group."
      ),
      tags$p(
        style = "color: #888; font-size: 12px; text-align: center; font-style: italic; margin-bottom: 15px;",
        "Mean Expression = Σ(counts) / (Total cells in group)"
      )
    )
  }
})

##----------------------------------------------------------------------------##
## UI element for sub-filtering of results (if toggled).
##----------------------------------------------------------------------------##
output[["most_expressed_genes_filter_subgroups_UI"]] <- renderUI({
  req(!is.null(input[["most_expressed_genes_table_filter_switch"]]))
  req(!is.null(input[["most_expressed_genes_metric_type"]]))
  selected_group <- input[['most_expressed_genes_selected_group']]
  req(selected_group %in% most_expressed_genes_available_groups())
  results_df <- most_expressed_genes_selected_table()
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  cerebroResultSubgroupUI(
    results_df,
    input[["most_expressed_genes_table_filter_switch"]],
    identical(colnames(results_df)[[1L]], selected_group),
    "most_expressed_genes_table_select_group_level"
  )
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_table"]] <- DT::renderDataTable({
  selected_group <- input[['most_expressed_genes_selected_group']]
  req(selected_group %in% most_expressed_genes_available_groups())
  req(!is.null(input[["most_expressed_genes_metric_type"]]))
  metric_type <- input[["most_expressed_genes_metric_type"]]
  results_df <- most_expressed_genes_selected_table()
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  grouped <- identical(colnames(results_df)[[1L]], selected_group)
  if (!isTRUE(input[["most_expressed_genes_table_filter_switch"]]) && grouped) {
    req(input[["most_expressed_genes_table_select_group_level"]])
  }
  results_df <- cerebroFilterResultRows(
    results_df,
    input[["most_expressed_genes_table_filter_switch"]],
    grouped,
    input[["most_expressed_genes_table_select_group_level"]]
  )
  value_col <- NULL
  if (identical(metric_type, "pct") && "pct" %in% colnames(results_df)) {
    results_df <- dplyr::rename(results_df, "% of cells expressing" = pct)
    value_col <- 3
  } else if (
    !identical(metric_type, "pct") &&
      "mean_expr" %in% colnames(results_df)
  ) {
    results_df <- dplyr::rename(results_df, "Mean expression" = mean_expr)
  }
  cerebroResultTable(
    results_df,
    paste0(
      ifelse(
        metric_type == "pct",
        "percent_expressed_",
        "mean_expression_"
      ),
      input[["most_expressed_genes_selected_group"]]
    ),
    hide_long_columns = FALSE,
    columns_percentage = value_col
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_message_no_data_found"]] <- renderText({
  "No data available."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "most_expressed_genes_info",
  most_expressed_genes_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
most_expressed_genes_info <- list(
  title = "Most expressed genes",
  text = HTML(
    "
    Table showing gene expression statistics for each group. These lists can help to identify/verify the dominant cell types.
    <h4>Expression metrics</h4>
    <b>Percent expressed</b><br>
    The percentage of cells in each group that have detectable expression (count > 0) of each gene. For example, if a gene shows 80%, it means 80% of cells in that group express this gene.<br>
    <br>
    <b>Mean expression</b><br>
    The average normalized expression level of each gene across all cells in each group. Higher values indicate genes with stronger overall expression in the group.<br>
    <h4>Options</h4>
    <b>Show results for all subgroups (no pre-filtering)</b><br>
    When active, the subgroup section element will disappear and instead the table will be shown for all subgroups. Subgroups can still be selected through the dedicated column filter, which also allows to select multiple subgroups at once. While using the column filter is more elegant, it can become laggy with very large tables, hence to option to filter the table beforehand. Please note that this feature only works if the first column was recognized as holding assignments to one of the grouping variables, e.g. 'sample' or 'clusters', otherwise your choice here will be ignored and the whole table shown without pre-filtering.<br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
