builder_stage_contract_source_runtime(environment())

test_that("app composes stage modules in deterministic order", {
  app <- builder_app_source_lines()
  sources <- vapply(
    c(
      "inspect_stage.R",
      "core_stage.R",
      "enhance_stage.R",
      "review_stage.R",
      "build_status.R"
    ),
    function(file) which(grepl(file, app, fixed = TRUE)),
    integer(1)
  )
  expect_true(all(diff(sources) > 0L))
  expect_true(any(grepl("builder_inspect_stage_ui(", app, fixed = TRUE)))
  expect_true(any(grepl("builder_core_stage_ui(", app, fixed = TRUE)))
  expect_true(any(grepl("builder_enhance_stage_ui(", app, fixed = TRUE)))
  expect_true(any(grepl("builder_review_stage_ui(", app, fixed = TRUE)))
  expect_true(any(grepl("builder_review_can_build(", app, fixed = TRUE)))
  expect_true(any(grepl('name = "core-name"', app, fixed = TRUE)))
  expect_true(any(grepl("input[[input_id]]", app, fixed = TRUE)))
  expect_true(any(grepl(
    'paste0("enhance-analysis_", step$id)',
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl('input[["enhance-table_files"]]', app, fixed = TRUE)))
  expect_false(any(grepl('input[["enhance-add_table"]]', app, fixed = TRUE)))
  expect_false(any(grepl('input[["enhance-table_path"]]', app, fixed = TRUE)))
  expect_false(any(grepl(
    'input[["enhance-tables_to_retain"]]',
    app,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    'input[["enhance-histology_to_retain"]]',
    app,
    fixed = TRUE
  )))
  expect_false(any(grepl("builder_enhance_retain", app, fixed = TRUE)))
  expect_true(any(grepl(
    "builder_enhance_analysis_profile",
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "settings$organism",
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl("builder_open_final_app", app, fixed = TRUE)))
  expect_true(any(grepl("builder_reveal_release", app, fixed = TRUE)))
  expect_true(any(grepl("builder_copy_result_path", app, fixed = TRUE)))
  expect_true(any(grepl("builder_release_error_result", app, fixed = TRUE)))
  expect_true(any(grepl(
    "plan$output_release$directory",
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl("release$handle$target", app, fixed = TRUE)))
  expect_true(any(grepl("abort_release_result", app, fixed = TRUE)))
  status_source <- readLines(
    builder_profile_inst_path("builder", "ui", "build_status.R"),
    warn = FALSE
  )
  expect_true(any(grepl(
    "builder_result_recovery_required",
    status_source,
    fixed = TRUE
  )))
  expect_false(any(grepl("detected = names(entry$profile)", app, fixed = TRUE)))
  expect_false(any(grepl('uiOutput("detail")', app, fixed = TRUE)))
  expect_false(any(grepl("result(list(error =", app, fixed = TRUE)))
  expect_false(any(grepl("ready_report <- reactive", app, fixed = TRUE)))
})

test_that("guided workbench has no unreachable legacy detail handlers", {
  app <- builder_app_source_text()
  legacy_contracts <- c(
    "output$detail <- renderUI",
    "\n  setting_inputs <- c(",
    "observeEvent(\n    input$assay",
    "observeEvent(input$analyses",
    "output$preview_plot <- plotly::renderPlotly",
    "output$palette_note <- renderUI",
    "output$color_swatches <- renderUI",
    "output$analysis_choices <- renderUI",
    "observeEvent(input$drop_table",
    "output$table_list <- renderUI"
  )

  for (contract in legacy_contracts) {
    expect_false(
      grepl(contract, app, fixed = TRUE),
      info = paste("legacy Builder contract remains reachable:", contract)
    )
  }
})
