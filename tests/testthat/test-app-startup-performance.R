library(shinytest2)

startup_inst_dir <- testthat::test_path("../../inst")
if (!file.exists(file.path(startup_inst_dir, "app.R"))) {
  startup_inst_dir <- system.file(package = "CerebroNexus")
}

test_that("the first browser flush renders the active page before hidden controls", {
  local_app_support(startup_inst_dir)
  app <- AppDriver$new(
    startup_inst_dir,
    name = "startup_first_flush",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())

  logs <- app$get_logs()
  values <- logs[
    logs$level == "info" & grepl("shiny:value", logs$message, fixed = TRUE),
    "message",
    drop = TRUE
  ]
  data_info <- which(grepl(
    "shiny:value load_data_number_of_cells",
    values,
    fixed = TRUE
  ))[[1L]]
  hidden <- which(grepl(
    "shiny:value expression_projection_additional_parameters_UI",
    values,
    fixed = TRUE
  ))

  expect_false(is.na(data_info))
  expect_true(length(hidden) == 0L || data_info < hidden[[1L]])

  Sys.sleep(1.2)
  expect_silent(app$get_value(output = "load_data_number_of_cells"))
  app$wait_for_js(
    paste0(
      "window.cerebroLinkedViewsState && ",
      "window.cerebroLinkedViewsState.ready() === true"
    ),
    timeout = 60000
  )
  expect_true(app$get_js(paste0(
    "document.querySelector(",
    "'li.active > a[href=\"#shiny-tab-loadData\"]') !== null"
  )))
})

test_that("startup source defines process preloading and delayed page warmup", {
  server <- paste(
    readLines(file.path(startup_inst_dir, "viewer", "shiny_server.R")),
    collapse = "\n"
  )

  expect_match(server, ".crb_raw_process_cache", fixed = TRUE)
  expect_match(server, "viewer_after_first_paint", fixed = TRUE)
  expect_match(server, "viewer_deferred_output_spacing_ms <- 50L", fixed = TRUE)
  expect_match(
    server,
    "lapply(seq_along(deferred_ids), function(index)",
    fixed = TRUE
  )
  expect_match(
    server,
    "delay = (index - 1L) * viewer_deferred_output_spacing_ms / 1000",
    fixed = TRUE
  )
  expect_match(
    server,
    "viewer_after_first_paint(TRUE)\n          deferred_ids <-",
    fixed = TRUE
  )
  expect_no_match(server, "delay = length(deferred_ids)", fixed = TRUE)
  expect_match(server, "viewer_dataset_capabilities", fixed = TRUE)
  for (path in c(
    "overview/obj_projection_data_to_plot.R",
    "gene_expression/obj_projection_data_to_plot.R",
    "spatial/obj_projection_data_to_plot.R",
    "trajectory/projection_plot.R"
  )) {
    source <- paste(
      readLines(file.path(startup_inst_dir, "viewer", path)),
      collapse = "\n"
    )
    expect_match(source, "debounceAfterFirst", fixed = TRUE)
  }
})

test_that("Linked Views pure helpers are parsed once per process", {
  server_file <- file.path(startup_inst_dir, "viewer", "shiny_server.R")
  linked_server_file <- file.path(
    startup_inst_dir,
    "viewer",
    "coordinated_views",
    "server.R"
  )
  server <- paste(readLines(server_file), collapse = "\n")
  linked_server <- paste(readLines(linked_server_file), collapse = "\n")

  expect_match(server, ".coordviews_runtime", fixed = TRUE)
  expect_match(server, "sys.source", fixed = TRUE)
  expect_match(server, "environment(value) <- environment()", fixed = TRUE)
  expect_false(grepl(
    '"/viewer/coordinated_views/bundle.R"',
    linked_server,
    fixed = TRUE
  ))
  expect_false(grepl(
    '"/viewer/coordinated_views/config.R"',
    linked_server,
    fixed = TRUE
  ))

  client <- paste(
    readLines(file.path(startup_inst_dir, "viewer", "www", "cell_views.js")),
    collapse = "\n"
  )
  expect_match(
    client,
    "Shiny.setInputValue('coordviews_visible', linkedVis)",
    fixed = TRUE
  )
  expect_match(
    client,
    "Shiny.setInputValue('coordviews_warmable', !singleId)",
    fixed = TRUE
  )
  expect_match(linked_server, "viewer_after_first_paint()", fixed = TRUE)
  expect_match(linked_server, 'input[["coordviews_warmable"]]', fixed = TRUE)
  expect_match(client, "function ensureSingleBase(payload)", fixed = TRUE)
  expect_match(client, "ensureSingleBase(singleViews[id])", fixed = TRUE)
})
