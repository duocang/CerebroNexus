async_contract_source_root <- normalizePath(
  testthat::test_path("..", ".."),
  mustWork = FALSE
)
async_contract_has_source <- all(file.exists(c(
  file.path(async_contract_source_root, "create_env.R"),
  file.path(async_contract_source_root, "R", "launchCerebro.R")
)))

test_that("runtime dependencies are declared for every app form", {
  description <- if (async_contract_has_source) {
    paste(
      readLines(file.path(async_contract_source_root, "DESCRIPTION")),
      collapse = "\n"
    )
  } else {
    paste(unlist(packageDescription("CerebroNexus")), collapse = "\n")
  }

  expect_match(description, "mirai (>= 2.7.0)", fixed = TRUE)
  expect_match(description, "promises (>= 1.3.0)", fixed = TRUE)
  expect_match(description, "shiny (>= 1.8.1)", fixed = TRUE)
  if (async_contract_has_source) {
    fields <- read.dcf(file.path(async_contract_source_root, "DESCRIPTION"))
    expect_false(grepl("mirai", fields[1L, "Imports"], fixed = TRUE))
    expect_match(fields[1L, "Suggests"], "mirai (>= 2.7.0)", fixed = TRUE)
    create_env <- readLines(file.path(
      async_contract_source_root,
      "create_env.R"
    ))
    expect_true(any(grepl('"mirai"', create_env, fixed = TRUE)))
    expect_true(any(grepl('"promises"', create_env, fixed = TRUE)))
  }
})

test_that("installed launcher owns the daemon lifecycle", {
  text <- if (async_contract_has_source) {
    paste(
      readLines(file.path(
        async_contract_source_root,
        "R",
        "launchCerebro.R"
      )),
      collapse = "\n"
    )
  } else {
    paste(
      deparse(CerebroNexus::launchCerebro, width.cutoff = 500L),
      collapse = "\n"
    )
  }

  expect_match(text, "mirai_options", fixed = TRUE)
  expect_match(text, '"?mirai"?\\s*=\\s*mirai_options', perl = TRUE)
  expect_match(text, 'paste0("viewer/async_runtime.R")', fixed = TRUE)
  expect_match(text, "cerebro_async_init", fixed = TRUE)
  expect_match(text, "cerebro_async_shutdown", fixed = TRUE)
  expect_false(grepl("required to launch CerebroNexus", text, fixed = TRUE))
})

test_that("source and generated apps own the daemon lifecycle", {
  standalone_path <- if (async_contract_has_source) {
    file.path(async_contract_source_root, "inst", "app.R")
  } else {
    system.file("app.R", package = "CerebroNexus")
  }
  standalone <- paste(
    readLines(standalone_path),
    collapse = "\n"
  )
  builder <- if (async_contract_has_source) {
    paste(
      readLines(file.path(
        async_contract_source_root,
        "R",
        "createShinyApp.R"
      )),
      collapse = "\n"
    )
  } else {
    paste(
      deparse(CerebroNexus::createShinyApp, width.cutoff = 500L),
      collapse = "\n"
    )
  }

  for (text in list(standalone, builder)) {
    expect_match(text, "viewer/async_runtime.R", fixed = TRUE)
    expect_match(text, "cerebro_async_init", fixed = TRUE)
    expect_match(text, "cerebro_async_shutdown", fixed = TRUE)
  }
})
