builder_repo_source("prerequisite.R")

test_that("the app privacy marker must be the exact integer contract", {
  namespace <- new.env(parent = emptyenv())

  expect_identical(builder_installed_app_contract_version(namespace), 0L)

  assign(".cerebro_bundle_privacy_contract_version", 1, namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 0L)

  assign(".cerebro_bundle_privacy_contract_version", 1L, namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 1L)

  assign(".cerebro_bundle_privacy_contract_version", c(1L, 1L), namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 0L)
})

test_that("a function-valued app privacy marker is never executed", {
  namespace <- new.env(parent = emptyenv())
  called <- FALSE
  marker <- function() {
    called <<- TRUE
    1L
  }
  assign(".cerebro_bundle_privacy_contract_version", marker, namespace)

  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  expect_false(called)
})

test_that("an active app privacy marker is never executed", {
  namespace <- new.env(parent = emptyenv())
  called <- FALSE
  makeActiveBinding(
    ".cerebro_bundle_privacy_contract_version",
    function(value) {
      called <<- TRUE
      1L
    },
    namespace
  )

  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  expect_false(called)
})

test_that("a delayed app privacy marker is never forced", {
  namespace <- new.env(parent = emptyenv())
  forced <- FALSE
  delayedAssign(
    ".cerebro_bundle_privacy_contract_version",
    {
      forced <<- TRUE
      1L
    },
    assign.env = namespace
  )

  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  expect_false(forced)
})

test_that("app capability explains the safe alternatives", {
  unavailable <- builder_app_capability(0L)

  expect_identical(
    names(unavailable),
    c("available", "version", "reason")
  )
  expect_false(unavailable$available)
  expect_identical(unavailable$version, 0L)
  expect_match(
    unavailable$reason,
    "private app publication",
    ignore.case = TRUE
  )
  expect_match(unavailable$reason, "CRB-only", fixed = TRUE)
  expect_false(grepl("PR #", unavailable$reason, fixed = TRUE))

  available <- builder_app_capability(1L)
  expect_true(available$available)
  expect_identical(available$version, 1L)
  expect_null(available$reason)

  expect_false(builder_app_capability(1)$available)
})

test_that("CRB-only plans remain available by default", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    expect_identical(formals(builder_make_plan)$make_app, FALSE)
    expect_false("app_capability" %in% names(formals(builder_make_plan)))
    plan <- builder_make_plan(
      list(builder_minimal_entry()),
      tempdir()
    )

    expect_null(plan$error)
    expect_false(plan$make_app)
    expect_identical(plan$app_contract_version, 0L)
    expect_length(plan$targets, 1L)
    expect_false(any(basename(plan$targets) == "cerebro_app"))
  })
})

test_that("app plans fail closed without privacy contract v1", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    capability <- builder_app_capability()

    plan <- builder_make_plan(
      list(builder_minimal_entry()),
      tempdir(),
      make_app = TRUE
    )

    expect_identical(plan$error, capability$reason)
  })
})

test_that("app plans freeze the accepted privacy contract version", {
  local({
    builder_repo_source("prerequisite.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 1L
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    plan <- builder_make_plan(
      list(builder_minimal_entry()),
      tempdir(),
      make_app = TRUE
    )

    expect_null(plan$error)
    expect_true(plan$make_app)
    expect_identical(plan$app_contract_version, 1L)
    expect_true(any(basename(plan$targets) == "cerebro_app"))
  })
})

test_that("unavailable app control is unchecked and disabled", {
  capability <- builder_app_capability(0L)
  html <- as.character(builder_app_control(capability, current_value = TRUE))

  expect_match(html, '<fieldset[^>]*disabled="disabled"')
  expect_match(html, 'id="make_app"', fixed = TRUE)
  expect_false(grepl('checked="checked"', html, fixed = TRUE))
  expect_match(html, capability$reason, fixed = TRUE)
})

test_that("available app control defaults checked and preserves current state", {
  capability <- builder_app_capability(1L)
  initial <- as.character(builder_app_control(capability))
  current_false <- as.character(
    builder_app_control(capability, current_value = FALSE)
  )
  current_true <- as.character(
    builder_app_control(capability, current_value = TRUE)
  )

  expect_false(grepl('disabled="disabled"', initial, fixed = TRUE))
  expect_match(initial, 'checked="checked"', fixed = TRUE)
  expect_false(grepl('checked="checked"', current_false, fixed = TRUE))
  expect_match(current_true, 'checked="checked"', fixed = TRUE)
})

test_that("builder UI loads the prerequisite before its plan", {
  app_path <- testthat::test_path("..", "..", "inst", "builder", "app.R")
  if (!file.exists(app_path)) {
    app_path <- system.file(
      file.path("builder", "app.R"),
      package = "CerebroNexus"
    )
  }
  lines <- readLines(app_path, warn = FALSE)
  text <- paste(lines, collapse = "\n")
  prerequisite_source <- grep(
    'source("prerequisite.R", local = TRUE)',
    lines,
    fixed = TRUE
  )
  plan_source <- grep('source("plan.R", local = TRUE)', lines, fixed = TRUE)

  expect_length(prerequisite_source, 1L)
  expect_length(plan_source, 1L)
  expect_lt(prerequisite_source, plan_source)
  expect_match(text, "app_capability <- builder_app_capability\\(\\)")
  expect_match(
    text,
    "builder_app_control\\("
  )
  expect_match(text, "current_value = isolate\\(input\\$make_app\\)")
})

test_that("worker setup loads the app prerequisite before planning", {
  session_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "session.R"
  )
  if (!file.exists(session_path)) {
    session_path <- system.file(
      file.path("builder", "session.R"),
      package = "CerebroNexus"
    )
  }
  lines <- readLines(session_path, warn = FALSE)
  prerequisite_source <- grep(
    'source(file.path(dir, "prerequisite.R"))',
    lines,
    fixed = TRUE
  )
  plan_source <- grep(
    'source(file.path(dir, "plan.R"))',
    lines,
    fixed = TRUE
  )

  expect_length(prerequisite_source, 1L)
  expect_length(plan_source, 1L)
  expect_lt(prerequisite_source, plan_source)
})

test_that("worker rejects a forged app plan before createShinyApp", {
  local({
    builder_repo_source("session.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 1L
    create_called <- FALSE
    testthat::local_mocked_bindings(
      createShinyApp = function(...) {
        create_called <<- TRUE
        stop("createShinyApp must not be called")
      },
      .package = "CerebroNexus"
    )
    rs <- list(call = function(fun, args) do.call(fun, args))
    plan <- list(
      out_dir = withr::local_tempdir(),
      items = list(),
      make_app = TRUE,
      app_contract_version = 0L,
      overwrite = FALSE
    )

    result <- builder_session_build(rs, plan)

    expect_match(result$error, "private app publication", ignore.case = TRUE)
    expect_false(create_called)
  })
})

test_that("worker rechecks the current installed app contract", {
  local({
    builder_repo_source("session.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 0L
    create_called <- FALSE
    testthat::local_mocked_bindings(
      createShinyApp = function(...) {
        create_called <<- TRUE
        stop("createShinyApp must not be called")
      },
      .package = "CerebroNexus"
    )
    rs <- list(call = function(fun, args) do.call(fun, args))
    plan <- list(
      out_dir = withr::local_tempdir(),
      items = list(),
      make_app = TRUE,
      app_contract_version = 1L,
      overwrite = FALSE
    )

    result <- builder_session_build(rs, plan)

    expect_match(result$error, "private app publication", ignore.case = TRUE)
    expect_false(create_called)
  })
})
