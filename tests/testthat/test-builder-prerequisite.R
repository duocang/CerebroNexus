builder_repo_source("prerequisite.R")
builder_repo_source("profile.R")
builder_repo_source("state.R")
builder_plan_contract_source_runtime(environment())

test_that("the app privacy marker must be the exact integer contract", {
  namespace <- new.env(parent = emptyenv())

  expect_identical(builder_installed_app_contract_version(namespace), 0L)

  assign(".cerebro_bundle_privacy_contract_version", 1, namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 0L)

  namespace <- new.env(parent = emptyenv())
  assign(".cerebro_bundle_privacy_contract_version", 1L, namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 1L)

  namespace <- new.env(parent = emptyenv())
  assign(".cerebro_bundle_privacy_contract_version", c(1L, 1L), namespace)
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 0L)
})

test_that("installed privacy contract v1 is eager and locked", {
  namespace <- asNamespace("CerebroNexus")
  marker <- ".cerebro_bundle_privacy_contract_version"

  expect_true(exists(marker, namespace, inherits = FALSE))
  expect_false(bindingIsActive(marker, namespace))
  expect_false(rlang::env_binding_are_lazy(namespace, marker))
  expect_true(bindingIsLocked(marker, namespace))
  expect_identical(get(marker, namespace, inherits = FALSE), 1L)
})

test_that("a function-valued app privacy marker is never executed", {
  namespace <- new.env(parent = emptyenv())
  called <- FALSE
  marker <- function() {
    called <<- TRUE
    1L
  }
  assign(".cerebro_bundle_privacy_contract_version", marker, namespace)
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)

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
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)

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
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)

  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  expect_false(forced)
})

test_that("source App contract takes precedence over the installed package", {
  source_root <- withr::local_tempdir()
  marker <- file.path(
    source_root,
    "inst",
    "builder",
    "app_bundle",
    "privacy-contract-version"
  )
  dir.create(dirname(marker), recursive = TRUE)
  writeLines("1", marker)

  expect_identical(builder_source_app_contract_version(source_root), 1L)

  available <- builder_app_capability(
    installed_contract_version = 0L,
    source_contract_version = 1L
  )
  expect_true(available$available)
  expect_identical(available$version, 1L)
  expect_null(available$reason)
})

test_that("an installed package directory is not mistaken for source", {
  installed_like <- withr::local_tempdir()
  file.copy(testthat::test_path("..", "..", "DESCRIPTION"), installed_like)
  dir.create(file.path(installed_like, "R"))

  expect_null(builder_source_package_root(installed_like))
})

test_that("Builder startup exports its active source to parent verification", {
  source_root <- withr::local_tempdir()
  file.copy(testthat::test_path("..", "..", "DESCRIPTION"), source_root)
  dir.create(file.path(source_root, "R"))
  file.create(file.path(source_root, "R", "source.R"))
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = NA_character_)

  expect_identical(
    builder_activate_source_package(source_root),
    normalizePath(source_root, winslash = "/", mustWork = TRUE)
  )
  expect_identical(
    Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = ""),
    normalizePath(source_root, winslash = "/", mustWork = TRUE)
  )
})

test_that("app capability explains a genuinely unavailable App contract", {
  unavailable <- builder_app_capability(
    installed_contract_version = 0L,
    source_contract_version = 0L
  )

  expect_identical(
    names(unavailable),
    c("available", "version", "reason")
  )
  expect_false(unavailable$available)
  expect_identical(unavailable$version, 0L)
  expect_identical(
    unavailable$reason,
    "This Builder cannot find a secure Viewer App export runtime."
  )
  expect_false(grepl("PR #", unavailable$reason, fixed = TRUE))

  available <- builder_app_capability(1L, source_contract_version = NULL)
  expect_true(available$available)
  expect_identical(available$version, 1L)
  expect_null(available$reason)

  expect_false(
    builder_app_capability(1, source_contract_version = NULL)$available
  )
})

test_that("login capability identifies only missing package requirements", {
  available <- function(package) package %in% c("shinymanager", "openssl")
  supported <- function(package) base::package_version("1.1.0")

  capability <- builder_auth_capability(available, supported)
  expect_true(capability$available)
  expect_identical(capability$missing, character())
  expect_null(capability$reason)

  missing_manager <- builder_auth_capability(
    function(package) identical(package, "openssl"),
    supported
  )
  expect_false(missing_manager$available)
  expect_identical(missing_manager$missing, "shinymanager (>= 1.1.0)")
  expect_match(missing_manager$reason, "Login is unavailable", fixed = TRUE)
  expect_match(
    missing_manager$reason,
    'install.packages("shinymanager")',
    fixed = TRUE
  )
  expect_match(missing_manager$reason, "restart Builder", fixed = TRUE)

  old_manager <- builder_auth_capability(
    available,
    function(package) base::package_version("1.0.9")
  )
  expect_false(old_manager$available)
  expect_identical(old_manager$missing, "shinymanager (>= 1.1.0)")

  unreadable_manager <- builder_auth_capability(
    available,
    function(package) stop("internal package read failure")
  )
  expect_false(unreadable_manager$available)
  expect_match(
    unreadable_manager$reason,
    "shinymanager (>= 1.1.0)",
    fixed = TRUE
  )
  expect_false(grepl(
    "internal package read failure",
    unreadable_manager$reason
  ))

  missing_openssl <- builder_auth_capability(
    function(package) identical(package, "shinymanager"),
    supported
  )
  expect_false(missing_openssl$available)
  expect_identical(missing_openssl$missing, "openssl")
})

test_that("Builder runtime capability blocks startup with exact guidance", {
  capability <- builder_runtime_capability(
    function(package) identical(package, "callr")
  )

  expect_false(capability$available)
  expect_identical(capability$missing, "openssl")
  expect_identical(
    capability$reason,
    paste(
      "Builder cannot start because this required R package is missing: openssl.",
      'Run install.packages("openssl"), then start Builder again.'
    )
  )

  unavailable <- builder_runtime_capability(function(package) FALSE)
  expect_identical(unavailable$missing, c("callr", "openssl"))
  expect_match(
    unavailable$reason,
    'install.packages(c("callr", "openssl"))',
    fixed = TRUE
  )
})

test_that("CRB-only plans remain available by default", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    expect_identical(formals(builder_make_plan)$make_app, FALSE)
    expect_false("app_capability" %in% names(formals(builder_make_plan)))
    plan <- builder_make_plan(
      list(builder_task6_entry()),
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
    builder_repo_source("prerequisite.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 0L
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    capability <- builder_app_capability(0L)

    plan <- builder_make_plan(
      list(builder_task6_entry()),
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
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE
    )

    expect_null(plan$error)
    expect_true(plan$make_app)
    expect_identical(plan$app_contract_version, 1L)
    expect_true(any(basename(plan$targets) == "cerebro_app"))
  })
})

test_that("builder UI loads the prerequisite before its plan", {
  lines <- builder_app_source_lines()
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
  expect_match(
    text,
    "app_capability <- builder_app_capability\\(\\)"
  )
})

test_that("builder UI activates source before loading App verification", {
  lines <- builder_app_source_lines()
  activate_source <- grep(
    "builder_activate_source_package()",
    lines,
    fixed = TRUE
  )
  app_bundle_source <- grep(
    'source("app_bundle.R", local = TRUE)',
    lines,
    fixed = TRUE
  )

  expect_length(activate_source, 1L)
  expect_length(app_bundle_source, 1L)
  expect_lt(activate_source, app_bundle_source)
})

test_that("worker setup loads the app prerequisite before planning", {
  session_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "worker.R"
  )
  if (!file.exists(session_path)) {
    session_path <- system.file(
      file.path("builder", "worker.R"),
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

test_that("worker rechecks the current active App contract", {
  local({
    builder_repo_source("session.R")
    builder_app_capability <- function(...) list(version = 0L)
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

test_that("session binds successful App results to the dispatched build id", {
  local({
    builder_repo_source("session.R")
    builder_app_capability <- function(...) list(version = 1L)
    builder_worker_require_capability <- function(name) invisible(TRUE)
    builder_execute_plan <- function(
      plan,
      stage,
      registry,
      auth_material = NULL,
      objects = list()
    ) {
      list(state = "success", publishable = TRUE, stage = stage)
    }
    builder_worker_response <- function(request, value = NULL, error = NULL) {
      if (!is.null(error)) list(error = error) else value
    }
    rs <- list(call = function(fun, args) do.call(fun, args))
    plan <- list(
      out_dir = withr::local_tempdir(),
      items = list(),
      make_app = TRUE,
      app_contract_version = 1L,
      overwrite = FALSE
    )

    result <- builder_session_build(
      rs,
      plan,
      request = list(build_id = "build-session")
    )

    expect_identical(result$build_id, "build-session")
  })
})
