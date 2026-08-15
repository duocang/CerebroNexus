builder_stage_contract_source_runtime(environment())

builder_open_app_runtime_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  app_dir <- file.path(root, "cerebro_app")
  database <- file.path(
    app_dir,
    "private-data",
    "auth",
    "credentials.sqlite"
  )
  dir.create(dirname(database), recursive = TRUE)
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  material <- builder_auth_create_material(
    accounts,
    root,
    .capability = function() list(available = TRUE, reason = NULL)
  )
  expect_true(file.copy(material$credentials, database))
  list(
    root = root,
    app_dir = app_dir,
    database = database,
    env_file = material$env_file,
    result = builder_result_success(
      published = TRUE,
      app_dir = app_dir,
      app_verified = TRUE,
      auth_enabled = TRUE,
      auth_env_file = material$env_file,
      release = list(target = root)
    )
  )
}

builder_open_app_child_in_callr <- function(path, env_file, previous = NULL) {
  app_bundle <- builder_profile_inst_path("builder", "app_bundle.R")
  build_status <- builder_profile_inst_path("builder", "ui", "build_status.R")
  callr::r(
    function(app_bundle, build_status, path, env_file, previous) {
      # `callr` starts outside the package checkout; make the assembly loader's
      # documented checkout-relative fallback available in this isolated child.
      setwd(dirname(dirname(dirname(app_bundle))))
      runtime <- new.env(parent = globalenv())
      sys.source(app_bundle, envir = runtime)
      sys.source(build_status, envir = runtime)
      launched <- FALSE
      if (!is.null(previous)) {
        do.call(
          Sys.setenv,
          stats::setNames(list(previous), "CEREBRO_AUTH_PASSPHRASE")
        )
      }
      before <- Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
      outcome <- tryCatch(
        {
          seen <- runtime$.builder_open_app_child(
            path,
            env_file,
            "CEREBRO_AUTH_PASSPHRASE",
            .run_app = function(...) {
              launched <<- TRUE
              Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
            }
          )
          list(ok = TRUE, seen = seen, error = NULL)
        },
        error = function(error) {
          list(ok = FALSE, seen = NULL, error = conditionMessage(error))
        }
      )
      list(
        outcome = outcome,
        launched = launched,
        before = before,
        after = Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
      )
    },
    args = list(
      app_bundle = app_bundle,
      build_status = build_status,
      path = path,
      env_file = env_file,
      previous = previous
    ),
    spinner = FALSE
  )
}

test_that("Open App child rejects post-verification authentication races", {
  skip_on_os("windows")
  skip_if_not_installed("callr")
  skip_if_not_installed("shinymanager")
  cases <- list(
    multiline = function(fixture) {
      writeLines(
        c(
          paste0("CEREBRO_AUTH_PASSPHRASE=", strrep("a", 64L)),
          "another=value"
        ),
        fixture$env_file
      )
      Sys.chmod(fixture$env_file, mode = "0600", use_umask = FALSE)
    },
    symlink = function(fixture) {
      target <- file.path(fixture$root, "alternate.env")
      expect_true(file.copy(fixture$env_file, target))
      unlink(fixture$env_file)
      expect_true(file.symlink(target, fixture$env_file))
    },
    hardlink = function(fixture) {
      target <- file.path(fixture$root, "alternate.env")
      expect_true(file.copy(fixture$env_file, target))
      unlink(fixture$env_file)
      expect_true(file.link(target, fixture$env_file))
    },
    mode = function(fixture) {
      Sys.chmod(fixture$env_file, mode = "0644", use_umask = FALSE)
    },
    mismatch = function(fixture) {
      writeLines(
        paste0("CEREBRO_AUTH_PASSPHRASE=", strrep("b", 64L)),
        fixture$env_file
      )
      Sys.chmod(fixture$env_file, mode = "0600", use_umask = FALSE)
    },
    database = function(fixture) {
      replacement <- file.path(fixture$root, "replacement.sqlite")
      writeBin(as.raw(seq_len(8L)), replacement)
      unlink(fixture$database)
      expect_true(file.rename(replacement, fixture$database))
    }
  )
  for (name in names(cases)) {
    fixture <- builder_open_app_runtime_fixture()
    child <- NULL
    opened <- builder_open_final_app(
      fixture$result,
      .open = function(path, env_file) {
        cases[[name]](fixture)
        child <<- builder_open_app_child_in_callr(path, env_file)
        FALSE
      }
    )
    expect_false(opened, info = name)
    expect_false(child$outcome$ok, info = name)
    expect_false(child$launched, info = name)
    expect_identical(
      child$outcome$error,
      "The authentication environment is invalid.",
      info = name
    )
    expect_true(is.na(child$after), info = name)
  }
})

test_that("Open App child launches only a matching pair and restores its environment", {
  skip_on_os("windows")
  skip_if_not_installed("callr")
  skip_if_not_installed("shinymanager")
  fixture <- builder_open_app_runtime_fixture()
  child <- builder_open_app_child_in_callr(
    fixture$app_dir,
    fixture$env_file,
    previous = "preexisting-auth-environment-value"
  )

  expect_true(child$outcome$ok)
  expect_true(child$launched)
  expect_identical(
    child$outcome$seen,
    builder_auth_read_env_file(fixture$env_file)
  )
  expect_identical(child$before, "preexisting-auth-environment-value")
  expect_identical(child$after, "preexisting-auth-environment-value")
})

test_that("Open App uses its default r_bg launcher without parent helpers", {
  skip_if_not_installed("callr")
  skip_if_not_installed("shinymanager")
  fixture <- builder_open_app_runtime_fixture()
  process <- NULL

  expect_true(builder_open_final_app(
    fixture$result,
    .run_app = function(...) {
      Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
    },
    .on_open = function(value) {
      process <<- value
    }
  ))
  expect_s3_class(process, "r_process")
  process$wait(10000)
  expect_false(process$is_alive())
  expect_identical(
    process$get_result(),
    builder_auth_read_env_file(fixture$env_file)
  )
})

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
