## Project dialogs and the save/open picker flow.

builder_dialogs_app <- function(env = parent.frame()) {
  withr::local_options(
    CerebroNexus.builder.project_worker_prewarm = FALSE,
    .local_envir = env
  )
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"), .local_envir = env)
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this dialog test.")
  }
  app_env
}

test_that("project pickers close the open dialog before the picker overlay", {
  skip_if_not_installed("shiny")
  app_env <- builder_dialogs_app()
  events <- new.env(parent = emptyenv())
  events$log <- character()
  testthat::local_mocked_bindings(
    removeModal = function(...) {
      events$log <- c(events$log, "remove")
      invisible()
    },
    .package = "shiny"
  )

  shiny::testServer(app_env$server, {
    server_env <- environment(request_builder_project_folder)
    server_env$builder_operation_allowed <- function(...) TRUE
    server_env$builder_project_open_needs_confirmation <- function() FALSE
    server_env$builder_schedule_native_picker <- function(kind, ...) {
      events$log <- c(events$log, paste0("picker:", kind))
      expect_identical(isolate(builder_project_operation_phase()), "choosing")
      invisible(TRUE)
    }

    events$log <- character()
    request_builder_project_folder()
    expect_identical(events$log, c("remove", "picker:project_directory"))

    builder_project_operation_phase("idle")
    events$log <- character()
    session$setInputs(open_builder_project = 1)
    expect_identical(events$log, c("remove", "picker:project_manifest"))
  })
})
