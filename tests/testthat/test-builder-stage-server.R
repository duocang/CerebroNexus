builder_stage_contract_source_runtime(environment())
builder_profile_source_runtime(environment())
builder_plan_contract_source_runtime(environment())

test_that("Builder shell and workflow UI separate all four stages", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  shell <- builder_app_source_text()
  expect_false(grepl('uiOutput("actionbar")', shell, fixed = TRUE))
  expect_false(grepl('uiOutput("result_card")', shell, fixed = TRUE))
  expect_identical(
    lengths(regmatches(
      shell,
      gregexpr('uiOutput("build_stage_status")', shell, fixed = TRUE)
    )),
    0L
  )
  expect_match(shell, 'uiOutput("workflow_progress")', fixed = TRUE)
  expect_match(shell, 'file.path("ui", "workflow.R")', fixed = TRUE)
  expect_match(shell, '"server/workflow.R"', fixed = TRUE)
  expect_match(shell, "Build your first Viewer in four steps", fixed = TRUE)

  progress <- app_env$builder_workflow_progress_ui(
    "configure",
    available = c(
      upload = TRUE,
      configure = TRUE,
      review = FALSE,
      build = FALSE
    ),
    confirmed = FALSE,
    locked = FALSE
  )
  progress_html <- htmltools::renderTags(progress)$html
  expect_match(progress_html, "builder-workflow-progress", fixed = TRUE)
  expect_match(progress_html, 'aria-label="Builder progress"', fixed = TRUE)
  expect_match(progress_html, 'aria-current="step"', fixed = TRUE)
  expect_match(progress_html, 'data-workflow-confirmed="false"', fixed = TRUE)
  expect_match(progress_html, "Data setup", fixed = TRUE)
  expect_false(grepl(">Configure<", progress_html, fixed = TRUE))
  expect_match(progress_html, 'id="workflow_stage_upload"', fixed = TRUE)
  expect_match(progress_html, 'aria-disabled="true"', fixed = TRUE)
  expect_match(progress_html, "is-unavailable", fixed = TRUE)
  expect_false(grepl("✓|✔|checkmark", progress_html, ignore.case = TRUE))
  confirmed_progress <- htmltools::renderTags(
    app_env$builder_workflow_progress_ui(
      "build",
      available = stats::setNames(
        rep(TRUE, 4L),
        c(
          "upload",
          "configure",
          "review",
          "build"
        )
      ),
      confirmed = TRUE,
      locked = FALSE
    )
  )$html
  expect_match(
    confirmed_progress,
    'data-workflow-confirmed="true"',
    fixed = TRUE
  )
  expect_identical(
    lengths(regmatches(
      progress_html,
      gregexpr("<li", progress_html, fixed = TRUE)
    )),
    4L
  )
  expect_error(
    app_env$builder_workflow_progress_ui(
      "future",
      available = stats::setNames(
        rep(FALSE, 4L),
        c(
          "upload",
          "configure",
          "review",
          "build"
        )
      ),
      confirmed = FALSE,
      locked = FALSE
    ),
    "valid Builder workflow stage"
  )

  actions <- app_env$builder_configure_actions_ui(
    "Wait for all datasets to finish loading.",
    can_continue = FALSE
  )
  actions_html <- htmltools::renderTags(actions)$html
  expect_match(actions_html, "builder-stage-footer", fixed = TRUE)
  expect_match(actions_html, "builder-stage-footer-status", fixed = TRUE)
  expect_match(actions_html, "builder-stage-footer-actions", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      actions_html,
      gregexpr('id="continue_to_review"', actions_html, fixed = TRUE)
    )),
    1L
  )
  expect_match(actions_html, ">Continue<", fixed = TRUE)
  expect_match(actions_html, " disabled", fixed = TRUE)
  expect_false(grepl("make_app", actions_html, fixed = TRUE))
  expect_false(grepl("Create a Viewer app", actions_html, fixed = TRUE))

  ready_html <- htmltools::renderTags(app_env$builder_configure_actions_ui(
    "1 dataset ready",
    can_continue = TRUE
  ))$html
  expect_match(ready_html, "1 dataset ready", fixed = TRUE)
  expect_false(grepl(" disabled", ready_html, fixed = TRUE))

  confirmation_html <- htmltools::renderTags(
    app_env$builder_review_confirmation_ui()
  )$html
  expect_identical(
    lengths(regmatches(
      confirmation_html,
      gregexpr('id="confirm_review"', confirmation_html, fixed = TRUE)
    )),
    1L
  )
  expect_identical(
    lengths(regmatches(
      confirmation_html,
      gregexpr('id="back_to_settings"', confirmation_html, fixed = TRUE)
    )),
    1L
  )
  expect_match(confirmation_html, "builder-stage-footer", fixed = TRUE)
  expect_match(confirmation_html, "CRB plan ready", fixed = TRUE)
  expect_match(
    confirmation_html,
    "Continue to Build",
    fixed = TRUE
  )
  expect_match(confirmation_html, "Back to Data setup", fixed = TRUE)
  expect_false(grepl("Ready to continue?", confirmation_html, fixed = TRUE))
  expect_false(grepl("<input|<select|<textarea", confirmation_html))
})

test_that("Build output UI locks CRB-only when external images require an App", {
  skip_if_not_installed("shiny")
  options <- builder_build_options(make_app = TRUE)
  html <- htmltools::renderTags(builder_build_options_ui(
    options,
    app_required = TRUE
  ))$html

  expect_match(
    html,
    "External spatial images require CRB files + Viewer App output.",
    fixed = TRUE
  )
  expect_match(
    html,
    '<input type="radio" name="build_output_mode" value="crb" disabled="disabled" aria-disabled="true"/>',
    fixed = TRUE
  )
  expect_match(
    html,
    '<input type="radio" name="build_output_mode" value="app" checked="checked"/>',
    fixed = TRUE
  )
  expect_error(
    builder_build_options_ui(options, app_required = c(TRUE, FALSE))
  )
  expect_error(builder_build_options_ui(options, app_required = NA))
})

test_that("shared stage layout primitives expose one visual grammar", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  header <- app_env$builder_stage_header_ui(
    "Data setup",
    "Choose data to include",
    "Define the content saved to each CRB file."
  )
  section <- app_env$builder_stage_section_ui(
    "Core content",
    htmltools::tags$p("Required content")
  )
  footer <- app_env$builder_stage_footer_ui(
    "1 dataset ready",
    shiny::actionButton("continue_to_review", "Continue")
  )
  html <- htmltools::renderTags(htmltools::tagList(
    header,
    section,
    footer
  ))$html

  expect_match(html, "builder-stage-header", fixed = TRUE)
  expect_match(html, "builder-stage-eyebrow", fixed = TRUE)
  expect_match(html, "builder-stage-section", fixed = TRUE)
  expect_match(html, "builder-stage-footer", fixed = TRUE)
  expect_match(html, "builder-stage-footer-status", fixed = TRUE)
  expect_match(html, "builder-stage-footer-actions", fixed = TRUE)
  expect_match(html, 'id="continue_to_review"', fixed = TRUE)
})

test_that("Build stage exclusively owns its live status projection", {
  app <- builder_app_source_text()
  workflow_ui <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  build_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  enhancements <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "enhancements.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  status_renderer <- substr(
    build_server,
    regexpr(
      "build_stage_status_projection <- reactive({",
      build_server,
      fixed = TRUE
    ),
    regexpr(
      "builder_build_confirmation_status <- function",
      build_server,
      fixed = TRUE
    ) -
      1L
  )

  expect_identical(
    lengths(regmatches(
      workflow_ui,
      gregexpr(
        'uiOutput("build_stage_status_content")',
        workflow_ui,
        fixed = TRUE
      )
    )),
    1L
  )
  expect_identical(
    lengths(regmatches(
      workflow_ui,
      gregexpr('uiOutput("build_stage_footer")', workflow_ui, fixed = TRUE)
    )),
    1L
  )
  expect_identical(
    lengths(regmatches(
      build_server,
      gregexpr(
        "output$build_stage_status_content <- renderUI({",
        build_server,
        fixed = TRUE
      )
    )),
    1L
  )
  expect_match(status_renderer, "flow = build_flow()", fixed = TRUE)
  expect_match(status_renderer, "protocol = protocol()", fixed = TRUE)
  expect_match(status_renderer, "note = busy_note()", fixed = TRUE)
  expect_match(status_renderer, "result = result()", fixed = TRUE)
  expect_match(status_renderer, "selected_output()", fixed = TRUE)
  expect_false(grepl("isolate(build_flow())", status_renderer, fixed = TRUE))
  expect_false(grepl("isolate(protocol())", status_renderer, fixed = TRUE))
  expect_false(grepl("output$result_card", enhancements, fixed = TRUE))
  expect_false(grepl('uiOutput("result_card")', app, fixed = TRUE))
  expect_match(
    build_server,
    'identical(workflow()$stage, "build")',
    fixed = TRUE
  )
})

test_that("Build result survives failed folder selection and clears on acceptance", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this folder lifecycle test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$builder_freeze_plan <- function(...) builder_stage_frozen_plan(FALSE)

  shiny::testServer(app_env$server, {
    real_session <- session
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a"),
      settings = list(name = "Dataset A")
    )
    use_state_only_fixture(list(entry))
    real_session$setInputs(make_app = FALSE)
    real_session$flushReact()
    plan <- isolate(frozen_review_plan())
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", plan = plan)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", plan = plan)
    ))
    selected_output("/old/output")
    protocol(app_env$builder_request_protocol("worker-folder"))
    success <- app_env$builder_result_success(
      published = TRUE,
      built = "/old/output/dataset.crb"
    )
    result(success)
    choices <- list(
      list(status = "cancelled", path = NULL),
      list(status = "error", path = NULL, error = "picker failed"),
      list(status = "selected", path = "/new/output")
    )
    folder_env <- environment(choose_build_folder)
    assign(
      "session",
      list(onFlushed = function(callback, once = FALSE) callback()),
      envir = folder_env
    )
    assign(
      "builder_choose_output_directory",
      function(...) {
        choice <- choices[[1L]]
        choices <<- choices[-1L]
        choice
      },
      envir = folder_env
    )
    assign("showNotification", function(...) NULL, envir = folder_env)

    pending_protocol <- app_env$builder_request_protocol("worker-pending")
    pending_protocol <- app_env$builder_enqueue(
      pending_protocol,
      app_env$builder_query("preview", "dataset-a", generation = 1L)
    )
    result(NULL)
    protocol(pending_protocol)
    busy_note("Preparing preview…")
    real_session$flushReact()
    pending_content <- paste(
      unlist(output$build_stage_status_content),
      collapse = " "
    )
    expect_match(pending_content, "Preparing preview…", fixed = TRUE)
    pending_footer <- paste(unlist(output$build_stage_footer), collapse = " ")
    expect_match(pending_footer, " disabled", fixed = TRUE)
    expect_length(output$busy, 0L)
    protocol(app_env$builder_request_protocol("worker-folder"))
    busy_note(NULL)
    result(success)

    choose_build_folder()
    real_session$flushReact()
    expect_identical(result(), success)
    expect_identical(selected_output(), "/old/output")
    choose_build_folder()
    real_session$flushReact()
    expect_identical(result(), success)
    expect_identical(selected_output(), "/old/output")
    choose_build_folder()
    real_session$flushReact()
    expect_null(result())
    expect_identical(selected_output(), "/new/output")
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    content <- paste(unlist(output$build_stage_footer), collapse = " ")
    expect_match(content, ">Build<", fixed = TRUE)
    expect_false(grepl(" disabled", content, fixed = TRUE))
  })
})

test_that("workflow server owns loading and Configure rendering", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this workflow test.")
  }
  shiny::testServer(app_env$server, {
    expect_identical(workflow()$stage, "upload")
    expect_match(
      paste(unlist(output$workflow_progress), collapse = " "),
      "Upload",
      fixed = TRUE
    )

    use_state_only_fixture(list(list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a"),
      settings = list(name = "Dataset A")
    )))
    session$flushReact()
    expect_identical(workflow()$stage, "configure")

    session$setInputs(workflow_stage_review = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, "configure")

    session$setInputs(workflow_stage_upload = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, "upload")
    session$flushReact()
    expect_identical(workflow()$stage, "upload")

    session$setInputs(workflow_stage_configure = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, "configure")
  })

  workflow_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(workflow_server, "builder_loading_workbench_ui", fixed = TRUE)
  expect_match(workflow_server, "render_configure_workbench", fixed = TRUE)
  expect_match(workflow_server, "render_review_workbench", fixed = TRUE)
  expect_match(workflow_server, "render_build_workbench", fixed = TRUE)
  expect_match(
    workflow_server,
    "Unsupported Builder workflow stage",
    fixed = TRUE
  )

  review_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    workflow_server,
    "plan <- isolate(frozen_review_plan())",
    fixed = TRUE
  )
  expect_match(review_server, "plan <- workflow()$review_plan", fixed = TRUE)
  expect_match(review_server, "input$back_to_settings", fixed = TRUE)
  expect_match(review_server, "input$confirm_review", fixed = TRUE)
  expect_false(grepl("review_current_dataset", review_server, fixed = TRUE))
  expect_false(grepl("dataset_review_footer", review_server, fixed = TRUE))
})

test_that("build completion preserves decisions and always has an idle ack path", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  release <- list(handle = list(target = "/release"))
  decision <- list(
    state = "needs_decision",
    publishable = FALSE,
    error = "Choose analyses to retry.",
    failed_analyses = "marker_genes",
    retry_closure = c("marker_genes", "enriched_pathways")
  )
  settled <- app_env$builder_app_settle_release(
    release,
    decision,
    .abort = function(handle) list(aborted = TRUE)
  )
  expect_s3_class(settled, "builder_result_needs_decision")
  expect_identical(settled$failed_analyses, "marker_genes")
  expect_identical(
    settled$retry_closure,
    c("marker_genes", "enriched_pathways")
  )
  expect_identical(
    app_env$builder_app_build_action(settled, "build-a")$type,
    "needs_decision"
  )

  cleanup_failure <- app_env$builder_app_settle_release(
    release,
    decision,
    .abort = function(handle) stop("stage cleanup failed"),
    .release_error = function(message, target) {
      expect_match(message, "cleanup failed", fixed = TRUE)
      expect_identical(target, "/release")
      app_env$builder_result_recovery_required(
        "Restore the preserved stage.",
        recovery = list(state = "recovery_required")
      )
    }
  )
  expect_s3_class(cleanup_failure, "builder_result_recovery_required")

  recovery <- app_env$builder_result_recovery_required("Restore the backup.")
  recovery_action <- app_env$builder_app_build_action(recovery, "build-a")
  expect_identical(recovery_action$type, "fail")
  expect_match(recovery_action$error, "Restore the backup", fixed = TRUE)

  protocol <- app_env$builder_request_protocol("worker-a")
  queued <- app_env$builder_enqueue(
    protocol,
    app_env$builder_command(
      "build",
      "session",
      payload = list(id = "build-a")
    )
  )
  dispatched <- app_env$builder_protocol_dispatch(queued)
  completed <- app_env$builder_protocol_complete(
    dispatched$protocol,
    app_env$builder_worker_response(
      dispatched$request,
      list(state = "recovery_required", error = "Restore the backup.")
    )
  )
  expect_length(completed$protocol$awaiting_ack, 1L)
  acknowledged <- app_env$builder_app_acknowledge_build(
    completed$protocol,
    dispatched$request$request_id
  )
  expect_length(acknowledged$awaiting_ack, 0L)
  expect_identical(acknowledged$build_status, "idle")

  app <- builder_app_source_text()
  expect_match(app, "on.exit(", fixed = TRUE)
  expect_match(app, "builder_app_acknowledge_build", fixed = TRUE)
})

test_that("Review inputs fail explicitly and recover without rebuilding inputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    invalid <- list(
      welcome_message = "Welcome",
      point_size = 5,
      variable_to_compare = FALSE,
      host = "127.0.0.1",
      port = 0,
      max_request_size = 8000,
      display_mode = "normal",
      launch_browser = TRUE,
      show_upload_ui = FALSE
    )
    expect_false(validate_review_inputs(invalid))
    expect_false(review_validation()$ok)
    expect_match(review_validation()$error, "Review options", fixed = TRUE)
    expect_identical(frozen_review_plan()$error_code, "empty_release")
    invalid_plan <- freeze_plan_for_output(
      tempfile("invalid-viewer-options-"),
      output_options = builder_build_options(make_app = TRUE)
    )
    expect_identical(invalid_plan$error_code, "empty_release")
    expect_false(app_env$builder_review_can_build(invalid_plan))

    invalid$port <- 8080L
    expect_true(validate_review_inputs(invalid))
    expect_true(review_validation()$ok)
    expect_s3_class(review_options(), "builder_review_options")
    expect_identical(frozen_review_plan()$error_code, "empty_release")
  })

  review_source <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(review_source, "render_configure_workbench", fixed = TRUE)
  expect_false(grepl(
    'uiOutput("review_app_options")',
    review_source,
    fixed = TRUE
  ))
  expect_false(grepl("output$workbench <-", review_source, fixed = TRUE))
  expect_false(grepl("output$actionbar <-", review_source, fixed = TRUE))
  expect_false(grepl("output$build_actions <-", review_source, fixed = TRUE))
})

test_that("external spatial images carry required App output through Review", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  real_builder_freeze_plan <- app_env$builder_freeze_plan
  app_env$builder_freeze_plan <- function(
    entries,
    out_dir,
    make_app,
    overwrite,
    app_options,
    app_auth
  ) {
    if (!isTRUE(make_app)) {
      return(real_builder_freeze_plan(
        entries = entries,
        out_dir = out_dir,
        make_app = make_app,
        overwrite = overwrite,
        app_options = app_options,
        app_auth = app_auth
      ))
    }
    plan <- builder_stage_frozen_plan(TRUE)
    plan$dataset_order <- vapply(entries, `[[`, character(1), "id")
    plan$app_options <- app_options
    plan$app_auth <- app_auth
    plan$out_dir <- out_dir
    plan$overwrite <- overwrite
    plan
  }

  shiny::testServer(app_env$server, {
    entry <- builder_task6_entry()
    image <- list(
      source = list(name = "H&E.png", type = "image/png", size = 4),
      source_uri = "data:image/png;base64,AAAA",
      uri = "data:image/png;base64,AAAA",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      dx = 0,
      dy = 0,
      scale = 1,
      rotation = 0,
      flip_x = FALSE,
      flip_y = FALSE,
      image_opacity = 0.8,
      point_opacity = 0.85,
      point_size = 5,
      saved = TRUE,
      outside = 0L,
      section_id = "fov",
      section_kind = "spatial"
    )
    entry$dataset_profile$spatial <- list(sections = "fov")
    entry$settings$images <- list(fov = list(`H&E` = image))
    entry$settings$spatial_image_storage <- "external"
    use_state_only_fixture(list(entry))

    plan <- frozen_review_plan()
    expect_null(plan$error)
    expect_identical(plan$readiness, "ready")
    expect_true(plan$make_app)

    configured <- app_env$builder_reduce_workflow(
      isolate(workflow()),
      list(type = "datasets_ready")
    )
    workflow(app_env$builder_reduce_workflow(
      configured,
      list(type = "open_review", plan = plan)
    ))
    session$setInputs(confirm_review = 1L)
    session$flushReact()
    expect_identical(workflow()$stage, "build")
    expect_true(build_mode())

    session$setInputs(build_output_mode = "crb")
    session$flushReact()
    expect_true(build_mode())

    app_env$app_capability$available <- FALSE
    session$setInputs(build_output_mode = "app")
    session$flushReact()
    expect_false(build_mode())

    blocked <- freeze_plan_for_output(
      tempfile("external-images-crb-"),
      output_options = builder_build_options(make_app = FALSE)
    )
    expect_identical(blocked$error_code, "external_images_require_app")
  })
})

test_that("workbench identity ignores settings writes but tracks selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    entry <- function(id) {
      list(
        id = id,
        revision = 0L,
        snapshot = list(
          path = paste0("/private/", id),
          owner_token = paste0("owner-", id),
          object_md5 = strrep(substr(id, nchar(id), nchar(id)), 32L)
        ),
        profile = list(marker = id),
        settings = list(name = id)
      )
    }
    use_state_only_fixture(list(entry("dataset-a"), entry("dataset-b")))
    session$flushReact()
    renders <- 0L
    tracker <- observe({
      current()
      renders <<- renders + 1L
    })
    withr::defer(tracker$destroy())
    session$flushReact()
    baseline <- renders

    changed <- sets()[[1L]]
    changed$settings$name <- "Dataset A renamed"
    expect_true(replace_entry(changed))
    session$flushReact()
    expect_identical(renders, baseline)

    use_state_only_fixture(list(entry("dataset-b"), entry("dataset-a")))
    session$flushReact()
    expect_identical(renders, baseline + 1L)
  })
})

test_that("Builder auth accepts only the exact typed browser payload", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    valid_accounts <- list(list(
      id = "auth-account-1",
      username = "user-a",
      password = "password-a"
    ))
    session$setInputs(
      builder_auth_accounts = list(
        enabled = "true",
        accounts = valid_accounts,
        nonce = 1
      )
    )
    session$flushReact()
    expect_false(auth_enabled())
    expect_length(auth_accounts(), 0L)

    build_mode(TRUE)
    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = valid_accounts,
        nonce = list(1)
      )
    )
    session$flushReact()
    expect_false(auth_enabled())
    expect_length(auth_accounts(), 0L)

    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = valid_accounts,
        nonce = 2
      )
    )
    session$flushReact()
    expect_true(auth_enabled())
    expect_s3_class(auth_accounts(), "builder_auth_accounts")
    expect_identical(auth_accounts()[[1L]]$username, "user-a")

    session$setInputs(builder_auth_accounts = NULL)
    session$flushReact()
    expect_true(auth_enabled())
    expect_length(auth_accounts(), 1L)

    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = list(list(
          id = "auth-account-1",
          username = "user-a",
          password = "short"
        )),
        nonce = 3
      )
    )
    session$flushReact()
    expect_true(auth_enabled())
    expect_length(auth_accounts(), 1L)
    expect_false(auth_validation()$ok)
    expect_identical(
      auth_validation()$error,
      "Login accounts could not be saved."
    )
    expect_false(grepl("short", auth_validation()$error, fixed = TRUE))

    build_mode(FALSE)
    session$flushReact()
    expect_false(auth_enabled())
    expect_s3_class(auth_accounts(), "builder_auth_accounts")
    expect_length(auth_accounts(), 0L)

    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = valid_accounts,
        nonce = 4
      )
    )
    session$flushReact()
    expect_false(auth_enabled())
    expect_length(auth_accounts(), 0L)
    expect_false(auth_validation()$ok)
    expect_identical(
      auth_validation()$error,
      "Login accounts could not be saved."
    )

    app_env$auth_capability$available <- FALSE
    build_mode(TRUE)
    session$setInputs(
      builder_auth_accounts = list(
        enabled = TRUE,
        accounts = valid_accounts,
        nonce = 5
      )
    )
    session$flushReact()
    expect_false(auth_enabled())
    expect_length(auth_accounts(), 0L)
    expect_false(auth_validation()$ok)
    expect_identical(
      auth_validation()$error,
      "Login accounts could not be saved."
    )
  })
})

test_that("Build-only auth changes preserve the confirmed CRB review", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$auth_capability$available <- TRUE
  app_env$builder_freeze_plan <- function(
    entries,
    out_dir,
    make_app,
    overwrite,
    app_options,
    app_auth
  ) {
    plan <- builder_stage_frozen_plan(make_app)
    plan$dataset_order <- vapply(entries, `[[`, character(1), "id")
    plan$app_options <- app_options
    plan$app_auth <- app_auth
    plan$out_dir <- out_dir
    plan$overwrite <- overwrite
    plan
  }

  shiny::testServer(app_env$server, {
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a"),
      settings = list(name = "Dataset A")
    )
    use_state_only_fixture(list(entry))
    accounts_a <- app_env$builder_auth_validate_payload(
      TRUE,
      list(list(
        id = "auth-account-1",
        username = "user-a",
        password = "password-a"
      ))
    )$accounts
    accounts_b <- app_env$builder_auth_validate_payload(
      TRUE,
      c(
        unclass(accounts_a),
        list(list(
          id = "auth-account-2",
          username = "user-b",
          password = "password-b"
        ))
      )
    )$accounts
    auth_enabled(TRUE)
    auth_accounts(accounts_a)
    build_mode(TRUE)
    session$flushReact()

    review_plan <- frozen_review_plan()
    expect_false(review_plan$make_app)
    expect_identical(review_plan$app_auth$account_count, 0L)
    reviewed <- app_env$builder_reduce_workflow(
      isolate(workflow()),
      list(type = "open_review", plan = review_plan)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", plan = review_plan)
    ))
    expect_identical(workflow()$stage, "build")

    review_validation(list(
      ok = FALSE,
      error = "Viewer App options are invalid."
    ))
    session$flushReact()
    expect_identical(workflow()$stage, "build")
    expect_true(builder_build_confirmation_matches(frozen_review_plan()))
    invalid_app_plan <- freeze_plan_for_output(
      tempfile("invalid-app-output-"),
      output_options = current_build_options()
    )
    expect_identical(invalid_app_plan$error_code, "invalid_review_options")
    build_mode(FALSE)
    crb_plan <- freeze_plan_for_output(
      tempfile("valid-crb-output-"),
      output_options = current_build_options()
    )
    expect_true(app_env$builder_review_can_build(crb_plan))
    build_mode(TRUE)
    review_validation(list(ok = TRUE, error = NULL))

    auth_accounts(accounts_b)
    session$flushReact()

    expect_identical(workflow()$stage, "build")
    expect_true(builder_build_confirmation_matches(review_plan))
    expect_identical(workflow()$review_plan, review_plan)

    review_options(builder_review_options(
      welcome_message = "Build-stage welcome",
      initial_page = "projection",
      host = "0.0.0.0",
      port = 4242L,
      launch_browser = FALSE,
      show_upload_ui = TRUE
    ))
    build_initial_dataset("dataset-a")
    plan_b <- freeze_plan_for_output(
      tempfile("builder-app-output-"),
      output_options = current_build_options()
    )
    expect_true(plan_b$make_app)
    expect_identical(plan_b$app_auth$account_count, 2L)
    expect_named(
      plan_b$app_auth,
      c("enabled", "account_count", "timeout_minutes")
    )
    expect_false("accounts" %in% names(plan_b$app_auth))
    expect_identical(plan_b$app_options$welcome_message, "Build-stage welcome")
    expect_identical(plan_b$app_options$host, "0.0.0.0")
    expect_identical(plan_b$app_options$port, 4242L)
    expect_false(plan_b$app_options$launch_browser)
    expect_true(plan_b$app_options$show_upload_ui)
    expect_identical(plan_b$app_options$initial_dataset, "dataset-a")
    expect_identical(plan_b$app_options$initial_page, "projection")
    expect_false(any(grepl(
      "password-a|password-b|user-a|user-b",
      capture.output(dput(plan_b))
    )))
    expect_true(builder_build_confirmation_matches(plan_b))

    build_flow(list(stage = "building", plan = NULL))
    session$setInputs(build_output_mode = "crb")
    session$flushReact()
    expect_true(build_mode())
  })
})

test_that("Build enqueue retains auth after failure and resets only after success", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  shiny::testServer(app_env$server, {
    expect_identical(
      names(formals(enqueue_build_plan)),
      c("plan", "auth_accounts", "expected_identity")
    )
    validate_auth <- get(
      "builder_auth_validate_payload",
      envir = environment(enqueue_build_plan),
      inherits = TRUE
    )
    summarize_auth <- get(
      "builder_auth_summary",
      envir = environment(enqueue_build_plan),
      inherits = TRUE
    )
    accounts <- validate_auth(
      TRUE,
      list(list(
        id = "auth-account-1",
        username = "user-a",
        password = "password-a"
      ))
    )$accounts
    plan <- builder_stage_frozen_plan()
    plan$app_auth <- summarize_auth(TRUE, accounts)
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", plan = plan)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", plan = plan)
    ))
    fn_env <- environment(enqueue_build_plan)
    messages <- list()
    assign(
      "session",
      list(sendCustomMessage = function(type, message) {
        messages[[length(messages) + 1L]] <<- list(
          type = type,
          message = message
        )
      }),
      envir = fn_env
    )
    worker(list(alive = TRUE))
    request_protocol <- get(
      "builder_request_protocol",
      envir = fn_env,
      inherits = TRUE
    )
    protocol(request_protocol("worker-a"))
    auth_enabled(TRUE)
    auth_accounts(accounts)
    auth_validation(list(ok = TRUE, error = NULL))

    queued_payload <- NULL
    assign(
      "enqueue",
      function(payload) {
        queued_payload <<- payload
        FALSE
      },
      envir = fn_env
    )
    changed_output <- plan
    changed_output$app_options$welcome_message <- "Changed before enqueue"
    expect_false(enqueue_build_plan(
      changed_output,
      auth_accounts = accounts,
      expected_identity = app_env$builder_final_build_identity(plan)
    ))
    expect_null(queued_payload)
    expect_identical(auth_accounts(), accounts)
    messages <- list()

    expect_false(enqueue_build_plan(plan, auth_accounts = accounts))
    expect_s3_class(queued_payload$auth_accounts, "builder_auth_accounts")
    expect_identical(queued_payload$auth_accounts, accounts)
    expect_identical(auth_accounts(), accounts)
    expect_true(auth_validation()$ok)
    expect_length(messages, 1L)
    expect_identical(messages[[1L]]$type, "builder_build_dialog")
    expect_identical(messages[[1L]]$message, list(action = "close"))

    messages <- list()
    assign("enqueue", function(payload) TRUE, envir = fn_env)
    expect_true(enqueue_build_plan(plan, auth_accounts = accounts))
    expect_s3_class(auth_accounts(), "builder_auth_accounts")
    expect_length(auth_accounts(), 0L)
    expect_false(auth_validation()$ok)
    expect_match(auth_validation()$error, "Set up", fixed = TRUE)
    expect_identical(
      vapply(messages, `[[`, character(1), "type"),
      "builder_auth_reset"
    )
    expect_identical(messages[[1L]]$message, list(reset = TRUE))
  })
})

test_that("Build dialogs cannot enqueue a stale frozen revision", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  shiny::testServer(app_env$server, {
    fn_env <- environment(builder_require_confirmed_build_plan)
    notifications <- character()
    dialog_messages <- list()
    assign(
      "showNotification",
      function(ui, ...) {
        notifications <<- c(notifications, as.character(ui))
      },
      envir = fn_env
    )
    assign(
      "session",
      list(sendCustomMessage = function(type, message) {
        dialog_messages[[length(dialog_messages) + 1L]] <<- list(
          type = type,
          message = message
        )
      }),
      envir = fn_env
    )
    plan_a <- builder_stage_frozen_plan()
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", plan = plan_a)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", plan = plan_a)
    ))
    expect_true(builder_build_confirmation_matches(plan_a))

    build_flow(list(stage = "confirming", plan = plan_a))
    workflow(app_env$builder_reduce_workflow(
      isolate(workflow()),
      list(type = "invalidate")
    ))
    expect_false(builder_require_confirmed_build_plan(plan_a))
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))

    workflow(app_env$builder_reduce_workflow(
      app_env$builder_reduce_workflow(
        app_env$builder_workflow_state(),
        list(type = "open_review", plan = plan_a)
      ),
      list(type = "confirm_review", plan = plan_a)
    ))
    relocated <- plan_a
    relocated$out_dir <- tempfile("relocated-output-")
    expect_true(builder_build_confirmation_matches(relocated))

    plan_b <- plan_a
    plan_b$items[[1L]]$name <- "Changed after dialog opened"
    build_flow(list(stage = "conflict", plan = plan_a))
    guard <- builder_build_confirmation_status(isolate(workflow()), plan_b)
    expect_identical(guard, list(ok = FALSE, reason = "identity_mismatch"))
    expect_false(builder_require_confirmed_build_plan(plan_b))
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(workflow()$stage, "configure")
    expect_null(workflow()$review_plan)
    expect_null(workflow()$confirmation)
    expect_match(
      tail(notifications, 1L),
      "Settings changed. Review the updated plan before building.",
      fixed = TRUE
    )
    expect_true(any(vapply(
      dialog_messages,
      function(message) identical(message$message, list(action = "close")),
      logical(1)
    )))
    enqueued <- FALSE
    assign(
      "enqueue",
      function(...) {
        enqueued <<- TRUE
        TRUE
      },
      envir = environment(enqueue_build_plan)
    )
    expect_false(enqueue_build_plan(
      plan_b,
      auth_accounts = app_env$builder_auth_empty_accounts()
    ))
    expect_false(enqueued)
  })

  build_source <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    build_source,
    "builder_require_confirmed_build_plan(plan, plan$out_dir)",
    fixed = TRUE
  )
  expect_match(build_source, 'reason = "output_mismatch"', fixed = TRUE)
  expect_match(
    build_source,
    'list(action = "close")',
    fixed = TRUE
  )
})

test_that("Build conflict actions preserve confirmation and fail closed", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  output_dir <- withr::local_tempdir()
  replacement_dir <- withr::local_tempdir()
  previous_target <- file.path(output_dir, "previous.crb")
  writeLines("existing", previous_target)
  writeLines(
    c("CEREBRO_BUILDER_RELEASE_V1", "F\tprevious.crb"),
    file.path(output_dir, ".cerebro-builder-release-v1")
  )
  app_env$builder_freeze_plan <- function(
    entries,
    out_dir,
    make_app,
    overwrite,
    app_options,
    app_auth
  ) {
    plan <- builder_stage_frozen_plan(FALSE)
    target <- file.path(out_dir, "artifact.crb")
    plan$out_dir <- out_dir
    plan$make_app <- FALSE
    plan$overwrite <- isTRUE(overwrite)
    plan$targets <- target
    plan$existing_targets <- target[file.exists(target)]
    plan$output_release$directory <- out_dir
    plan$output_release$overwrite <- isTRUE(overwrite)
    plan$output_release$replacement_policy <- if (isTRUE(overwrite)) {
      "replace_existing_atomically"
    } else {
      "preserve_existing"
    }
    plan$output_release$targets <- target
    plan
  }

  shiny::testServer(app_env$server, {
    real_session <- session
    dialog_messages <- list()
    notifications <- character()
    fn_env <- environment(builder_require_confirmed_build_plan)
    assign(
      "session",
      list(
        token = real_session$token,
        sendCustomMessage = function(type, message) {
          dialog_messages[[length(dialog_messages) + 1L]] <<- list(
            type = type,
            message = message
          )
        },
        onFlushed = function(callback, once = FALSE) callback()
      ),
      envir = fn_env
    )
    assign(
      "showNotification",
      function(ui, ...) {
        notifications <<- c(notifications, as.character(ui))
      },
      envir = fn_env
    )
    assign(
      "builder_choose_output_directory",
      function(...) list(status = "selected", path = replacement_dir),
      envir = fn_env
    )
    enqueued <- list()
    assign(
      "enqueue",
      function(payload) {
        enqueued[[length(enqueued) + 1L]] <<- payload
        queued_protocol <- isolate(protocol())
        queued_protocol$build_status <- "queued"
        protocol(queued_protocol)
        TRUE
      },
      envir = fn_env
    )
    worker(list(alive = TRUE))
    protocol(app_env$builder_request_protocol("worker-a"))

    use_state_only_fixture(list(list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a"),
      settings = list(name = "Dataset A")
    )))
    real_session$setInputs(make_app = FALSE)
    real_session$flushReact()
    live <- frozen_review_plan()
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", plan = live)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", plan = live)
    ))
    selected_output(output_dir)

    real_session$setInputs(build = 1L)
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    expect_length(enqueued, 0L)
    conflict <- Filter(
      function(message) {
        identical(message$type, "builder_build_dialog") &&
          identical(message$message$type, "conflict")
      },
      dialog_messages
    )
    expect_length(conflict, 1L)
    expect_identical(conflict[[1L]]$message$files, "previous.crb")
    conflict_nonce <- conflict[[1L]]$message$nonce
    expect_true(
      is.character(conflict_nonce) &&
        length(conflict_nonce) == 1L &&
        nzchar(conflict_nonce)
    )
    expect_identical(build_flow()$nonce, conflict_nonce)

    real_session$setInputs(
      builder_build_dialog = list(action = "cancel", nonce = conflict_nonce)
    )
    real_session$flushReact()
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(selected_output(), output_dir)
    expect_true(app_env$builder_workflow_confirmation_matches(
      isolate(workflow()),
      live
    ))
    expect_length(enqueued, 0L)

    real_session$setInputs(build = 2L)
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    next_conflict <- tail(
      Filter(
        function(message) {
          identical(message$type, "builder_build_dialog") &&
            identical(message$message$type, "conflict")
        },
        dialog_messages
      ),
      1L
    )[[1L]]$message
    expect_false(identical(next_conflict$nonce, conflict_nonce))
    real_session$setInputs(
      builder_build_dialog = list(
        action = "choose_another",
        nonce = conflict_nonce
      )
    )
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    expect_identical(selected_output(), output_dir)
    real_session$setInputs(
      builder_build_dialog = list(
        action = "choose_another",
        nonce = next_conflict$nonce
      )
    )
    real_session$flushReact()
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(selected_output(), replacement_dir)
    expect_true(app_env$builder_workflow_confirmation_matches(
      isolate(workflow()),
      live
    ))
    expect_length(enqueued, 0L)

    selected_output(output_dir)
    real_session$setInputs(build = 3L)
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    third_nonce <- build_flow()$nonce
    worker(NULL)
    real_session$setInputs(
      builder_build_dialog = list(action = "replace", nonce = third_nonce)
    )
    real_session$flushReact()
    expect_length(enqueued, 0L)
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(selected_output(), output_dir)
    expect_match(tail(notifications, 1L), "worker", ignore.case = TRUE)
    expect_true(any(vapply(
      dialog_messages,
      function(message) identical(message$message, list(action = "close")),
      logical(1)
    )))

    worker(list(alive = TRUE))
    protocol(app_env$builder_request_protocol("worker-a"))
    real_session$setInputs(build = 4L)
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    fourth_nonce <- build_flow()$nonce
    real_session$setInputs(
      builder_build_dialog = list(action = "forged", nonce = fourth_nonce)
    )
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    expect_length(enqueued, 0L)
    real_session$setInputs(
      builder_build_dialog = list(action = "replace", nonce = fourth_nonce)
    )
    real_session$flushReact()
    expect_length(enqueued, 1L)
    expect_true(enqueued[[1L]]$plan$overwrite)
    expect_identical(enqueued[[1L]]$plan$out_dir, output_dir)
    expect_identical(build_flow()$stage, "building")
    real_session$setInputs(
      builder_build_dialog = list(action = "replace", nonce = fourth_nonce)
    )
    real_session$setInputs(
      builder_build_dialog = list(action = "cancel", nonce = fourth_nonce)
    )
    real_session$setInputs(
      builder_build_dialog = list(action = "forged", nonce = "forged-nonce")
    )
    real_session$flushReact()
    expect_identical(build_flow()$stage, "building")
    expect_length(enqueued, 1L)

    build_flow(list(stage = "idle", plan = NULL))
    busy_protocol <- app_env$builder_request_protocol("worker-b")
    busy_protocol$build_status <- "running"
    protocol(busy_protocol)
    real_session$setInputs(build = 5L)
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    fifth_nonce <- build_flow()$nonce
    notification_count <- length(notifications)
    real_session$setInputs(
      builder_build_dialog = list(action = "replace", nonce = fifth_nonce)
    )
    real_session$flushReact()
    expect_length(enqueued, 1L)
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_gt(length(notifications), notification_count)
    expect_match(tail(notifications, 1L), "worker", ignore.case = TRUE)

    protocol(app_env$builder_request_protocol("worker-c"))
    real_session$setInputs(build = 6L)
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    sixth_nonce <- build_flow()$nonce
    real_session$setInputs(
      builder_build_dialog = list(action = "replace", nonce = sixth_nonce)
    )
    real_session$flushReact()
    expect_length(enqueued, 2L)
    expect_true(enqueued[[2L]]$plan$overwrite)

    build_flow(list(stage = "idle", plan = NULL))
    protocol(app_env$builder_request_protocol("worker-d"))
    selected_output(output_dir)
    real_session$setInputs(build = 7L)
    real_session$flushReact()
    expect_identical(build_flow()$stage, "conflict")
    seventh_nonce <- build_flow()$nonce
    workflow(app_env$builder_reduce_workflow(
      isolate(workflow()),
      list(type = "invalidate")
    ))
    real_session$setInputs(
      builder_build_dialog = list(action = "replace", nonce = seventh_nonce)
    )
    real_session$flushReact()
    expect_length(enqueued, 2L)
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_null(selected_output())
    expect_identical(workflow()$stage, "configure")
    expect_match(
      tail(notifications, 1L),
      "Settings changed. Review the updated plan before building.",
      fixed = TRUE
    )
  })
})

test_that("active Build states reject forged stage actions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$builder_freeze_plan <- function(...) builder_stage_frozen_plan(FALSE)

  shiny::testServer(app_env$server, {
    real_session <- session
    picker_calls <- 0L
    notifications <- character()
    fn_env <- environment(builder_require_confirmed_build_plan)
    assign(
      "session",
      list(
        sendCustomMessage = function(...) NULL,
        onFlushed = function(callback, once = FALSE) callback()
      ),
      envir = fn_env
    )
    assign(
      "showNotification",
      function(ui, ...) {
        notifications <<- c(notifications, as.character(ui))
      },
      envir = fn_env
    )
    assign(
      "builder_choose_output_directory",
      function(...) {
        picker_calls <<- picker_calls + 1L
        list(status = "selected", path = "/new/output")
      },
      envir = fn_env
    )
    use_state_only_fixture(list(list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a"),
      settings = list(name = "Dataset A")
    )))
    real_session$setInputs(make_app = FALSE)
    real_session$flushReact()
    plan <- frozen_review_plan()
    reviewed <- app_env$builder_reduce_workflow(
      app_env$builder_workflow_state(),
      list(type = "open_review", plan = plan)
    )
    workflow(app_env$builder_reduce_workflow(
      reviewed,
      list(type = "confirm_review", plan = plan)
    ))
    selected_output("/confirmed/output")

    for (index in seq_along(c("queued", "building", "conflict"))) {
      stage <- c("queued", "building", "conflict")[[index]]
      frozen_flow <- list(
        stage = stage,
        plan = if (identical(stage, "conflict")) plan else NULL
      )
      build_flow(frozen_flow)
      real_session$setInputs(back_to_review = index)
      real_session$setInputs(choose_output_folder = index)
      real_session$setInputs(build = index)
      real_session$flushReact()

      expect_identical(workflow()$stage, "build", info = stage)
      expect_identical(build_flow(), frozen_flow, info = stage)
      expect_identical(selected_output(), "/confirmed/output", info = stage)
      expect_identical(picker_calls, 0L, info = stage)
      expect_length(notifications, 0L)
    }

    build_flow(list(stage = "idle", plan = NULL))
    real_session$setInputs(choose_output_folder = 4L)
    real_session$flushReact()
    expect_identical(picker_calls, 1L)
    expect_identical(selected_output(), "/new/output")
    navigation_result <- app_env$builder_result_success(
      published = TRUE,
      built = "/new/output/dataset.crb"
    )
    result(navigation_result)
    real_session$setInputs(back_to_review = 4L)
    real_session$flushReact()
    expect_identical(workflow()$stage, "review")
    expect_identical(result(), navigation_result)
  })
})

test_that("active builds lock dataset imports and rail mutations", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)

  shiny::testServer(app_env$server, {
    real_session <- session
    notifications <- character()
    assign(
      "showNotification",
      function(ui, ...) {
        notifications <<- c(notifications, as.character(ui))
      },
      envir = environment(start_load)
    )
    worker(list(alive = TRUE, snapshot_root = tempdir()))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-lock"))
    entries <- lapply(c("dataset-a", "dataset-b"), function(id) {
      list(
        id = id,
        source_id = id,
        output_id = id,
        selector_value = id,
        path = file.path(tempdir(), paste0(id, ".rds")),
        snapshot = builder_task6_snapshot_identity(),
        format = "RDS",
        profile = list(
          n_cells = 12L,
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA"
        ),
        settings = list(
          name = id,
          groups = "cluster",
          reductions = "umap",
          layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          palette = list(cluster = c(one = "#111111")),
          analyses = character()
        )
      )
    })
    store(app_env$builder_reduce_state(
      app_env$builder_state(entries),
      list(type = "remove", id = "dataset-b")
    ))
    expect_true(store()$can_undo_remove)

    for (index in seq_along(c("queued", "building"))) {
      stage <- c("queued", "building")[[index]]
      build_flow(list(stage = stage, plan = NULL))
      active_protocol <- isolate(protocol())
      active_protocol$build_status <- c("queued", "running")[[index]]
      protocol(active_protocol)
      before <- isolate(store())
      before_imports <- isolate(imports())
      before_protocol <- isolate(protocol())

      expect_false(start_load(
        "example",
        paste0("locked-example-", index),
        "Locked example"
      ))
      real_session$setInputs(
        reorder_ds = list(id = "dataset-a", direction = "down"),
        drop_ds = list(id = "dataset-a", confirmed = TRUE),
        undo_remove = index,
        use_example = c("all_content", "spatial")[[index]],
        dataset_files = data.frame(
          name = "forged.rds",
          datapath = tempfile(fileext = ".rds"),
          size = 1,
          type = "application/octet-stream"
        )
      )
      real_session$flushReact()

      expect_identical(store(), before, info = stage)
      expect_identical(imports(), before_imports, info = stage)
      expect_identical(protocol(), before_protocol, info = stage)
    }
    expect_true(any(grepl(
      "Wait for the active build to finish before changing datasets.",
      notifications,
      fixed = TRUE
    )))

    build_flow(list(stage = "idle", plan = NULL))
    protocol(app_env$builder_request_protocol("worker-lock"))
    expect_true(start_load("example", "idle-example", "Idle example"))
    expect_length(imports()$entries, 1L)
    real_session$setInputs(undo_remove = 3L)
    real_session$flushReact()
    expect_false(store()$can_undo_remove)
    expect_length(store()$datasets, 2L)
  })
})

test_that("Build recovery actions preserve confirmation only when safe", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this recovery test.")
  }
  app_env$auth_capability$available <- TRUE
  rlang::local_bindings(
    builder_viewer_page_catalog = app_env$builder_viewer_page_catalog,
    .env = environment(builder_stage_frozen_plan)
  )
  app_env$builder_freeze_plan <- function(
    entries,
    out_dir,
    make_app,
    overwrite,
    app_options,
    app_auth
  ) {
    plan <- builder_stage_frozen_plan(make_app)
    plan$revision <- max(vapply(entries, `[[`, integer(1), "revision"))
    plan$dataset_order <- vapply(entries, `[[`, character(1), "id")
    plan$items <- list(plan$items[[1L]])
    plan$items[[1L]]$id <- entries[[1L]]$id
    plan$items[[1L]]$analyses <- entries[[1L]]$settings$analyses %||%
      character()
    plan$app_auth <- app_auth
    plan$out_dir <- out_dir
    plan$overwrite <- overwrite
    plan$existing_targets <- character()
    plan
  }

  shiny::testServer(app_env$server, {
    real_session <- session
    notifications <- character()
    enqueued <- list()
    restart_succeeds <- TRUE
    server_env <- environment(start_confirmed_build)
    assign(
      "showNotification",
      function(ui, ...) {
        notifications <<- c(notifications, as.character(ui))
      },
      envir = server_env
    )
    assign(
      "restart_worker_protocol",
      function(...) restart_succeeds,
      envir = server_env
    )
    assign(
      "enqueue",
      function(payload) {
        enqueued[[length(enqueued) + 1L]] <<- payload
        queued <- isolate(protocol())
        queued$build_status <- "queued"
        protocol(queued)
        TRUE
      },
      envir = server_env
    )

    action_nonce <- 0L
    valid_accounts <- app_env$builder_auth_validate_payload(
      TRUE,
      list(list(
        id = "auth-account-1",
        username = "user-a",
        password = "password-a"
      ))
    )$accounts
    set_case <- function(
      value,
      auth_missing = FALSE,
      auth_ready = FALSE,
      stale = FALSE,
      analyses = "marker_genes"
    ) {
      entry <- list(
        id = "dataset-a",
        revision = 0L,
        snapshot = list(
          path = "/private/dataset-a",
          owner_token = "owner-a",
          object_md5 = strrep("a", 32L)
        ),
        profile = list(marker = "a"),
        settings = list(name = "Dataset A", analyses = analyses)
      )
      use_state_only_fixture(list(entry))
      auth_enabled(isTRUE(auth_ready))
      auth_accounts(
        if (isTRUE(auth_ready)) {
          valid_accounts
        } else {
          app_env$builder_auth_empty_accounts()
        }
      )
      build_mode(isTRUE(auth_ready))
      real_session$flushReact()
      plan <- isolate(frozen_review_plan())
      if (isTRUE(auth_missing)) {
        plan$app_auth <- list(
          enabled = TRUE,
          account_count = 1L,
          timeout_minutes = 15L
        )
        auth_enabled(TRUE)
      }
      if (isTRUE(stale)) {
        plan$revision <- plan$revision - 1L
      }
      reviewed <- app_env$builder_reduce_workflow(
        app_env$builder_workflow_state(),
        list(type = "open_review", plan = plan)
      )
      workflow(app_env$builder_reduce_workflow(
        reviewed,
        list(type = "confirm_review", plan = plan)
      ))
      selected_output("/private/host/output")
      build_flow(list(stage = "idle", plan = NULL))
      worker(list(alive = TRUE))
      protocol(app_env$builder_request_protocol("worker-recovery"))
      result(value)
      notifications <<- character()
      enqueued <<- list()
      invisible(plan)
    }
    click_action <- function(id) {
      action_nonce <<- action_nonce + 1L
      do.call(
        real_session$setInputs,
        stats::setNames(list(action_nonce), id)
      )
      real_session$flushReact()
    }

    restart_failure <- app_env$builder_result_failure(
      "Worker stopped.",
      restartable_worker = TRUE
    )
    set_case(restart_failure)
    restart_succeeds <- TRUE
    click_action("restart_worker")
    expect_null(result())
    expect_identical(workflow()$stage, "build")
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(selected_output(), "/private/host/output")
    expect_match(
      paste(unlist(output$build_stage_footer), collapse = " "),
      ">Build<",
      fixed = TRUE
    )

    set_case(restart_failure)
    restart_succeeds <- FALSE
    click_action("restart_worker")
    expect_identical(result(), restart_failure)
    expect_match(
      tail(notifications, 1L),
      "could not restart",
      ignore.case = TRUE
    )

    set_case(restart_failure, auth_missing = TRUE)
    restart_succeeds <- TRUE
    click_action("restart_worker")
    expect_identical(workflow()$stage, "configure")
    expect_null(selected_output())
    expect_null(result())
    expect_match(
      tail(notifications, 1L),
      "Re-enter login accounts and review the plan before retrying.",
      fixed = TRUE
    )

    decision <- app_env$builder_result_needs_decision(
      "Choose one.",
      retry_closure = "marker_genes",
      failed_dataset_id = "dataset-a"
    )
    set_case(decision)
    click_action("remove_failed_analysis")
    expect_false("marker_genes" %in% entry_of("dataset-a")$settings$analyses)
    expect_identical(workflow()$stage, "configure")
    expect_null(selected_output())
    expect_null(result())
    expect_length(enqueued, 0L)
    expect_match(
      tail(notifications, 1L),
      "Optional work removed. Review the updated plan before building.",
      fixed = TRUE
    )

    missing_decision <- app_env$builder_result_needs_decision(
      "Choose one.",
      retry_closure = "marker_genes",
      failed_dataset_id = "missing"
    )
    set_case(missing_decision)
    click_action("remove_failed_analysis")
    expect_identical(result(), missing_decision)
    expect_length(enqueued, 0L)
    expect_match(
      tail(notifications, 1L),
      "could not be removed",
      ignore.case = TRUE
    )

    set_case(decision)
    click_action("retry_failed_analysis")
    expect_length(enqueued, 1L)
    expect_null(result())
    expect_identical(build_flow()$stage, "building")

    set_case(decision, auth_ready = TRUE)
    click_action("retry_failed_analysis")
    expect_length(enqueued, 1L)
    expect_null(result())
    expect_identical(build_flow()$stage, "building")
    expect_length(auth_accounts(), 0L)
    expect_false(auth_validation()$ok)

    set_case(decision, auth_missing = TRUE)
    click_action("retry_failed_analysis")
    expect_length(enqueued, 0L)
    expect_identical(workflow()$stage, "configure")
    expect_null(selected_output())
    expect_null(result())
    expect_match(
      tail(notifications, 1L),
      "Re-enter login accounts and review the plan before retrying.",
      fixed = TRUE
    )

    set_case(decision, stale = TRUE)
    click_action("retry_failed_analysis")
    expect_length(enqueued, 0L)
    expect_identical(workflow()$stage, "configure")
    expect_null(selected_output())
    expect_match(tail(notifications, 1L), "Review", fixed = TRUE)
    expect_match(tail(notifications, 1L), "before retrying", fixed = TRUE)
  })
})

test_that("Viewer and spatial preview contracts ignore settings-only revisions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  expect_true(exists(
    "builder_projection_preview_contract",
    envir = app_env,
    inherits = FALSE
  ))
  expect_true(exists(
    "builder_trajectory_preview_contract",
    envir = app_env,
    inherits = FALSE
  ))
  expect_true(exists(
    "builder_preview_revision_independent",
    envir = app_env,
    inherits = FALSE
  ))
  if (
    !exists(
      "builder_projection_preview_contract",
      envir = app_env,
      inherits = FALSE
    ) ||
      !exists(
        "builder_trajectory_preview_contract",
        envir = app_env,
        inherits = FALSE
      ) ||
      !exists(
        "builder_preview_revision_independent",
        envir = app_env,
        inherits = FALSE
      )
  ) {
    return(invisible(NULL))
  }

  expect_true(app_env$builder_preview_revision_independent(
    "projection_previews"
  ))
  expect_true(app_env$builder_preview_revision_independent(
    "trajectory_previews"
  ))
  expect_true(app_env$builder_preview_revision_independent(
    "spatial_preview"
  ))
  expect_false(app_env$builder_preview_revision_independent("preview"))

  entry <- list(
    id = "dataset-a",
    revision = 1L,
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    settings = list(
      default_group = "cluster",
      overview_point_size = 5,
      included_projections = "umap",
      default_projection = "umap",
      included_trajectories = list(monocle2 = "lineage_a"),
      default_trajectory = list(method = "monocle2", name = "lineage_a"),
      group_color_overrides = list()
    )
  )
  projections <- c("umap", "pca")
  trajectories <- list(monocle2 = c("lineage_a", "lineage_b"))
  projection_contract <- app_env$builder_projection_preview_contract(
    entry,
    projections
  )
  trajectory_contract <- app_env$builder_trajectory_preview_contract(
    entry,
    trajectories
  )

  settings_only <- entry
  settings_only$revision <- 9L
  settings_only$settings$overview_point_size <- 12
  settings_only$settings$included_projections <- c("umap", "pca")
  settings_only$settings$default_projection <- "pca"
  settings_only$settings$included_trajectories <- list(
    monocle2 = "lineage_b"
  )
  settings_only$settings$default_trajectory <- list(
    method = "monocle2",
    name = "lineage_b"
  )
  settings_only$settings$group_color_overrides <- list(
    cluster = c(A = "#123456")
  )

  expect_identical(
    app_env$builder_projection_preview_contract(settings_only, projections),
    projection_contract
  )
  expect_identical(
    app_env$builder_trajectory_preview_contract(settings_only, trajectories),
    trajectory_contract
  )

  regrouped <- settings_only
  regrouped$settings$default_group <- "sample"
  expect_false(identical(
    app_env$builder_projection_preview_contract(regrouped, projections),
    projection_contract
  ))
  expect_identical(
    app_env$builder_trajectory_preview_contract(regrouped, trajectories),
    trajectory_contract
  )

  resnapshotted <- settings_only
  resnapshotted$snapshot$object_md5 <- strrep("b", 32L)
  expect_false(identical(
    app_env$builder_projection_preview_contract(resnapshotted, projections),
    projection_contract
  ))
  expect_false(identical(
    app_env$builder_trajectory_preview_contract(resnapshotted, trajectories),
    trajectory_contract
  ))
  expect_false(identical(
    app_env$builder_projection_preview_contract(
      settings_only,
      c(projections, "tsne")
    ),
    projection_contract
  ))
  expect_false(identical(
    app_env$builder_trajectory_preview_contract(
      settings_only,
      list(monocle2 = c("lineage_a", "lineage_b", "lineage_c"))
    ),
    trajectory_contract
  ))
})

test_that("dynamic Core and Enhance contracts update only their owned controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  original_enhance_stage_ui <- app_env$builder_enhance_stage_ui
  enhance_stage_renders <- 0L
  app_env$builder_enhance_stage_ui <- function(...) {
    enhance_stage_renders <<- enhance_stage_renders + 1L
    original_enhance_stage_ui(...)
  }
  original_enhance_modules_ui <- app_env$builder_enhance_modules_ui
  enhance_module_renders <- 0L
  app_env$builder_enhance_modules_ui <- function(...) {
    enhance_module_renders <<- enhance_module_renders + 1L
    original_enhance_modules_ui(...)
  }
  original_inspect_stage_ui <- app_env$builder_inspect_stage_ui
  inspect_stage_renders <- 0L
  app_env$builder_inspect_stage_ui <- function(...) {
    inspect_stage_renders <<- inspect_stage_renders + 1L
    original_inspect_stage_ui(...)
  }
  select_updates <- list()
  retain_updates <- list()
  app_env$updateSelectInput <- function(
    session,
    inputId,
    label = NULL,
    choices = NULL,
    selected = NULL
  ) {
    select_updates[[inputId]] <<- list(
      choices = choices,
      selected = selected
    )
  }
  app_env$updateCheckboxGroupInput <- function(
    session,
    inputId,
    label = NULL,
    choices = NULL,
    selected = NULL,
    inline = FALSE
  ) {
    retain_updates[[inputId]] <<- list(
      choices = choices,
      selected = selected
    )
  }
  table_path <- withr::local_tempfile()
  writeLines(c("sample,value", "a,1"), table_path)

  shiny::testServer(app_env$server, {
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(
        n_cells = 80L,
        n_genes = 230L,
        organism_guess = "hg",
        assays = c("RNA", "SCT"),
        layers = c("data", "counts"),
        default_layer = "data",
        nUMI = "nCount_RNA",
        nGene = "nFeature_RNA",
        assay_profiles = list(
          RNA = list(
            layers = c("data", "counts"),
            default_layer = "data",
            nUMI_choices = "nCount_RNA",
            nGene_choices = "nFeature_RNA",
            nUMI = "nCount_RNA",
            nGene = "nFeature_RNA"
          ),
          SCT = list(
            layers = c("scale.data", "counts"),
            default_layer = "scale.data",
            nUMI_choices = "nCount_SCT",
            nGene_choices = "nFeature_SCT",
            nUMI = "nCount_SCT",
            nGene = "nFeature_SCT"
          )
        ),
        extras = list(),
        images = character(),
        group_candidates = c(cluster = "cluster", sample = "sample"),
        group_preselect = "cluster",
        group_counts = list(
          cluster = c(A = 50L, B = 30L),
          sample = c(one = 40L, two = 40L)
        ),
        qc_values = list(
          nCount_RNA = c(100, 200),
          nFeature_RNA = c(20, 40),
          nCount_SCT = c(90, 180),
          nFeature_SCT = c(18, 36)
        ),
        reductions = c("umap", "pca"),
        viewer_content = list(
          projections = list(
            umap = list(
              id = "umap",
              name = "umap",
              kind = "umap",
              dimensions = 2L,
              cell_count = 80L,
              available = TRUE
            ),
            pca = list(
              id = "pca",
              name = "pca",
              kind = "pca",
              dimensions = 20L,
              cell_count = 80L,
              available = TRUE
            )
          ),
          trajectories = list(
            list(
              method = "monocle2",
              name = "lineage_a",
              selectable = TRUE,
              cell_count = 60L,
              coverage = .75,
              state_count = 3L,
              edge_count = 2L
            ),
            list(
              method = "monocle2",
              name = "lineage_b",
              selectable = TRUE,
              cell_count = 50L,
              coverage = .625,
              state_count = 2L,
              edge_count = 1L
            )
          )
        )
      ),
      levels = list(cluster = c("A", "B"), sample = c("one", "two")),
      settings = list(
        name = "Dataset A",
        organism = "hg",
        viewer_content_schema_version = 1L,
        groups = c("cluster", "sample"),
        included_groups = c("cluster", "sample"),
        default_group = "cluster",
        reductions = "umap",
        included_projections = "umap",
        default_projection = "umap",
        overview_point_size = 5,
        included_trajectories = list(
          monocle2 = c("lineage_a", "lineage_b")
        ),
        default_trajectory = list(
          method = "monocle2",
          name = "lineage_a"
        ),
        assay = "RNA",
        layer = "data",
        nUMI = "nCount_RNA",
        nGene = "nFeature_RNA",
        expression_backend = "embedded",
        analyses = character(),
        tables = list(),
        images = list(),
        palette = "cerebro",
        group_color_overrides = list(sample = c(one = "#123456"))
      )
    )
    use_state_only_fixture(list(entry))
    session$flushReact()
    invisible(output$workbench)
    invisible(output[["enhance-analysis_modules"]])
    invisible(output[["inspect_stage"]])
    session$flushReact()
    baseline_enhance_stage_renders <- enhance_stage_renders

    top_level_runs <- 0L
    tracker <- observe({
      current()
      top_level_runs <<- top_level_runs + 1L
    })
    withr::defer(tracker$destroy())
    session$flushReact()
    baseline <- top_level_runs

    session$setInputs(
      `core-rendered_for` = "dataset-a",
      `core-name` = "Dataset A",
      `core-organism` = "hg",
      `core-default_group` = "cluster",
      `core-default_projection` = "umap",
      `core-assay` = "SCT",
      `core-layer` = "data",
      `core-nUMI` = "nCount_RNA",
      `core-nGene` = "nFeature_RNA",
      `core-backend` = "embedded"
    )
    session$flushReact()
    expect_identical(top_level_runs, baseline)
    expect_identical(
      names(select_updates),
      c("core-layer", "core-nUMI", "core-nGene")
    )
    expect_identical(
      select_updates[["core-layer"]]$choices,
      c("scale.data", "counts")
    )
    expect_identical(
      select_updates[["core-layer"]]$selected,
      "scale.data"
    )
    expect_identical(
      select_updates[["core-nUMI"]],
      list(choices = "nCount_SCT", selected = "nCount_SCT")
    )
    expect_identical(
      select_updates[["core-nGene"]],
      list(choices = "nFeature_SCT", selected = "nFeature_SCT")
    )
    expect_identical(sets()[[1L]]$settings$assay, "SCT")
    expect_identical(sets()[[1L]]$settings$layer, "scale.data")
    expect_identical(sets()[[1L]]$settings$nUMI, "nCount_SCT")
    expect_identical(sets()[[1L]]$settings$nGene, "nFeature_SCT")

    before_groups <- sets()[[1L]]$revision
    session$setInputs(
      `core-group_action` = list(
        action = "set",
        included = c("cluster", "sample"),
        default = "sample",
        nonce = 1
      )
    )
    session$flushReact()
    grouped <- sets()[[1L]]
    expect_identical(grouped$settings$included_groups, c("cluster", "sample"))
    expect_identical(grouped$settings$default_group, "sample")
    expect_gt(grouped$revision, before_groups)

    session$setInputs(
      `core-group_action` = list(
        action = "set",
        included = c("cluster", "sample"),
        default = "cluster",
        nonce = 2
      )
    )
    session$flushReact()
    restored_group <- sets()[[1L]]
    expect_identical(restored_group$settings$default_group, "cluster")
    before_focus <- restored_group$revision
    session$setInputs(
      `core-group_focus` = list(group = "sample", nonce = 1)
    )
    session$flushReact()
    expect_identical(sets()[[1L]]$revision, before_focus)
    session$setInputs(
      `core-group_focus` = list(group = "cluster", nonce = 2)
    )
    session$flushReact()
    expect_identical(sets()[[1L]]$revision, before_focus)

    before_projection <- sets()[[1L]]$revision
    session$setInputs(
      `core-projection_action` = list(
        action = "set",
        included = c("umap", "pca"),
        default = "pca",
        nonce = 1
      )
    )
    session$flushReact()
    projected <- sets()[[1L]]
    expect_identical(projected$settings$included_projections, c("umap", "pca"))
    expect_identical(projected$settings$default_projection, "pca")
    expect_gt(projected$revision, before_projection)

    before_point_size <- projected$revision
    session$setInputs(`core-point_size` = 8)
    session$flushReact()
    resized <- sets()[[1L]]
    expect_identical(resized$settings$overview_point_size, 8)
    expect_gt(resized$revision, before_point_size)

    before_cell_percentage <- resized$revision
    session$setInputs(`core-percentage_cells_to_show` = 60)
    session$flushReact()
    sampled <- sets()[[1L]]
    expect_identical(
      sampled$settings$overview_percentage_cells_to_show,
      60
    )
    expect_gt(sampled$revision, before_cell_percentage)

    before_trajectory <- sampled$revision
    session$setInputs(
      `core-trajectory_action` = list(
        action = "set",
        included = list(
          list(method = "monocle2", name = "lineage_a"),
          list(method = "monocle2", name = "lineage_b")
        ),
        default = list(method = "monocle2", name = "lineage_b"),
        nonce = 1
      )
    )
    session$flushReact()
    trajectory <- sets()[[1L]]
    expect_identical(
      trajectory$settings$included_trajectories,
      list(monocle2 = c("lineage_a", "lineage_b"))
    )
    expect_identical(
      trajectory$settings$default_trajectory,
      list(method = "monocle2", name = "lineage_b")
    )
    expect_gt(trajectory$revision, before_trajectory)

    before_gallery_view <- trajectory$revision
    invisible(output[["core-projection_gallery"]])
    invisible(output[["core-trajectory_gallery"]])
    session$flushReact()
    expect_identical(sets()[[1L]]$revision, before_gallery_view)

    session$setInputs(
      `core-projection_action` = list(
        action = "set",
        included = c("umap", "pca"),
        default = "umap",
        nonce = 2
      )
    )
    session$flushReact()

    before_color <- sets()[[1L]]$revision
    session$setInputs(
      `core-group_color` = list(
        group = "cluster",
        level = "B",
        color = "#e76f51",
        nonce = 1
      )
    )
    session$flushReact()
    colored <- sets()[[1L]]
    expect_identical(
      colored$settings$group_color_overrides$cluster[["B"]],
      "#E76F51"
    )
    expect_identical(
      colored$settings$group_color_overrides$sample[["one"]],
      "#123456"
    )
    expect_identical(colored$settings$default_projection, "umap")
    expect_gt(colored$revision, before_color)

    before_reset <- colored$revision
    session$setInputs(`core-reset_colors` = 1L)
    session$flushReact()
    reset <- sets()[[1L]]
    expect_null(reset$settings$group_color_overrides$cluster)
    expect_identical(
      reset$settings$group_color_overrides$sample[["one"]],
      "#123456"
    )
    expect_gt(reset$revision, before_reset)
    expect_identical(top_level_runs, baseline)

    session$setInputs(`core-organism` = "other")
    session$flushReact()
    other_html <- paste(
      as.character(output[["enhance-analysis_modules"]]),
      collapse = ""
    )
    expect_false(grepl("percent_mt_ribo", other_html, fixed = TRUE))
    expect_identical(top_level_runs, baseline)

    session$setInputs(`core-organism` = "hg")
    session$flushReact()
    blocked_html <- paste(
      as.character(output[["enhance-analysis_modules"]]),
      collapse = ""
    )
    expect_match(blocked_html, "Select Marker genes first", fixed = TRUE)
    before_most_expressed_module_renders <- enhance_module_renders
    before_most_expressed_inspect_renders <- inspect_stage_renders
    session$setInputs(
      `enhance-rendered_for` = "dataset-a",
      `enhance-analysis_most_expressed` = TRUE
    )
    session$flushReact()
    expect_true(
      "most_expressed" %in% sets()[[1L]]$settings$analyses
    )
    expect_identical(
      enhance_stage_renders,
      baseline_enhance_stage_renders
    )
    expect_identical(
      enhance_module_renders,
      before_most_expressed_module_renders
    )
    invisible(output[["inspect_stage"]])
    expect_lte(
      inspect_stage_renders,
      before_most_expressed_inspect_renders + 1L
    )

    session$setInputs(
      `enhance-rendered_for` = "dataset-a",
      `enhance-analysis_marker_genes_action` = 1
    )
    session$flushReact()
    session$setInputs(
      `enhance-marker_genes_calculate` = 1
    )
    session$flushReact()
    enabled_html <- paste(
      as.character(output[["enhance-analysis_modules"]]),
      collapse = ""
    )
    expect_false(grepl(
      "Select Marker genes first",
      enabled_html,
      fixed = TRUE
    ))
    expect_identical(top_level_runs, baseline)

    session$setInputs(
      `enhance-table_files` = data.frame(
        name = "clinical-results.csv",
        size = file.info(table_path)$size,
        type = "text/csv",
        datapath = table_path,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()
    table_list_html <- paste(
      as.character(output[["enhance-table_list"]]),
      collapse = ""
    )
    expect_match(table_list_html, "Added tables", fixed = TRUE)
    expect_match(table_list_html, "Table name", fixed = TRUE)
    expect_match(table_list_html, "CSV", fixed = TRUE)
    expect_match(table_list_html, "bytes", fixed = TRUE)
    expect_match(table_list_html, "Ready", fixed = TRUE)
    expect_match(table_list_html, "builder-file-list", fixed = TRUE)
    expect_match(table_list_html, "builder-file-item", fixed = TRUE)
    expect_false(grepl(table_path, table_list_html, fixed = TRUE))
    expect_false(grepl("fakepath", table_list_html, fixed = TRUE))
    expect_false(grepl("Tables to retain", table_list_html, fixed = TRUE))
    session$setInputs(
      `enhance-table_action` = list(
        action = "rename",
        key = "clinical-results",
        name = "Clinical results",
        nonce = 1
      )
    )
    session$flushReact()
    expect_identical(names(sets()[[1L]]$settings$tables), "Clinical results")
    expect_identical(
      sets()[[1L]]$settings$tables[[1L]]$file_name,
      "clinical-results.csv"
    )
    session$setInputs(
      `enhance-table_action` = list(
        action = "remove",
        key = "Clinical results",
        nonce = 2
      )
    )
    session$flushReact()
    expect_length(sets()[[1L]]$settings$tables, 0L)
    expect_identical(top_level_runs, baseline)

    alignment <- list(
      uri = "data:image/png;base64,AA==",
      bounds = list(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
    )
    saved <- sets()[[1L]]
    commit_enhance_images(saved, list(`section-a` = alignment))
    expect_null(retain_updates[["enhance-histology_to_retain"]])
    expect_identical(
      names(sets()[[1L]]$settings$images),
      "section-a"
    )
    expect_identical(top_level_runs, baseline)

    picture <- list(
      uri = "data:image/png;base64,AA==",
      bytes = 2,
      width = 10,
      height = 10,
      source_width = 10,
      source_height = 10,
      extent_width = 10,
      extent_height = 10,
      display_width = 10,
      display_height = 10
    )
    per_section <- list(
      `section-a` = list(
        bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
        cover = list(outside = 0L, total = 2L)
      ),
      `section-b` = list(
        bounds = list(xmin = 10, xmax = 20, ymin = 10, ymax = 20),
        cover = list(outside = 0L, total = 2L)
      )
    )
    apply_section_bounds("dataset-a", per_section, picture)
    expect_null(retain_updates[["enhance-histology_to_retain"]])
    expect_identical(
      names(sets()[[1L]]$settings$images),
      c("section-a", "section-b")
    )
    expect_identical(top_level_runs, baseline)
  })

  server <- paste(
    readLines(
      builder_profile_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    server,
    "commit_section(entry, section, record)",
    fixed = TRUE
  )
  expect_match(
    server,
    "builder_alignment_apply_transform_to_matching_label",
    fixed = TRUE
  )
  expect_match(
    server,
    "commit_section(entry, section, NULL, label = label)",
    fixed = TRUE
  )
})
