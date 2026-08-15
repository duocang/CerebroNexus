withr::local_dir(builder_profile_inst_path("builder"))
sys.source(
  file.path("..", "viewer", "core", "viewer_content_contract.R"),
  envir = globalenv()
)
sys.source("app.R", envir = environment())

test_that("Review model translates a frozen plan into user language", {
  plan <- builder_stage_frozen_plan()
  plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  model <- builder_review_model(plan)

  expect_identical(model$revision, 17L)
  expect_null(model$contract)
  expect_null(model$manifest)
  expect_null(model$app)
  expect_identical(model$dataset_count, 2L)
  expect_identical(model$output_label, "CRB files")
  expect_identical(model$datasets[[1L]]$name, "Dataset B")
  expect_identical(model$datasets[[1L]]$group_count, 2L)
  expect_identical(model$datasets[[1L]]$projection_count, 1L)
  expect_identical(model$output$existing_files, "Keep existing files")
  expect_identical(model$output$estimated_size, "4 KB")
  expect_identical(model$output$estimated_time, "A few minutes")
  expect_true(all(
    c("Data info", "Linked views", "Marker genes") %in%
      vapply(model$pages, `[[`, character(1), "label")
  ))
  expect_false(any(
    c("marker_genes", "spatial") %in%
      vapply(model$pages, `[[`, character(1), "id")
  ))
  expect_length(model$warnings, 0L)
  expect_true(model$can_build)
})

test_that("Review excludes login and all App configuration", {
  plan <- builder_stage_frozen_plan()
  plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_false(grepl("Login", html, fixed = TRUE))
  expect_false(grepl("Welcome", html, fixed = TRUE))
  expect_false(grepl("Viewer App", html, fixed = TRUE))
  expect_false(grepl("auth-user-a-7f31", html, fixed = TRUE))
  expect_false(grepl("auth-password-a-7f31", html, fixed = TRUE))
  expect_false(grepl('value="auth-', html, fixed = TRUE))
})

test_that("Review disables only login when optional auth is unavailable", {
  options <- builder_review_options()
  html <- builder_stage_html(builder_review_controls_ui(
    "review",
    options,
    auth = list(
      enabled = FALSE,
      account_count = 0L,
      error = NULL,
      available = FALSE
    )
  ))

  expect_match(
    html,
    'id="review-require_login"[^>]*disabled="disabled"'
  )
  expect_match(html, "optional authentication packages", fixed = TRUE)
  expect_false(grepl("shinymanager", html, fixed = TRUE))
  expect_false(grepl("openssl", html, fixed = TRUE))
  expect_false(grepl(
    'id="review-show_upload_ui" disabled=',
    html,
    fixed = TRUE
  ))
  expect_false(grepl('class="builder-auth-open"', html, fixed = TRUE))
})

test_that("Review auth controls consume only enabled count and error", {
  html <- builder_stage_html(builder_review_controls_ui(
    "review",
    builder_review_options(),
    auth = list(
      enabled = TRUE,
      account_count = 2L,
      error = NULL,
      available = TRUE
    )
  ))

  expect_match(html, "Login required · 2 accounts", fixed = TRUE)
  expect_match(html, "Edit accounts", fixed = TRUE)
})

test_that("Review presents the frozen CRB data plan", {
  crbs <- builder_review_model(builder_stage_frozen_plan(FALSE))
  expect_identical(crbs$output_label, "CRB files")

  app <- builder_review_model(builder_stage_frozen_plan(TRUE))
  html <- builder_stage_html(builder_review_stage_ui("review", app))

  expect_match(
    html,
    "Check the CRB data plan before choosing build outputs.",
    fixed = TRUE
  )
  expect_match(html, "2 datasets", fixed = TRUE)
  expect_match(html, "Frozen plan revision 17", fixed = TRUE)
  expect_match(html, "Creates CRB files", fixed = TRUE)
  expect_match(html, "2 CRB files", fixed = TRUE)
  expect_false(grepl("1 secret env file", html, fixed = TRUE))
  expect_match(html, "Datasets", fixed = TRUE)
  expect_match(html, "Dataset B", fixed = TRUE)
  expect_match(html, "2 cells · 3 genes", fixed = TRUE)
  expect_match(html, "Groups", fixed = TRUE)
  expect_match(html, "2 included · Default: Cluster", fixed = TRUE)
  expect_match(html, "Projections", fixed = TRUE)
  expect_match(html, "UMAP", fixed = TRUE)
  expect_match(html, "Output file:", fixed = TRUE)
  expect_match(html, "01-dataset-b.crb", fixed = TRUE)
  expect_false(grepl("App experience", html, fixed = TRUE))
  expect_false(grepl("Welcome, lab team!", html, fixed = TRUE))
  expect_match(html, "Content available from the CRBs", fixed = TRUE)
  expect_match(html, "Data info", fixed = TRUE)
  expect_match(html, "Marker genes", fixed = TRUE)
  expect_match(html, "Output", fixed = TRUE)
  expect_match(html, "Folder", fixed = TRUE)
  expect_match(html, "/private/host/output", fixed = TRUE)
  expect_false(grepl("Existing files", html, fixed = TRUE))
  expect_false(grepl("Keep existing files", html, fixed = TRUE))
  expect_match(html, "4 KB", fixed = TRUE)
  expect_match(html, "A few minutes", fixed = TRUE)
  expect_false(grepl("Private App", html, fixed = TRUE))
  forbidden <- c(
    "App contract",
    "Artifact mode",
    "automatic",
    "Planned payload members",
    "Replacement policy",
    "Technical plan details",
    "Viewer page expectations",
    "Expected after build",
    "Private assets",
    "BuildPlan",
    "manifest",
    "Host:",
    "Port:",
    "Request limit",
    "Display mode",
    "Launch browser",
    "sidecars",
    "HTTP-public",
    "barcode",
    "backend"
  )
  expect_false(any(vapply(
    forbidden,
    grepl,
    logical(1),
    x = html,
    fixed = TRUE
  )))
  expect_false(grepl("Needs attention", html, fixed = TRUE))
})

test_that("Review requires a typed frozen plan revision", {
  plan <- builder_stage_frozen_plan()
  for (revision in list(NULL, NA_integer_, 17, "", c("17", "18"))) {
    plan$revision <- revision
    expect_error(
      builder_review_model(plan),
      "typed frozen plan revision",
      fixed = TRUE
    )
  }
  plan$revision <- "release-17"
  expect_identical(builder_review_model(plan)$revision, "release-17")
})

test_that("Build shows the confirmed frozen plan revision", {
  model <- builder_review_model(builder_stage_frozen_plan())
  html <- builder_stage_html(builder_build_workbench_ui(model))

  expect_match(html, "builder-stage-shell", fixed = TRUE)
  expect_false(grepl("builder-stage-build builder-card", html, fixed = TRUE))
  expect_match(html, "builder-stage-summary", fixed = TRUE)
  expect_match(html, 'id="build_stage_footer"', fixed = TRUE)
  expect_false(grepl("Build status", html, fixed = TRUE))
  expect_match(html, "Confirmed plan revision 17", fixed = TRUE)
})

test_that("Review has one global confirmation and no editable controls", {
  model <- builder_review_model(builder_stage_frozen_plan(TRUE))
  stage <- builder_review_stage_ui(
    "review",
    model,
    footer = builder_review_confirmation_ui()
  )
  html <- builder_stage_html(stage)

  expect_length(
    regmatches(
      html,
      gregexpr("Continue to Build", html, fixed = TRUE)
    )[[1L]],
    1L
  )
  expect_length(
    regmatches(html, gregexpr("Back to Data setup", html, fixed = TRUE))[[1L]],
    1L
  )
  expect_match(html, "builder-stage-shell", fixed = TRUE)
  expect_match(html, "builder-stage-footer", fixed = TRUE)
  expect_match(html, "CRB plan ready", fixed = TRUE)
  expect_false(grepl("Ready to continue?", html, fixed = TRUE))
  expect_false(grepl(
    "Confirm this frozen revision to open the Build step.",
    html,
    fixed = TRUE
  ))
  expect_match(html, 'id="confirm_review"', fixed = TRUE)
  expect_match(html, 'id="back_to_settings"', fixed = TRUE)
  expect_false(grepl("review_current_dataset", html, fixed = TRUE))
  expect_false(grepl("dataset_review_footer", html, fixed = TRUE))
  expect_false(grepl("<input|<select|<textarea", html))
})

test_that("Review flattens a single dataset but bounds multiple datasets", {
  model <- builder_review_model(builder_stage_frozen_plan())
  multiple <- builder_stage_html(builder_review_stage_ui("review", model))
  expect_match(multiple, "builder-object", fixed = TRUE)
  expect_false(grepl("is-single-dataset", multiple, fixed = TRUE))

  model$datasets <- model$datasets[1L]
  model$dataset_count <- 1L
  single <- builder_stage_html(builder_review_stage_ui("review", model))
  expect_match(single, "is-single-dataset", fixed = TRUE)
})

test_that("Review remains CRB-only for every downstream output draft", {
  crb_plan <- builder_stage_frozen_plan(FALSE)
  public_plan <- builder_stage_frozen_plan(TRUE)
  login_plan <- builder_stage_frozen_plan(TRUE)
  login_plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )

  crb_model <- builder_review_model(crb_plan)
  public_model <- builder_review_model(public_plan)
  login_model <- builder_review_model(login_plan)

  expect_identical(crb_model$output_label, "CRB files")
  expect_identical(public_model$output_label, "CRB files")
  expect_identical(login_model$output_label, "CRB files")
  expect_identical(
    public_model$output$estimated_size,
    crb_model$output$estimated_size
  )
  public_html <- builder_stage_html(builder_review_stage_ui(
    "review",
    public_model
  ))
  login_html <- builder_stage_html(builder_review_stage_ui(
    "review",
    login_model
  ))
  expect_match(public_html, "2 CRB files", fixed = TRUE)
  expect_match(login_html, "2 CRB files", fixed = TRUE)
  expect_false(grepl("Login", login_html, fixed = TRUE))
  expect_false(grepl("CRB files + private App", public_html, fixed = TRUE))
})

test_that("Review summarizes saved and points-only spatial sections", {
  plan <- builder_stage_frozen_plan(TRUE)
  plan$items[[1L]]$spatial_alignment <- list(
    section_count = 2L,
    image_count = 1L,
    saved_count = 1L,
    points_only = "section-b"
  )
  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_identical(model$datasets[[1L]]$spatial_alignment$saved_count, 1L)
  expect_match(html, "Spatial", fixed = TRUE)
  expect_match(html, "2 sections · 1 images · Embedded in CRB", fixed = TRUE)
  expect_match(html, "1 section remains points-only", fixed = TRUE)
  expect_false(grepl("section-b", html, fixed = TRUE))
  expect_false(grepl("histology_image_bounds", html, fixed = TRUE))
})

test_that("Review keeps group colors compact and distinguishes custom colors", {
  plan <- builder_stage_frozen_plan(TRUE)
  plan$items[[1L]]$colors$cluster <- c(
    A = "#111111",
    B = "#222222",
    C = "#333333",
    D = "#444444",
    E = "#555555",
    F = "#666666",
    G = "#777777",
    H = "#888888"
  )
  plan$items[[1L]]$color_custom_count <- 3L
  plan$items[[2L]]$default_group <- "cell_type"
  plan$items[[2L]]$color_custom_count <- 0L

  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_identical(model$datasets[[1L]]$group_colors$group, "cluster")
  expect_identical(model$datasets[[1L]]$group_colors$custom_count, 3L)
  expect_lte(length(model$datasets[[1L]]$group_colors$preview), 5L)
  expect_match(html, "3 colors customized", fixed = TRUE)
  expect_match(html, "0 colors customized", fixed = TRUE)
  expect_false(grepl("review-group-color-dot", html, fixed = TRUE))
  expect_false(grepl(">#111111<", html, fixed = TRUE))
  expect_false(grepl("palettes are frozen", html, ignore.case = TRUE))
})

test_that("Review translates policies, bounds pages, and names actionable issues", {
  expect_identical(
    vapply(
      c("preserve_existing", "overwrite", "error_if_exists"),
      builder_review_existing_files,
      character(1)
    ),
    c(
      preserve_existing = "Keep existing files",
      overwrite = "Replace existing files",
      error_if_exists = "Stop if files already exist"
    )
  )
  expect_match(builder_review_human_size(514285), "KB", fixed = TRUE)
  expect_match(builder_review_human_size(5 * 1024^2), "MB", fixed = TRUE)

  plan <- builder_stage_frozen_plan(TRUE)
  plan$existing_targets <- "/private/host/output/01-dataset-b.crb"
  plan$overwrite <- FALSE
  plan$required_settings <- "dataset-b: choose a different output folder."
  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_false(model$can_build)
  expect_match(model$warnings[[1L]], "Dataset B", fixed = TRUE)
  expect_false(grepl("dataset-b:", model$warnings[[1L]], fixed = TRUE))
  expect_match(html, "Needs attention", fixed = TRUE)
  expect_match(html, "Show 1 more", fixed = TRUE)
})

test_that("Review translates network-dependent runtime into user language", {
  plan <- builder_stage_frozen_plan(TRUE)
  plan$output_release$estimated_runtime <- "network-dependent"

  expect_identical(
    builder_review_model(plan)$output$estimated_time,
    "Depends on network response"
  )
})

test_that("Review gives a useful next step when the plan is not ready", {
  html <- builder_stage_html(builder_review_blocked_ui(
    "review",
    "Choose a valid output folder."
  ))

  expect_match(html, "Review", fixed = TRUE)
  expect_match(html, "Needs attention", fixed = TRUE)
  expect_match(html, "Choose a valid output folder.", fixed = TRUE)
  expect_match(html, "Correct the highlighted settings", fixed = TRUE)
  expect_false(grepl(
    "Choose the required dataset settings before building.",
    html,
    fixed = TRUE
  ))
})

test_that("Review handles one dataset and long output folders", {
  plan <- builder_stage_frozen_plan(TRUE)
  plan$items <- plan$items[1L]
  plan$dataset_order <- "dataset-b"
  plan$output_release$directory <- paste0(
    "/private/host/",
    paste(rep("long-folder-name", 8L), collapse = "/")
  )
  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_match(html, "1 dataset", fixed = TRUE)
  expect_false(grepl("1 datasets", html, fixed = TRUE))
  expect_match(html, plan$output_release$directory, fixed = TRUE)
  expect_null(model$app)
})

test_that("Build status has four top-level types and warning Success variant", {
  success_result <- builder_result_success(
    published = TRUE,
    built = "/release/dataset.crb",
    warnings = "One optional analysis was skipped"
  )
  decision_result <- builder_result_needs_decision("Choose whether to retry.")
  failure_result <- builder_result_failure("failed")
  recovery_result <- builder_result_recovery_required(
    "Restore the backup manually."
  )
  success <- builder_build_status_model(success_result)
  decision <- builder_build_status_model(decision_result)
  failure <- builder_build_status_model(failure_result)
  recovery <- builder_build_status_model(recovery_result)

  expect_s3_class(success_result, "builder_result_success")
  expect_identical(success_result$state, "success")
  expect_identical(success$type, "success")
  expect_identical(success$variant, "warnings")
  expect_identical(decision$type, "needs_decision")
  expect_identical(failure$type, "failure")
  expect_identical(recovery$type, "recovery_required")
  expect_error(builder_build_status_model(list(error = "legacy")), "typed")
})

test_that("build pipeline only renders server-known states", {
  queued <- builder_stage_html(builder_build_pipeline_ui("queued"))
  building <- builder_stage_html(builder_build_pipeline_ui("building"))
  complete <- builder_stage_html(builder_build_pipeline_ui("complete"))
  failure <- builder_stage_html(builder_build_pipeline_ui("failure"))

  expect_match(queued, 'data-pipeline-state="queued"', fixed = TRUE)
  expect_match(building, 'data-pipeline-state="building"', fixed = TRUE)
  expect_match(complete, 'data-pipeline-state="complete"', fixed = TRUE)
  expect_match(failure, 'data-pipeline-state="failure"', fixed = TRUE)
  expect_false(grepl("Verify", paste(queued, building, failure)))

  decision <- builder_stage_html(
    builder_build_status_ui(builder_result_needs_decision("Choose one."))
  )
  expect_false(grepl("builder-build-pipeline", decision, fixed = TRUE))
})

test_that("release recovery evidence produces a recovery-required result", {
  recovery <- list(
    state = "recovery_required",
    message = "Restore the preserved backup before retrying.",
    backup = "release.backup"
  )
  mapped <- builder_release_error_result(
    "Publishing failed.",
    "/release",
    .recovery = function(target) {
      expect_identical(target, "/release")
      recovery
    }
  )
  ordinary <- builder_release_error_result(
    "Validation failed.",
    "/release",
    .recovery = function(target) list(state = "ready")
  )

  expect_s3_class(mapped, "builder_result_recovery_required")
  expect_identical(mapped$state, "recovery_required")
  expect_identical(mapped$recovery, recovery)
  expect_s3_class(ordinary, "builder_result_failure")
})

test_that("recovery actions require explicit typed evidence", {
  undecidable <- builder_stage_html(builder_build_status_ui(
    builder_result_needs_decision("Choose one.", retry_closure = "marker_genes")
  ))
  targeted <- builder_stage_html(builder_build_status_ui(
    builder_result_needs_decision(
      "Choose one.",
      retry_closure = "marker_genes",
      failed_dataset_id = "ds1"
    )
  ))
  ordinary_failure <- builder_stage_html(builder_build_status_ui(
    builder_result_failure("Export failed.")
  ))
  worker_failure <- builder_stage_html(builder_build_status_ui(
    builder_result_failure("Worker stopped.", restartable_worker = TRUE)
  ))

  expect_match(undecidable, "Retry optional work", fixed = TRUE)
  expect_false(grepl("Remove and rebuild", undecidable, fixed = TRUE))
  expect_match(targeted, "Remove and review", fixed = TRUE)
  expect_false(grepl("Remove and rebuild", targeted, fixed = TRUE))
  expect_false(grepl("Restart worker", ordinary_failure, fixed = TRUE))
  expect_match(worker_failure, "Restart worker", fixed = TRUE)
})

test_that("a real publish restore failure maps to recovery required", {
  local({
    builder_repo_source("publish.R")
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "build-result-recovery",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    moves <- 0L
    fail_publish_and_restore <- function(from, to) {
      moves <<- moves + 1L
      if (moves %in% c(2L, 3L)) {
        return(FALSE)
      }
      file.rename(from, to)
    }
    failure <- tryCatch(
      builder_publish_release(handle, .move = fail_publish_and_restore),
      error = function(error) error
    )

    mapped <- builder_release_error_result(
      conditionMessage(failure),
      target,
      .recovery = builder_discover_recovery
    )

    expect_s3_class(mapped, "builder_result_recovery_required")
    expect_identical(mapped$recovery$state, "recovery_required")
    expect_true(dir.exists(mapped$recovery$backup))
  })
})

test_that("Open App requires a verified published final App directory", {
  no_app <- builder_build_status_ui(builder_result_success(
    published = TRUE,
    built = "/release/dataset.crb"
  ))
  verified_app <- builder_build_status_ui(builder_result_success(
    published = TRUE,
    built = "/release/dataset.crb",
    app_dir = "/release/cerebro_app",
    app_verified = TRUE,
    report_path = "/release/build-report.json"
  ))

  expect_false(grepl("Open App", builder_stage_html(no_app), fixed = TRUE))
  verified_html <- builder_stage_html(verified_app)
  expect_match(verified_html, "Open App", fixed = TRUE)
  expect_match(verified_html, "Reveal Folder", fixed = TRUE)
  expect_match(verified_html, "Copy Path", fixed = TRUE)
  expect_match(verified_html, "Copy Report", fixed = TRUE)
  expect_match(verified_html, 'data-path="/release/cerebro_app"', fixed = TRUE)
  expect_match(
    verified_html,
    'data-report="/release/build-report.json"',
    fixed = TRUE
  )
})

test_that("result actions execute through injected platform boundaries", {
  opened <- revealed <- copied <- character()
  release_dir <- withr::local_tempdir()
  app_dir <- file.path(release_dir, "cerebro_app")
  dir.create(app_dir)
  app <- builder_result_success(
    published = TRUE,
    built = file.path(release_dir, "dataset.crb"),
    app_dir = app_dir,
    app_verified = TRUE,
    report_path = file.path(release_dir, "build-report.json"),
    release = list(target = release_dir)
  )

  expect_true(builder_open_final_app(app, .open = function(path, env_file) {
    opened <<- list(path = path, env_file = env_file)
    TRUE
  }))
  expect_true(builder_reveal_release(app, .reveal = function(path) {
    revealed <<- path
    TRUE
  }))
  expect_true(builder_copy_result_path(app, "release", .copy = function(value) {
    copied <<- value
    TRUE
  }))
  expect_identical(
    opened,
    list(
      path = normalizePath(app_dir, winslash = "/", mustWork = TRUE),
      env_file = NULL
    )
  )
  expect_identical(revealed, release_dir)
  expect_identical(copied, release_dir)
  expect_error(
    builder_open_final_app(builder_result_success(published = TRUE)),
    "verified final App"
  )
})

test_that("result auth fields are typed and status releases point to the root", {
  release_dir <- withr::local_tempdir()
  app_dir <- file.path(release_dir, "cerebro_app")
  dir.create(app_dir)
  env_file <- file.path(release_dir, "viewer-auth.env")
  writeLines(
    paste0("CEREBRO_AUTH_PASSPHRASE=", strrep("a", 64L)),
    env_file,
    useBytes = TRUE
  )
  Sys.chmod(env_file, mode = "0600", use_umask = FALSE)

  public <- builder_result_success(
    published = TRUE,
    app_dir = app_dir,
    app_verified = TRUE,
    auth_enabled = FALSE,
    auth_env_file = NULL,
    release = list(target = release_dir)
  )
  login <- builder_result_success(
    published = TRUE,
    app_dir = app_dir,
    app_verified = TRUE,
    auth_enabled = TRUE,
    auth_env_file = env_file,
    release = list(target = release_dir)
  )

  expect_identical(builder_build_status_model(public)$release_dir, release_dir)
  expect_identical(builder_build_status_model(login)$release_dir, release_dir)
  expect_match(
    builder_stage_html(builder_build_status_ui(login)),
    "Login: Required",
    fixed = TRUE
  )
  opened <- NULL
  expect_true(builder_open_final_app(
    login,
    .verify_auth = function(app_dir, env_file) TRUE,
    .open = function(path, env_file) {
      opened <<- list(path = path, env_file = env_file)
      TRUE
    }
  ))
  expect_identical(
    opened,
    list(
      path = normalizePath(app_dir, winslash = "/", mustWork = TRUE),
      env_file = normalizePath(env_file, winslash = "/", mustWork = TRUE)
    )
  )

  expect_error(
    builder_result_success(auth_enabled = TRUE, auth_env_file = NULL),
    "authentication"
  )
  expect_error(
    builder_as_result(list(
      state = "success",
      auth_enabled = FALSE,
      auth_env_file = env_file
    )),
    "authentication"
  )
  expect_error(
    builder_as_result(structure(
      list(
        state = "success",
        auth_enabled = NA,
        auth_env_file = NULL
      ),
      class = c("builder_result_success", "builder_result", "list")
    )),
    "authentication"
  )
  for (invalid in list(
    list(auth_enabled = TRUE, auth_env_file = character()),
    list(auth_enabled = FALSE, auth_env_file = env_file),
    list(auth_enabled = "true", auth_env_file = NULL),
    list(auth_enabled = c(TRUE, FALSE), auth_env_file = env_file)
  )) {
    expect_error(
      do.call(builder_result_success, invalid),
      "authentication"
    )
  }
  expect_error(
    builder_result_failure("no", auth_enabled = FALSE),
    "Only successful"
  )
})

test_that("Open App rejects malformed authentication files before launch", {
  root <- withr::local_tempdir()
  app_dir <- file.path(root, "cerebro_app")
  dir.create(app_dir)
  env_file <- file.path(root, "viewer-auth.env")
  writeLines(c("CEREBRO_AUTH_PASSPHRASE=", "second=line"), env_file)
  Sys.chmod(env_file, mode = "0600", use_umask = FALSE)
  launched <- FALSE

  expect_error(
    .builder_open_app_child(
      app_dir,
      env_file,
      "CEREBRO_AUTH_PASSPHRASE",
      .run_app = function(...) {
        launched <<- TRUE
        TRUE
      }
    ),
    "authentication environment is invalid"
  )
  expect_false(launched)

  login <- builder_result_success(
    published = TRUE,
    app_dir = app_dir,
    app_verified = TRUE,
    auth_enabled = TRUE,
    auth_env_file = env_file
  )
  expect_error(
    builder_open_final_app(login, .open = function(...) TRUE),
    "Authentication files are incomplete"
  )
})

test_that("typed Review controls expose only accepted App options", {
  options <- builder_review_options(
    welcome_message = "Welcome, team!",
    initial_page = "projection",
    point_size = 5,
    variable_to_compare = TRUE,
    host = "127.0.0.1",
    port = 4242L,
    max_request_size = 512,
    display_mode = "normal",
    launch_browser = FALSE,
    show_upload_ui = FALSE
  )

  expect_s3_class(options, "builder_review_options")
  frozen <- builder_review_options_for_plan(
    options,
    initial_dataset = "dataset-a"
  )
  expect_identical(
    names(frozen),
    c(
      "show_upload_ui",
      "initial_dataset",
      "initial_page",
      "welcome_message",
      "variable_to_compare",
      "host",
      "port",
      "max_request_size",
      "display_mode",
      "launch_browser"
    )
  )
  expect_null(frozen$point_size)
  expect_identical(frozen$initial_page, "projection")
  expect_error(builder_review_options(port = 0), "Review options")
  expect_error(
    builder_review_options(initial_page = "missing"),
    "Review options"
  )

  page_choices <- builder_review_initial_page_choices(list(
    always = builder_viewer_page_catalog()$always,
    visible_conditional = "trajectory"
  ))
  html <- builder_stage_html(builder_review_controls_ui(
    "review",
    options,
    page_choices
  ))
  for (label in c(
    "Starting page",
    "Welcome message",
    "Variable to compare",
    "Allow uploads"
  )) {
    expect_match(html, label, fixed = TRUE)
  }
  expect_match(html, "review-initial_page", fixed = TRUE)
  expect_match(html, "Linked views", fixed = TRUE)
  expect_match(html, "Trajectory", fixed = TRUE)
  expect_false(grepl("Spatial", html, fixed = TRUE))
  for (label in c(
    "Point size",
    "Host",
    "Port",
    "Request size",
    "Display mode",
    "Launch browser"
  )) {
    expect_false(grepl(label, html, fixed = TRUE))
  }
})
