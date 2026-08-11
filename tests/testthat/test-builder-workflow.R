builder_repo_source("workflow.R")

builder_workflow_test_plan <- function(
  revision = 1L,
  out_dir = tempfile("builder-output-"),
  welcome_message = "Welcome"
) {
  structure(
    list(
      error = NULL,
      readiness = "ready",
      revision = revision,
      dataset_order = "dataset-a",
      out_dir = out_dir,
      make_app = TRUE,
      app_contract_version = "1",
      overwrite = FALSE,
      items = list(list(id = "dataset-a", readiness = "ready")),
      targets = file.path(out_dir, "dataset-a.crb"),
      existing_targets = character(),
      manifest = list(files = "dataset-a.crb"),
      app_options = list(welcome_message = welcome_message),
      app_auth = list(enabled = FALSE),
      acknowledgements = list(character())
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("review identity includes CRB intent and excludes output intent", {
  plan <- builder_workflow_test_plan()
  relocated <- plan
  relocated$out_dir <- tempfile("relocated-builder-output-")
  relocated$targets <- file.path(relocated$out_dir, "dataset-a.crb")
  relocated$existing_targets <- relocated$targets

  identity <- builder_review_plan_identity(plan)

  expect_named(
    identity,
    c(
      "revision",
      "dataset_order",
      "items",
      "manifest",
      "acknowledgements"
    )
  )
  expect_identical(identity, builder_review_plan_identity(relocated))

  changed_viewer <- plan
  changed_viewer$app_options$welcome_message <- "Changed"
  changed_viewer$make_app <- FALSE
  changed_viewer$app_auth <- list(enabled = TRUE, account_count = 2L)
  expect_identical(
    identity,
    builder_review_plan_identity(changed_viewer)
  )

  changed_crb <- plan
  changed_crb$items[[1L]]$included_groups <- "cell_type"
  expect_false(identical(
    identity,
    builder_review_plan_identity(changed_crb)
  ))
})

test_that("review identity requires a ready frozen BuildPlan", {
  plan <- builder_workflow_test_plan()

  expect_error(
    builder_review_plan_identity(unclass(plan)),
    "A ready frozen BuildPlan is required\\.",
    fixed = FALSE
  )

  plan$readiness <- "blocked"
  expect_error(
    builder_review_plan_identity(plan),
    "A ready frozen BuildPlan is required\\.",
    fixed = FALSE
  )
})

test_that("final build identity includes output-only settings", {
  plan <- builder_workflow_test_plan()
  changed <- plan
  changed$app_options$welcome_message <- "Changed at Build"

  expect_identical(
    builder_final_build_identity(plan)$review,
    builder_final_build_identity(changed)$review
  )
  expect_false(identical(
    builder_final_build_identity(plan),
    builder_final_build_identity(changed)
  ))

  relocated <- plan
  relocated$out_dir <- tempfile("final-output-")
  relocated$overwrite <- TRUE
  relocated$targets <- file.path(relocated$out_dir, "dataset-a.crb")
  expect_false(identical(
    builder_final_build_identity(plan),
    builder_final_build_identity(relocated)
  ))
})

test_that("review identity accepts frozen BuildPlan subclasses", {
  plan <- builder_workflow_test_plan()
  class(plan) <- c("special_builder_plan", class(plan))

  expect_identical(
    builder_review_plan_identity(plan),
    builder_review_plan_identity(builder_workflow_test_plan())
  )
})

test_that("workflow advances through review and confirmation", {
  plan <- builder_workflow_test_plan()
  state <- builder_workflow_state()

  expect_s3_class(state, "builder_workflow_state")
  expect_identical(
    state,
    structure(
      list(
        stage = "upload",
        review_plan = NULL,
        confirmation = NULL,
        revision = 0L
      ),
      class = c("builder_workflow_state", "list")
    )
  )

  state <- builder_reduce_workflow(
    state,
    list(type = "datasets_ready")
  )
  expect_identical(state$stage, "configure")
  expect_identical(state$revision, 1L)
  expect_error(
    builder_reduce_workflow(
      state,
      list(type = "confirm_review", plan = plan)
    ),
    "Review must be open"
  )

  state <- builder_reduce_workflow(
    state,
    list(type = "open_review", plan = plan)
  )
  expect_identical(state$stage, "review")
  expect_identical(state$review_plan, plan)

  plan$out_dir <- tempfile("confirmed-builder-output-")
  plan$targets <- file.path(plan$out_dir, "dataset-a.crb")
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", plan = plan)
  )
  expect_identical(state$stage, "build")
  expect_identical(state$confirmation$plan_revision, plan$revision)
  expect_true(builder_workflow_confirmation_matches(state, plan))

  changed_viewer <- plan
  changed_viewer$app_options$welcome_message <- "Changed"
  expect_true(builder_workflow_confirmation_matches(state, changed_viewer))

  state <- builder_reduce_workflow(state, list(type = "back_to_review"))
  expect_identical(state$stage, "review")
  expect_true(builder_workflow_confirmation_matches(state, plan))

  state <- builder_reduce_workflow(state, list(type = "back_to_settings"))
  expect_identical(state$stage, "configure")
  expect_identical(state$review_plan, plan)
  expect_true(builder_workflow_confirmation_matches(state, plan))

  state <- builder_reduce_workflow(state, list(type = "back_to_review"))
  expect_identical(state$stage, "review")
  expect_true(builder_workflow_confirmation_matches(state, plan))

  state <- builder_reduce_workflow(state, list(type = "invalidate"))
  expect_identical(state$stage, "configure")
  expect_null(state$review_plan)
  expect_null(state$confirmation)
})

test_that("available workflow stages can be revisited without losing confirmation", {
  plan <- builder_workflow_test_plan()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "datasets_ready")
  )
  expect_identical(
    builder_workflow_stage_availability(state, datasets_ready = TRUE),
    c(upload = TRUE, configure = TRUE, review = FALSE, build = FALSE)
  )

  state <- builder_reduce_workflow(
    state,
    list(type = "open_review", plan = plan)
  )
  expect_identical(
    builder_workflow_stage_availability(state, datasets_ready = TRUE),
    c(upload = TRUE, configure = TRUE, review = TRUE, build = FALSE)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", plan = plan)
  )
  expect_true(all(builder_workflow_stage_availability(
    state,
    datasets_ready = TRUE
  )))

  revisited <- builder_reduce_workflow(
    state,
    list(type = "navigate", stage = "upload")
  )
  expect_identical(revisited$stage, "upload")
  expect_identical(revisited$review_plan, plan)
  expect_true(builder_workflow_confirmation_matches(revisited, plan))

  expect_error(
    builder_reduce_workflow(
      builder_workflow_state(),
      list(type = "navigate", stage = "review")
    ),
    "not available"
  )
})

test_that("review confirmation rejects a changed BuildPlan", {
  plan <- builder_workflow_test_plan()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", plan = plan)
  )
  changed <- plan
  changed$items[[1L]]$included_groups <- "cell_type"

  expect_error(
    builder_reduce_workflow(
      state,
      list(type = "confirm_review", plan = changed)
    ),
    "The reviewed BuildPlan changed before confirmation\\."
  )
})

test_that("datasets becoming ready invalidates an old confirmation", {
  plan <- builder_workflow_test_plan()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", plan = plan)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", plan = plan)
  )
  expect_true(builder_workflow_confirmation_matches(state, plan))

  state <- builder_reduce_workflow(
    state,
    list(type = "datasets_ready")
  )

  expect_identical(state$stage, "configure")
  expect_null(state$review_plan)
  expect_null(state$confirmation)
  expect_false(builder_workflow_confirmation_matches(state, plan))
})

test_that("opening review preserves only the matching confirmation", {
  plan_a <- builder_workflow_test_plan()
  confirmed <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", plan = plan_a)
  )
  confirmed <- builder_reduce_workflow(
    confirmed,
    list(type = "confirm_review", plan = plan_a)
  )

  relocated <- plan_a
  relocated$out_dir <- tempfile("relocated-builder-output-")
  matching <- builder_reduce_workflow(
    confirmed,
    list(type = "open_review", plan = relocated)
  )
  expect_true(builder_workflow_confirmation_matches(matching, relocated))

  plan_b <- plan_a
  plan_b$items[[1L]]$included_groups <- "cell_type"
  changed <- builder_reduce_workflow(
    confirmed,
    list(type = "open_review", plan = plan_b)
  )
  expect_null(changed$confirmation)
  expect_false(builder_workflow_confirmation_matches(changed, plan_a))
})

test_that("workflow reset events clear reviewed and confirmed plans", {
  plan <- builder_workflow_test_plan()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", plan = plan)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", plan = plan)
  )

  invalidated <- builder_reduce_workflow(
    state,
    list(type = "invalidate", stage = "upload")
  )
  expect_identical(invalidated$stage, "upload")
  expect_null(invalidated$review_plan)
  expect_null(invalidated$confirmation)
  expect_named(
    invalidated,
    c("stage", "review_plan", "confirmation", "revision")
  )
  expect_identical(
    builder_reduce_workflow(
      invalidated,
      list(type = "datasets_ready")
    )$stage,
    "configure"
  )

  emptied <- builder_reduce_workflow(state, list(type = "empty"))
  expect_identical(emptied$stage, "upload")
  expect_null(emptied$review_plan)
  expect_null(emptied$confirmation)
  expect_named(
    emptied,
    c("stage", "review_plan", "confirmation", "revision")
  )
  expect_identical(
    builder_reduce_workflow(
      emptied,
      list(type = "datasets_ready")
    )$stage,
    "configure"
  )
})

test_that("workflow guards malformed state and events", {
  plan <- builder_workflow_test_plan()
  state <- builder_workflow_state()

  expect_false(builder_workflow_confirmation_matches(list(), plan))
  expect_false(builder_workflow_confirmation_matches(state, plan))
  expect_false(builder_workflow_confirmation_matches(state, list()))

  expect_error(
    builder_reduce_workflow(list(), list(type = "empty")),
    "A valid Builder workflow event is required\\."
  )
  expect_error(
    builder_reduce_workflow(state, NULL),
    "A valid Builder workflow event is required\\."
  )
  expect_error(
    builder_reduce_workflow(state, list(type = character())),
    "A valid Builder workflow event is required\\."
  )
  expect_error(
    builder_reduce_workflow(state, list(type = "future_event")),
    "The Builder workflow event is not supported\\."
  )
  expect_error(
    builder_reduce_workflow(state, list(type = "open_review", plan = list())),
    "A ready frozen BuildPlan is required\\."
  )
  expect_error(
    builder_reduce_workflow(state, list(type = "back_to_review")),
    "No reviewed BuildPlan is available\\."
  )
})

test_that("workflow validator rejects impossible review and build states", {
  plan <- builder_workflow_test_plan()
  state <- builder_workflow_state()

  impossible_review <- state
  impossible_review$stage <- "review"
  expect_false(.builder_workflow_state_valid(impossible_review))

  impossible_build <- state
  impossible_build$stage <- "build"
  expect_false(.builder_workflow_state_valid(impossible_build))

  malformed_build <- impossible_build
  malformed_build$review_plan <- plan
  malformed_build$confirmation <- list(
    identity = list(revision = plan$revision),
    plan_revision = plan$revision
  )
  expect_false(.builder_workflow_state_valid(malformed_build))
})

test_that("invalidate returns only to stages that need no confirmation", {
  plan <- builder_workflow_test_plan()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", plan = plan)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", plan = plan)
  )

  expect_error(
    builder_reduce_workflow(
      state,
      list(type = "invalidate", stage = "build")
    ),
    "Invalidation must return to upload or configure.",
    fixed = TRUE
  )
  expect_error(
    builder_reduce_workflow(
      state,
      list(type = "invalidate", stage = "review")
    ),
    "Invalidation must return to upload or configure.",
    fixed = TRUE
  )
})

test_that("every reducer branch returns a valid workflow state", {
  plan <- builder_workflow_test_plan()
  state <- builder_workflow_state()
  expect_true(.builder_workflow_state_valid(state))

  state <- builder_reduce_workflow(state, list(type = "datasets_ready"))
  expect_true(.builder_workflow_state_valid(state))

  state <- builder_reduce_workflow(
    state,
    list(type = "open_review", plan = plan)
  )
  expect_true(.builder_workflow_state_valid(state))

  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", plan = plan)
  )
  expect_true(.builder_workflow_state_valid(state))

  state <- builder_reduce_workflow(state, list(type = "back_to_review"))
  expect_true(.builder_workflow_state_valid(state))

  state <- builder_reduce_workflow(state, list(type = "back_to_settings"))
  expect_true(.builder_workflow_state_valid(state))

  state <- builder_reduce_workflow(state, list(type = "back_to_review"))
  expect_true(.builder_workflow_state_valid(state))

  state <- builder_reduce_workflow(state, list(type = "invalidate"))
  expect_true(.builder_workflow_state_valid(state))

  state <- builder_reduce_workflow(state, list(type = "empty"))
  expect_true(.builder_workflow_state_valid(state))
})

test_that("app and source helper load workflow immediately after state", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  app_state <- grep('source("state.R", local = TRUE)', app, fixed = TRUE)
  app_workflow <- grep(
    'source("workflow.R", local = TRUE)',
    app,
    fixed = TRUE
  )

  expect_length(app_state, 1L)
  expect_length(app_workflow, 1L)
  expect_identical(app_workflow, app_state + 1L)
  expect_true("workflow.R" %in% builder_app_source_files)
})
