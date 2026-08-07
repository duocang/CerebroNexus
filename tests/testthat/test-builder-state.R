builder_profile_source_runtime(globalenv())

builder_state_path <- builder_profile_inst_path("builder", "state.R")
if (nzchar(builder_state_path) && file.exists(builder_state_path)) {
  sys.source(builder_state_path, envir = globalenv())
}

builder_state_api <- c(
  "builder_dataset_state",
  "builder_reduce_dataset",
  "builder_build_state",
  "builder_reduce_build"
)
builder_state_api_available <- all(vapply(
  builder_state_api,
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
))

builder_state_manifest_entry <- function(
  status = "valid",
  required_action = NULL
) {
  builder_manifest_entry(
    id = "expression",
    source = list(type = "example", location = "state-fixture"),
    status = status,
    disposition = if (identical(status, "blocking")) {
      "rejected"
    } else {
      "preserved"
    },
    artifact_scope = "both",
    pages = "gene_expression",
    required_action = required_action
  )
}

builder_state_entry <- function(status = "valid") {
  list(
    id = "dataset-a",
    acknowledgements = character(),
    settings = list(analyses = character()),
    dataset_profile = list(
      manifest = builder_content_manifest(list(
        builder_state_manifest_entry(status)
      )),
      content = list()
    )
  )
}

capture_builder_state_error <- function(expr) {
  tryCatch(
    {
      force(expr)
      NULL
    },
    builder_state_error = function(error) error,
    error = function(error) error
  )
}

test_that("the pure Builder state API is available", {
  expect_true(builder_state_api_available)
})

if (builder_state_api_available) {
  test_that("dataset state requires a recognized entry and inert settings", {
    legacy <- list(
      id = "legacy-a",
      profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
      settings = list(analyses = character())
    )
    expect_identical(builder_dataset_state(legacy)$readiness, "ready")
    expect_identical(
      builder_dataset_state(builder_state_entry())$readiness,
      "ready"
    )

    missing_settings <- legacy
    missing_settings$settings <- NULL
    atomic_settings <- legacy
    atomic_settings$settings <- "settings"
    classed_settings <- legacy
    classed_settings$settings <- structure(
      list(analyses = character()),
      class = "builder_settings"
    )
    reference_settings <- legacy
    attr(reference_settings$settings, "mutable") <-
      new.env(parent = emptyenv())
    cases <- list(
      empty = list(),
      unrecognized = list(settings = list(analyses = character())),
      missing_settings = missing_settings,
      atomic_settings = atomic_settings,
      classed_settings = classed_settings,
      reference_settings = reference_settings
    )
    expected_codes <- c(
      empty = "invalid_dataset_entry",
      unrecognized = "invalid_dataset_entry",
      missing_settings = "invalid_dataset_settings",
      atomic_settings = "invalid_dataset_settings",
      classed_settings = "invalid_dataset_settings",
      reference_settings = "invalid_dataset_settings"
    )
    for (label in names(cases)) {
      error <- capture_builder_state_error(
        builder_dataset_state(cases[[label]])
      )
      expect_s3_class(error, "builder_state_error")
      expect_identical(error$code, expected_codes[[label]], info = label)
    }
  })

  test_that("dataset reducers always recompute manifest readiness", {
    state <- builder_dataset_state(builder_state_entry())
    blocking <- builder_state_entry("blocking")$dataset_profile$manifest

    reduced <- builder_reduce_dataset(
      state,
      list(type = "replace_manifest", manifest = blocking)
    )

    expect_identical(state$readiness, "ready")
    expect_identical(reduced$readiness, "blocked")
    expect_identical(reduced$blocking_ids, "expression")
    expect_gt(reduced$revision, state$revision)
  })

  test_that("dataset state keeps canonical acknowledgement semantics", {
    acknowledge <- builder_state_entry()
    acknowledge$dataset_profile$manifest <- builder_content_manifest(list(
      builder_state_manifest_entry(
        status = "attention",
        required_action = list(type = "acknowledge", token = "accepted")
      )
    ))
    acknowledge$acknowledgements <- "accepted"

    choose <- builder_state_entry()
    choose$dataset_profile$manifest <- builder_content_manifest(list(
      builder_state_manifest_entry(
        status = "attention",
        required_action = list(type = "choose")
      )
    ))
    choose$acknowledgements <- "expression"

    expect_identical(builder_dataset_state(acknowledge)$readiness, "ready")
    expect_identical(
      builder_dataset_state(choose)$readiness,
      "needs_attention"
    )
  })

  test_that("dataset reducers reject unknown actions", {
    error <- capture_builder_state_error(builder_reduce_dataset(
      builder_dataset_state(builder_state_entry()),
      list(type = "invented")
    ))

    expect_s3_class(error, "builder_state_error")
    expect_identical(error$code, "unknown_dataset_action")
  })

  test_that("build state rejects a second in-flight build", {
    state <- builder_build_state()
    running <- builder_reduce_build(
      state,
      list(type = "start", id = "build-1", revision = 4L)
    )
    error <- capture_builder_state_error(builder_reduce_build(
      running,
      list(type = "start", id = "build-2", revision = 5L)
    ))

    expect_identical(running$status, "running")
    expect_identical(running$id, "build-1")
    expect_identical(running$plan_revision, 4L)
    expect_s3_class(error, "builder_state_error")
    expect_identical(error$code, "build_in_flight")
  })
}
