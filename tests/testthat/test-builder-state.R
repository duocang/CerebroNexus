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

builder_viewer_settings_entry <- function(id = "dataset-a") {
  entry <- builder_minimal_entry(id = id, name = paste("Dataset", id))
  entry$profile$group_candidates <- c(
    "Cluster (3 groups)" = "cluster",
    "Sample (2 groups)" = "sample"
  )
  entry$profile$reductions <- c("umap", "pca")
  entry$profile$viewer_content <- list(
    metadata = list(
      cluster = list(group_eligible = TRUE),
      sample = list(group_eligible = TRUE),
      Phase = list(
        name = "Phase",
        classification = "categorical",
        group_eligible = TRUE,
        distinct_count = 3L,
        sample_values = c("G1", "S", "G2M")
      ),
      score = list(group_eligible = FALSE)
    ),
    projections = list(
      umap = list(available = TRUE),
      pca = list(available = TRUE)
    ),
    trajectories = list(
      list(
        method = "monocle2",
        name = "lineage",
        selectable = TRUE
      ),
      list(
        method = "slingshot",
        name = "curve",
        selectable = FALSE
      )
    )
  )
  entry$settings$groups <- c("cluster", "sample")
  entry$settings$reductions <- "umap"
  entry$settings$color_overrides <- list(
    cluster = c(A = "#112233")
  )
  entry
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
  test_that("legacy settings upgrade to one canonical Viewer content shape", {
    upgraded <- builder_upgrade_viewer_content_entry(
      builder_viewer_settings_entry()
    )
    settings <- upgraded$settings

    expect_identical(settings$viewer_content_schema_version, 1L)
    expect_identical(settings$included_groups, c("cluster", "sample"))
    expect_identical(settings$default_group, "cluster")
    expect_identical(
      settings$group_color_overrides$cluster[["A"]],
      "#112233"
    )
    expect_identical(settings$included_projections, "umap")
    expect_identical(settings$default_projection, "umap")
    expect_identical(settings$overview_point_size, 5)
    expect_identical(settings$overview_percentage_cells_to_show, 100)
    expect_identical(settings$cell_cycle_columns, "Phase")
    expect_identical(
      settings$included_trajectories,
      list(monocle2 = "lineage")
    )
    expect_identical(
      settings$default_trajectory,
      list(method = "monocle2", name = "lineage")
    )
  })

  test_that("points-only Spatial appearance is upgraded and validated per FOV", {
    entry <- builder_viewer_settings_entry()
    entry$settings$spatial_point_appearance <- list(
      "fov-a" = list(point_opacity = 0.7, point_size = 6)
    )
    upgraded <- builder_upgrade_viewer_content_entry(entry)

    expect_identical(
      upgraded$settings$spatial_point_appearance,
      list("fov-a" = list(point_opacity = 0.7, point_size = 6))
    )

    entry$settings$spatial_point_appearance <- list(
      "fov-a" = list(point_opacity = 1.1, point_size = 6)
    )
    error <- capture_builder_state_error(
      builder_upgrade_viewer_content_entry(entry)
    )
    expect_s3_class(error, "builder_state_error")
    expect_identical(error$code, "invalid_spatial_point_appearance")
  })

  test_that("initial Viewer cell percentage rejects values outside 10 to 100", {
    entry <- builder_upgrade_viewer_content_entry(
      builder_viewer_settings_entry()
    )
    entry$settings$overview_percentage_cells_to_show <- 0

    expect_error(
      builder_dataset_state(entry),
      class = "builder_state_error"
    )
  })

  test_that("cell-cycle settings reject fields outside the detected catalog", {
    entry <- builder_upgrade_viewer_content_entry(
      builder_viewer_settings_entry()
    )
    entry$settings$cell_cycle_columns <- "sample"

    expect_error(
      builder_dataset_state(entry),
      class = "builder_state_error"
    )
  })

  test_that("canonical Viewer defaults must belong to included content", {
    entry <- builder_upgrade_viewer_content_entry(
      builder_viewer_settings_entry()
    )
    entry$settings$default_group <- "score"
    expect_error(
      builder_dataset_state(entry),
      class = "builder_state_error"
    )

    entry <- builder_upgrade_viewer_content_entry(
      builder_viewer_settings_entry()
    )
    entry$settings$default_projection <- "pca"
    expect_error(
      builder_dataset_state(entry),
      class = "builder_state_error"
    )

    entry <- builder_upgrade_viewer_content_entry(
      builder_viewer_settings_entry()
    )
    entry$settings$default_trajectory <- list(
      method = "monocle2",
      name = "missing"
    )
    expect_error(
      builder_dataset_state(entry),
      class = "builder_state_error"
    )
  })

  test_that("Viewer content settings stay with their dataset across rail edits", {
    a <- builder_upgrade_viewer_content_entry(
      builder_viewer_settings_entry("a")
    )
    b <- builder_upgrade_viewer_content_entry(
      builder_viewer_settings_entry("b")
    )
    a$settings$default_group <- "cluster"
    b$settings$default_group <- "sample"
    b$settings$group_color_overrides <- list(
      sample = c(one = "#AABBCC")
    )

    state <- builder_state(list(a, b))
    moved <- builder_reduce_state(
      state,
      list(type = "reorder", order = c("b", "a"))
    )
    expect_identical(moved$datasets[[1L]]$id, "b")
    expect_identical(moved$datasets[[1L]]$settings$default_group, "sample")
    expect_identical(
      moved$datasets[[1L]]$settings$group_color_overrides$sample[["one"]],
      "#AABBCC"
    )
    expect_identical(moved$datasets[[2L]]$id, "a")
    expect_identical(moved$datasets[[2L]]$settings$default_group, "cluster")

    removed <- builder_reduce_state(
      moved,
      list(type = "remove", id = "b")
    )
    expect_identical(
      vapply(removed$datasets, `[[`, character(1), "id"),
      "a"
    )
    expect_identical(removed$datasets[[1L]]$settings$default_group, "cluster")
  })

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

  test_that("saved CRB descriptors do not require source-derived profiles", {
    entry <- list(
      id = "artifact-a",
      source_id = "artifact-a",
      output_id = "artifact-a",
      selector_value = "artifact-a",
      revision = 1L,
      load_state = "artifact_ready",
      settings = list(name = "Saved artifact"),
      profile = list(n_cells = 10L, n_genes = 5L),
      dataset_profile = list(),
      project_artifact = list(
        status = "ready",
        reusable = TRUE,
        path = "artifacts/artifact-a.crb",
        plan_item = list(
          analyses = character(),
          manifest = list(),
          metadata_policy = list(),
          viewer_page_expectations = list()
        )
      )
    )

    state <- builder_state(list(entry))

    expect_identical(state$datasets[[1L]]$load_state, "artifact_ready")
    expect_identical(
      builder_dataset_state(state$datasets[[1L]])$readiness,
      "artifact_ready"
    )
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
