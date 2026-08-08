builder_stage_contract_source_runtime(environment())

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
    invalid_plan <- frozen_review_plan()
    expect_identical(invalid_plan$error_code, "invalid_review_options")
    expect_false(app_env$builder_review_can_build(invalid_plan))

    invalid$port <- 8080L
    expect_true(validate_review_inputs(invalid))
    expect_true(review_validation()$ok)
    expect_s3_class(review_options(), "builder_review_options")
    expect_identical(frozen_review_plan()$error_code, "empty_release")
  })

  lines <- builder_app_source_lines()
  app_block <- function(start, finish) {
    first <- grep(start, lines, fixed = TRUE)[1L]
    last <- grep(finish, lines, fixed = TRUE)
    last <- last[last > first][1L]
    paste(lines[first:(last - 1L)], collapse = "\n")
  }
  workbench <- app_block(
    "output$workbench <- renderUI({",
    "output$review_stage <- renderUI({"
  )
  review_stage <- app_block(
    "output$review_stage <- renderUI({",
    "output$actionbar <- renderUI({"
  )
  review_app_options <- app_block(
    'output[["review_app_options"]] <- renderUI({',
    'output[["dataset_review_footer"]] <- renderUI({'
  )
  actionbar <- app_block(
    "output$actionbar <- renderUI({",
    "output$review_action_summary <- renderUI({"
  )
  expect_match(workbench, "entry <- isolate(entry_of(id))", fixed = TRUE)
  expect_false(grepl("frozen_review_plan()", workbench, fixed = TRUE))
  expect_match(workbench, 'uiOutput("review_stage")', fixed = TRUE)
  expect_match(workbench, 'uiOutput("review_app_options")', fixed = TRUE)
  expect_false(grepl("builder_review_controls_ui", workbench, fixed = TRUE))
  expect_match(
    review_app_options,
    "builder_review_controls_ui",
    fixed = TRUE
  )
  expect_match(review_stage, "frozen_review_plan()", fixed = TRUE)
  expect_false(grepl("builder_review_controls_ui", review_stage, fixed = TRUE))
  expect_match(actionbar, 'uiOutput("review_action_summary"', fixed = TRUE)
  expect_false(grepl("review_report()", actionbar, fixed = TRUE))
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
    reviewed <- isolate(store())
    reviewed$datasets[[1L]]$reviewed_revision <- before_groups
    store(reviewed)
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
    expect_false(identical(grouped$reviewed_revision, grouped$revision))

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
    reviewed <- isolate(store())
    reviewed$datasets[[1L]]$reviewed_revision <- before_projection
    store(reviewed)
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
    expect_false(identical(projected$reviewed_revision, projected$revision))

    before_point_size <- projected$revision
    reviewed <- isolate(store())
    reviewed$datasets[[1L]]$reviewed_revision <- before_point_size
    store(reviewed)
    session$setInputs(`core-point_size` = 8)
    session$flushReact()
    resized <- sets()[[1L]]
    expect_identical(resized$settings$overview_point_size, 8)
    expect_gt(resized$revision, before_point_size)
    expect_false(identical(resized$reviewed_revision, resized$revision))

    before_trajectory <- resized$revision
    reviewed <- isolate(store())
    reviewed$datasets[[1L]]$reviewed_revision <- before_trajectory
    store(reviewed)
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
    expect_false(identical(trajectory$reviewed_revision, trajectory$revision))

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
    marked <- isolate(store())
    marked$datasets[[1L]]$reviewed_revision <- before_color
    store(marked)
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
    expect_false(identical(colored$reviewed_revision, colored$revision))

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
      `enhance-analysis_marker_genes` = TRUE
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

    alignment <- list(uri = "data:image/png;base64,AA==")
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
        bounds = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
        cover = list(outside = 0L, total = 2L)
      ),
      `section-b` = list(
        bounds = c(xmin = 10, xmax = 20, ymin = 10, ymax = 20),
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

    active_slice("section-a")
    session$setInputs(`enhance-drop_image` = 1L)
    session$flushReact()
    expect_null(retain_updates[["enhance-histology_to_retain"]])
    expect_identical(
      names(sets()[[1L]]$settings$images),
      "section-b"
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
    "builder_alignment_apply_transform_to_all",
    fixed = TRUE
  )
  expect_match(
    server,
    "commit_section(entry, section, NULL)",
    fixed = TRUE
  )
})
