builder_plan_contract_source_runtime(environment())

test_that("Build preflight blocks a layer no longer present in the profile", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    entry <- builder_task6_entry()
    entry$settings$layer <- "data"
    entry$profile$assay_profiles[[entry$settings$assay]]$layers <- "counts"

    plan <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)

    expect_identical(plan$error_code, "missing_layer")
    expect_match(plan$error, "no longer available", fixed = TRUE)
  })
})

test_that("BuildPlan freezes only a safe login summary", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    entry <- builder_task6_entry()
    auth <- list(enabled = TRUE, account_count = 2L, timeout_minutes = 15L)
    plan <- builder_freeze_plan(
      entries = list(entry),
      out_dir = withr::local_tempdir(),
      make_app = TRUE,
      app_auth = auth
    )

    expect_s3_class(plan, "builder_build_plan")
    expect_identical(plan$app_auth, auth)
    expect_false(builder_auth_value_contains(plan, "auth-user-a-7f31"))
    expect_false(builder_auth_value_contains(plan, "auth-password-a-7f31"))
  })
})

test_that("login summary is impossible without App output", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    plan <- builder_freeze_plan(
      entries = list(builder_task6_entry()),
      out_dir = withr::local_tempdir(),
      make_app = FALSE,
      app_auth = list(enabled = TRUE, account_count = 1L, timeout_minutes = 15L)
    )

    expect_identical(plan$error_code, "invalid_app_auth")
    expect_match(plan$error, "requires App output", fixed = TRUE)
  })
})

test_that("frozen release targets are mutually exclusive by output mode", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    out_dir <- withr::local_tempdir()
    entry <- builder_task6_entry()
    crb <- builder_freeze_plan(list(entry), out_dir, make_app = FALSE)
    public <- builder_freeze_plan(
      list(entry),
      out_dir,
      make_app = TRUE,
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    )
    login <- builder_freeze_plan(
      list(entry),
      out_dir,
      make_app = TRUE,
      app_auth = list(enabled = TRUE, account_count = 2L, timeout_minutes = 15L)
    )
    expect_setequal(
      crb$output_release$targets,
      file.path(
        crb$out_dir,
        c(crb$items[[1L]]$filename, crb$items[[1L]]$sidecars)
      )
    )
    expect_identical(
      public$output_release$targets,
      file.path(public$out_dir, "cerebro_app")
    )
    expect_identical(
      login$output_release$targets,
      file.path(
        login$out_dir,
        c("cerebro_app", "viewer-auth.env")
      )
    )
  })
})

test_that("legacy seventh positional argument remains prior identity", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    builder_repo_source("publish.R")
    out_dir <- withr::local_tempdir()
    prior <- builder_release_identity(out_dir)
    plan <- builder_freeze_plan(
      list(builder_task6_entry()),
      out_dir,
      FALSE,
      FALSE,
      NULL,
      list(),
      prior
    )

    expect_s3_class(plan, "builder_build_plan")
    expect_identical(plan$expected_prior_identity, prior)
    expect_identical(
      plan$app_auth,
      list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
    )
  })
})

test_that("profiles expose safe layer choices for every assay", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("inspect.R")

    counts <- Matrix::Matrix(
      matrix(
        seq_len(40),
        nrow = 5,
        dimnames = list(
          paste0("G", seq_len(5)),
          paste0("cell", seq_len(8))
        )
      ),
      sparse = TRUE
    )
    object <- SeuratObject::CreateSeuratObject(counts)
    object[["ADT"]] <- SeuratObject::CreateAssay5Object(counts = counts)
    object[["RNA"]] <- split(
      object[["RNA"]],
      f = rep(c("sample1", "sample2"), each = 4)
    )
    object$nCount_ADT <- seq_len(8)
    object$nFeature_ADT <- seq_len(8) + 10

    profile <- describe_seurat(object)

    expect_setequal(names(profile$assay_profiles), c("RNA", "ADT"))
    expect_identical(profile$assay_profiles$RNA$layers, "counts")
    expect_identical(profile$assay_profiles$RNA$default_layer, "counts")
    expect_identical(profile$assay_profiles$ADT$layers, "counts")
    expect_identical(profile$assay_profiles$ADT$nUMI, "nCount_ADT")
    expect_identical(profile$assay_profiles$ADT$nGene, "nFeature_ADT")

    prepared <- builder_prepare_export_layer(object, "RNA", "counts")
    expect_identical(
      dim(SeuratObject::LayerData(prepared[["RNA"]], layer = "counts")),
      c(5L, 8L)
    )
  })
})

test_that("new dataset settings start with canonical Viewer content fields", {
  local({
    builder_repo_source("plan.R")
    legacy <- list(
      default_assay = "RNA",
      assay_profiles = list(
        RNA = list(
          default_layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA"
        )
      ),
      group_preselect = c("cluster", "sample"),
      reduction_preselect = "umap",
      organism_guess = "hg"
    )
    modern <- list(
      viewer_content = list(
        trajectories = list(list(
          method = "monocle2",
          name = "lineage",
          selectable = TRUE
        ))
      )
    )

    settings <- builder_default_settings(
      legacy,
      "Dataset A",
      dataset_profile = modern
    )

    expect_identical(settings$included_groups, c("cluster", "sample"))
    expect_identical(settings$default_group, "cluster")
    expect_identical(settings$included_projections, "umap")
    expect_identical(settings$default_projection, "umap")
    expect_identical(settings$overview_point_size, 5)
    expect_identical(
      settings$included_trajectories,
      list(monocle2 = "lineage")
    )
    expect_identical(
      settings$default_trajectory,
      list(method = "monocle2", name = "lineage")
    )
  })
})

test_that("layer choices require exact cell identities", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("inspect.R")

    counts <- Matrix::Matrix(
      matrix(
        seq_len(40),
        nrow = 5,
        dimnames = list(
          paste0("G", seq_len(5)),
          paste0("cell", seq_len(8))
        )
      ),
      sparse = TRUE
    )
    object <- SeuratObject::CreateSeuratObject(counts)
    object[["RNA"]] <- split(
      object[["RNA"]],
      f = rep(c("sample1", "sample2"), each = 4)
    )

    choices <- builder_layer_choices(
      object[["RNA"]],
      expected_cells = SeuratObject::Cells(object)
    )

    expect_identical(choices, "counts")
    expect_false(any(grepl("^counts[.]", choices)))
    expect_error(
      builder_layer_choices(object[["RNA"]]),
      "expected_cells"
    )

    wrong <- builder_profile_wrong_assay()
    expect_identical(
      builder_layer_choices(
        wrong$assay,
        expected_cells = wrong$expected
      ),
      character()
    )
  })
})

test_that("build plans use collision-proof filenames and resolved colours", {
  local({
    builder_repo_source("prerequisite.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 1L
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    entries <- list(
      list(
        id = "ds1",
        profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
        levels = list(cluster = c("A", "B")),
        settings = list(
          name = "A/B",
          organism = "hg",
          assay = "RNA",
          layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          groups = "cluster",
          reductions = "umap",
          analyses = character(),
          tables = list(),
          images = list(),
          palette = "okabe_ito",
          color_overrides = list(cluster = c(B = "#ff00aa"))
        )
      ),
      list(
        id = "ds2",
        profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
        levels = list(cluster = c("A", "B")),
        settings = list(
          name = "A:B",
          organism = "hg",
          assay = "RNA",
          layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          groups = "cluster",
          reductions = "umap",
          analyses = character(),
          tables = list(),
          images = list(),
          palette = "cerebro",
          color_overrides = list()
        )
      )
    )

    out_dir <- withr::local_tempdir()
    plan <- builder_make_plan(
      entries,
      out_dir,
      make_app = TRUE
    )

    expect_null(plan$error)
    expect_length(unique(vapply(plan$items, `[[`, "", "filename")), 2)
    expect_match(plan$items[[1]]$filename, "^01-a-b-[a-z0-9]+[.]crb$")
    expect_match(plan$items[[2]]$filename, "^02-a-b-[a-z0-9]+[.]crb$")
    expect_identical(
      unname(plan$items[[1]]$colors$cluster[["B"]]),
      "#FF00AA"
    )

    sys.source(
      builder_profile_inst_path("builder", "app_bundle.R"),
      envir = environment()
    )
    built <- file.path(
      out_dir,
      vapply(plan$items, `[[`, character(1), "filename")
    )
    lapply(built, function(path) saveRDS(list(valid = TRUE), path))
    labels <- vapply(plan$items, `[[`, character(1), "name")
    names(built) <- labels

    expect_s3_class(
      builder_app_bundle_request(plan, built, labels),
      "builder_app_bundle_request"
    )
  })
})

test_that("resolved group colors are shared by every projection", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    settings <- list(
      groups = c("cluster", "sample"),
      palette = "cerebro",
      color_overrides = list(cluster = c(B = "#e76f51")),
      default_projection = "umap"
    )
    levels <- list(cluster = c("A", "B"), sample = c("one", "two"))
    umap <- builder_resolve_colors(settings, levels)
    settings$default_projection <- "pca"
    pca <- builder_resolve_colors(settings, levels)

    expect_identical(umap, pca)
    expect_identical(umap$cluster[["B"]], "#E76F51")
    expect_setequal(names(umap), c("cluster", "sample"))
    expect_false(any(c("umap", "pca", "tsne") %in% names(umap)))
  })
})

test_that("plan validation rejects blank and duplicate labels", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    entry <- function(id, name) {
      list(
        id = id,
        profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
        levels = list(cluster = c("A", "B")),
        settings = list(
          name = name,
          organism = "hg",
          assay = "RNA",
          layer = "data",
          groups = "cluster",
          reductions = "umap",
          analyses = character(),
          tables = list(),
          images = list(),
          palette = "cerebro",
          color_overrides = list()
        )
      )
    }

    blank <- builder_make_plan(list(entry("ds1", "  ")), tempdir())
    duplicate <- builder_make_plan(
      list(entry("ds1", "PBMC"), entry("ds2", " PBMC ")),
      tempdir()
    )

    expect_match(blank$error, "name", ignore.case = TRUE)
    expect_match(duplicate$error, "unique", ignore.case = TRUE)
  })
})

test_that("plan validation requires explicit QC fields", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    entry <- list(
      id = "ds1",
      profile = list(nUMI = NA_character_, nGene = NA_character_),
      levels = list(cluster = c("A", "B")),
      settings = list(
        name = "PBMC",
        organism = "hg",
        assay = "RNA",
        layer = "data",
        groups = "cluster",
        reductions = "umap",
        analyses = character(),
        tables = list(),
        images = list(),
        palette = "cerebro",
        color_overrides = list()
      )
    )

    plan <- builder_make_plan(list(entry), tempdir())

    expect_match(plan$error, "UMI|count", ignore.case = TRUE)
  })
})

test_that("analysis dependencies are normalized before a build", {
  local({
    builder_repo_source("plan.R")

    expect_identical(
      builder_normalize_analyses(
        c("marker_genes", "enriched_pathways"),
        has_marker_genes = FALSE
      ),
      c("marker_genes", "enriched_pathways")
    )
    expect_identical(
      builder_normalize_analyses(
        "enriched_pathways",
        has_marker_genes = FALSE
      ),
      character()
    )
    expect_identical(
      builder_normalize_analyses(
        "enriched_pathways",
        has_marker_genes = TRUE
      ),
      "enriched_pathways"
    )
  })
})
