## -------------------------------------------------------------------------
## Builder planning contracts.
##
## The builder UI is only a front end for these decisions. Keeping them in
## small pure helpers makes the expensive worker predictable and lets us test
## edge cases without starting Shiny.
## -------------------------------------------------------------------------

builder_repo_source <- function(file, local = parent.frame()) {
  path <- testthat::test_path("..", "..", "inst", "builder", file)
  if (!file.exists(path)) {
    path <- system.file(file.path("builder", file), package = "CerebroNexus")
  }
  source(path, local = local)
}

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

test_that("partial split layers are not offered as full expression matrices", {
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

    choices <- builder_layer_choices(object[["RNA"]], n_cells = ncol(object))

    expect_identical(choices, "counts")
    expect_false(any(grepl("^counts[.]", choices)))
  })
})

test_that("build plans use collision-proof filenames and resolved colours", {
  local({
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

    plan <- builder_make_plan(entries, tempdir(), make_app = TRUE)

    expect_null(plan$error)
    expect_length(unique(vapply(plan$items, `[[`, "", "filename")), 2)
    expect_match(plan$items[[1]]$filename, "^01-a-b-[a-z0-9]+[.]crb$")
    expect_match(plan$items[[2]]$filename, "^02-a-b-[a-z0-9]+[.]crb$")
    expect_identical(
      unname(plan$items[[1]]$colors$cluster[["B"]]),
      "#ff00aa"
    )
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
