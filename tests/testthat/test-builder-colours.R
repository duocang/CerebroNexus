##----------------------------------------------------------------------------##
## The builder's group colours.
##
## Colours are keyed by group level, and `apply_configured_colors()` matches
## them with `intersect()` -- so a key that does not match the exported level is
## dropped in silence and the user simply sees default colours. The builder
## therefore MIRRORS the exporter's level rule rather than approximating it, and
## these tests pin the mirror so the two cannot drift apart unnoticed.
##----------------------------------------------------------------------------##

builder_src <- function(file) {
  path <- system.file(file.path("builder", file), package = "CerebroNexus")
  skip_if(!nzchar(path) || !file.exists(path), "builder not installed")
  path
}

test_that("builder level names match what the exporter actually produces", {
  skip_if_not_installed("Seurat")

  local({
    source(builder_src("inspect.R"), local = TRUE)

    set.seed(3)
    n <- 60
    obj <- make_synthetic_spatial_seurat(n_cells = n)

    ## Three shapes the exporter treats differently: an unsorted character
    ## column, a factor with a deliberately non-alphabetical level order, and a
    ## character column containing NA.
    obj$chr_group <- rep(c("zebra", "alpha", "mid"), length.out = n)
    obj$fct_group <- factor(
      rep(c("late", "early"), length.out = n),
      levels = c("late", "early")
    )
    obj$na_group <- rep(c("a", "b", NA), length.out = n)

    groups <- c("chr_group", "fct_group", "na_group")
    mirror <- builder_group_levels_for(obj, groups)

    dir <- withr::local_tempdir()
    exportFromSeurat(
      object = obj,
      assay = "Spatial",
      slot = "data",
      file = file.path(dir, "levels.crb"),
      experiment_name = "levels",
      organism = "mouse",
      groups = groups,
      nUMI = "nCount_Spatial",
      nGene = "nFeature_Spatial",
      verbose = FALSE
    )
    crb <- readRDS(file.path(dir, "levels.crb"))

    for (g in groups) {
      expect_identical(
        mirror[[g]],
        crb$getGroupLevels(g),
        info = paste(
          g,
          "-- builder_group_levels() must equal the exported levels exactly,",
          "or the configured palette is dropped without a word"
        )
      )
    }

    ## The specifics the mirror has to get right, spelled out so a future
    ## change to either side fails loudly rather than subtly.
    expect_identical(mirror$chr_group, c("alpha", "mid", "zebra"))
    expect_identical(mirror$fct_group, c("late", "early"))
    expect_true("N/A" %in% mirror$na_group)
    expect_identical(mirror$na_group[length(mirror$na_group)], "N/A")
  })
})

test_that("a palette covers every level, and N/A stays the viewer's grey", {
  local({
    source(builder_src("preview.R"), local = TRUE)

    lv <- c("alpha", "mid", "zebra", "N/A")
    cols <- builder_level_colors(lv, "cerebro")
    expect_identical(names(cols), lv)
    expect_true(all(grepl("^#", cols)))
    ## The viewer forces N/A to this grey; a preview that disagrees is lying.
    expect_identical(unname(cols[["N/A"]]), "#898989")
  })
})

test_that("hand-picked colours survive a change of palette", {
  local({
    source(builder_src("preview.R"), local = TRUE)

    lv <- c("a", "b", "c")
    override <- c(b = "#ff00aa")

    one <- builder_level_colors(lv, "cerebro", override)
    two <- builder_level_colors(lv, "okabe_ito", override)

    ## The touched level keeps its colour; the untouched ones follow the preset.
    expect_identical(unname(one[["b"]]), "#FF00AA")
    expect_identical(unname(two[["b"]]), "#FF00AA")
    expect_false(identical(unname(one[["a"]]), unname(two[["a"]])))
  })
})

test_that("builder color edits normalize valid hex values and reject invalid ones", {
  local({
    source(builder_src("preview.R"), local = TRUE)

    overrides <- list(
      cluster = c(A = "#112233"),
      sample = c(donor_1 = "#445566")
    )
    changed <- builder_update_color_override(
      overrides,
      "cluster",
      "B",
      "#e76f51"
    )

    expect_identical(changed$cluster, c(A = "#112233", B = "#E76F51"))
    expect_identical(changed$sample, overrides$sample)
    expect_identical(
      builder_update_color_override(changed, "cluster", "A", "red"),
      changed
    )
    expect_identical(
      builder_update_color_override(changed, "cluster", "A", ""),
      changed
    )
  })
})

test_that("group palettes stay isolated and reset only the requested group", {
  local({
    source(builder_src("preview.R"), local = TRUE)

    dataset_a <- list(
      cluster = c(A = "#111111"),
      sample = c(one = "#222222")
    )
    dataset_b <- list(cluster = c(A = "#333333"))

    next_a <- builder_update_color_override(
      dataset_a,
      "cluster",
      "A",
      "#abcdef"
    )
    expect_identical(next_a$cluster[["A"]], "#ABCDEF")
    expect_identical(next_a$sample, dataset_a$sample)
    expect_identical(dataset_b$cluster[["A"]], "#333333")

    reset <- builder_reset_color_overrides(next_a, "cluster")
    expect_null(reset$cluster)
    expect_identical(reset$sample, dataset_a$sample)
  })
})

test_that("existing palette values survive while new levels get stable defaults", {
  local({
    source(builder_src("preview.R"), local = TRUE)

    existing <- c(A = "#E76F51")
    one <- builder_level_colors(c("A", "B", "N/A"), "cerebro", existing)
    two <- builder_level_colors(c("A", "B", "N/A"), "cerebro", existing)

    expect_identical(one, two)
    expect_identical(one[["A"]], "#E76F51")
    expect_match(one[["B"]], "^#[0-9A-F]{6}$")
    expect_identical(one[["N/A"]], "#898989")
    expect_identical(builder_group_level_label("N/A"), "Missing")
  })
})

test_that("legacy resolved colors migrate only where canonical overrides are absent", {
  local({
    source(builder_src("preview.R"), local = TRUE)

    settings <- list(
      colors = list(
        cluster = c(A = "#111111"),
        sample = c(one = "#222222")
      ),
      color_overrides = list(cluster = c(A = "#abcdef"))
    )
    migrated <- builder_settings_color_overrides(settings)

    expect_identical(migrated$cluster[["A"]], "#ABCDEF")
    expect_identical(migrated$sample[["one"]], "#222222")
    expect_null(settings$color_overrides$sample)
  })
})

test_that("Viewer-content group colors take precedence over older settings", {
  local({
    source(builder_src("preview.R"), local = TRUE)

    settings <- list(
      colors = list(cluster = c(A = "#111111")),
      color_overrides = list(cluster = c(A = "#222222")),
      group_color_overrides = list(cluster = c(A = "#abcdef"))
    )
    resolved <- builder_settings_color_overrides(settings)

    expect_identical(resolved$cluster[["A"]], "#ABCDEF")
  })
})

test_that("every offered palette returns one usable colour per level", {
  local({
    source(builder_src("preview.R"), local = TRUE)

    for (p in builder_palettes()) {
      for (n in c(1L, 3L, 8L, 50L)) {
        cols <- p$colors(n)
        expect_length(cols, n)
        expect_false(
          any(is.na(cols)),
          info = paste(p$id, "at", n, "levels")
        )
        ## Anything createShinyApp cannot read as a colour is rejected at build
        ## time, so every palette has to survive col2rgb().
        expect_silent(grDevices::col2rgb(cols))
      }
    }
  })
})

test_that("the palette reaches the generated app keyed by its dataset label", {
  skip_if_not_installed("Seurat")

  local({
    source(builder_src("preview.R"), local = TRUE)
    source(builder_src("inspect.R"), local = TRUE)

    obj <- make_synthetic_spatial_seurat(n_cells = 40)
    dir <- withr::local_tempdir()
    crb <- convert_synthetic_to_crb(obj, dir, "colours")

    label <- "A label with spaces (and brackets)"
    levels <- builder_group_levels(obj, "seurat_clusters")
    palette <- builder_level_colors(levels, "okabe_ito", c(C1 = "#ff00aa"))

    app_dir <- file.path(dir, "app")
    createShinyApp(
      cerebro_data = stats::setNames(crb, label),
      result_dir = app_dir,
      colors = stats::setNames(
        list(list(seurat_clusters = palette)),
        label
      ),
      launch_browser = FALSE,
      verbose = FALSE
    )

    cfg <- readRDS(file.path(app_dir, "cerebro_config.rds"))

    ## resolve_configured_colors() matches the palette to the running data set
    ## by exact string comparison of this label. One character of difference and
    ## the whole palette is discarded with no message at all.
    expect_identical(names(cfg$colors), names(cfg$crb_file_to_load))
    expect_identical(names(cfg$colors), label)
    expect_identical(
      unname(cfg$colors[[label]]$seurat_clusters[["C1"]]),
      "#FF00AA"
    )
  })
})
