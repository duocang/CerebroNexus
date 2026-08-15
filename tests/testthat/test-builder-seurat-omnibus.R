builder_omnibus_env <- new.env(parent = globalenv())
sys.source(
  builder_profile_inst_path("builder", "io.R"),
  envir = builder_omnibus_env
)

test_that("Builder gallery contains one synthetic Seurat omnibus", {
  catalog <- builder_omnibus_env$builder_example_catalog()
  directory <- builder_omnibus_env$builder_example_directory()

  expect_identical(names(catalog), "all_content")
  expect_identical(
    unname(vapply(directory, `[[`, character(1), "id")),
    "all_content"
  )
  expect_identical(catalog$all_content$provenance, "synthetic")
  expect_match(catalog$all_content$serialized_path, "all_content\\.rds$")
})

test_that("omnibus fixture models a user-uploaded Xenium Seurat object", {
  object <- builder_omnibus_env$builder_example_catalog()$all_content$make()$object

  expect_s4_class(object, "Seurat")
  expect_s4_class(
    SeuratObject::LayerData(object, layer = "counts"),
    "sparseMatrix"
  )
  expect_true(all(
    c(
      "patient_id",
      "section_id",
      "fov_id",
      "sample_id",
      "condition",
      "cell_type",
      "cluster",
      "region",
      "nCount_RNA",
      "nFeature_RNA"
    ) %in%
      colnames(object[[]])
  ))
  expect_setequal(
    unique(as.character(object$patient_id)),
    c(
      "patient_a",
      "patient_b",
      "patient_c"
    )
  )
  expect_setequal(SeuratObject::Reductions(object), c("pca", "umap", "tsne"))

  fovs <- c(
    "section_a_1_fov_1",
    "section_a_2_fov_1",
    "section_b_1_fov_1",
    "section_b_2_fov_1",
    "section_b_3_fov_1",
    "section_c_1_fov_1"
  )
  expect_setequal(SeuratObject::Images(object), fovs)
  section_map <- unique(data.frame(
    patient_id = as.character(object$patient_id),
    section_id = as.character(object$section_id),
    stringsAsFactors = FALSE
  ))
  expect_identical(
    as.integer(table(section_map$patient_id)),
    c(2L, 3L, 1L)
  )
  expect_true(all(vapply(
    split(object$section_id, object$fov_id),
    function(x) length(unique(x)) == 1L,
    logical(1)
  )))
  expect_true(all(vapply(
    split(object$patient_id, object$section_id),
    function(x) length(unique(x)) == 1L,
    logical(1)
  )))
  for (fov in fovs) {
    fov_cells <- SeuratObject::Cells(object[[fov]])
    expect_gt(length(fov_cells), 0L)
    expect_true(all(fov_cells %in% colnames(object)))
    expect_true(all(as.character(object$fov_id[fov_cells]) == fov))
    coordinates <- SeuratObject::GetTissueCoordinates(object, image = fov)
    expect_true(all(c("x", "y") %in% colnames(coordinates)))
    expect_true(all(is.finite(coordinates$x)))
    expect_true(all(is.finite(coordinates$y)))
    expect_gt(length(unique(coordinates$x)), 1L)
    expect_gt(length(unique(coordinates$y)), 1L)
    expect_identical(
      unique(as.character(object$section_id[fov_cells])),
      sub("_fov_[0-9]+$", "", fov)
    )
  }
  coordinate_system <- object@misc$spatial_coordinate_system
  expect_setequal(names(coordinate_system), fovs)
  expect_true(all(vapply(
    coordinate_system,
    function(item) {
      identical(item$unit, "micron") &&
        identical(item$origin, "top-left") &&
        identical(item$x_direction, "right") &&
        identical(item$y_direction, "down")
    },
    logical(1)
  )))

  forbidden <- c(
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectories",
    "extra_material",
    "immune_repertoire",
    "hla_typing"
  )
  expect_false(any(forbidden %in% names(object@misc)))

  trekker <- object@misc$trekker
  expect_type(trekker, "list")
  expect_true(length(trekker$barcodes) > 0L)
  expect_identical(anyDuplicated(trekker$barcodes), 0L)
  expect_true(all(trekker$barcodes %in% colnames(object)))
})

test_that("omnibus Trekker payload satisfies the Builder content contract", {
  for (relative in c(
    "viewer/core/spatial_coordinate_contract.R",
    "builder/spatial.R",
    "builder/content_spatial.R"
  )) {
    sys.source(
      builder_profile_inst_path(relative),
      envir = builder_omnibus_env
    )
  }
  object <- builder_omnibus_env$builder_example_catalog()$all_content$make()$object
  profile <- builder_omnibus_env$builder_profile_trekker_payload(
    object@misc$trekker,
    list(
      cells = SeuratObject::Cells(object),
      features = SeuratObject::Features(object)
    )
  )

  expect_true(profile$valid, info = paste(profile$diagnostics, collapse = ", "))
  expect_identical(profile$diagnostics, character())
  expect_identical(profile$page_candidates, "trekker")
})

test_that("omnibus fixture declares section-owned multi-image histology sidecars", {
  record <- builder_omnibus_env$builder_example_catalog()$all_content
  expected <- c(
    "section_a_1_he.png",
    "section_a_1_dapi.png",
    "section_a_2_he.png",
    "section_a_2_dapi.png",
    "section_b_1_he.png",
    "section_b_1_if.png",
    "section_b_1_pas.png"
  )

  expect_setequal(record$expected_supporting_content, expected)
  expect_setequal(names(record$histology_images), sub("\\.png$", "", expected))
  expect_true(all(mapply(
    function(id, image) identical(image$id, id),
    names(record$histology_images),
    record$histology_images,
    USE.NAMES = FALSE
  )))
  images_by_fov <- split(
    names(record$histology_images),
    unlist(lapply(record$histology_images, `[[`, "fov_ids"), use.names = FALSE)
  )
  expect_identical(
    images_by_fov[["section_a_1_fov_1"]],
    c("section_a_1_he", "section_a_1_dapi")
  )
  expect_identical(
    images_by_fov[["section_a_2_fov_1"]],
    c("section_a_2_he", "section_a_2_dapi")
  )
  expect_identical(
    images_by_fov[["section_b_1_fov_1"]],
    c("section_b_1_he", "section_b_1_if", "section_b_1_pas")
  )
  expect_false("section_c_1_fov_1" %in% names(images_by_fov))
  expect_true(all(file.exists(file.path(
    dirname(record$serialized_path),
    vapply(record$histology_images, `[[`, character(1), "path")
  ))))
  expect_setequal(
    list.files(dirname(record$serialized_path)),
    c("all_content.rds", expected)
  )
})
