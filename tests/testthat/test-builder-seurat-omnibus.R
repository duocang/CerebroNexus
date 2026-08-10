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
  object <- builder_omnibus_env$builder_make_permanent_fixture("all_content")

  expect_s4_class(object, "Seurat")
  expect_s4_class(
    SeuratObject::LayerData(object, layer = "counts"),
    "sparseMatrix"
  )
  expect_true(all(
    c(
      "patient",
      "section",
      "cell_type",
      "cluster",
      "region",
      "nCount_RNA",
      "nFeature_RNA"
    ) %in%
      colnames(object[[]])
  ))
  expect_setequal(
    unique(as.character(object$patient)),
    c(
      "patient_a",
      "patient_b",
      "patient_c"
    )
  )
  expect_setequal(SeuratObject::Reductions(object), c("pca", "umap", "tsne"))

  sections <- c(
    "patient_a_section_1",
    "patient_a_section_2",
    "patient_b_section_1",
    "patient_b_section_2",
    "patient_b_section_3",
    "patient_c_section_1"
  )
  expect_setequal(SeuratObject::Images(object), sections)
  expect_identical(
    unname(table(sub("_section_[0-9]+$", "", sections))),
    c(2L, 3L, 1L)
  )
  for (section in sections) {
    expect_gt(
      length(SeuratObject::Cells(object[[section]])),
      0L,
      info = section
    )
  }

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
  expect_false(anyDuplicated(trekker$barcodes))
  expect_true(all(trekker$barcodes %in% colnames(object)))
})

test_that("omnibus fixture declares five patient A/B histology sidecars", {
  record <- builder_omnibus_env$builder_example_catalog()$all_content
  expected <- c(
    "patient_a_section_1.png",
    "patient_a_section_2.png",
    "patient_b_section_1.png",
    "patient_b_section_2.png",
    "patient_b_section_3.png"
  )

  expect_setequal(record$expected_supporting_content, expected)
  expect_false(any(grepl("patient_c", expected, fixed = TRUE)))
  expect_true(all(file.exists(file.path(
    dirname(record$serialized_path),
    expected
  ))))
})
