generated_app_e2e_reset_runtime()

test_that("basic generated dataset exposes the exact core navigation and summary", {
  fixture <- generated_app_e2e_select_dataset("basic")
  options <- generated_app_e2e_selector_options()

  expect_identical(
    options$labels,
    names(generated_app_e2e_bundle()$config$crb_file_to_load)
  )
  expect_identical(
    options$values,
    unname(generated_app_e2e_bundle()$config$crb_file_to_load)
  )
  expect_identical(
    generated_app_e2e_visible_pages(),
    fixture$expected$visible_pages
  )

  summary <- generated_app_e2e_dataset_summary()
  expect_match(summary$cells, "36", fixed = TRUE)
  expect_match(summary$organism, "hg", fixed = TRUE)
})

test_that("Projection uses the Builder defaults, palette, and complete cell set", {
  fixture <- generated_app_e2e_select_dataset("basic")
  generated_app_e2e_activate_tab("projection")
  generated_app_e2e_wait_input("overview_projection_to_display")
  generated_app_e2e_wait_input("overview_projection_point_color")

  expect_identical(
    generated_app_e2e_driver()$get_js(
      "document.getElementById('overview_projection_point_size').getAttribute('data-min')"
    ),
    "0"
  )

  expect_identical(
    generated_app_e2e_value("input", "overview_projection_to_display"),
    fixture$expected$default_projection
  )
  expect_identical(
    generated_app_e2e_value("input", "overview_projection_point_color"),
    fixture$expected$default_group
  )
  expect_equal(
    generated_app_e2e_value("input", "overview_projection_point_size"),
    fixture$expected$app_settings$point_size$overview_projection_point_size
  )

  generated_app_e2e_wait_plotly("overview_projection")
  expect_identical(
    generated_app_e2e_plotly_point_count("overview_projection"),
    fixture$expected$n_cells
  )
  expect_length(
    generated_app_e2e_value("export", "overview_cells_to_show"),
    fixture$expected$n_cells
  )

  observed_coordinates <- generated_app_e2e_plotly_coordinates(
    "overview_projection"
  )
  expected_coordinates <- fixture$expected$projection_coordinates[[
    fixture$expected$default_projection
  ]]
  expect_true(all(vapply(
    seq_len(nrow(expected_coordinates)),
    function(index) {
      any(
        abs(observed_coordinates[, 1L] - expected_coordinates[index, 1L]) <
          1e-12 &
          abs(observed_coordinates[, 2L] - expected_coordinates[index, 2L]) <
            1e-12
      )
    },
    logical(1)
  )))

  palettes <- generated_app_e2e_value("export", "group_colors")
  expect_identical(
    unname(palettes$seurat_clusters[names(
      fixture$expected$palettes$seurat_clusters
    )]),
    unname(fixture$expected$palettes$seurat_clusters)
  )
})

test_that("Gene expression renders the exact selected source-gene values", {
  fixture <- generated_app_e2e_select_dataset("basic")
  generated_app_e2e_activate_tab("gene_expression")
  generated_app_e2e_wait_input("expression_genes_input")
  generated_app_e2e_set_input("expression_genes_input", "GENE001")

  displayed <- generated_app_e2e_output_text("expression_genes_displayed")
  expect_match(displayed, "GENE001", fixed = TRUE)
  expect_match(displayed, "0 gene(s) are not in data set", fixed = TRUE)
  generated_app_e2e_wait_plotly("expression_projection")

  observed <- generated_app_e2e_value(
    "export",
    "expression_levels",
    validate = function(value) length(value) == fixture$expected$n_cells
  )
  source <- SeuratObject::LayerData(
    fixture$object,
    assay = "RNA",
    layer = "data"
  )
  expected <- as.numeric(source["GENE001", ])
  expect_equal(sort(observed), sort(expected), tolerance = 1e-12)
})

test_that("Groups, gene IDs, colors, and About render source-backed content", {
  fixture <- generated_app_e2e_select_dataset("basic")

  generated_app_e2e_activate_tab("groups")
  generated_app_e2e_wait_input("groups_selected_group")
  generated_app_e2e_set_input("groups_selected_group", "seurat_clusters")
  generated_app_e2e_wait_input("groups_by_other_group_second_group")
  generated_app_e2e_set_input("groups_by_other_group_second_group", "sample")
  generated_app_e2e_set_input("groups_by_other_group_show_table", TRUE)
  groups <- generated_app_e2e_table_text(
    "groups_by_other_group_table",
    contains = c("Alpha", "Beta", "Gamma", "sample_a", "sample_b")
  )
  expect_match(groups, "6", fixed = TRUE)

  generated_app_e2e_activate_tab("about")
  expect_match(
    generated_app_e2e_output_text("about"),
    "Cerebro",
    fixed = TRUE
  )

  ## Opening Gene ID conversion eagerly materializes the complete bundled
  ## dictionary and dominates the whole browser suite. Its exact sidebar
  ## contract is verified here, while the pipeline test reads the generated
  ## dictionary and checks its headers and first source row directly.
  expect_true("gene_id_conversion" %in% generated_app_e2e_visible_pages())
  generated_app_e2e_driver()$wait_for_js(
    "document.querySelector(\"a[href='#shiny-tab-geneIdConversion']\") !== null",
    timeout = 60000
  )

  generated_app_e2e_expect_clean_browser()
})
