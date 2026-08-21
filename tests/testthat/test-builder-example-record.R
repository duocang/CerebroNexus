builder_repo_source("io.R", local = globalenv())

test_that("Builder example records validate their public list contract", {
  valid <- list(
    id = "contract",
    label = "Contract example",
    detail = "A valid example record",
    provenance = "synthetic",
    serialized_path = "fixture.rds",
    make = function() list(object = NULL, format = "test"),
    expected_manifest = c("expression", "metadata"),
    expected_dispositions = c(
      expression = "preserved",
      metadata = "converted"
    ),
    expected_pages = c("marker_genes", "spatial"),
    expected_supporting_content = c("section_a_1_he.png", "extra_material"),
    histology_images = list(
      section_a_1_he = list(
        id = "section_a_1_he",
        label = "H&E",
        stain = "H&E",
        path = "section_a_1_he.png",
        section_id = "section_a_1",
        fov_ids = "section_a_1_fov_1"
      )
    ),
    gallery_visible = FALSE
  )
  record <- do.call(builder_example_record, valid)
  expect_identical(
    names(record$expected_dispositions),
    record$expected_manifest
  )
  expect_identical(record$gallery_visible, FALSE)
  expect_true(is.function(record$make))
  expect_identical(record$expected_pages, valid$expected_pages)
  expect_identical(
    record$expected_supporting_content,
    valid$expected_supporting_content
  )
  expect_identical(record$histology_images, valid$histology_images)

  for (field in c("id", "label", "detail", "provenance")) {
    invalid <- valid
    invalid[[field]] <- ""
    expect_error(
      do.call(builder_example_record, invalid),
      paste0("`", field, "` must be a non-empty string")
    )
  }
  invalid <- valid
  invalid$provenance <- "generated"
  expect_error(
    do.call(builder_example_record, invalid),
    "`provenance` must be one of: real, synthetic"
  )
  invalid <- valid
  invalid$make <- "not a function"
  expect_error(
    do.call(builder_example_record, invalid),
    "`make` must be a function"
  )
  invalid <- valid
  invalid$serialized_path <- c("one.rds", "two.rds")
  expect_error(
    do.call(builder_example_record, invalid),
    "`serialized_path` must be a single string"
  )
  invalid <- valid
  invalid$expected_dispositions <- unname(invalid$expected_dispositions)
  expect_error(
    do.call(builder_example_record, invalid),
    "named `expected_dispositions`"
  )
  invalid <- valid
  invalid$expected_manifest <- rev(invalid$expected_manifest)
  expect_error(
    do.call(builder_example_record, invalid),
    "must exactly match"
  )
  invalid <- valid
  invalid$expected_manifest <- c("expression", "expression")
  expect_error(
    do.call(builder_example_record, invalid),
    "`expected_manifest` must contain unique non-empty strings"
  )
  for (value in list(1L, list("expression"), new.env(), NA_character_, "")) {
    invalid <- valid
    invalid$expected_manifest <- value
    expect_error(
      do.call(builder_example_record, invalid),
      "`expected_manifest` must contain unique non-empty strings"
    )
  }
  invalid <- valid
  names(invalid$expected_dispositions) <- c("expression", "expression")
  expect_error(
    do.call(builder_example_record, invalid),
    "named `expected_dispositions` with unique non-empty names"
  )
  for (bad_name in list(c("expression", ""), c("expression", NA_character_))) {
    invalid <- valid
    names(invalid$expected_dispositions) <- bad_name
    expect_error(
      do.call(builder_example_record, invalid),
      "named `expected_dispositions` with unique non-empty names"
    )
  }
  for (value in list(
    c(expression = 1L, metadata = 2L),
    c(expression = "preserved", metadata = NA_character_),
    c(expression = "preserved", metadata = ""),
    c(expression = "preserved", metadata = "generated")
  )) {
    invalid <- valid
    invalid$expected_dispositions <- value
    expect_error(
      do.call(builder_example_record, invalid),
      "must use only: preserved, converted"
    )
  }
  invalid_vectors <- list(
    1L,
    list("marker_genes"),
    new.env(),
    NA_character_,
    "",
    c("marker_genes", "marker_genes"),
    stats::setNames("marker_genes", "named")
  )
  for (field in c("expected_pages", "expected_supporting_content")) {
    for (value in invalid_vectors) {
      invalid <- valid
      invalid[[field]] <- value
      expect_error(
        do.call(builder_example_record, invalid),
        paste0(
          "`",
          field,
          "` must contain unique non-empty strings without names"
        )
      )
    }
  }
  invalid <- valid
  invalid$gallery_visible <- NA
  expect_error(
    do.call(builder_example_record, invalid),
    "`gallery_visible` must be TRUE or FALSE"
  )
  invalid <- valid
  invalid$histology_images[[1L]]$fov_ids <- character()
  expect_error(
    do.call(builder_example_record, invalid),
    "`histology_images` entries must declare unique non-empty `fov_ids`"
  )
  invalid <- valid
  invalid$histology_images[[1L]]$id <- "other"
  expect_error(
    do.call(builder_example_record, invalid),
    "`histology_images` entry IDs must match their list names"
  )
})
