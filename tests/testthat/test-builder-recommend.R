builder_profile_source_runtime()

recommend_path <- builder_profile_inst_path("builder", "recommend.R")
if (nzchar(recommend_path) && file.exists(recommend_path)) {
  builder_repo_source("recommend.R", local = globalenv())
}

recommendation_profile <- function(n_cells = 100L, columns = list()) {
  structure(
    list(
      schema_version = 2L,
      identity = list(cells = list(count = n_cells)),
      metadata = list(columns = columns)
    ),
    class = c("builder_dataset_profile", "list")
  )
}

metadata_fact <- function(
  name,
  class = "character",
  non_missing = 100L,
  unique_non_missing = 2L,
  supported = TRUE
) {
  list(
    name = name,
    class = class,
    storage_type = switch(
      class,
      integer = "integer",
      double = "double",
      logical = "logical",
      list = "list",
      "character"
    ),
    count = non_missing,
    missing = 0L,
    blanks = 0L,
    unique = unique_non_missing,
    non_missing = non_missing,
    unique_non_missing = unique_non_missing,
    supported = supported
  )
}

test_that("the live recommendation contract is available", {
  expect_true(all(vapply(
    c(
      "builder_recommend_metadata",
      "builder_nomenclature_choices",
      "builder_validate_nomenclature"
    ),
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  )))
})

test_that("metadata profiles record exact missing and distinct facts", {
  fact <- .builder_profile_metadata_column(
    c(NA_real_, NaN, 1, 1, 2),
    "score"
  )

  expect_identical(fact$non_missing, 3L)
  expect_identical(fact$unique_non_missing, 2L)
  expect_identical(fact$missing, 2L)
})

test_that("reserved and required metadata fail closed", {
  profile <- recommendation_profile(
    columns = list(
      cell_barcode = metadata_fact("cell_barcode"),
      cell_type = metadata_fact("cell_type")
    )
  )
  recommendation <- builder_recommend_metadata(
    profile,
    required = "missing_qc",
    dependency_ids = list(missing_qc = "core.qc")
  )

  expect_identical(
    recommendation$columns$cell_barcode$disposition,
    "blocking"
  )
  expect_true("cell_type" %in% recommendation$included)
  expect_true("missing_qc" %in% recommendation$blocking)
  expect_identical(
    recommendation$columns$missing_qc$dependency_ids,
    "core.qc"
  )
})

test_that("malformed metadata facts fail closed", {
  expect_error(
    builder_recommend_metadata(list(schema_version = 1L)),
    "DatasetProfile v2"
  )
  expect_error(
    builder_recommend_metadata(recommendation_profile(
      columns = list(
        bad = list(
          name = "bad",
          class = "character",
          non_missing = NA_integer_,
          unique_non_missing = 2L,
          supported = TRUE
        )
      )
    )),
    "malformed"
  )
  expect_error(
    builder_recommend_metadata(recommendation_profile(), required = ""),
    "[Rr]equired"
  )
})

test_that("profile counts outside the integer contract fail clearly", {
  profile <- recommendation_profile(
    n_cells = as.double(.Machine$integer.max) + 1
  )
  expect_error(
    builder_recommend_metadata(profile),
    "malformed cell count"
  )
})

test_that("nomenclature validation stays species-specific", {
  expect_identical(
    builder_nomenclature_choices("hg"),
    c("name", "ensembl", "gencode_v27")
  )
  expect_identical(
    builder_nomenclature_choices("mm"),
    c("name", "ensembl", "gencode_vM16")
  )
  expect_identical(
    builder_validate_nomenclature("hg", "gencode_v27"),
    "gencode_v27"
  )
  expect_error(
    builder_validate_nomenclature("hg", "gencode_vM16"),
    "nomenclature",
    ignore.case = TRUE
  )
})
