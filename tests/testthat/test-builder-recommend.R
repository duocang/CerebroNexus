builder_profile_source_runtime()
builder_repo_source("state.R", local = globalenv())
builder_repo_source("marker_import.R", local = globalenv())

recommend_path <- builder_profile_inst_path("builder", "recommend.R")
if (nzchar(recommend_path) && file.exists(recommend_path)) {
  builder_repo_source("recommend.R", local = globalenv())
}

recommend_api <- c(
  "builder_recommend_metadata",
  "builder_recommend_groups",
  "builder_recommend_projections",
  "builder_recommend_organism",
  "builder_nomenclature_choices",
  "builder_validate_nomenclature",
  "builder_recommend_nomenclature",
  "builder_gene_conversion_initial_table",
  "builder_recommend_backend",
  "builder_recommend_dataset"
)
recommend_api_available <- all(vapply(
  recommend_api,
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
))

test_that("the recommendation contract is available", {
  expect_true(recommend_api_available)
})

if (recommend_api_available) {
  recommendation_profile <- function(
    n_cells = 100L,
    columns = list(),
    reductions = list(),
    organism = list(
      code = "other",
      confidence = 0,
      reason = "No organism inferred."
    ),
    features = character()
  ) {
    cells <- paste0("cell", seq_len(n_cells))
    structure(
      list(
        schema_version = 2L,
        identity = list(
          cells = list(ids = cells, count = n_cells, valid = TRUE),
          features = list(
            ids = features,
            count = length(features),
            valid = TRUE
          )
        ),
        metadata = list(columns = columns),
        reductions = reductions,
        organism = organism
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

  reduction_fact <- function(name, exportable = TRUE, is_pca = FALSE) {
    list(name = name, exportable = exportable, is_pca = is_pca)
  }

  expect_recommendation_record <- function(record) {
    expect_true(is.list(record))
    expect_true(all(
      c(
        "value",
        "reason",
        "confidence",
        "requires_confirmation"
      ) %in%
        names(record)
    ))
    expect_false(any(c("mode", "default") %in% names(record)))
    expect_true(is.character(record$reason))
    expect_length(record$reason, 1L)
    expect_false(is.na(record$reason))
    expect_true(is.numeric(record$confidence))
    expect_length(record$confidence, 1L)
    expect_gte(record$confidence, 0)
    expect_lte(record$confidence, 1)
    expect_type(record$requires_confirmation, "logical")
    expect_length(record$requires_confirmation, 1L)
    expect_false(is.na(record$requires_confirmation))
  }

  test_that("metadata profiles record exact missing and distinct facts", {
    values <- c(NA_real_, NaN, 1, 1, 2)
    fact <- .builder_profile_metadata_column(values, "score")

    expect_identical(fact$non_missing, 3L)
    expect_identical(fact$unique_non_missing, 2L)
    expect_identical(fact$missing, 2L)
    expect_identical(fact$unique, length(unique(values)))
  })

  test_that("the reserved cell barcode name remains one blocking identity", {
    profile <- recommendation_profile(
      n_cells = 100L,
      columns = list(
        cell_barcode = metadata_fact(
          "cell_barcode",
          non_missing = 100L,
          unique_non_missing = 2L
        ),
        cell_type = metadata_fact(
          "cell_type",
          non_missing = 100L,
          unique_non_missing = 2L
        )
      )
    )
    recommendation <- builder_recommend_metadata(profile)

    expect_identical(sum(names(recommendation$columns) == "cell_barcode"), 1L)
    identity <- recommendation$columns$cell_barcode
    expect_identical(identity$disposition, "blocking")
    expect_true(identity$effective_included)
    expect_false(identity$preview_allowed)
    expect_true("cell_barcode" %in% recommendation$included)
    expect_true("cell_barcode" %in% recommendation$blocking)
    expect_match(identity$reason, "reserved", ignore.case = TRUE)
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
      builder_recommend_metadata(
        recommendation_profile(),
        required = ""
      ),
      "[Rr]equired"
    )
  })

  test_that("group recommendations only use eligible included metadata", {
    profile <- recommendation_profile(
      n_cells = 200L,
      columns = list(
        batch = metadata_fact("batch", unique_non_missing = 2L),
        annotation = metadata_fact("annotation", unique_non_missing = 2L),
        cell_type = metadata_fact("cell_type", unique_non_missing = 2L),
        donor_id = metadata_fact("donor_id", unique_non_missing = 2L),
        score = metadata_fact(
          "score",
          class = "double",
          unique_non_missing = 2L
        )
      )
    )
    metadata <- builder_recommend_metadata(profile, required = "donor_id")
    groups <- builder_recommend_groups(profile, metadata)

    expect_recommendation_record(groups)
    expect_identical(groups$value, "cell_type")
    expect_true(groups$value %in% groups$included)
    expect_identical(groups$included, c("batch", "annotation", "cell_type"))
    expect_false(any(
      c("cell_barcode", "donor_id", "score") %in% groups$included
    ))

    none <- builder_recommend_groups(
      recommendation_profile(n_cells = 20L, columns = list()),
      builder_recommend_metadata(recommendation_profile(
        n_cells = 20L,
        columns = list()
      ))
    )
    expect_null(none$value)
    expect_true(none$requires_confirmation)
  })

  test_that("projection recommendations prefer non-PCA viewer choices", {
    mixed <- recommendation_profile(
      reductions = list(
        pca = reduction_fact("pca", is_pca = TRUE),
        harmony = reduction_fact("harmony"),
        tsne = reduction_fact("tsne"),
        umap = reduction_fact("umap"),
        broken = reduction_fact("broken", exportable = FALSE)
      )
    )
    recommendation <- builder_recommend_projections(mixed)

    expect_recommendation_record(recommendation)
    expect_identical(recommendation$included, c("harmony", "tsne", "umap"))
    expect_identical(recommendation$value, "umap")
    expect_true(recommendation$value %in% recommendation$included)

    single_pca <- builder_recommend_projections(recommendation_profile(
      reductions = list(pca = reduction_fact("pca", is_pca = TRUE))
    ))
    expect_identical(single_pca$included, "pca")
    expect_identical(single_pca$value, "pca")
    expect_true(single_pca$requires_confirmation)
    expect_match(single_pca$reason, "fallback", ignore.case = TRUE)

    multiple_pca <- builder_recommend_projections(recommendation_profile(
      reductions = list(
        pca = reduction_fact("pca", is_pca = TRUE),
        pca_harmony = reduction_fact("pca_harmony", is_pca = TRUE)
      )
    ))
    expect_identical(multiple_pca$included, c("pca", "pca_harmony"))
    expect_null(multiple_pca$value)
    expect_true(multiple_pca$requires_confirmation)

    invalid <- builder_recommend_projections(recommendation_profile(
      reductions = list(bad = reduction_fact("bad", exportable = FALSE))
    ))
    expect_identical(invalid$included, character())
    expect_null(invalid$value)
  })

  test_that("organism and nomenclature recommendations stay independent", {
    human <- recommendation_profile(
      organism = list(
        code = "hg",
        confidence = 0.92,
        reason = "Human-style symbols."
      ),
      features = c("ENSG00000141510.18", "ENSG00000155657")
    )
    organism <- builder_recommend_organism(human)
    nomenclature <- builder_recommend_nomenclature(human, organism)

    expect_recommendation_record(organism)
    expect_identical(organism$value, "hg")
    expect_true(organism$requires_confirmation)
    expect_recommendation_record(nomenclature)
    expect_identical(nomenclature$value, "ensembl")
    expect_false(grepl("gencode", nomenclature$value, fixed = TRUE))
    expect_identical(
      builder_nomenclature_choices("hg"),
      c("name", "ensembl", "gencode_v27")
    )
    expect_identical(
      builder_nomenclature_choices("mm"),
      c("name", "ensembl", "gencode_vM16")
    )
    expect_identical(builder_nomenclature_choices("other"), character())
    expect_identical(
      builder_validate_nomenclature("hg", "gencode_v27"),
      "gencode_v27"
    )
    expect_error(
      builder_validate_nomenclature("hg", "gencode_vM16"),
      "nomenclature",
      ignore.case = TRUE
    )
    expect_error(
      builder_validate_nomenclature("other", "name"),
      "nomenclature",
      ignore.case = TRUE
    )

    mouse <- recommendation_profile(
      features = c("ENSMUSG00000059552.3", "ENSMUSG00000064341")
    )
    expect_identical(
      builder_recommend_nomenclature(mouse, "mm")$value,
      "ensembl"
    )
    symbols <- recommendation_profile(features = c("TP53", "MS4A1", "CD3D"))
    expect_identical(
      builder_recommend_nomenclature(symbols, "hg")$value,
      "name"
    )
    mixed <- recommendation_profile(features = c("TP53", "ENSG00000141510"))
    mixed_recommendation <- builder_recommend_nomenclature(mixed, "hg")
    expect_null(mixed_recommendation$value)
    expect_true(mixed_recommendation$requires_confirmation)
    empty <- builder_recommend_nomenclature(recommendation_profile(), "hg")
    expect_null(empty$value)
    expect_true(empty$requires_confirmation)

    expect_null(builder_gene_conversion_initial_table("hg", confirmed = FALSE))
    expect_identical(
      builder_gene_conversion_initial_table("hg", confirmed = TRUE),
      "human"
    )
    expect_identical(
      builder_gene_conversion_initial_table("mm", confirmed = TRUE),
      "mouse"
    )
    expect_null(
      builder_gene_conversion_initial_table("other", confirmed = TRUE)
    )
  })

  test_that("malformed organism facts keep nomenclature fail closed", {
    for (organism in list(
      NULL,
      list(code = c("hg", "mm"), confidence = 1, reason = "ambiguous")
    )) {
      profile <- recommendation_profile(
        organism = organism,
        features = c("TP53", "MS4A1")
      )
      recommendation <- builder_recommend_dataset(
        profile,
        matrix_summary = list(estimated_bytes = 1024, sparse = TRUE),
        available = list(
          build = list(bpcells = FALSE, h5 = FALSE),
          viewer = list(bpcells = FALSE, h5 = FALSE)
        )
      )
      expect_recommendation_record(recommendation$nomenclature)
      expect_null(recommendation$nomenclature$value)
      expect_true(recommendation$nomenclature$requires_confirmation)
    }
  })

  test_that("gene conversion rejects missing and non-scalar organisms", {
    expect_null(builder_gene_conversion_initial_table(NULL, confirmed = TRUE))
    expect_null(builder_gene_conversion_initial_table(
      c("hg", "mm"),
      confirmed = TRUE
    ))
  })

  test_that("profile counts outside the integer contract fail clearly", {
    too_large <- as.double(.Machine$integer.max) + 1
    oversized_cells <- recommendation_profile()
    oversized_cells$identity$cells$count <- too_large
    expect_error(
      builder_recommend_metadata(oversized_cells),
      "malformed cell count"
    )

    oversized_metadata <- recommendation_profile(
      n_cells = 100L,
      columns = list(
        big = metadata_fact(
          "big",
          non_missing = 100L,
          unique_non_missing = 2L
        )
      )
    )
    oversized_metadata$metadata$columns$big$non_missing <- too_large
    expect_error(
      builder_recommend_metadata(oversized_metadata),
      "malformed metadata fact"
    )
  })

  test_that("backend recommendations require complete build and viewer support", {
    mib <- 1024^2
    all_available <- list(
      build = list(bpcells = TRUE, h5 = TRUE),
      viewer = list(bpcells = TRUE, h5 = TRUE)
    )
    embedded <- builder_recommend_backend(
      list(estimated_bytes = 256 * mib, sparse = FALSE),
      all_available
    )
    expect_recommendation_record(embedded)
    expect_identical(embedded$value, "embedded")

    sparse <- builder_recommend_backend(
      list(estimated_bytes = 256 * mib + 1, sparse = TRUE),
      all_available
    )
    expect_identical(sparse$value, "bpcells")

    h5_fallback <- builder_recommend_backend(
      list(estimated_bytes = 300 * mib, sparse = TRUE),
      list(
        build = list(bpcells = TRUE, h5 = TRUE),
        viewer = list(bpcells = FALSE, h5 = TRUE)
      )
    )
    expect_identical(h5_fallback$value, "h5")

    dense_shorthand <- builder_recommend_backend(
      list(estimated_bytes = 300 * mib, sparse = FALSE),
      c(bpcells = TRUE, h5 = TRUE)
    )
    expect_null(dense_shorthand$value)
    expect_true(length(dense_shorthand$blocking) > 0L)

    blocked <- builder_recommend_backend(
      list(estimated_bytes = 300 * mib, sparse = TRUE),
      list(
        build = list(bpcells = TRUE, h5 = FALSE),
        viewer = list(bpcells = FALSE, h5 = TRUE)
      )
    )
    expect_null(blocked$value)
    expect_true(blocked$requires_confirmation)
    expect_true(length(blocked$blocking) > 0L)
    expect_true(length(blocked$dependency_actions) > 0L)
  })

  test_that("backend support requires every explicit build and viewer pair", {
    mib <- 1024^2
    combinations <- expand.grid(
      build = c(FALSE, TRUE),
      viewer = c(FALSE, TRUE),
      KEEP.OUT.ATTRS = FALSE
    )
    for (backend in c("bpcells", "h5")) {
      for (index in seq_len(nrow(combinations))) {
        build <- combinations$build[[index]]
        viewer <- combinations$viewer[[index]]
        available <- list(
          build = list(bpcells = FALSE, h5 = FALSE),
          viewer = list(bpcells = FALSE, h5 = FALSE)
        )
        available$build[[backend]] <- build
        available$viewer[[backend]] <- viewer
        recommendation <- builder_recommend_backend(
          list(
            estimated_bytes = 300 * mib,
            sparse = identical(backend, "bpcells")
          ),
          available
        )
        if (build && viewer) {
          expect_identical(recommendation$value, backend)
        } else {
          expect_null(recommendation$value)
          expect_true(length(recommendation$blocking) > 0L)
        }
      }
    }
  })

  test_that("backend facts fail closed without touching matrix payloads", {
    accessed <- character()
    assign(
      "$.builder_matrix_summary_sentinel",
      function(value, name) {
        accessed <<- c(accessed, name)
        if (identical(name, "matrix")) {
          stop("matrix payload was touched")
        }
        unclass(value)[[name]]
      },
      envir = .GlobalEnv
    )
    on.exit(
      rm("$.builder_matrix_summary_sentinel", envir = .GlobalEnv),
      add = TRUE
    )
    summary <- structure(
      list(
        estimated_bytes = 1024,
        sparse = TRUE,
        matrix = "must remain opaque"
      ),
      class = c("builder_matrix_summary_sentinel", "list")
    )
    expect_identical(
      builder_recommend_backend(
        summary,
        list(
          build = list(bpcells = FALSE, h5 = FALSE),
          viewer = list(bpcells = FALSE, h5 = FALSE)
        )
      )$value,
      "embedded"
    )
    expect_false("matrix" %in% accessed)
    expect_true(all(c("estimated_bytes", "sparse") %in% accessed))

    invalid <- list(
      list(estimated_bytes = NA_real_, sparse = TRUE),
      list(estimated_bytes = Inf, sparse = TRUE),
      list(estimated_bytes = -1, sparse = TRUE),
      list(estimated_bytes = c(1, 2), sparse = TRUE),
      list(estimated_bytes = 1, sparse = NA),
      list(estimated_bytes = 1, sparse = c(TRUE, FALSE))
    )
    for (fact in invalid) {
      recommendation <- builder_recommend_backend(
        fact,
        list(
          build = list(bpcells = TRUE, h5 = TRUE),
          viewer = list(bpcells = TRUE, h5 = TRUE)
        )
      )
      expect_null(recommendation$value)
      expect_true(recommendation$requires_confirmation)
      expect_true(length(recommendation$blocking) > 0L)
    }
  })

  test_that("dataset recommendations aggregate the same records", {
    profile <- recommendation_profile(
      n_cells = 100L,
      columns = list(
        cell_type = metadata_fact(
          "cell_type",
          non_missing = 100L,
          unique_non_missing = 2L
        )
      ),
      reductions = list(umap = reduction_fact("umap")),
      organism = list(
        code = "hg",
        confidence = 0.9,
        reason = "Human-style symbols."
      ),
      features = c("TP53", "MS4A1")
    )
    recommendation <- builder_recommend_dataset(
      profile,
      matrix_summary = list(estimated_bytes = 1024, sparse = TRUE),
      available = list(
        build = list(bpcells = FALSE, h5 = FALSE),
        viewer = list(bpcells = FALSE, h5 = FALSE)
      )
    )

    expect_named(
      recommendation,
      c(
        "metadata",
        "groups",
        "projections",
        "organism",
        "nomenclature",
        "backend"
      )
    )
    lapply(recommendation, expect_recommendation_record)
  })
}
