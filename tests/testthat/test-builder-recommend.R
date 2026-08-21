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

  test_that("metadata recommendations use one deterministic value contract", {
    columns <- list(
      constant = metadata_fact("constant", unique_non_missing = 1L),
      cell_type = metadata_fact("cell_type", unique_non_missing = 2L),
      exactly_five_percent = metadata_fact(
        "exactly_five_percent",
        unique_non_missing = 2L
      ),
      over_five_percent = metadata_fact(
        "over_five_percent",
        unique_non_missing = 3L
      ),
      seurat_clusters = metadata_fact(
        "seurat_clusters",
        class = "integer",
        unique_non_missing = 2L
      ),
      continuous = metadata_fact(
        "continuous",
        class = "double",
        unique_non_missing = 2L
      ),
      unique_cell = metadata_fact(
        "unique_cell",
        unique_non_missing = 40L
      ),
      unsupported = metadata_fact(
        "unsupported",
        class = "list",
        supported = FALSE
      ),
      patientName = metadata_fact(
        "patientName",
        unique_non_missing = 2L
      ),
      donor_id = metadata_fact("donor_id", unique_non_missing = 2L)
    )
    columns <- lapply(columns, function(column) {
      column$count <- 40L
      column$non_missing <- 40L
      column
    })
    profile <- recommendation_profile(n_cells = 40L, columns = columns)
    before <- serialize(profile, NULL)
    first <- builder_recommend_metadata(profile)
    second <- builder_recommend_metadata(profile)

    expect_identical(first, second)
    expect_identical(serialize(profile, NULL), before)
    expect_recommendation_record(first)
    expect_named(first$columns, c("cell_barcode", names(columns)))
    lapply(first$columns, expect_recommendation_record)
    expect_identical(first$columns$cell_barcode$value, "included")
    expect_identical(first$columns$cell_barcode$disposition, "included")
    expect_false(first$columns$cell_barcode$preview_allowed)
    expect_identical(
      first$columns$cell_barcode$dependency_ids,
      "core.cell_identity"
    )
    expect_true(all(
      c(
        "name",
        "class",
        "non_missing",
        "unique_non_missing",
        "reason",
        "disposition",
        "dependency_ids",
        "preview_allowed"
      ) %in%
        names(first$columns$cell_type)
    ))
    expect_true("cell_type" %in% first$included)
    expect_true("exactly_five_percent" %in% first$included)
    expect_true("over_five_percent" %in% first$included)
    expect_identical(
      first$columns$over_five_percent$disposition,
      "attention"
    )
    expect_true(first$columns$over_five_percent$requires_confirmation)
    expect_false(first$columns$over_five_percent$effective_included)
    expect_true("over_five_percent" %in% first$attention)
    expect_true("constant" %in% first$included)
    expect_true("unique_cell" %in% first$included)
    expect_true("continuous" %in% first$included)
    expect_identical(first$columns$continuous$disposition, "attention")
    expect_true(first$columns$continuous$requires_confirmation)
    expect_false(first$columns$continuous$effective_included)
    expect_identical(first$columns$seurat_clusters$disposition, "included")
    expect_false(first$columns$seurat_clusters$requires_confirmation)
    expect_true(first$columns$seurat_clusters$group_eligible)
    expect_true("seurat_clusters" %in% first$included)
    expect_identical(first$columns$unsupported$disposition, "excluded")
    expect_identical(first$columns$donor_id$disposition, "attention")
    expect_false("patientName" %in% first$included)
    expect_false("donor_id" %in% first$included)
    expect_true(first$columns$constant$retain_in_crb)
    expect_false(first$columns$constant$group_eligible)
    expect_true(first$columns$continuous$retain_in_crb)
    expect_false(first$columns$continuous$group_eligible)
    expect_true(first$columns$unique_cell$retain_in_crb)
    expect_false(first$columns$unique_cell$group_eligible)
    expect_false(first$columns$unsupported$retain_in_crb)
    expect_false(first$columns$patientName$retain_in_crb)
    expect_true(first$columns$patientName$requires_confirmation)
    expect_contains(
      first$retained,
      c("constant", "continuous", "unique_cell")
    )
    expect_false("unsupported" %in% first$retained)
  })

  test_that("constant orig.ident is retained but not recommended as a Group", {
    profile <- recommendation_profile(
      n_cells = 100L,
      columns = list(
        orig.ident = metadata_fact(
          "orig.ident",
          class = "factor",
          non_missing = 100L,
          unique_non_missing = 1L
        )
      )
    )
    policy <- builder_recommend_metadata(profile)

    expect_true(policy$columns$orig.ident$retain_in_crb)
    expect_false(policy$columns$orig.ident$group_eligible)
    expect_contains(policy$retained, "orig.ident")
    expect_false("orig.ident" %in% policy$group_candidates)
  })

  test_that("metadata cardinality boundaries are strict for every sample size", {
    boundary <- recommendation_profile(
      n_cells = 1000L,
      columns = list(
        one = metadata_fact(
          "one",
          non_missing = 1000L,
          unique_non_missing = 1L
        ),
        two = metadata_fact(
          "two",
          non_missing = 1000L,
          unique_non_missing = 2L
        ),
        fifty = metadata_fact(
          "fifty",
          non_missing = 1000L,
          unique_non_missing = 50L
        ),
        fifty_one = metadata_fact(
          "fifty_one",
          non_missing = 1000L,
          unique_non_missing = 51L
        )
      )
    )
    recommendation <- builder_recommend_metadata(boundary)

    expect_false("one" %in% recommendation$group_candidates)
    expect_true("two" %in% recommendation$group_candidates)
    expect_true("fifty" %in% recommendation$group_candidates)
    expect_false("fifty_one" %in% recommendation$group_candidates)
    expect_identical(
      recommendation$columns$fifty_one$disposition,
      "attention"
    )
    expect_true(recommendation$columns$fifty_one$requires_confirmation)
    expect_false(recommendation$columns$fifty_one$effective_included)
    expect_true("fifty_one" %in% recommendation$attention)

    small <- builder_recommend_metadata(recommendation_profile(
      n_cells = 20L,
      columns = list(
        two = metadata_fact(
          "two",
          non_missing = 20L,
          unique_non_missing = 2L
        )
      )
    ))
    expect_false("two" %in% small$group_candidates)
  })

  test_that("required and sensitive metadata fail closed", {
    profile <- recommendation_profile(
      n_cells = 100L,
      columns = list(
        donor_id = metadata_fact("donor_id", unique_non_missing = 80L),
        patientName = metadata_fact(
          "patientName",
          unique_non_missing = 80L
        ),
        sample.id = metadata_fact("sample.id", unique_non_missing = 2L),
        email_address = metadata_fact(
          "email_address",
          unique_non_missing = 2L
        ),
        sampleidentifier = metadata_fact(
          "sampleidentifier",
          unique_non_missing = 2L
        ),
        seurat_clusters = metadata_fact(
          "seurat_clusters",
          class = "integer",
          unique_non_missing = 2L
        ),
        list_col = metadata_fact(
          "list_col",
          class = "list",
          supported = FALSE
        ),
        sample_group = metadata_fact("sample_group", unique_non_missing = 2L)
      )
    )
    recommendation <- builder_recommend_metadata(
      profile,
      required = c(
        "missing_required",
        "list_col",
        "donor_id",
        "seurat_clusters"
      ),
      dependency_ids = list(
        donor_id = c("z.last", "a.first", "z.last"),
        list_col = "table.unsupported"
      )
    )

    expect_identical(recommendation$columns$donor_id$disposition, "attention")
    expect_true(recommendation$columns$donor_id$requires_confirmation)
    expect_true("donor_id" %in% recommendation$included)
    expect_identical(
      recommendation$columns$donor_id$dependency_ids,
      c("a.first", "z.last")
    )
    expect_true(all(
      c(
        "donor_id",
        "patientName",
        "sample.id",
        "email_address",
        "sampleidentifier"
      ) %in%
        recommendation$attention
    ))
    expect_true("sample_group" %in% recommendation$included)
    expect_identical(
      recommendation$columns$seurat_clusters$disposition,
      "included"
    )
    expect_false(
      recommendation$columns$seurat_clusters$requires_confirmation
    )
    expect_true(recommendation$columns$seurat_clusters$preview_allowed)
    expect_true(recommendation$columns$seurat_clusters$group_eligible)
    expect_true("seurat_clusters" %in% recommendation$included)
    expect_identical(recommendation$columns$list_col$disposition, "blocking")
    expect_identical(
      recommendation$columns$missing_required$disposition,
      "blocking"
    )
    expect_setequal(
      recommendation$blocking,
      c("list_col", "missing_required")
    )
    expect_false(any(recommendation$blocking %in% recommendation$included))
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

  test_that("real Seurat facts flow through recommendations and planning", {
    skip_if_not_installed("SeuratObject")
    builder_profile_source_runtime(local = globalenv())
    builder_repo_source("inspect.R", local = globalenv())
    builder_repo_source("prerequisite.R", local = globalenv())
    builder_repo_source("preview.R", local = globalenv())
    builder_repo_source("plan.R", local = globalenv())

    object <- builder_profile_pbmc()
    profile <- builder_dataset_profile(
      object,
      builder_profile_source_fixture()
    )
    recommendations <- builder_recommend_dataset(
      profile,
      matrix_summary = list(estimated_bytes = 1024, sparse = TRUE),
      available = list(
        build = list(bpcells = FALSE, h5 = FALSE),
        viewer = list(bpcells = FALSE, h5 = FALSE)
      )
    )
    legacy <- describe_seurat(object)
    settings <- builder_default_settings(
      legacy,
      "pbmc_small",
      recommendations = recommendations
    )
    levels <- builder_group_levels_for(object, settings$groups)
    plan <- builder_make_plan(
      list(list(
        id = "pbmc-small",
        profile = legacy,
        levels = levels,
        settings = settings
      )),
      tempdir()
    )

    expect_s3_class(profile, "builder_dataset_profile")
    expect_true(settings$default_group %in% settings$groups)
    expect_true(settings$default_projection %in% settings$reductions)
    expect_null(plan$error)
    expect_identical(plan$items[[1L]]$recommendations, recommendations)
  })

  test_that("plan settings carry validated recommendation decisions", {
    builder_repo_source("prerequisite.R")
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    legacy <- list(
      default_assay = "RNA",
      default_layer = "data",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      assay_profiles = list(
        RNA = list(
          default_layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA"
        )
      ),
      organism_guess = "hg",
      group_preselect = "cell_type",
      reduction_preselect = "umap"
    )
    recommendations <- list(
      organism = list(
        value = "hg",
        reason = "Human-style symbols.",
        confidence = 0.9,
        requires_confirmation = TRUE
      ),
      groups = list(
        value = "cell_type",
        included = c("cell_type", "batch"),
        reason = "Safe groups.",
        confidence = 1,
        requires_confirmation = FALSE
      ),
      projections = list(
        value = "umap",
        included = c("umap", "tsne"),
        reason = "Viewer projections.",
        confidence = 1,
        requires_confirmation = FALSE
      ),
      metadata = list(
        value = c("cell_barcode", "cell_type"),
        reason = "Safe metadata.",
        confidence = 1,
        requires_confirmation = FALSE,
        included = c("cell_barcode", "cell_type")
      ),
      nomenclature = list(
        value = "name",
        reason = "Symbols.",
        confidence = 1,
        requires_confirmation = FALSE
      ),
      backend = list(
        value = "embedded",
        reason = "Small matrix.",
        confidence = 1,
        requires_confirmation = FALSE
      )
    )
    settings <- builder_default_settings(
      legacy,
      "PBMC",
      recommendations = recommendations
    )

    expect_identical(settings$organism, "hg")
    expect_identical(settings$groups, c("cell_type", "batch"))
    expect_identical(settings$reductions, c("umap", "tsne"))
    expect_identical(settings$default_group, "cell_type")
    expect_identical(settings$default_projection, "umap")
    expect_identical(settings$metadata_policy, recommendations$metadata)
    expect_identical(settings$nomenclature, "name")
    expect_identical(settings$expression_backend, "embedded")
    expect_identical(settings$recommendations, recommendations)

    entry <- list(
      id = "ds1",
      profile = c(legacy, list(extras = list())),
      levels = list(cell_type = c("A", "B"), batch = c("one", "two")),
      settings = settings
    )
    plan <- builder_make_plan(list(entry), tempdir())
    expect_null(plan$error)
    item <- plan$items[[1L]]
    expect_identical(item$default_group, "cell_type")
    expect_identical(item$default_projection, "umap")
    expect_identical(item$metadata_policy, recommendations$metadata)
    expect_identical(item$nomenclature, "name")
    expect_identical(item$expression_backend, "embedded")
    expect_identical(item$recommendations, recommendations)

    outside_group <- entry
    outside_group$settings$default_group <- "missing"
    expect_match(
      builder_make_plan(list(outside_group), tempdir())$error,
      "default group",
      ignore.case = TRUE
    )
    unselected_group <- entry
    unselected_group$settings$groups <- "batch"
    expect_match(
      builder_make_plan(list(unselected_group), tempdir())$error,
      "default group",
      ignore.case = TRUE
    )
    outside_projection <- entry
    outside_projection$settings$default_projection <- "missing"
    expect_match(
      builder_make_plan(list(outside_projection), tempdir())$error,
      "default projection",
      ignore.case = TRUE
    )
    unselected_projection <- entry
    unselected_projection$settings$reductions <- "tsne"
    expect_match(
      builder_make_plan(list(unselected_projection), tempdir())$error,
      "default projection",
      ignore.case = TRUE
    )

    invalid_nomenclature <- entry
    invalid_nomenclature$settings$organism <- "hg"
    invalid_nomenclature$settings$nomenclature <- "gencode_vM16"
    expect_match(
      builder_make_plan(list(invalid_nomenclature), tempdir())$error,
      "nomenclature",
      ignore.case = TRUE
    )

    pending_other <- entry
    pending_other$settings$organism <- "other"
    pending_other$settings$nomenclature <- NULL
    expect_null(builder_make_plan(list(pending_other), tempdir())$error)
  })
}
