.builder_eval_repo_root <- function() {
  source_files <- unlist(lapply(
    sys.frames(),
    function(frame) get0("ofile", envir = frame, inherits = FALSE)
  ))
  source_files <- source_files[
    is.character(source_files) & grepl("fixtures[.]R$", source_files)
  ]
  candidates <- c(
    if (length(source_files)) {
      file.path(dirname(tail(source_files, 1L)), "..", "..")
    },
    if ("testthat" %in% loadedNamespaces()) {
      testthat::test_path("..", "..")
    },
    getwd()
  )
  candidates <- normalizePath(candidates, mustWork = FALSE)
  matches <- candidates[file.exists(file.path(candidates, "DESCRIPTION"))]
  if (!length(matches)) {
    stop("The Builder evaluation repository root is unavailable.")
  }
  matches[[1L]]
}

.builder_eval_root <- .builder_eval_repo_root()
for (.builder_eval_helper in c(
  "helper-builder-00-paths.R",
  "helper-builder-profile-fixtures.R",
  "helper-builder-synthetic-fixtures.R",
  "helper-builder-content-tables-fixtures.R",
  "helper-builder-content-spatial-fixtures.R",
  "helper-builder-content-immune-fixtures.R"
)) {
  sys.source(
    file.path(.builder_eval_root, "tests", "testthat", .builder_eval_helper),
    envir = environment()
  )
}
rm(.builder_eval_helper, .builder_eval_root)

builder_eval_capability_families <- function() {
  c(
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectory",
    "extra_material",
    "immune_repertoire",
    "hla",
    "spatial",
    "trekker"
  )
}

.builder_eval_truth <- function(...) {
  truth <- stats::setNames(
    rep(FALSE, length(builder_eval_capability_families())),
    builder_eval_capability_families()
  )
  enabled <- unlist(list(...), use.names = FALSE)
  if (length(enabled)) {
    truth[enabled] <- TRUE
  }
  truth
}

.builder_eval_multi_assay <- function() {
  object <- .builder_fixture_object(12L)
  counts <- SeuratObject::LayerData(object[["RNA"]], layer = "counts")
  object[["ADT"]] <- SeuratObject::CreateAssay5Object(counts = counts[1:5, ])
  object
}

.builder_eval_trajectory <- function() {
  object <- .builder_fixture_object(12L)
  trajectory <- builder_table_trajectory(colnames(object)[seq_len(2L)])
  object@misc$trajectories <- list(monocle2 = list(lineage = trajectory))
  object
}

.builder_eval_table_content <- function(variant = 1L) {
  object <- .builder_fixture_object(12L)
  genes <- rownames(object)[seq_len(2L)]
  groups <- levels(object$cell_type)[seq_len(2L)]
  marker <- data.frame(
    cell_type = groups,
    gene = genes,
    avg_log2FC = c(2.1, 1.7),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  most <- data.frame(
    cell_type = groups,
    gene = rev(genes),
    pct = c(90, 85) - variant,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  mean <- data.frame(
    cell_type = groups,
    gene = genes,
    mean_expr = c(2.5, 1.8) + variant / 10,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  enrichment <- data.frame(
    cell_type = groups,
    Term = c("Neural signaling", "Immune signaling"),
    Combined.Score = c(8.2, 7.5) + variant / 10,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  method <- paste0("method_", variant)
  object@misc$marker_genes <- stats::setNames(
    list(list(cell_type = marker)),
    method
  )
  object@misc$most_expressed_genes <- list(cell_type = most)
  object@misc$mean_expression <- list(cell_type = mean)
  object@misc$enriched_pathways <- stats::setNames(
    list(list(cell_type = enrichment)),
    method
  )
  object@misc$trajectories <- list(
    monocle2 = list(
      lineage = builder_table_trajectory(colnames(object)[seq_len(2L)])
    )
  )
  object@misc$extra_material <- list(
    tables = stats::setNames(
      list(data.frame(
        metric = c("cells", "features"),
        value = c(ncol(object), nrow(object)),
        stringsAsFactors = FALSE
      )),
      paste0("summary_", variant)
    )
  )
  object
}

.builder_eval_spatial_trekker <- function(variant = 1L) {
  object <- .builder_fixture_spatial()
  payload <- .builder_content_spatial_demo_payload()
  replacement <- colnames(object)[seq_along(payload$barcodes)]
  payload$barcodes <- replacement
  payload$evidence <- lapply(payload$evidence, function(entry) {
    entry$bc <- replacement[[entry$cell + 1L]]
    entry
  })
  for (index in seq_along(payload$moran)) {
    payload$moran[[index]]$gene <- rownames(object)[[index]]
  }
  object@misc$trekker <- payload
  if (identical(variant, 2L)) {
    names(object@images) <- paste0("replicate_", seq_along(object@images))
  }
  object
}

.builder_eval_invalid_trajectory <- function() {
  object <- .builder_fixture_object(12L)
  trajectory <- builder_table_trajectory(c(colnames(object)[[1L]], "ghost"))
  trajectory$meta$DR_1[[1L]] <- NaN
  object@misc$trajectories <- list(monocle2 = list(lineage = trajectory))
  object
}

.builder_eval_invalid_extra_material <- function() {
  object <- .builder_fixture_object(12L)
  tables <- list(data.frame(value = 1), data.frame(value = 2))
  names(tables) <- c("summary", "summary")
  object@misc$extra_material <- list(tables = tables)
  object
}

.builder_eval_invalid_spatial <- function() {
  object <- .builder_fixture_spatial()
  methods::slot(object@images[[1L]], "assay") <- "missing"
  object
}

.builder_eval_invalid_trekker <- function() {
  object <- .builder_eval_spatial_trekker()
  object@misc$trekker$clusters <- as.character(object@misc$trekker$clusters)
  object
}

.builder_eval_invalid_immune <- function() {
  object <- .builder_fixture_immune("tcr_hla")
  object@misc$immune_repertoire[[1L]]$CTgene <- NULL
  object
}

.builder_eval_invalid_hla <- function() {
  object <- .builder_fixture_immune("tcr_hla")
  object@misc$hla_typing <- data.frame(
    sample = "donor1",
    allele = "banana",
    stringsAsFactors = FALSE
  )
  object@misc$hla_typing_source_type <- "genotyped"
  object
}

.builder_eval_malformed <- function() {
  object <- .builder_fixture_object(12L)
  object@misc$marker_genes <- list(malformed = "not a marker table")
  object
}

.builder_eval_seeded <- function(seed, make) {
  force(seed)
  force(make)
  function() {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit(
      {
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (
          exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        ) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )
    set.seed(seed)
    .builder_fixture_stabilize(make())
  }
}

builder_eval_fixtures <- function() {
  fixture <- function(id, make, truth = .builder_eval_truth(), seed) {
    list(
      id = id,
      make = .builder_eval_seeded(seed, make),
      truth = truth
    )
  }
  list(
    minimal = fixture(
      "minimal",
      function() .builder_fixture_object(12L),
      seed = 101L
    ),
    multi_assay = fixture(
      "multi_assay",
      .builder_eval_multi_assay,
      seed = 102L
    ),
    spatial = fixture(
      "spatial",
      function() .builder_eval_spatial_trekker(1L),
      .builder_eval_truth("spatial", "trekker"),
      seed = 103L
    ),
    spatial_b = fixture(
      "spatial_b",
      function() .builder_eval_spatial_trekker(2L),
      .builder_eval_truth("spatial", "trekker"),
      seed = 104L
    ),
    immune_hla = fixture(
      "immune_hla",
      function() .builder_fixture_immune("tcr_hla"),
      .builder_eval_truth("immune_repertoire", "hla"),
      seed = 105L
    ),
    immune_hla_b = fixture(
      "immune_hla_b",
      function() .builder_fixture_immune("tcr_hla"),
      .builder_eval_truth("immune_repertoire", "hla"),
      seed = 106L
    ),
    trajectory = fixture(
      "trajectory",
      .builder_eval_trajectory,
      .builder_eval_truth("trajectory"),
      seed = 107L
    ),
    extra_content = fixture(
      "extra_content",
      function() .builder_eval_table_content(1L),
      .builder_eval_truth(
        "marker_genes",
        "most_expressed_genes",
        "mean_expression",
        "enriched_pathways",
        "trajectory",
        "extra_material"
      ),
      seed = 108L
    ),
    table_content_b = fixture(
      "table_content_b",
      function() .builder_eval_table_content(2L),
      .builder_eval_truth(
        "marker_genes",
        "most_expressed_genes",
        "mean_expression",
        "enriched_pathways",
        "trajectory",
        "extra_material"
      ),
      seed = 109L
    ),
    malformed = fixture(
      "malformed",
      .builder_eval_malformed,
      seed = 110L
    )
  )
}

builder_eval_fixture_support <- function(fixtures) {
  families <- builder_eval_capability_families()
  truth <- do.call(rbind, lapply(fixtures, `[[`, "truth"))
  data.frame(
    capability = families,
    positive_support = as.integer(colSums(truth)),
    negative_support = as.integer(colSums(!truth)),
    stringsAsFactors = FALSE
  )
}

builder_eval_challenges <- function() {
  challenge <- function(id, capability, make, seed) {
    list(
      id = id,
      capability = capability,
      expected_state = "invalid",
      make = .builder_eval_seeded(seed, make)
    )
  }
  list(
    invalid_marker = challenge(
      "invalid_marker",
      "marker_genes",
      .builder_eval_malformed,
      201L
    ),
    invalid_trajectory = challenge(
      "invalid_trajectory",
      "trajectory",
      .builder_eval_invalid_trajectory,
      202L
    ),
    invalid_extra_material = challenge(
      "invalid_extra_material",
      "extra_material",
      .builder_eval_invalid_extra_material,
      203L
    ),
    invalid_spatial = challenge(
      "invalid_spatial",
      "spatial",
      .builder_eval_invalid_spatial,
      204L
    ),
    invalid_trekker = challenge(
      "invalid_trekker",
      "trekker",
      .builder_eval_invalid_trekker,
      205L
    ),
    invalid_immune_repertoire = challenge(
      "invalid_immune_repertoire",
      "immune_repertoire",
      .builder_eval_invalid_immune,
      206L
    ),
    invalid_hla = challenge(
      "invalid_hla",
      "hla",
      .builder_eval_invalid_hla,
      207L
    )
  )
}
