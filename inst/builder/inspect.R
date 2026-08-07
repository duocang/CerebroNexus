##----------------------------------------------------------------------------##
## Read a Seurat object and work out what the export needs to know.
##
## Pure: takes an object, returns a plain list. No Shiny, so it can be tried
## from the console and tested without a running app.
##----------------------------------------------------------------------------##

#' Layers that represent every cell in an assay.
#'
#' Seurat v5 may split one logical layer into physical layers such as
#' `counts.sample1` and `counts.sample2`. Each physical layer contains only a
#' subset of cells, and passing one of them to the exporter silently truncates
#' the data set. Offer the logical name only when its physical layers form a
#' complete, non-overlapping partition.
builder_layer_choices <- function(assay, n_cells) {
  layers <- tryCatch(
    SeuratObject::Layers(assay),
    error = function(e) character()
  )
  if (!length(layers)) {
    return(character())
  }

  layer_cells <- function(layer) {
    mat <- tryCatch(
      suppressWarnings(SeuratObject::LayerData(assay, layer = layer)),
      error = function(e) NULL
    )
    if (is.null(mat)) character() else colnames(mat)
  }
  cells <- lapply(layers, layer_cells)
  names(cells) <- layers

  full <- layers[vapply(
    cells,
    function(x) length(x) == n_cells && !anyDuplicated(x),
    logical(1)
  )]

  layer_root <- function(layer) {
    if (startsWith(layer, "scale.data.")) {
      "scale.data"
    } else {
      sub("[.].*$", "", layer)
    }
  }
  roots <- unique(vapply(layers, layer_root, character(1)))
  joined <- Filter(
    function(root) {
      members <- layers[startsWith(layers, paste0(root, "."))]
      if (length(members) < 2L) {
        return(FALSE)
      }
      all_cells <- unlist(cells[members], use.names = FALSE)
      length(all_cells) == n_cells &&
        length(unique(all_cells)) == n_cells
    },
    roots
  )

  choices <- unique(c(full, joined))
  preferred <- c("data", "counts", "scale.data")
  c(intersect(preferred, choices), setdiff(choices, preferred))
}

#' Join a safe logical split layer before export.
#'
#' Exact full-cell layers are returned unchanged. A logical split layer is
#' materialized only after `builder_layer_choices()` has proved that its
#' physical members cover every cell exactly once.
builder_prepare_export_layer <- function(object, assay, layer) {
  if (!assay %in% names(object@assays)) {
    stop("Assay `", assay, "` is not present in the object.")
  }
  choices <- builder_layer_choices(object[[assay]], ncol(object))
  if (!layer %in% choices) {
    stop(
      "Layer `",
      layer,
      "` does not contain every cell in assay `",
      assay,
      "`."
    )
  }

  physical <- tryCatch(
    SeuratObject::Layers(object[[assay]]),
    error = function(e) character()
  )
  if (layer %in% physical) {
    matrix <- suppressWarnings(
      SeuratObject::LayerData(object[[assay]], layer = layer)
    )
    if (ncol(matrix) == ncol(object)) {
      return(object)
    }
  }

  object[[assay]] <- SeuratObject::JoinLayers(
    object[[assay]],
    layers = layer,
    new = layer
  )
  object
}

#' Describe a Seurat object in the terms the exporter cares about.
describe_seurat <- function(object) {
  meta <- object@meta.data
  n_cells <- ncol(object)

  assays <- names(object@assays)
  default_assay <- tryCatch(
    SeuratObject::DefaultAssay(object),
    error = function(e) assays[1]
  )

  numeric_cols <- colnames(meta)[vapply(meta, is.numeric, logical(1))]
  qc_choices <- function(assay, kind) {
    if (identical(kind, "umi")) {
      preferred <- c(
        paste0("nCount_", assay),
        "nUMI",
        "nCount_RNA"
      )
      pattern <- "^(nCount|nUMI)"
    } else {
      preferred <- c(
        paste0("nFeature_", assay),
        "nGene",
        "nFeature_RNA"
      )
      pattern <- "^(nFeature|nGene)"
    }
    relevant <- numeric_cols[grepl(pattern, numeric_cols, ignore.case = TRUE)]
    unique(c(intersect(preferred, numeric_cols), relevant))
  }

  assay_profiles <- lapply(assays, function(assay) {
    layers <- builder_layer_choices(object[[assay]], n_cells)
    umi <- qc_choices(assay, "umi")
    genes <- qc_choices(assay, "gene")
    list(
      layers = layers,
      default_layer = if ("data" %in% layers) {
        "data"
      } else if (length(layers)) {
        layers[1]
      } else {
        NULL
      },
      nUMI_choices = umi,
      nGene_choices = genes,
      nUMI = if (length(umi)) umi[1] else NA_character_,
      nGene = if (length(genes)) genes[1] else NA_character_
    )
  })
  names(assay_profiles) <- assays
  default_profile <- assay_profiles[[default_assay]]

  ## A column is usable as a grouping variable when it has more than one value
  ## and not one value per cell. Say why the others were left out rather than
  ## hiding them.
  ## Columns whose name says they are a measurement, not a label. An integer
  ## count column like nFeature_RNA has few enough distinct values to look like
  ## a grouping variable and is never one.
  qc_pattern <- paste0(
    "^(nCount|nFeature|nUMI|nGene)_?|",
    "^percent[._]|[._]score$|^S\\.Score$|^G2M\\.Score$"
  )

  candidates <- character()
  struck <- character()
  for (col in colnames(meta)) {
    values <- meta[[col]]
    if (!(is.character(values) || is.factor(values) || is.integer(values))) {
      next
    }
    n <- length(unique(values))
    if (grepl(qc_pattern, col, ignore.case = TRUE)) {
      struck <- c(struck, paste0(col, " (QC metric, not a group)"))
    } else if (n < 2) {
      struck <- c(struck, paste0(col, " (only one value)"))
    } else if (n >= n_cells) {
      struck <- c(struck, paste0(col, " (one value per cell)"))
    } else if (n > 100) {
      struck <- c(struck, paste0(col, " (", n, " values; too many)"))
    } else {
      candidates <- c(candidates, col)
    }
  }
  if (length(candidates)) {
    names(candidates) <- paste0(
      candidates,
      "  (",
      vapply(
        candidates,
        function(c) {
          length(unique(meta[[c]]))
        },
        integer(1)
      ),
      " groups)"
    )
  }

  preselect <- intersect(
    c("seurat_clusters", "cell_type", "celltype", "sample", "orig.ident"),
    unname(candidates)
  )
  if (!length(preselect)) {
    preselect <- if (length(candidates)) unname(candidates)[1] else character()
  }

  ## The exporter drops every reduction whose name contains "pca", so
  ## preselecting them would promise something it will not deliver.
  reductions <- names(object@reductions)
  reduction_preselect <- reductions[
    !grepl("pca", reductions, ignore.case = TRUE)
  ]

  ## Gene name shape is the only cheap signal for species.
  genes <- utils::head(rownames(object), 200)
  organism_guess <- if (mean(genes == toupper(genes)) > 0.8) {
    "hg"
  } else if (mean(grepl("^[A-Z][a-z]", genes)) > 0.6) {
    "mm"
  } else {
    "other"
  }

  list(
    n_cells = n_cells,
    n_genes = nrow(object),
    assays = assays,
    default_assay = default_assay,
    assay_profiles = assay_profiles,
    ## Kept for older callers; the UI now reads the selected assay profile.
    layers = default_profile$layers,
    default_layer = default_profile$default_layer,
    group_candidates = candidates,
    group_preselect = preselect,
    group_struck = struck,
    reductions = reductions,
    reduction_preselect = reduction_preselect,
    images = tryCatch(names(object@images), error = function(e) character()),
    nUMI = default_profile$nUMI,
    nGene = default_profile$nGene,
    organism_guess = organism_guess,
    ## What the object already carries that Cerebro can show. Worth surfacing:
    ## these light up whole pages, and their absence is the usual reason a
    ## page the user expected is missing.
    extras = describe_misc(object),
    suggested_dir = file.path(path.expand("~"), "cerebro")
  )
}

#' What is in `@misc` that the viewer knows how to display.
describe_misc <- function(object) {
  misc <- object@misc
  present <- function(key) !is.null(misc[[key]]) && length(misc[[key]]) > 0
  entries <- list(
    list(
      key = "marker_genes",
      label = "Marker genes",
      found = present("marker_genes")
    ),
    list(
      key = "immune_repertoire",
      label = "Immune repertoire (TCR/BCR)",
      found = present("immune_repertoire") ||
        present("tcr_data") ||
        present("bcr_data")
    ),
    list(
      key = "enriched_pathways",
      label = "Enriched pathways",
      found = present("enriched_pathways")
    ),
    list(
      key = "most_expressed_genes",
      label = "Most expressed genes",
      found = present("most_expressed_genes")
    ),
    list(
      key = "trajectories",
      label = "Trajectory (monocle2)",
      found = present("trajectories")
    ),
    list(
      key = "hla_typing",
      label = "HLA typing",
      found = present("hla_typing")
    ),
    ## Trekker is the one modality exportFromSeurat() does not read off @misc,
    ## so the builder writes it into the .crb afterwards. Worth listing for the
    ## same reason as the rest: its absence is why a page the user expected is
    ## missing.
    list(
      key = "trekker",
      label = "Trekker spatial mapping",
      found = present("trekker")
    ),
    list(
      key = "extra_material",
      label = "Supplementary tables / plots",
      found = present("extra_material")
    )
  )
  entries
}

#' The levels a grouping variable will have AFTER export, in export order.
#'
#' Colours are keyed by level name, and `apply_configured_colors()` matches them
#' with `intersect()` -- so a key that differs from the exported level is
#' dropped silently and the user simply sees default colours. That makes this a
#' mirror of `exportFromSeurat()`, not an approximation:
#'
#'   * character column -> sort(unique(values), na.last = NA), plus "N/A"
#'     appended when the column contains NA
#'   * factor column    -> levels(), plus "N/A" appended likewise
#'
#' test-builder-colours.R pins this against what the exporter actually produces,
#' so the two cannot drift apart unnoticed.
builder_group_levels <- function(object, group) {
  if (!group %in% colnames(object@meta.data)) {
    return(character())
  }
  values <- object@meta.data[[group]]
  if (is.factor(values)) {
    lv <- levels(values)
    if (any(is.na(values)) && !("N/A" %in% lv)) {
      lv <- c(lv, "N/A")
    }
    return(lv)
  }
  lv <- sort(unique(as.character(values)), na.last = NA)
  if (any(is.na(values))) {
    lv <- c(lv, "N/A")
  }
  lv
}

#' Levels for every grouping variable the user selected, as a named list.
builder_group_levels_for <- function(object, groups) {
  if (!length(groups)) {
    return(list())
  }
  out <- lapply(groups, function(g) builder_group_levels(object, g))
  names(out) <- groups
  out
}
