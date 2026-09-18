## ---- Reactive: raw repertoire data (as stored in crb) ------------------ ##
ir_data_raw <- reactive({
  req(!is.null(data_set()))
  data <- getImmuneRepertoire()
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(NULL)
  }
  data
})

## The default Clonal projection is backed by the compact Viewer Pack. Keep its
## availability gate on the backend summary so opening the page does not hydrate
## the full repertoire sidecar merely to decide whether controls may render.
ir_repertoire_available <- reactive({
  isTRUE(getImmuneRepertoireSummary()$available)
})

## ---- Standard scRepertoire columns (not usable as grouping) ----------- ##
ir_scr_cols <- c(
  "barcode",
  "CTgene",
  "CTnt",
  "CTaa",
  "CTstrict",
  "clonalProportion",
  "clonalFrequency",
  "cloneSize",
  "Frequency",
  "frequency",
  "cloneType"
)

## ---- Join only requested metadata onto IR rows by barcode -------------- ##
## Most repertoire views do not group by cell metadata. Copying every metadata
## column onto 500k receptor rows therefore wastes both time and memory. Track
## the active grouping controls and attach just those columns, using one match
## over the concatenated barcode vector rather than rebuilding a 1M-cell lookup
## for every sample.
ir_requested_metadata_columns <- reactive({
  requested <- c(
    input[["ir_groupBy"]],
    input[["ir_p_umap_group_by"]]
  )
  requested <- as.character(requested)
  unique(requested[!is.na(requested) & nzchar(requested)])
})

ir_annotate_metadata <- function(data, metadata, columns = character()) {
  if (
    is.null(metadata) ||
      !("cell_barcode" %in% colnames(metadata)) ||
      !length(columns)
  ) {
    return(data)
  }
  columns <- intersect(
    unique(columns),
    setdiff(colnames(metadata), "cell_barcode")
  )
  add <- lapply(data, function(df) {
    if (is.null(df) || !("barcode" %in% colnames(df))) {
      return(character())
    }
    setdiff(columns, colnames(df))
  })
  join <- lengths(add) > 0L
  if (!any(join)) {
    return(data)
  }
  joined_indices <- which(join)
  n_rows <- vapply(data[joined_indices], nrow, integer(1))
  ends <- cumsum(n_rows)
  idx <- match(
    unlist(lapply(data[joined_indices], `[[`, "barcode"), use.names = FALSE),
    metadata$cell_barcode
  )
  n_miss <- sum(is.na(idx))
  if (n_miss > 0L) {
    warning(sprintf(
      paste0(
        "[IR] %d / %d clonotype barcodes not found in cell metadata; ",
        "grouping/splitting by metadata columns may be incomplete."
      ),
      n_miss,
      length(idx)
    ))
  }
  out <- data
  for (position in seq_along(joined_indices)) {
    data_index <- joined_indices[[position]]
    n <- n_rows[[position]]
    rows <- seq.int(ends[[position]] - n + 1L, length.out = n)
    frame_idx <- idx[rows]
    for (column in add[[data_index]]) {
      out[[data_index]][[column]] <- metadata[[column]][frame_idx]
    }
  }
  out
}

ir_data_annotated <- reactive({
  data <- ir_data_raw()
  if (is.null(data)) {
    return(NULL)
  }
  md <- tryCatch(getMetaData(), error = function(e) NULL)
  ir_annotate_metadata(data, md, ir_requested_metadata_columns())
})

## ---- Reactive: repertoire data --------------------------------------- ##
## Returns the metadata-annotated repertoire list as loaded. Grouping is not
## done here: scRepertoire's own group.by rbinds the list and re-splits on the
## chosen column (.groupList), so an in-app re-split would only duplicate — with
## a narrower, sample-only column set — what group.by already does. Comparison
## units are therefore expressed solely through ir_groupBy / group.by.
ir_data <- reactive({
  ir_data_annotated()
})

## ---- Helper: read a dynamic function-specific parameter --------------- ##
## The function-specific controls (IR_PARAM_SPEC) are rendered into a dynamic
## panel, so an input may be absent on tabs where the parameter doesn't apply.
## Returns `default` when the input is missing/empty.
ir_param <- function(id, default = NULL) {
  v <- input[[id]]
  if (
    is.null(v) ||
      length(v) == 0 ||
      (is.character(v) && length(v) == 1 && !nzchar(v))
  ) {
    default
  } else {
    v
  }
}

## ---- Resolve the generic "Order groups" (order.by) value -------------- ##
## Maps the ir_p_order_by control to scRepertoire's order.by argument: the
## empty default becomes NULL (scRepertoire's own ordering), otherwise the
## chosen value (e.g. "alphanumeric") is passed through.
ir_order_by <- function() {
  v <- input[["ir_p_order_by"]]
  if (is.null(v) || !nzchar(v)) NULL else v
}

## ---- Parse the Homeostasis clone-size thresholds ---------------------- ##
## clonalHomeostasis' cloneSize is a *named* numeric vector of upper bounds
## (Rare < Small < ... < Hyperexpanded). The UI takes them as a comma-separated
## list of 5 increasing numbers; this builds the named vector. Returns NULL on
## anything malformed so scRepertoire falls back to its own default.
IR_CLONE_SIZE_NAMES <- c(
  "Rare",
  "Small",
  "Medium",
  "Large",
  "Hyperexpanded"
)
IR_CLONE_SIZE_DEFAULT <- setNames(
  c(1e-04, 0.001, 0.01, 0.1, 1),
  IR_CLONE_SIZE_NAMES
)
ir_clone_size <- function() {
  v <- input[["ir_p_clone_size"]]
  if (is.null(v) || !nzchar(v)) {
    return(IR_CLONE_SIZE_DEFAULT)
  }
  nums <- suppressWarnings(as.numeric(trimws(strsplit(v, ",")[[1]])))
  if (
    length(nums) != length(IR_CLONE_SIZE_NAMES) ||
      any(is.na(nums)) ||
      is.unsorted(nums, strictly = TRUE)
  ) {
    return(IR_CLONE_SIZE_DEFAULT)
  }
  setNames(nums, IR_CLONE_SIZE_NAMES)
}

## ---- Comparable groups for Scatter / Compare ------------------------- ##
## clonalScatter (x.axis/y.axis) and clonalCompare (samples) operate on the
## *names of the groups that group.by produces*. With group.by = None the
## groups are the list elements (samples); with group.by = <column> they are
## that column's levels. This reactive returns those group names so the Scatter
## X/Y and Compare selectors stay in sync with the active grouping.
ir_compare_groups <- reactive({
  data <- ir_data()
  if (is.null(data)) {
    return(character(0))
  }
  gb <- input$ir_groupBy
  if (is.null(gb) || !nzchar(gb)) {
    return(names(data))
  }
  vals <- unique(unlist(lapply(data, function(df) {
    if (gb %in% colnames(df)) as.character(df[[gb]]) else character(0)
  })))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) names(data) else sort(vals)
})

## ---- Candidate columns for the Sharing "unit" selector ---------------- ##
## The Sharing tab needs a "smallest sharing unit" (default: sample). Offer any
## metadata column that is categorical (character/factor with > 1 distinct
## non-empty value) and is not a raw scRepertoire column (ir_scr_cols). `sample`
## is placed first so it becomes the default selection.
ir_sharing_unit_choices <- reactive({
  data <- ir_data()
  if (is.null(data)) {
    return(character(0))
  }
  common <- Reduce(intersect, lapply(data, colnames))
  merged <- do.call(
    rbind,
    lapply(data, function(df) {
      df[, common, drop = FALSE]
    })
  )
  cand <- setdiff(common, ir_scr_cols)
  keep <- vapply(
    cand,
    function(col) {
      v <- merged[[col]]
      (is.character(v) || is.factor(v)) &&
        length(unique(v[!is.na(v) & nzchar(as.character(v))])) > 1
    },
    logical(1)
  )
  cols <- cand[keep]
  if ("sample" %in% cols) c("sample", setdiff(cols, "sample")) else cols
})

## ---- Reactive: parameters --------------------------------------------- ##
ir_params <- reactive({
  gb <- input$ir_groupBy
  if (is.null(gb) || gb == "") {
    gb <- NULL
  }
  list(
    cloneCall = input$ir_cloneCall,
    chain = input$ir_chain,
    groupBy = gb
  )
})

## ---- Reactive: number of groups for faceted plots --------------------- ##
n_groups <- reactive({
  gb <- ir_params()$groupBy
  if (is.null(gb)) {
    return(1L)
  }
  data <- ir_data()
  if (is.null(data)) {
    return(1L)
  }
  lvls <- unique(unlist(lapply(data, function(df) {
    if (gb %in% names(df)) unique(as.character(df[[gb]])) else character(0)
  })))
  max(1L, length(lvls))
})

## ---- Dynamic gene parameter for vizGenes/percentGeneUsage ------------- ##
default_gene_family <- reactive({
  chains <- detect_chains(ir_data())
  tcr_chains <- intersect(chains, c("TRA", "TRB", "TRG", "TRD"))
  bcr_chains <- intersect(chains, c("IGH", "IGK", "IGL"))
  if (length(tcr_chains) > 0 && "TRB" %in% tcr_chains) {
    return("TRBV")
  }
  if (length(tcr_chains) > 0) {
    return(paste0(tcr_chains[1], "V"))
  }
  if (length(bcr_chains) > 0 && "IGH" %in% bcr_chains) {
    return("IGHV")
  }
  if (length(bcr_chains) > 0) {
    return(paste0(bcr_chains[1], "V"))
  }
  "TRBV"
})

## ---- Resolve chain: for functions that don't accept "both" ------------ ##
specific_chain <- reactive({
  ch <- input$ir_chain
  if (is.null(ch) || ch == "both") {
    chains <- detect_chains(ir_data())
    if ("TRB" %in% chains) {
      return("TRB")
    }
    if (length(chains) > 0) {
      return(chains[1])
    }
    return("TRB")
  }
  ch
})

## ---- Count unique genes for dynamic plot height ----------------------- ##
n_genes <- reactive({
  data <- ir_data()
  if (is.null(data)) {
    return(0L)
  }
  gene_family <- default_gene_family()
  # Gather all gene values across samples
  all_genes <- unique(unlist(lapply(data, function(df) {
    # CTgene has format like "TRBV1.TRBJ2" — extract the gene family portion
    ct <- as.character(df$CTgene)
    ct <- ct[!is.na(ct)]
    # Split by "." and keep segments matching the gene family prefix
    segments <- unlist(strsplit(ct, "[._]"))
    segments[grepl(paste0("^", gene_family), segments, ignore.case = TRUE)]
  })))
  length(all_genes)
})

ir_plot_height <- function(facet_mode = c("none", "grid", "wrap")) {
  facet_mode <- match.arg(facet_mode)
  n <- n_genes()
  ng <- n_groups()
  base_h <- max(450, min(n * 25, 2500))
  if (ng <= 1 || facet_mode == "none") {
    return(base_h)
  }
  if (facet_mode == "grid") {
    # facet_grid(Group ~ .): each group stacked vertically
    return(base_h * ng)
  }
  # facet_wrap: ggplot default ncol = ceiling(sqrt(n))
  ncol <- ceiling(sqrt(ng))
  nrow <- ceiling(ng / ncol)
  base_h * nrow
}

##----------------------------------------------------------------------------##
## Clonal UMAP data layer
##----------------------------------------------------------------------------##

## ---- Chains that define each receptor class --------------------------- ##
IR_TCR_CHAINS <- CEREBRO_TCR_CHAINS
IR_BCR_CHAINS <- CEREBRO_BCR_CHAINS

## ---- Which receptor classes are present in the data ------------------- ##
## Returns a named vector ("TCR" / "BCR") of the receptor types actually
## detected, so the Clonal UMAP selector only offers what exists. The names
## are the labels shown to the user; values feed ir_umap_chains().
ir_receptor_types <- reactive({
  pack <- viewerPackCurrent()
  packed <- if (is.list(pack)) {
    as.character(pack$manifest$immune_receptors)
  } else {
    character(0)
  }
  packed <- intersect(packed, c("TCR", "BCR"))
  if (length(packed)) {
    return(stats::setNames(packed, packed))
  }
  chains <- as.character(getImmuneRepertoireSummary()$chains)
  present <- c(
    if (any(chains %in% IR_TCR_CHAINS)) "TCR" else character(0),
    if (any(chains %in% IR_BCR_CHAINS)) "BCR" else character(0)
  )
  if (length(present)) {
    return(stats::setNames(present, present))
  }
  present <- tryCatch(
    # Receptor availability is structural. Using the metadata-annotated
    # reactive here makes the parameter panel depend on its own grouping
    # select input; changing that input then rebuilds the panel and resets the
    # selection before the grouped plot can render.
    cerebro_receptors_present(ir_data_raw()),
    error = function(e) character(0)
  )
  stats::setNames(present, present)
})

## ---- Chains belonging to the selected receptor type ------------------- ##
ir_umap_chains <- function(receptor) {
  if (identical(receptor, "BCR")) IR_BCR_CHAINS else IR_TCR_CHAINS
}

## ---- Clone-size bin breaks / labels (scRepertoire cloneSize defaults) -- ##
## A clone's size = number of cells carrying that clonotype (within the
## selected receptor). Cells are binned into the standard expansion levels.
IR_CLONE_BINS <- CEREBRO_CLONE_BINS
IR_CLONE_LABELS <- CEREBRO_CLONE_LABELS

ir_clone_expansion <- function(clones) {
  clone_ids <- match(clones, unique(clones))
  sizes <- tabulate(clone_ids)
  cut(
    sizes[clone_ids],
    breaks = IR_CLONE_BINS,
    labels = IR_CLONE_LABELS,
    right = TRUE,
    include.lowest = TRUE
  )
}

ir_clonal_abundance_counts <- function(data, clone_col) {
  packed <- if (
    exists("viewerPackCurrent", mode = "function") &&
      exists("viewerPackImmuneAbundance", mode = "function")
  ) {
    viewerPackImmuneAbundance(viewerPackCurrent(), clone_col)
  } else {
    NULL
  }
  if (!is.null(packed)) {
    return(packed)
  }
  samples <- names(data)
  if (is.null(samples)) {
    samples <- as.character(seq_along(data))
  }
  rows <- Map(
    function(frame, sample) {
      if (is.null(frame) || !(clone_col %in% colnames(frame))) {
        return(NULL)
      }
      clones <- as.character(frame[[clone_col]])
      clones <- clones[!is.na(clones) & nzchar(clones)]
      if (!length(clones)) {
        return(NULL)
      }
      clone_ids <- match(clones, unique(clones))
      abundance <- tabulate(clone_ids)
      bins <- base::table(abundance)
      data.frame(
        sample = sample,
        abundance = as.integer(names(bins)),
        n_clones = as.integer(bins),
        stringsAsFactors = FALSE
      )
    },
    data,
    samples
  )
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(
      sample = character(),
      abundance = integer(),
      n_clones = integer()
    ))
  }
  do.call(rbind, rows)
}

## ---- Which CT* column a cloneCall maps to ----------------------------- ##
ir_clonecall_col <- function(cloneCall) {
  cerebro_clonecall_col(cloneCall)
}

## ---- Clonal UMAP data: coords + per-cell expansion level --------------- ##
## Joins the chosen projection's UMAP coordinates (barcode-indexed) with each
## cell's clone-expansion level, restricted to the selected receptor (TCR/BCR).
## Returns a data.frame (x, y, expansion, barcode) or NULL when it cannot be
## built (no projection, no data for the receptor, no overlapping barcodes).
##
##   projection : a name from availableProjections()
##   receptor   : "TCR" | "BCR"
##   cloneCall  : "gene" | "nt" | "aa" | "strict" (clone identity column)
##   show_all   : when TRUE, also include every other cell in the projection
##                with expansion = NA (drawn as a grey background by the
##                renderer), so the receptor cells are shown in context.
##   cells      : optional character vector of barcodes to restrict to (e.g.
##                from the Group filters); NULL = all cells in the projection.
ir_clonal_umap_data <- function(
  projection,
  receptor,
  cloneCall = "gene",
  show_all = TRUE,
  cells = NULL,
  percentage = 100,
  max_background = 100000L
) {
  if (is.null(projection) || !nzchar(projection)) {
    return(NULL)
  }
  if (
    !(projection %in%
      tryCatch(availableProjections(), error = function(e) character(0)))
  ) {
    return(NULL)
  }
  clone_col <- ir_clonecall_col(cloneCall)
  pack <- NULL
  packed <- NULL
  if (
    identical(clone_col, "CTgene") &&
      exists("viewerPackCurrent", mode = "function") &&
      exists("viewerPackImmuneIndex", mode = "function")
  ) {
    pack <- viewerPackCurrent()
    packed <- viewerPackImmuneIndex(pack, receptor)
  }
  indexed_pack <- !is.null(packed) &&
    isTRUE(pack$canonical_order) &&
    exists("viewerProjectionFirstFrameCoordinates", mode = "function")
  coords <- tryCatch(
    if (indexed_pack) {
      viewerProjectionFirstFrameCoordinates(projection)
    } else {
      getProjection(projection)
    },
    error = function(e) NULL
  )
  if (is.null(coords) || nrow(coords) == 0) {
    return(NULL)
  }
  percentage <- suppressWarnings(as.numeric(percentage))
  if (length(percentage) != 1L || !is.finite(percentage)) {
    percentage <- 100
  }
  percentage <- min(100, max(0, percentage))
  max_background <- suppressWarnings(as.integer(max_background))
  if (
    length(max_background) != 1L || is.na(max_background) || max_background < 0L
  ) {
    max_background <- 100000L
  }
  visible_indices <- seq_len(nrow(coords))
  if (!is.null(cells) && indexed_pack) {
    canonical <- if (exists("viewerPackCellBarcodes", mode = "function")) {
      viewerPackCellBarcodes(pack)
    } else {
      NULL
    }
    if (is.null(canonical)) {
      return(NULL)
    }
    visible_indices <- visible_indices[canonical %in% cells]
    if (!length(visible_indices)) {
      return(NULL)
    }
  } else if (!is.null(cells)) {
    coords <- coords[rownames(coords) %in% cells, , drop = FALSE]
    if (nrow(coords) == 0) {
      return(NULL)
    }
    visible_indices <- seq_len(nrow(coords))
  }

  coord_bc <- rownames(coords)
  if (indexed_pack) {
    coord_index <- suppressWarnings(as.integer(packed$cell_index))
    clones <- packed$clone
    valid <- !is.na(clones) &
      nzchar(clones) &
      !is.na(coord_index) &
      coord_index >= 1L &
      coord_index <= nrow(coords) &
      coord_index %in% visible_indices
    clones <- clones[valid]
    coord_index <- coord_index[valid]
    barcodes <- NULL
  } else if (!is.null(packed)) {
    canonical <- pack$cells
    barcodes <- canonical[packed$cell_index]
    clones <- packed$clone
  } else {
    data <- ir_data_annotated()
    if (is.null(data)) {
      return(NULL)
    }
    keep_chains <- ir_umap_chains(receptor)
    rows <- lapply(data, function(df) {
      if (is.null(df) || !all(c("barcode", clone_col) %in% colnames(df))) {
        return(NULL)
      }
      chain_ref <- if ("CTstrict" %in% colnames(df)) {
        as.character(df$CTstrict)
      } else {
        as.character(df[[clone_col]])
      }
      in_receptor <- grepl(paste(keep_chains, collapse = "|"), chain_ref)
      if (!any(in_receptor)) {
        return(NULL)
      }
      list(
        barcode = as.character(df$barcode[in_receptor]),
        clone = as.character(df[[clone_col]][in_receptor])
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    barcodes <- unlist(lapply(rows, `[[`, "barcode"), use.names = FALSE)
    clones <- unlist(lapply(rows, `[[`, "clone"), use.names = FALSE)
  }
  if (!indexed_pack) {
    valid <- !is.na(clones) & nzchar(clones)
    coord_index <- match(barcodes, coord_bc)
    valid <- valid & !is.na(coord_index)
    barcodes <- barcodes[valid]
    clones <- clones[valid]
    coord_index <- coord_index[valid]
  }
  has_receptor <- length(coord_index) > 0L
  if (!has_receptor) {
    # No receptor cells. With show_all we can still draw the grey background;
    # otherwise there is nothing to plot.
    if (!isTRUE(show_all)) {
      return(NULL)
    }
    expansion <- factor(levels = IR_CLONE_LABELS)
  } else {
    expansion <- if (!is.null(packed)) {
      factor(
        packed$expansion[valid],
        levels = seq_along(IR_CLONE_LABELS),
        labels = IR_CLONE_LABELS
      )
    } else {
      ir_clone_expansion(clones)
    }
  }
  receptor_indices <- unique(coord_index)

  # Coloured layer: receptor cells with an expansion level, joined to coords.
  if (has_receptor) {
    idx <- coord_index
    keep_n <- min(length(idx), ceiling(length(idx) * percentage / 100))
    if (keep_n < length(idx)) {
      keep <- if (keep_n > 0L) {
        unique(as.integer(round(seq(1, length(idx), length.out = keep_n))))
      } else {
        integer()
      }
      if (indexed_pack) {
        coord_index <- coord_index[keep]
      } else {
        barcodes <- barcodes[keep]
      }
      expansion <- expansion[keep]
      idx <- idx[keep]
    }
  } else {
    idx <- integer(0)
  }
  if (length(idx) == 0 && !isTRUE(show_all)) {
    return(NULL)
  }
  coloured <- if (length(idx) > 0) {
    xy <- coords[idx, 1:2, drop = FALSE]
    out <- data.frame(
      x = as.numeric(xy[, 1L]),
      y = as.numeric(xy[, 2L]),
      expansion = factor(expansion, levels = IR_CLONE_LABELS),
      stringsAsFactors = FALSE
    )
    if (indexed_pack) {
      out$cell_index <- coord_index
    } else {
      out$barcode <- barcodes
    }
    out
  } else {
    NULL
  }

  # Background layer: every other cell in the projection, expansion = NA, so the
  # renderer can draw them in grey. Only when show_all is requested.
  background <- NULL
  if (isTRUE(show_all)) {
    bg_mask <- seq_len(nrow(coords)) %in% visible_indices
    bg_mask[receptor_indices] <- FALSE
    if (any(bg_mask)) {
      bg_idx <- which(bg_mask)
      target <- min(
        length(bg_idx),
        ceiling(length(bg_idx) * percentage / 100),
        as.integer(max_background)
      )
      if (target < length(bg_idx)) {
        keep <- if (target > 0L) {
          unique(as.integer(round(seq(1, length(bg_idx), length.out = target))))
        } else {
          integer()
        }
        bg_idx <- bg_idx[keep]
      }
      xy_bg <- coords[bg_idx, 1:2, drop = FALSE]
      background <- data.frame(
        x = as.numeric(xy_bg[, 1L]),
        y = as.numeric(xy_bg[, 2L]),
        expansion = factor(NA, levels = IR_CLONE_LABELS),
        stringsAsFactors = FALSE
      )
      if (indexed_pack) {
        background$cell_index <- bg_idx
      } else {
        background$barcode <- coord_bc[bg_idx]
      }
    }
  }

  out <- rbind(background, coloured)
  if (is.null(out) || nrow(out) == 0) {
    return(NULL)
  }
  out
}

ir_clonal_umap_barcodes <- function(data) {
  if ("barcode" %in% colnames(data)) {
    return(as.character(data$barcode))
  }
  if (!("cell_index" %in% colnames(data))) {
    return(NULL)
  }
  if (
    exists("viewerPackCurrent", mode = "function") &&
      exists("viewerPackCellBarcodes", mode = "function")
  ) {
    cells <- viewerPackCellBarcodes(viewerPackCurrent(), data$cell_index)
    if (!is.null(cells)) {
      return(cells)
    }
  }
  metadata <- tryCatch(getMetaData(), error = function(e) NULL)
  if (
    !is.data.frame(metadata) ||
      !("cell_barcode" %in% colnames(metadata)) ||
      any(data$cell_index < 1L | data$cell_index > nrow(metadata))
  ) {
    return(NULL)
  }
  as.character(metadata$cell_barcode[data$cell_index])
}

##----------------------------------------------------------------------------##
## Clone-segment parsing (shared by the Definition and Sharing tabs)
##----------------------------------------------------------------------------##

## ---- Parse V / J / CDR3 for one chain out of the CT* columns ----------- ##
## scRepertoire packs all chains of a cell into single strings:
##   CTgene : "<chainA gene segs>_<chainB gene segs>"  (chains joined by "_")
##            each chain's segs are V.D.J.C joined by "."  (empty D -> "..")
##            a chain's two alleles are joined by ";"      (take the first)
##   CTaa   : "<chainA CDR3>_<chainB CDR3>"              (chains joined by "_")
## "NA" marks a missing chain. This returns one row per cell that HAS the
## requested chain, with parsed v_gene / j_gene / cdr3 and a combined
## clone_vjc = "v;j;cdr3" id, plus every metadata column already joined onto
## the IR data (so callers can group/split by any of them).
##
##   data  : the metadata-annotated IR list (ir_data_annotated())
##   chain : chain prefix, e.g. "TRB" / "TRA" / "IGH"
ir_parse_segments <- function(data, chain) {
  if (is.null(data) || length(data) == 0 || is.null(chain) || !nzchar(chain)) {
    return(NULL)
  }
  # For each cell's CT* string (chains joined by "_"), find the positional index
  # (1-based) of the chain matching `chain`, then return that slot's value from
  # both CTgene and CTaa in parallel so gene and CDR3 stay aligned.
  # Returns NA when the chain is absent or its slot is "NA".
  chain_slot_index <- function(ct_gene_vec) {
    ct_gene_vec <- as.character(ct_gene_vec)
    vapply(
      strsplit(ct_gene_vec, "_", fixed = TRUE),
      function(parts) {
        # Take first allele of each slot to test chain membership.
        first <- sub(";.*$", "", parts)
        idx <- which(grepl(chain, first, fixed = TRUE) & first != "NA")
        if (length(idx) == 0) NA_integer_ else idx[1]
      },
      integer(1)
    )
  }
  # Given a CT* vector and a per-row slot index, pick that slot's value.
  # strsplit the whole vector once, then index each row's slot in one mapply
  # pass (avoids re-splitting per cell). NA slot, out-of-range slot, and a
  # literal "NA" / empty value all collapse to NA_character_, as before.
  pick_slot <- function(ct_vec, slot_idx) {
    if (length(ct_vec) == 0) {
      return(character(0))
    }
    split_all <- strsplit(as.character(ct_vec), "_", fixed = TRUE)
    val <- mapply(
      function(parts, i) {
        if (is.na(i) || i > length(parts)) NA_character_ else parts[i]
      },
      split_all,
      slot_idx,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    val[is.na(val) | val == "NA" | !nzchar(val)] <- NA_character_
    val
  }
  # Take the first allele (before ";") of a chain segment.
  first_allele <- function(x) {
    ifelse(is.na(x), NA_character_, sub(";.*$", "", x))
  }

  rows <- lapply(data, function(df) {
    if (is.null(df) || !all(c("barcode", "CTgene", "CTaa") %in% colnames(df))) {
      return(NULL)
    }
    slot_idx <- chain_slot_index(df$CTgene)
    gene_seg <- first_allele(pick_slot(df$CTgene, slot_idx))
    cdr3 <- first_allele(pick_slot(df$CTaa, slot_idx))
    # From "TRBV6-2..TRBJ2-6.TRBC2" pull the V and J tokens (segs split on ".").
    # NA gene_seg -> strsplit yields NA -> no token matches -> NA gene, dropped.
    pull_token <- function(prefix) {
      vapply(
        strsplit(gene_seg, ".", fixed = TRUE),
        function(toks) {
          hit <- toks[grepl(paste0("^", chain, prefix), toks)]
          if (length(hit) == 0) NA_character_ else hit[1]
        },
        character(1)
      )
    }
    v_gene <- pull_token("V")
    j_gene <- pull_token("J")
    keep <- !is.na(v_gene) & !is.na(j_gene) & !is.na(cdr3) & nzchar(cdr3)
    if (!any(keep)) {
      return(NULL)
    }
    out <- df[keep, , drop = FALSE]
    out$v_gene <- v_gene[keep]
    out$j_gene <- j_gene[keep]
    out$cdr3 <- cdr3[keep]
    out$clone_vjc <- paste(out$v_gene, out$j_gene, out$cdr3, sep = ";")
    out
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(NULL)
  }
  # Align columns before rbind. Use the UNION of all sample columns and NA-fill
  # any a given sample lacks, so per-cohort metadata columns are never silently
  # dropped (intersect would lose them). Column order follows the first sample,
  # with any extra columns from later samples appended.
  all_cols <- unique(unlist(lapply(rows, colnames)))
  rows <- lapply(rows, function(df) {
    miss <- setdiff(all_cols, colnames(df))
    for (col in miss) {
      df[[col]] <- NA
    }
    df[, all_cols, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

## ---- Definition-resolution level order (used by the Definition tab) ----- ##
IR_DEFINITION_LEVELS <- c(
  "cells",
  "V",
  "J",
  "V+J",
  "CDR3",
  "V+CDR3",
  "V+J+CDR3"
)

## ---- Count unique entities at each clone-definition resolution --------- ##
## Given the per-cell segment table from ir_parse_segments(), count how many
## distinct entities exist at each of the 7 resolution levels:
##   cells -> V -> J -> V+J -> CDR3 -> V+CDR3 -> V+J+CDR3
## When `group` names a column, counts are computed within each group value.
## Returns a long data.frame: definition (factor, ordered), n, and (if grouped)
## the group column.
ir_definition_counts <- function(seg, group = NULL) {
  if (is.null(seg) || nrow(seg) == 0) {
    return(NULL)
  }
  count_block <- function(df) {
    data.frame(
      definition = factor(
        IR_DEFINITION_LEVELS,
        levels = IR_DEFINITION_LEVELS,
        ordered = TRUE
      ),
      n = c(
        nrow(df),
        length(unique(df$v_gene)),
        length(unique(df$j_gene)),
        length(unique(paste(df$v_gene, df$j_gene, sep = ";"))),
        length(unique(df$cdr3)),
        length(unique(paste(df$v_gene, df$cdr3, sep = ";"))),
        length(unique(df$clone_vjc))
      ),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(group) || !nzchar(group) || !(group %in% colnames(seg))) {
    return(count_block(seg))
  }
  groups <- split(seg, seg[[group]], drop = TRUE)
  out <- do.call(
    rbind,
    lapply(names(groups), function(g) {
      blk <- count_block(groups[[g]])
      blk[[group]] <- g
      blk
    })
  )
  rownames(out) <- NULL
  out
}

## ---- Sharing-class factor levels --------------------------------------- ##
IR_SHARING_LEVELS_3 <- c(
  "Private",
  "Public (within-group)",
  "Public (cross-group)"
)
IR_SHARING_LEVELS_2 <- c("Private", "Public")

## ---- Classify each clonotype by how it is shared ----------------------- ##
## For every distinct clone_vjc, count how many `unit_col` values carry it and
## how many `group_col` values it spans, then label it:
##   Private               : found in exactly 1 unit
##   Public (within-group) : >= 2 units, all in the same group
##   Public (cross-group)  : spans >= 2 groups
## With group_col = NULL there is no group dimension, so it degrades to
## Private / Public (>= 2 units). Returns one row per clonotype: clone_vjc,
## n_units, n_groups (0 when ungrouped), sharing (factor). NA unit/group
## values are ignored when counting, so a stray NA row cannot inflate a
## clonotype into a spurious Public / cross-group classification.
ir_sharing_classify <- function(seg, unit_col, group_col = NULL) {
  if (is.null(seg) || nrow(seg) == 0 || !(unit_col %in% colnames(seg))) {
    return(NULL)
  }
  has_group <- !is.null(group_col) &&
    nzchar(group_col) &&
    group_col %in% colnames(seg)
  # Distinct-count helper: for each clonotype (all clones share a stable factor
  # level set), count how many distinct non-NA values of `col` it carries.
  # Vectorised over all clonotypes at once via unique-then-tabulate, instead of
  # splitting per clonotype and building a one-row data.frame each time.
  clones <- factor(seg$clone_vjc)
  n_distinct_by_clone <- function(col) {
    vals <- as.character(seg[[col]])
    ok <- !is.na(vals)
    pairs <- !duplicated(data.frame(c = clones[ok], v = vals[ok]))
    tabulate(clones[ok][pairs], nbins = nlevels(clones))
  }
  n_units <- n_distinct_by_clone(unit_col)
  n_groups <- if (has_group) n_distinct_by_clone(group_col) else 0L
  sharing <- if (!has_group) {
    ifelse(n_units <= 1, "Private", "Public")
  } else {
    ifelse(
      n_units <= 1,
      "Private",
      ifelse(n_groups >= 2, "Public (cross-group)", "Public (within-group)")
    )
  }
  lvls <- if (has_group) IR_SHARING_LEVELS_3 else IR_SHARING_LEVELS_2
  out <- data.frame(
    clone_vjc = levels(clones),
    n_units = n_units,
    n_groups = if (has_group) n_groups else 0L,
    sharing = factor(sharing, levels = lvls),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

## ---- Is the chain a BCR chain? ----------------------------------------- ##
## IGH/IGK/IGL undergo somatic hypermutation, so identical-CDR3 clone calling
## is over-strict for them (one clone can split into near-neighbour variants).
## The Definition / Sharing plots surface this as a subtitle caveat.
ir_is_bcr_chain <- function(chain) {
  is.character(chain) &&
    length(chain) == 1 &&
    !is.na(chain) &&
    any(startsWith(chain, IR_BCR_CHAINS))
}

## ---- BCR caveat line appended to plot subtitles ------------------------ ##
IR_BCR_SHM_CAVEAT <- "BCR: CDR3 not collapsed by SHM; clones may be split."

## ---- Build the Definition (resolution waterfall) ggplot ---------------- ##
## Parses V/J/CDR3 for `chain`, counts entities at the 7 resolution levels
## (optionally within `group_by`), and returns the bar ggplot. Returns NULL
## when there are no cells for the chain (caller renders the empty state).
## Shared by the live renderer and the Example-modal demo.
ir_build_definition_plot <- function(data, chain, group_by = NULL) {
  seg <- ir_parse_segments(data, chain)
  if (is.null(seg) || nrow(seg) == 0) {
    return(NULL)
  }
  df <- ir_definition_counts(seg, group = group_by)
  subtitle <- "V = V gene; J = J gene; CDR3 = complementarity region 3."
  if (ir_is_bcr_chain(chain)) {
    subtitle <- paste(subtitle, IR_BCR_SHM_CAVEAT, sep = "\n")
  }
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = definition, y = n, fill = definition)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(n)),
      vjust = -0.3,
      size = 3
    ) +
    ggplot2::scale_fill_manual(
      values = cerebro_group_colors(length(unique(df$definition)))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15)),
      labels = scales::comma
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Unique count",
      title = "Clone definition resolution",
      subtitle = subtitle
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "none"
    )
  if (!is.null(group_by) && nzchar(group_by) && group_by %in% colnames(df)) {
    p <- p +
      ggplot2::facet_wrap(stats::as.formula(paste0("~ `", group_by, "`")))
  }
  p
}

## ---- Friendly display labels for the sharing classes ------------------- ##
## The data layer (ir_sharing_classify) keeps immunology-standard labels
## (Private / Public (within-group) / Public (cross-group)); the plot maps
## them to self-explanatory axis labels. Two-class mode reuses the first two.
IR_SHARING_DISPLAY_LABELS <- c(
  "Private" = "Private (1 sample)",
  "Public (within-group)" = "Shared within group",
  "Public (cross-group)" = "Shared across groups",
  "Public" = "Shared (≥ 2 samples)"
)

## ---- Build the Clone Sharing ggplot ------------------------------------ ##
## Classifies each clonotype (via ir_sharing_classify) and bars the class
## counts, using friendly x-axis labels. Returns NULL on empty data or when
## the unit column is absent. Shared by the live renderer and the demo.
ir_build_sharing_plot <- function(data, chain, unit_col, group_by = NULL) {
  seg <- ir_parse_segments(data, chain)
  if (is.null(seg) || nrow(seg) == 0 || !(unit_col %in% colnames(seg))) {
    return(NULL)
  }
  cls <- ir_sharing_classify(seg, unit_col = unit_col, group_col = group_by)
  if (is.null(cls)) {
    return(NULL)
  }
  counts <- as.data.frame(table(sharing = cls$sharing))
  counts$pct <- counts$Freq / sum(counts$Freq) * 100
  # Map the raw class labels to friendly display labels, preserving order.
  counts$display <- factor(
    IR_SHARING_DISPLAY_LABELS[as.character(counts$sharing)],
    levels = IR_SHARING_DISPLAY_LABELS[levels(counts$sharing)]
  )
  # Clean, human-readable hover text. ggplotly otherwise derives the tooltip
  # from the raw aes names (display, Freq) and repeats `display` because both
  # the bar fill and the geom_text label map it — so we supply an explicit
  # `text` aes and pass tooltip = "text" to ggplotly (see visualizations.R).
  counts$tooltip <- sprintf(
    "%s\n%d clonotypes (%.1f%%)",
    as.character(counts$display),
    counts$Freq,
    counts$pct
  )
  same_col <- !is.null(group_by) &&
    nzchar(group_by) &&
    identical(group_by, unit_col)
  subtitle <- paste(
    "Each clonotype = one receptor.",
    "Private = in a single unit; Shared = in ≥ 2 units."
  )
  if (same_col) {
    subtitle <- paste(
      subtitle,
      "Group and sharing unit are the same column; within/cross is undefined.",
      sep = "\n"
    )
  } else if (is.null(group_by) || !nzchar(group_by)) {
    subtitle <- paste(
      subtitle,
      "No group selected — showing Private / Shared only.",
      sep = "\n"
    )
  }
  if (ir_is_bcr_chain(chain)) {
    subtitle <- paste(subtitle, IR_BCR_SHM_CAVEAT, sep = "\n")
  }
  counts$label <- sprintf("%d (%.1f%%)", counts$Freq, counts$pct)
  # Lift the labels a fixed fraction of the tallest bar above each bar top, so
  # they clear the bar edge and a zero-height (Freq == 0) bar's label is not
  # clamped onto the axis. vjust = 0 anchors the label bottom at that y.
  label_lift <- 0.04 * max(counts$Freq, 1)
  ggplot2::ggplot(
    counts,
    ggplot2::aes(x = display, y = Freq, fill = display)
  ) +
    # `text` is not a ggplot aesthetic (ggplot warns and ignores it on static
    # render); it exists purely to feed ggplotly's hover tooltip. Suppress the
    # known, harmless "unknown aesthetics: text" warning so it doesn't noise up
    # test output.
    suppressWarnings(ggplot2::geom_col(
      ggplot2::aes(text = tooltip),
      width = 0.6
    )) +
    ggplot2::geom_text(
      ggplot2::aes(label = label, y = Freq + label_lift),
      vjust = 0,
      size = 3.2
    ) +
    ggplot2::scale_fill_manual(
      values = cerebro_group_colors(length(unique(counts$display)))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Number of clonotypes",
      # Clear the fill legend title only (keep the legend itself): ggplotly
      # ignores the static legend.position = "none" and shows the legend, and
      # without this its title falls back to the raw fill column name ("display").
      fill = NULL,
      title = "Clonotype sharing",
      subtitle = subtitle
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "none")
}
