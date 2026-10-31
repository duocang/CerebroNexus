## Immune receptor (AIRR) data in AnnData / MuData files -----------------------
##
## scirpy keeps the receptor chains of every cell as an awkward array in
## `obsm["airr"]`: one (possibly empty) list of chains per cell, each chain a
## record of AIRR Rearrangement fields (locus, v_call, junction_aa, ...). anndata
## writes an awkward array as a group of flat buffers plus the array's "form"
## (a JSON description of its nesting) in an attribute:
##
##   obsm/airr            attrs: encoding-type = "awkward-array", form, length
##   obsm/airr/node0-offsets, node4-data, node2-index, ...
##
## The buffers are named <form_key>-<part>, so the form is all that is needed to
## rebuild the array. This is read natively with rhdf5 -- no Python and no
## awkward -- into one flat table with a row per chain, then assembled into the
## per-cell clone calls the Viewer reads (scRepertoire's job, as on the Seurat
## route).

#' Take elements by 0-based position; a negative position gives NA. The same
#' positions are applied to every field of a record.
#' @keywords internal
#' @noRd
.awkTake <- function(x, index) {
  if (inherits(x, "awk_record")) {
    return(structure(
      lapply(unclass(x), .awkTake, index = index),
      class = "awk_record"
    ))
  }
  if (inherits(x, "awk_list")) {
    stop("Cannot index into a nested awkward list.", call. = FALSE)
  }
  position <- index + 1
  position[index < 0 | is.na(index)] <- NA_real_
  x[position]
}

#' @keywords internal
#' @noRd
.awkLength <- function(x) {
  if (inherits(x, "awk_record")) {
    return(if (length(x)) .awkLength(x[[1L]]) else 0L)
  }
  length(x)
}

#' Rebuild an awkward node from its form. Returns a vector for leaves, an
#' `awk_record` (a named list of equally long nodes) for records, and an
#' `awk_list` (starts, stops, content) for lists; strings are lists of bytes and
#' come back as character vectors.
#' @keywords internal
#' @noRd
.awkDecode <- function(ctx, container, form) {
  buffer <- function(part) {
    key <- form[["form_key"]]
    if (is.null(key)) {
      stop("awkward form node without a form_key.", call. = FALSE)
    }
    value <- .h5adRead(
      ctx,
      paste0(container, "/", key, "-", part),
      bit64conversion = "double"
    )
    .h5adVector(value)
  }
  class_name <- form[["class"]]
  parameters <- form[["parameters"]]
  array_type <- if (is.null(parameters[["__array__"]])) {
    ""
  } else {
    parameters[["__array__"]]
  }
  decode_content <- function() .awkDecode(ctx, container, form[["content"]])

  switch(
    class_name,
    NumpyArray = {
      if (length(form[["inner_shape"]])) {
        stop(
          "Multi-dimensional awkward NumpyArray is not supported.",
          call. = FALSE
        )
      }
      value <- buffer("data")
      if (identical(form[["primitive"]], "bool")) as.logical(value) else value
    },
    EmptyArray = logical(0),
    ListOffsetArray = ,
    ListArray = {
      if (identical(class_name, "ListOffsetArray")) {
        offsets <- buffer("offsets")
        starts <- offsets[-length(offsets)]
        stops <- offsets[-1L]
      } else {
        starts <- buffer("starts")
        stops <- buffer("stops")
      }
      if (array_type %in% c("string", "bytestring")) {
        .awkBytesToStrings(decode_content(), starts, stops)
      } else {
        structure(
          list(starts = starts, stops = stops, content = decode_content()),
          class = "awk_list"
        )
      }
    },
    RegularArray = {
      size <- form[["size"]]
      content <- decode_content()
      count <- if (size > 0) .awkLength(content) %/% size else 0L
      starts <- seq_len(count) * size - size
      stops <- starts + size
      if (array_type %in% c("string", "bytestring")) {
        .awkBytesToStrings(content, starts, stops)
      } else {
        structure(
          list(starts = starts, stops = stops, content = content),
          class = "awk_list"
        )
      }
    },
    IndexedArray = ,
    IndexedOptionArray = .awkTake(decode_content(), buffer("index")),
    ByteMaskedArray = {
      valid <- as.logical(buffer("mask")) == isTRUE(form[["valid_when"]])
      content <- decode_content()
      .awkTake(content, ifelse(valid, seq_along(valid) - 1, -1))
    },
    BitMaskedArray = {
      bits <- as.logical(rawToBits(as.raw(buffer("mask"))))
      if (!isTRUE(form[["lsb_order"]])) {
        bits <- as.vector(matrix(bits, nrow = 8L)[8:1, , drop = FALSE])
      }
      content <- decode_content()
      n <- .awkLength(content)
      valid <- bits[seq_len(n)] == isTRUE(form[["valid_when"]])
      .awkTake(content, ifelse(valid, seq_len(n) - 1, -1))
    },
    UnmaskedArray = decode_content(),
    RecordArray = {
      fields <- form[["fields"]]
      contents <- form[["contents"]]
      if (is.null(fields)) {
        fields <- as.character(seq_along(contents) - 1L)
      }
      columns <- lapply(contents, function(content) {
        .awkDecode(ctx, container, content)
      })
      names(columns) <- unlist(fields)
      structure(columns, class = "awk_record")
    },
    stop(
      "Unsupported awkward array layout `",
      class_name,
      "` in `",
      container,
      "`.",
      call. = FALSE
    )
  )
}

#' Strings out of a flat byte buffer: bytes[starts[i]:stops[i]) of each element.
#' @keywords internal
#' @noRd
.awkBytesToStrings <- function(bytes, starts, stops) {
  n <- length(starts)
  if (!n) {
    return(character(0))
  }
  if (!length(bytes) || all(starts == stops)) {
    return(rep("", n))
  }
  ## All-ASCII buffers (gene names, sequences) are cut by position in one go;
  ## anything else is decoded one string at a time.
  if (all(bytes < 128L)) {
    whole <- rawToChar(as.raw(bytes))
    return(substring(whole, starts + 1, stops))
  }
  raw_bytes <- as.raw(bytes)
  out <- vapply(
    seq_len(n),
    function(i) {
      if (stops[[i]] > starts[[i]]) {
        rawToChar(raw_bytes[(starts[[i]] + 1):stops[[i]]])
      } else {
        ""
      }
    },
    character(1)
  )
  Encoding(out) <- "UTF-8"
  out
}

#' Read an awkward array of records-in-lists (one list per row, e.g. the chains
#' of a cell) as a flat table with a row per element plus `row`, the index of
#' the list it came from.
#' @return A list with `n` (rows of the array) and `chains` (data.frame).
#' @keywords internal
#' @noRd
.h5adReadAwkwardChains <- function(ctx, path) {
  attrs <- .h5adAttrs(ctx, path)
  if (!identical(as.character(attrs[["encoding-type"]]), "awkward-array")) {
    stop("`", path, "` is not an awkward array.", call. = FALSE)
  }
  form <- jsonlite::fromJSON(
    as.character(attrs[["form"]]),
    simplifyVector = FALSE
  )
  n <- as.integer(attrs[["length"]])
  top <- .awkDecode(ctx, path, form)
  if (!inherits(top, "awk_list") || !inherits(top$content, "awk_record")) {
    stop(
      "`",
      path,
      "` is not a list of records per cell; expected scirpy's AIRR layout.",
      call. = FALSE
    )
  }
  counts <- as.integer(top$stops - top$starts)
  row <- rep.int(seq_len(n), counts)
  within <- sequence(counts)
  position <- rep(top$starts, counts) + within - 1
  records <- .awkTake(top$content, position)
  fields <- names(records)
  keep <- vapply(
    unclass(records)[fields],
    function(column) is.atomic(column) && length(column) == length(row),
    logical(1)
  )
  chains <- data.frame(row = row, stringsAsFactors = FALSE)
  for (field in fields[keep]) {
    chains[[field]] <- unclass(records)[[field]]
  }
  list(n = n, chains = chains)
}

## From chains to clone calls ----------------------------------------------------

.airrTcrLoci <- c("TRA", "TRB", "TRG", "TRD")
.airrBcrLoci <- c("IGH", "IGK", "IGL")

#' First of `fields` present in the chain table, empty strings as NA; NA when
#' none is there.
#' @keywords internal
#' @noRd
.airrField <- function(chains, fields) {
  for (field in fields) {
    if (field %in% names(chains)) {
      value <- chains[[field]]
      if (is.character(value)) {
        value[!nzchar(value)] <- NA_character_
      }
      return(value)
    }
  }
  rep(NA, nrow(chains))
}

#' Chains as 10x-style contig rows, the input scRepertoire assembles into
#' clones. `junction_aa` / `junction` (CDR3 including the conserved C and F/W)
#' are what 10x calls `cdr3` / `cdr3_nt`.
#' @keywords internal
#' @noRd
.airrChainsToContigs <- function(chains) {
  gene <- function(x) {
    x <- .airrField(chains, x)
    x[is.na(x)] <- "None"
    x
  }
  productive <- if ("productive" %in% names(chains)) {
    flag <- chains$productive
    if (is.character(flag)) {
      flag <- tolower(flag) %in% c("t", "true", "1", "yes")
    }
    flag %in% TRUE
  } else {
    rep(TRUE, nrow(chains))
  }
  data.frame(
    barcode = chains$barcode,
    is_cell = TRUE,
    contig_id = paste0(chains$barcode, "_contig_", seq_len(nrow(chains))),
    high_confidence = TRUE,
    length = NA_integer_,
    chain = chains$locus,
    v_gene = gene("v_call"),
    d_gene = gene("d_call"),
    j_gene = gene("j_call"),
    c_gene = gene("c_call"),
    full_length = TRUE,
    productive = productive,
    cdr3 = .airrField(chains, c("junction_aa", "cdr3_aa")),
    cdr3_nt = .airrField(chains, c("junction", "cdr3")),
    reads = as.numeric(.airrField(chains, "consensus_count")),
    umis = as.numeric(.airrField(chains, "duplicate_count")),
    raw_clonotype_id = NA_character_,
    raw_consensus_id = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' Assemble AIRR chains into the unified repertoire: a list named by sample, each
#' element one row per cell with barcode, CTgene, CTnt, CTaa and CTstrict.
#'
#' @param chains Chain table from `.h5adReadAwkwardChains()`.
#' @param airr_cells Names of the cells the `row` index refers to.
#' @param cells Cells of the expression object; chains of other cells are dropped.
#' @param sample Sample of each entry of `cells` (named by cell).
#' @keywords internal
#' @noRd
.airrToRepertoire <- function(
  chains,
  airr_cells,
  cells,
  sample,
  verbose = TRUE
) {
  if (!requireNamespace("scRepertoire", quietly = TRUE)) {
    stop(
      "Importing immune receptor data from AnnData needs the scRepertoire ",
      "package, which assembles chains into clones. Install it, or set ",
      "`airr = FALSE` to skip the receptor data.",
      call. = FALSE
    )
  }
  if (!"locus" %in% names(chains)) {
    stop("AIRR chains have no `locus` field.", call. = FALSE)
  }
  chains$barcode <- airr_cells[chains$row]
  total <- nrow(chains)
  chains <- chains[chains$barcode %in% cells, , drop = FALSE]
  not_in_expression <- total - nrow(chains)
  chains$locus <- as.character(chains$locus)
  chains <- chains[
    chains$locus %in% c(.airrTcrLoci, .airrBcrLoci),
    ,
    drop = FALSE
  ]
  chains <- chains[
    !is.na(.airrField(chains, c("junction_aa", "cdr3_aa"))),
    ,
    drop = FALSE
  ]

  if (!nrow(chains)) {
    if (verbose) {
      message(
        "[INFO] AIRR data holds no chain with a CDR3 for the cells read; skipped."
      )
    }
    return(NULL)
  }

  ## A cell carries one receptor type. A cell that shows both (a doublet, or
  ## ambient RNA) is given to the type with more chains, then more UMIs.
  is_tcr <- chains$locus %in% .airrTcrLoci
  umis <- if ("duplicate_count" %in% names(chains)) {
    as.numeric(chains$duplicate_count)
  } else {
    rep(0, nrow(chains))
  }
  umis[is.na(umis)] <- 0
  per_cell <- function(flag) {
    list(
      chains = tapply(flag, chains$barcode, sum),
      umis = tapply(ifelse(flag, umis, 0), chains$barcode, sum)
    )
  }
  tcr <- per_cell(is_tcr)
  bcr <- per_cell(!is_tcr)
  tcr_wins <- (tcr$chains > bcr$chains) |
    (tcr$chains == bcr$chains & tcr$umis >= bcr$umis)
  cell_is_tcr <- stats::setNames(as.vector(tcr_wins), names(tcr_wins))
  conflicting <- sum((tcr$chains > 0) & (bcr$chains > 0))
  chains <- chains[is_tcr == cell_is_tcr[chains$barcode], , drop = FALSE]
  is_tcr <- chains$locus %in% .airrTcrLoci

  assemble <- function(rows, kind) {
    if (!any(rows)) {
      return(NULL)
    }
    part <- chains[rows, , drop = FALSE]
    contigs <- .airrChainsToContigs(part)
    samples <- unname(sample[part$barcode])
    pieces <- lapply(split(contigs, samples), function(contig) {
      contig[order(contig$barcode), , drop = FALSE]
    })
    sample_names <- names(pieces)
    combined <- suppressWarnings(
      if (identical(kind, "TCR")) {
        scRepertoire::combineTCR(unname(pieces), samples = sample_names)
      } else {
        scRepertoire::combineBCR(unname(pieces), samples = sample_names)
      }
    )
    ## combine*(samples =) prefixes the barcodes; the cell names here are the
    ## ones the object already has, so take the prefix off again.
    for (name in names(combined)) {
      prefix <- paste0(name, "_")
      barcode <- as.character(combined[[name]]$barcode)
      has_prefix <- startsWith(barcode, prefix)
      barcode[has_prefix] <- substring(barcode[has_prefix], nchar(prefix) + 1L)
      combined[[name]]$barcode <- barcode
    }
    combined
  }

  repertoire <- .mergeRepertoiresBySample(
    assemble(is_tcr, "TCR"),
    assemble(!is_tcr, "BCR")
  )
  repertoire <- .dropEmptyRepertoireSamples(repertoire)
  if (verbose) {
    n_cells <- sum(vapply(repertoire, nrow, integer(1)))
    message(
      "[INFO] Immune receptors: ",
      n_cells,
      " cells in ",
      length(repertoire),
      " sample(s)",
      if (not_in_expression) {
        paste0(
          "; ",
          not_in_expression,
          " chain(s) of cells absent from the expression data dropped"
        )
      },
      if (conflicting) {
        paste0(
          "; ",
          conflicting,
          " cell(s) with both TCR and BCR chains assigned to one type"
        )
      },
      "."
    )
  }
  repertoire
}

## Locating the AIRR data in a file -----------------------------------------------

#' Which AIRR source to read: a MuData modality, or an obsm entry of the
#' expression AnnData. Returns NULL when there is none (and `airr` is "auto").
#' @return NULL, or list(kind = "modality"|"obsm", name = ...).
#' @keywords internal
#' @noRd
.h5adFindAirr <- function(ctx, airr, available_modalities, file) {
  if (is.null(airr) || isFALSE(airr)) {
    return(NULL)
  }
  if (!(is.character(airr) && length(airr) == 1L && !is.na(airr))) {
    stop(
      "`airr` must be \"auto\", FALSE, or the name of the AIRR modality (or ",
      "obsm entry).",
      call. = FALSE
    )
  }
  explicit <- !identical(airr, "auto")
  if (length(available_modalities)) {
    if (explicit) {
      if (!airr %in% available_modalities) {
        stop(
          "AIRR modality `",
          airr,
          "` not found; the file has: ",
          paste(available_modalities, collapse = ", "),
          call. = FALSE
        )
      }
      return(list(kind = "modality", name = airr))
    }
    hit <- available_modalities[tolower(available_modalities) == "airr"]
    if (length(hit)) {
      return(list(kind = "modality", name = hit[[1L]]))
    }
    return(NULL)
  }
  entry <- if (explicit) airr else "airr"
  path <- paste0("obsm/", entry)
  if (
    .h5adHas(ctx, path) && identical(.h5adEncoding(ctx, path), "awkward-array")
  ) {
    return(list(kind = "obsm", name = entry))
  }
  if (explicit) {
    stop(
      "`airr = \"",
      airr,
      "\"` is not an awkward-array entry of obsm in `",
      file,
      "`.",
      call. = FALSE
    )
  }
  NULL
}

#' Sample column: the one asked for, else the usual names, else a single sample.
#' @keywords internal
#' @noRd
.airrSampleOf <- function(col_data, cells, sample_col) {
  if (!is.null(sample_col)) {
    if (
      !is.character(sample_col) ||
        length(sample_col) != 1L ||
        !sample_col %in% names(col_data)
    ) {
      stop(
        "`airr_sample` must name a column of obs: ",
        paste(names(col_data), collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    found <- intersect(
      c("orig.ident", "sample", "Sample", "sample_id"),
      names(col_data)
    )
    sample_col <- if (length(found)) found[[1L]] else NULL
  }
  if (is.null(sample_col)) {
    return(stats::setNames(rep("Sample_1", length(cells)), cells))
  }
  value <- as.character(col_data[[sample_col]])
  value[is.na(value) | !nzchar(value)] <- "unknown"
  stats::setNames(value, cells)
}

#' Read the immune repertoire of a file into the unified shape, or NULL.
#' @param ctx_expression Context of the expression AnnData (for obsm["airr"]).
#' @keywords internal
#' @noRd
.h5adReadRepertoire <- function(
  file,
  ctx_expression,
  source,
  cells,
  col_data,
  sample_col = NULL,
  verbose = FALSE
) {
  if (identical(source$kind, "modality")) {
    ctx <- .h5adContext(file, root = paste0("mod/", source$name))
    candidates <- c("airr", .h5adChildren(ctx, "obsm"))
    path <- paste0(
      "obsm/",
      candidates[vapply(
        candidates,
        function(entry) {
          .h5adHas(ctx, paste0("obsm/", entry)) &&
            identical(
              .h5adEncoding(ctx, paste0("obsm/", entry)),
              "awkward-array"
            )
        },
        logical(1)
      )][1L]
    )
    if (identical(path, "obsm/NA")) {
      stop(
        "Modality `",
        source$name,
        "` has no awkward-array AIRR data in obsm.",
        call. = FALSE
      )
    }
    airr_cells <- .h5adReadDataFrame(ctx, "obs", verbose)$index
  } else {
    ctx <- ctx_expression
    path <- paste0("obsm/", source$name)
    airr_cells <- cells
  }
  decoded <- .h5adReadAwkwardChains(ctx, path)
  if (decoded$n != length(airr_cells)) {
    stop(
      "AIRR array has ",
      decoded$n,
      " rows but its AnnData has ",
      length(airr_cells),
      " cells.",
      call. = FALSE
    )
  }
  if (!any(airr_cells %in% cells)) {
    stop(
      "No cell of the AIRR data (e.g. ",
      paste(utils::head(airr_cells, 2L), collapse = ", "),
      ") is a cell of the expression data (e.g. ",
      paste(utils::head(cells, 2L), collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  .airrToRepertoire(
    decoded$chains,
    airr_cells = airr_cells,
    cells = cells,
    sample = .airrSampleOf(col_data, cells, sample_col),
    verbose = verbose
  )
}
