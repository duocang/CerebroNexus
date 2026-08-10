##----------------------------------------------------------------------------##
## Coordinated views (Linked Views) — per-dataset bundle builders.
##
## Pure functions that turn a loaded Cerebro object into ONE serialisable bundle
## (cells, categorical groups, every 2-D "space", per-cell clone identity). They
## carry NO input/output/session/reactive dependency, so this file is sourced by
## server.R (source(..., local = TRUE)) at runtime AND unit-tested in isolation
## (tests/testthat/test-coordinated-views.R). cv_build_bundle() tolerates the
## app-only helpers (reactive_colors / cerebro_group_colors / Cerebro.options)
## being absent — each is guarded — so it produces the same structure outside
## the running app, just with the fallback palette.
##
## The I()/AsIs wrapping in cv_group / cv_space / cv_clone is the load-bearing
## invariant: shiny serialises the bundle with auto_unbox = TRUE, so any array
## field that happens to be length 1 (a single-level group, a single-clonotype
## data set) must be forced to a JSON array here, or the client indexes a bare
## scalar and throws mid-update, leaving the previous data set on screen.
##----------------------------------------------------------------------------##

## Null-coalescing helper (local, so we don't depend on rlang/shiny exporting it).
`%||%` <- function(a, b) if (is.null(a)) b else a

## Categorical palette (mirrors the app's plotly categorical colours).
cv_palette <- c(
  "#636EFA",
  "#EF553B",
  "#00CC96",
  "#AB63FA",
  "#FFA15A",
  "#19D3F3",
  "#FF6692",
  "#B6E880",
  "#FF97FF",
  "#FECB52",
  "#2f6fd6",
  "#f97316",
  "#16a34a",
  "#9a5cd0",
  "#e05780",
  "#38b2ac",
  "#d97706",
  "#7bb0e8"
)
cv_colors_for <- function(levels) {
  n <- length(levels)
  cv_palette[((seq_len(n) - 1) %% length(cv_palette)) + 1]
}

## Per-cell clone identity + human label (CTaa) from the IR list, aligned to
## `cells`. NA where a cell carries no receptor of the selected class.
##
## The clone column and the receptor scoping both come from clone_contract.R,
## not from here. This used to read CTstrict and take every receptor at once,
## while the Clonal UMAP read CTgene within one receptor -- so the two pages
## disagreed about which cells share a clone, and the stricter column split
## clones the other page reported as one. `receptor` defaults to whichever class
## the Clonal UMAP would offer first, so the default views match.
cv_clone_per_cell <- function(ir, cells, receptor = NULL) {
  if (is.null(ir) || !length(ir)) {
    return(NULL)
  }
  present <- cerebro_receptors_present(ir)
  if (is.null(receptor)) {
    receptor <- if (length(present)) present[1] else "TCR"
  }
  clone_col <- cerebro_clonecall_col()
  rows <- do.call(
    rbind,
    lapply(ir, function(df) {
      if (
        is.null(df) ||
          !("barcode" %in% names(df)) ||
          !(clone_col %in% names(df))
      ) {
        return(NULL)
      }
      keep <- cerebro_rows_in_receptor(df, receptor, clone_col)
      df <- df[keep, , drop = FALSE]
      if (!nrow(df)) {
        return(NULL)
      }
      data.frame(
        barcode = as.character(df$barcode),
        clone = as.character(df[[clone_col]]),
        CTaa = if ("CTaa" %in% names(df)) {
          as.character(df$CTaa)
        } else {
          as.character(df[[clone_col]])
        },
        stringsAsFactors = FALSE
      )
    })
  )
  if (is.null(rows) || !nrow(rows)) {
    return(NULL)
  }
  rows <- rows[!is.na(rows$clone) & nzchar(rows$clone), , drop = FALSE]
  rows <- rows[!duplicated(rows$barcode), , drop = FALSE]
  idx <- match(cells, rows$barcode)
  list(clone = rows$clone[idx], ctaa = rows$CTaa[idx], receptor = receptor)
}

## Resolve a configured image only inside the app's public image roots. The
## canonical containment check is deliberately performed after symlink
## resolution: a configured `spatial-assets/link.png` must not become a read
## primitive for a file outside the generated app.
cv_authorized_external_image_path <- function(path, cerebro_root) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !is.character(cerebro_root) ||
      length(cerebro_root) != 1L ||
      is.na(cerebro_root)
  ) {
    return(NULL)
  }
  canonicalize <- function(candidate) {
    tryCatch(
      suppressWarnings(
        normalizePath(candidate, winslash = "/", mustWork = TRUE)
      ),
      error = function(error) NULL
    )
  }
  image_path <- canonicalize(file.path(cerebro_root, path))
  trusted_roots <- Filter(
    Negate(is.null),
    lapply(
      c("spatial-assets", "extdata"),
      function(root) canonicalize(file.path(cerebro_root, root))
    )
  )
  if (is.null(image_path) || !length(trusted_roots)) {
    return(NULL)
  }
  comparison_path <- image_path
  if (.Platform$OS.type == "windows") {
    comparison_path <- tolower(comparison_path)
    trusted_roots <- lapply(trusted_roots, tolower)
  }
  inside <- any(vapply(
    trusted_roots,
    function(root) {
      startsWith(comparison_path, paste0(sub("/+$", "", root), "/"))
    },
    logical(1)
  ))
  if (!inside) NULL else image_path
}

## Resolve an EXTERNAL histology image (Cerebro.options$spatial_images) for the
## CURRENTLY selected dataset, plus its alignment preset, base64-encoded so the
## browser can show it. Returns list(uri, preset) or NULL. The preset offset is
## in DATA units and scale is a unitless multiplier (same contract the Spatial
## page uses), so it transfers to the coordinated-views canvas unchanged.
cv_external_images <- function() {
  if (
    !exists("Cerebro.options") ||
      is.null(Cerebro.options[["spatial_images"]])
  ) {
    return(list())
  }
  si <- Cerebro.options[["spatial_images"]]
  nm <- NULL
  if (exists("available_crb_files") && !is.null(available_crb_files$selected)) {
    sel <- available_crb_files$selected
    idx <- which(available_crb_files$files == sel)
    if (length(idx)) {
      nm <- names(available_crb_files$files)[idx[1]]
      if (is.null(nm) || is.na(nm)) {
        nm <- available_crb_files$names[idx[1]]
      }
    }
  }
  if (is.null(nm) || is.na(nm) || !(nm %in% names(si))) {
    return(list())
  }
  paths <- si[[nm]]
  root <- Cerebro.options[["cerebro_root"]]
  pget <- function(key, d) {
    v <- Cerebro.options[[key]]
    if (is.null(v) || is.null(names(v)) || !(nm %in% names(v))) d else v[[nm]]
  }
  ## The alignment options are indexed by DATA SET, not by image, so every
  ## external image of one data set starts from the same numbers. That is the
  ## configuration contract as it stands; per-image presets would need a new
  ## option shape, and the client remembers what the user does to each image
  ## separately regardless.
  preset <- list(
    offsetX = as.numeric(pget("spatial_images_offset_x", 0)),
    offsetY = as.numeric(pget("spatial_images_offset_y", 0)),
    scaleX = as.numeric(pget("spatial_images_scale_x", 1)),
    scaleY = as.numeric(pget("spatial_images_scale_y", 1)),
    flipX = isTRUE(pget("spatial_images_flip_x", FALSE)),
    flipY = isTRUE(pget("spatial_images_flip_y", FALSE)),
    opacity = 0.6
  )
  out <- list()
  for (i in seq_along(paths)) {
    path <- paths[i]
    img_path <- cv_authorized_external_image_path(path, root)
    if (is.null(img_path) || !requireNamespace("base64enc", quietly = TRUE)) {
      next
    }
    ext <- tolower(tools::file_ext(img_path))
    mime <- switch(
      ext,
      "jpg" = "image/jpeg",
      "jpeg" = "image/jpeg",
      "png" = "image/png",
      "svg" = "image/svg+xml",
      "image/png"
    )
    base <- basename(path)
    label <- if (!is.null(names(paths)) && nzchar(names(paths)[i] %||% "")) {
      names(paths)[i]
    } else {
      base
    }
    out[[length(out) + 1]] <- list(
      ## The index keeps two files of the same basename apart, and keeps the id
      ## stable: it is the position in the configured list, not a hash of
      ## content or a counter that moves when another data set is opened.
      id = paste0("ext", i, ":", base),
      label = label,
      uri = paste0(
        "data:",
        mime,
        ";base64,",
        base64enc::base64encode(img_path)
      ),
      preset = preset
    )
  }
  out
}

## Kept for callers wanting only the primary image (and for the previous
## single-image contract): the first external image, or NULL.
cv_external_image <- function() {
  imgs <- cv_external_images()
  if (!length(imgs)) NULL else imgs[[1]]
}

## Bundle constructors — the ONE place the "which fields are JS arrays" contract
## lives. shiny serialises the bundle with auto_unbox = TRUE, which is correct for
## the genuine scalars (n, K, defaults, max, ...) but wrong for any vector the
## client indexes as an array: a length-1 vector (a single-level group, a
## single-clonotype data set) would serialise as a bare JSON scalar, the client
## does g.levels.map()/D.clone.size[r] and throws, aborting onData mid-update and
## leaving the previous data set on screen. I() forces array serialisation. Wrap
## every array-typed field HERE so the invariant is structural — impossible to
## forget when a new field is added — instead of a scattered, easy-to-miss I().
cv_group <- function(values, levels, colors) {
  list(values = I(values), levels = I(levels), colors = I(colors))
}
cv_space <- function(id, label, x, y) {
  list(id = id, label = label, x = I(x), y = I(y))
}

## A continuous colouring. `v` is quantised to 0..scale (an integer vector keeps
## the bundle small); `min`/`max` carry the TRUE range so the client can render a
## real-valued colourbar and hover value instead of the quantised index. Trekker's
## own fields arrive pre-quantised to 0-255, hence the per-field `scale` rather
## than one global constant. Must match FIELD_PREFIX in www/coordviews.js.
cv_field_mode <- "__field__"
cv_field_scale <- 1000L
cv_field <- function(
  label,
  v,
  min,
  max,
  scale = cv_field_scale,
  source = NULL,
  desc = NULL,
  by_type = NULL
) {
  list(
    label = label,
    v = I(v),
    min = min,
    max = max,
    scale = scale,
    source = source,
    desc = desc,
    by_type = by_type
  )
}

cv_trekker_by_type <- function(value) {
  if (is.null(value) || !length(value)) {
    return(NULL)
  }
  if (is.list(value) && all(vapply(value, is.list, logical(1)))) {
    return(I(unname(value)))
  }
  labels <- names(value) %||% dimnames(value)[[1]]
  if (is.null(labels) || length(labels) != length(value)) {
    return(NULL)
  }
  I(Map(
    function(type, median) list(type = type, median = as.numeric(median)),
    as.character(labels),
    as.numeric(value)
  ))
}
cv_clone <- function(
  id,
  label,
  size,
  n_clones,
  n_receptor,
  receptor = NA_character_,
  n_cdr3 = NULL
) {
  ## id/label/size are arrays; n_clones/n_receptor are true scalars (left bare).
  ## The expansion bins travel WITH the data: the client re-lays-out the clone
  ## space between representations without a server round-trip, and a second
  ## copy of the thresholds in JavaScript is a second thing to keep in step with
  ## clone_contract.R. `tiers` are the upper bounds, `tier_labels` their names.
  list(
    id = I(id),
    label = I(label),
    size = I(size),
    ## How many distinct CDR3s each clone covers. 1 means the label names the
    ## clone exactly; more means it names the clone's dominant sequence, and the
    ## client has to say so rather than present it as the clonotype.
    n_cdr3 = I(if (is.null(n_cdr3)) rep(1L, length(size)) else n_cdr3),
    n_clones = n_clones,
    n_receptor = n_receptor,
    receptor = receptor,
    tiers = I(utils::head(CEREBRO_CLONE_BINS[-1], -1)),
    tier_labels = I(CEREBRO_CLONE_LABELS)
  )
}

## Categorical groupings: each md group column -> {values, levels, colors},
## coloured via colors_fn (the caller's reactive_colors()-backed resolver).
cv_build_groups <- function(crb, md, colors_fn) {
  group_names <- tryCatch(crb$getGroups(), error = function(e) character(0))
  groups <- list()
  for (g in group_names) {
    v <- md[[g]]
    if (is.null(v)) {
      next
    }
    lev <- if (is.factor(v)) levels(v) else sort(unique(as.character(v)))
    lev <- lev[!is.na(lev)]
    if (!length(lev)) {
      next
    }
    groups[[g]] <- cv_group(
      match(as.character(v), lev) - 1L,
      lev,
      colors_fn(g, lev)
    )
  }
  groups
}

## Continuous colourings from the meta data — every numeric column, aligned to
## the meta data's row order (which IS `cells` order). This is the Projection
## tab's "Color cells by" list minus the categorical columns: it deliberately
## includes the QC columns (nUMI / nGene / percent.mt / nCount_*), because
## "colour the embedding by percent.mt and see which blob is junk" is one of the
## most-used actions on that page. Constant and all-NA columns are skipped —
## there is no colouring to build from them.
cv_build_fields <- function(md, skip = "cell_barcode") {
  fields <- list()
  for (mc in colnames(md)) {
    if (mc %in% skip) {
      next
    }
    v <- md[[mc]]
    if (!is.numeric(v)) {
      next
    }
    rng <- suppressWarnings(range(v, na.rm = TRUE))
    if (!all(is.finite(rng)) || rng[2] <= rng[1]) {
      next
    }
    q <- as.integer(round((v - rng[1]) / (rng[2] - rng[1]) * cv_field_scale))
    fields[[paste0("meta:", mc)]] <- cv_field(
      mc,
      q,
      round(rng[1], 4),
      round(rng[2], 4)
    )
  }
  fields
}

## Categorical columns that are NOT registered grouping variables. The Projection
## tab offers every meta column in "Color cells by" while its "Group filters" box
## only lists getGroups(); this mirrors that split — these are colourings (legend,
## legend-hiding) but they do not become filters.
##
## Returns list(groups, skipped). A column with (nearly) as many levels as cells
## is an identifier rather than a grouping: one colour per cell, and a legend
## thousands of rows long. Those cannot be coloured by, but they are REPORTED
## instead of dropped — `skipped` is name -> level count, which the client shows
## greyed out in the picker. Silently omitting them left the two tabs offering
## different lists with no way to tell why.
cv_build_extra_groups <- function(md, group_names, colors_fn) {
  n <- nrow(md)
  max_levels <- max(2L, min(60L, as.integer(n / 2)))
  extra <- list()
  skipped <- list()
  for (mc in colnames(md)) {
    if (mc == "cell_barcode" || mc %in% group_names) {
      next
    }
    v <- md[[mc]]
    if (!(is.character(v) || is.factor(v) || is.logical(v))) {
      next
    }
    lev <- if (is.factor(v)) levels(v) else sort(unique(as.character(v)))
    lev <- lev[!is.na(lev)]
    if (!length(lev)) {
      next
    }
    if (length(lev) > max_levels) {
      skipped[[mc]] <- length(lev)
      next
    }
    extra[[mc]] <- cv_group(
      match(as.character(v), lev) - 1L,
      lev,
      colors_fn(mc, lev)
    )
  }
  list(groups = extra, skipped = skipped)
}

## Every projection's coordinates travel in the bundle, keyed by name, so the
## expression panel can switch between UMAP / tSNE / PCA client-side with no
## server round-trip (the "one bundle per dataset, instant" contract).
##
## A 3-D embedding also sends its third dimension, so the client can orbit it
## rather than show a flattened shadow of it. `ndim` travels either way — the
## client needs to know which panels can rotate and which are flat.
cv_build_projections <- function(crb, cells) {
  proj_names <- tryCatch(crb$availableProjections(), error = function(e) NULL)
  projections <- list()
  for (pn in proj_names) {
    pj <- tryCatch(crb$getProjection(pn), error = function(e) NULL)
    if (is.null(pj)) {
      next
    }
    pjidx <- match(cells, rownames(pj))
    nd <- as.integer(ncol(pj))
    entry <- list(
      x = round(as.numeric(pj[pjidx, 1]), 4),
      y = round(as.numeric(pj[pjidx, 2]), 4),
      ndim = nd
    )
    ## I() so a single-cell data set still serialises z as an array, the same
    ## invariant cv_space() enforces for x/y.
    if (nd >= 3) {
      entry$z <- I(round(as.numeric(pj[pjidx, 3]), 4))
      ## The three axis names, for the tripod the client draws on a rotated
      ## cloud. Its own column names rather than a generic X/Y/Z: on a PCA those
      ## carry which components are being shown, which is the whole question
      ## when only three of many are drawn.
      ax <- colnames(pj)[1:3]
      entry$axes <- I(
        if (is.null(ax) || anyNA(ax)) {
          paste0("dim ", 1:3)
        } else {
          as.character(ax)
        }
      )
    }
    projections[[pn]] <- entry
  }
  projections
}

## One spatial SAMPLE: its per-cell x/y (aligned to `cells`, NA off-sample) plus
## its histology image. Two image sources, ONE contract: a base64 data URI + the
## data-space bounds it covers + an alignment preset (offset in DATA units, scale
## unitless, flip). The client maps the bounds to screen with the same transform
## as the cells, so the image aligns; preset/user transforms adjust on top.
##   - EMBEDDED (Xenium/MERFISH): image + its own bounds travel in the .crb.
##   - EXTERNAL (Visium H&E): separate files with a hand-tuned Cerebro.options
##     preset. Configured per DATA SET, so offered on every section.
## Returns list(name, x, y, image) or NULL.
cv_spatial_one <- function(crb, cells, nm, allow_external) {
  sd <- tryCatch(crb$getSpatialData(nm), error = function(e) NULL)
  co <- if (!is.null(sd)) sd$coordinates else NULL
  if (is.null(co)) {
    return(NULL)
  }
  sidx <- match(cells, rownames(co))
  xr <- range(co[, 1], na.rm = TRUE)
  yr <- range(co[, 2], na.rm = TRUE)
  identity_preset <- list(
    offsetX = 0,
    offsetY = 0,
    scaleX = 1,
    scaleY = 1,
    flipX = FALSE,
    flipY = FALSE,
    opacity = 0.6
  )
  ## Every background this section can be shown against, as objects with their
  ## own identity and calibration -- not one image tucked into the section.
  ## Embedded and external used to be exclusive, so an object carrying its own
  ## histology silently dropped whatever the deployment had configured, and only
  ## the first configured file was read at all.
  bounds_default <- list(
    xmin = xr[1],
    xmax = xr[2],
    ymin = yr[1],
    ymax = yr[2]
  )
  span <- c(diff(xr), diff(yr))
  images <- list()
  embedded <- sd$histology_images
  if (is.null(embedded) || !length(embedded)) {
    embedded <- if (!is.null(sd$histology_image)) {
      list("Embedded histology" = sd$histology_image)
    } else {
      list()
    }
  }
  for (embedded_index in seq_along(embedded)) {
    entry <- embedded[[embedded_index]]
    embedded_names <- names(embedded)
    entry_name <- if (
      !is.null(embedded_names) && length(embedded_names) >= embedded_index
    ) {
      embedded_names[[embedded_index]]
    } else {
      ""
    }
    if (is.list(entry)) {
      emb <- entry$image %||% entry$uri %||% entry$data
      b <- entry$bounds %||% sd$histology_image_bounds
      label <- entry$label %||% entry_name
    } else {
      emb <- entry
      b <- sd$histology_image_bounds
      label <- entry_name
    }
    if (!is.character(emb) || length(emb) != 1L || is.na(emb) || !nzchar(emb)) {
      next
    }
    if (is.null(b)) {
      b <- bounds_default
    }
    if (is.null(label) || !length(label) || is.na(label) || !nzchar(label)) {
      label <- if (length(embedded) == 1L) {
        "Embedded histology"
      } else {
        paste("Embedded histology", embedded_index)
      }
    }
    ## Every embedded image comes out of the same pipeline as the coordinates,
    ## so it needs no hand alignment and the first remains the automatic default.
    images[[length(images) + 1]] <- list(
      id = if (embedded_index == 1L) {
        "embedded"
      } else {
        paste0("embedded-", embedded_index)
      },
      label = label,
      uri = emb,
      bounds = list(
        xmin = as.numeric(b$xmin),
        xmax = as.numeric(b$xmax),
        ymin = as.numeric(b$ymin),
        ymax = as.numeric(b$ymax)
      ),
      preset = identity_preset,
      coord_span = span
    )
  }
  if (allow_external) {
    for (ex in cv_external_images()) {
      images[[length(images) + 1]] <- list(
        id = ex$id,
        label = ex$label,
        uri = ex$uri,
        bounds = bounds_default,
        preset = ex$preset,
        coord_span = span
      )
    }
  }
  ## The default background, as a REFERENCE rather than a copy. A histology
  ## image is megabytes of base64; carrying the same one under both `image` and
  ## `images[1]` doubled it, and once more again for the space's own default --
  ## measured at 1.6 MB per copy on the Xenium demo, in a 3.6 MB bundle. Older
  ## readers of the singular field get the id and can look it up.
  image <- if (length(images)) {
    list(
      id = images[[1]]$id,
      label = images[[1]]$label,
      bounds = images[[1]]$bounds,
      preset = images[[1]]$preset,
      coord_span = images[[1]]$coord_span
    )
  } else {
    NULL
  }

  list(
    name = nm,
    x = round(as.numeric(co[sidx, 1]), 3),
    y = round(as.numeric(co[sidx, 2]), 3),
    ## `image` is the default one, kept so anything reading the older singular
    ## contract still works; `images` is the list the picker is built from.
    image = image,
    images = images
  )
}

## Standard spatial space (id "spatial"). Its default coords/image are the first
## sample; when the object carries MORE than one spatial sample, every sample also
## travels in `$samples` so Linked views can switch between them client-side (the
## "Spatial data" picker), each donor's tissue section being its own coordinate
## system + image. Returns the space or NULL when there is no spatial.
cv_build_spatial <- function(crb, cells) {
  sp_names <- tryCatch(crb$availableSpatial(), error = function(e) NULL)
  if (!length(sp_names)) {
    return(NULL)
  }
  built <- lapply(
    seq_along(sp_names),
    ## `spatial_images` is configured PER DATA SET -- its shape carries no way
    ## to say which section a file belongs to -- so its images are offered on
    ## every section rather than silently attached to the first one. A reader
    ## who has a background for their second section could not reach it at all.
    ## Saying which image goes with which section needs a new option shape; the
    ## client already remembers the choice per section.
    function(i) cv_spatial_one(crb, cells, sp_names[i], allow_external = TRUE)
  )
  built <- Filter(Negate(is.null), built)
  if (!length(built)) {
    return(NULL)
  }
  first <- built[[1]]
  space <- cv_space(
    "spatial",
    paste0(first$name, " (spatial)"),
    first$x,
    first$y
  )
  if (!is.null(first$image)) {
    space$image <- first$image
  }
  ## Only when there is no `samples` list to hold them: with one, the space's
  ## default section IS samples[[1]] and repeating its images here would send
  ## every one of them twice.
  if (length(first$images) && length(built) == 1) {
    space$images <- I(first$images)
  }
  if (length(built) > 1) {
    space$samples <- lapply(built, function(s) {
      list(
        name = s$name,
        label = paste0(s$name, " (spatial)"),
        x = I(s$x),
        y = I(s$y),
        image = s$image,
        images = I(s$images)
      )
    })
  }
  space
}

## Trekker single-cell spatial mapping: its physical coordinates live in the
## `trekker` slot, not `spatial`, so availableSpatial() misses them. Exposed as
## its OWN space (id "trekker"), distinct from a standard `spatial` space, so a
## data set carrying BOTH keeps both — the right panel switches between them
## rather than one silently swallowing the other. Aligned to `cells` via barcodes
## (NA where a cell was not positioned). Returns list(space, bundle) or NULL.
cv_build_trekker <- function(crb, cells, md) {
  tk <- tryCatch(crb$getTrekker(), error = function(e) NULL)
  if (
    is.null(tk) ||
      is.null(tk$x) ||
      is.null(tk$y) ||
      is.null(tk$barcodes)
  ) {
    return(NULL)
  }
  tk_idx <- match(cells, tk$barcodes)
  if (!any(!is.na(tk_idx))) {
    return(NULL)
  }
  space <- cv_space(
    "trekker",
    "Physical (Trekker)",
    round(as.numeric(tk$x)[tk_idx], 2),
    round(as.numeric(tk$y)[tk_idx], 2)
  )
  ## Bring the Trekker page's extra controls into Linked views: continuous
  ## physical fields to colour by, per-cell positioning confidence (dissolve),
  ## and a positioning-evidence flag (nuclei markers). All aligned to `cells`;
  ## positioned-only fields are NA where unpositioned. Numeric META columns are
  ## NOT built here — cv_build_fields() offers every one of them for every data
  ## set, Trekker or not.
  flds <- list()
  for (fn in names(tk$fields)) {
    f <- tk$fields[[fn]]
    if (is.null(f$v)) {
      next
    }
    ## Trekker's own fields arrive pre-quantised to 0-255.
    flds[[fn]] <- cv_field(
      f$label %||% fn,
      as.integer(f$v)[tk_idx],
      f$min %||% 0,
      f$max %||% 1,
      scale = 255L,
      source = "trekker",
      desc = f$desc %||% NULL,
      by_type = cv_trekker_by_type(f$by_type %||% NULL)
    )
  }
  conf_v <- if (!is.null(tk$conf) && !is.null(tk$conf$prop_top)) {
    I(round(as.numeric(tk$conf$prop_top)[tk_idx], 3))
  } else {
    NULL
  }
  ## The rest of what Trekker recorded about a position, per cell. `conf` stays a
  ## bare vector because the dissolve slider indexes it directly; these travel
  ## beside it. Without them the workspace could say how confident a placement
  ## was but not how noisy the beads under it were or how many spatial barcodes
  ## it rested on -- the two numbers the dedicated page shows next to it, and the
  ## ones that say whether the confidence is worth anything.
  conf_extra <- list()
  if (!is.null(tk$conf)) {
    for (k in c("prop_noise", "sb_total", "sb_umi_top")) {
      v <- tk$conf[[k]]
      if (is.null(v)) {
        next
      }
      conf_extra[[k]] <- I(round(as.numeric(v)[tk_idx], 4))
    }
  }
  ev_flag <- NULL
  ev_img <- NULL
  if (length(tk$evidence)) {
    ev_bc <- vapply(
      tk$evidence,
      function(e) e$bc %||% "",
      character(1)
    )
    ev_flag <- I(as.integer(cells %in% ev_bc))
    ## Keep the evidence aligned to the bundle's cell order. Most entries are
    ## NULL (the vendor ships images for only a small subset), so this adds the
    ## actual explanation to the detail card without duplicating barcodes or
    ## forcing a server round-trip for every click.
    ev_img <- vector("list", length(cells))
    for (e in tk$evidence) {
      at <- match(as.character(e$bc %||% ""), cells)
      img <- e$img %||% NULL
      if (!is.na(at) && !is.null(img) && nzchar(img)) {
        ev_img[at] <- list(as.character(img))
      }
    }
    ev_img <- I(ev_img)
  }
  bundle <- list(
    conf = conf_v,
    conf_noise = conf_extra$prop_noise,
    conf_sb = conf_extra$sb_total,
    conf_sb_umi = conf_extra$sb_umi_top,
    evidence = ev_flag,
    evidence_img = ev_img,
    ## Dataset-level (not per-cell, no `tk_idx` re-indexing needed): the
    ## same coordinate-source / QC / Moran's I detail the Trekker page
    ## shows, surfaced here via a modal (see coordviews.js `cv-tk-info-btn`).
    qc = tk$qc,
    moran = tk$moran
  )
  ## `fields` goes to the bundle's TOP-LEVEL field list, not into $trekker: the
  ## client reads one list of continuous colourings regardless of where each came
  ## from, so there is a single place to add, look up and render them.
  list(space = space, bundle = bundle, fields = flds)
}

## Immune axis: clone identity, sizes, ranks, a clone "space", expansion level.
## Returns list(space, group, bundle) or NULL when there is no receptor data.
cv_build_clone <- function(crb, cells, n) {
  ir <- tryCatch(crb$getImmuneRepertoire(), error = function(e) NULL)
  cp <- cv_clone_per_cell(ir, cells)
  if (is.null(cp) || !any(!is.na(cp$clone))) {
    return(NULL)
  }
  ct <- cp$clone
  tab <- sort(table(ct[!is.na(ct)]), decreasing = TRUE)
  clone_keys <- names(tab)
  clone_size <- as.integer(tab)
  cell_clone <- match(ct, clone_keys) # 1..K, NA if none
  ## A clone is called on CTgene, and one CTgene clone routinely spans several
  ## CDR3s -- in the omnibus demo every clone does, one of them across twelve.
  ## Labelling it with whichever CDR3 happened to come first therefore named the
  ## row after one of its members: the table showed a single sequence while
  ## clicking it selected cells carrying eleven others. The dominant sequence is
  ## still the useful handle, so it stays, but the count of the rest travels with
  ## it and the column no longer claims to be a CDR3.
  ctaa <- cp$ctaa
  clone_cdr3 <- vapply(
    clone_keys,
    function(k) {
      aa <- ctaa[which(ct == k)]
      aa <- aa[!is.na(aa) & nzchar(aa)]
      if (!length(aa)) {
        return(NA_integer_)
      }
      length(unique(aa))
    },
    integer(1)
  )
  clone_label <- vapply(
    clone_keys,
    function(k) {
      aa <- ctaa[which(ct == k)]
      aa <- aa[!is.na(aa) & nzchar(aa)]
      if (!length(aa)) {
        return(k)
      }
      names(sort(table(aa), decreasing = TRUE))[1]
    },
    character(1)
  )
  K <- length(clone_keys)
  cx <- rep(NA_real_, n)
  cy <- rep(NA_real_, n)
  counter <- integer(K)
  for (i in seq_len(n)) {
    ci <- cell_clone[i]
    if (is.na(ci)) {
      next
    }
    counter[ci] <- counter[ci] + 1L
    cx[i] <- ci
    cy[i] <- counter[ci] - 1L
  }
  maxstack <- max(clone_size)
  na_cells <- which(is.na(cell_clone))
  if (length(na_cells)) {
    set.seed(1)
    cx[na_cells] <- -max(1, round(K * 0.06))
    cy[na_cells] <- stats::runif(length(na_cells), 0, maxstack)
  }
  ## Name the receptor in the label. The clonotypes shown are one class only
  ## (mixing TCR and BCR into one ranking is not something any page here does),
  ## and a data set carrying both would otherwise give no clue which is on screen.
  space <- cv_space(
    "clone",
    if (is.na(cp$receptor)) {
      "Clonal expansion"
    } else {
      paste0("Clonal expansion (", cp$receptor, ")")
    },
    cx,
    cy
  )
  size_per_cell <- ifelse(
    is.na(cell_clone),
    NA_integer_,
    clone_size[cell_clone]
  )
  ## Bins and labels come from clone_contract.R, so a clone lands in the same
  ## expansion level here as it does on the Clonal UMAP. "No receptor" is this
  ## page's own extra level: the Clonal UMAP draws those cells as a grey
  ## background layer rather than a level, but here every cell is in the same
  ## legend, so the absence has to be nameable.
  lvl <- as.character(cerebro_clone_expansion(size_per_cell))
  lvl[is.na(lvl)] <- "No receptor"
  lev <- c("No receptor", CEREBRO_CLONE_LABELS)
  group <- cv_group(
    match(lvl, lev) - 1L,
    lev,
    c("#e0e0e0", "#c6dbef", "#6baed6", "#f97316", "#c2410c", "#7f1d1d")
  )
  bundle <- cv_clone(
    ifelse(is.na(cell_clone), -1L, cell_clone - 1L),
    unname(clone_label),
    clone_size,
    K,
    sum(!is.na(cell_clone)),
    cp$receptor,
    unname(clone_cdr3)
  )
  list(space = space, group = group, bundle = bundle)
}

## Assemble the bundle from the loaded Cerebro object. Each modality is built by
## its own cv_build_* helper; this function wires them into the final list and
## owns the colour resolver (which depends on the reactive colour map).
cv_build_bundle <- function(crb) {
  md <- crb$getMetaData()
  if (is.null(md) || !("cell_barcode" %in% colnames(md))) {
    return(NULL)
  }
  cells <- as.character(md$cell_barcode)
  n <- length(cells)

  ## Colours must MATCH the Projection tab, not be freshly invented here. The app
  ## keeps the authoritative per-group level->colour map in reactive_colors()
  ## (seeded from the object, user-editable in "Color management"); the Projection
  ## tab reads it via assignColorsToGroups(). Pull the same map, matched to this
  ## group's level order, and fall back to cerebro_group_colors() for any level
  ## without an assignment — the identical fallback assignColorsToGroups() uses.
  ## Called inside the coordviews_bundle reactive, so recolouring in Color
  ## management re-pushes the bundle and the panels recolour live.
  rc <- tryCatch(reactive_colors(), error = function(e) NULL)
  cv_group_colors <- function(group_name, lev) {
    cols <- rep(NA_character_, length(lev))
    if (!is.null(rc) && group_name %in% names(rc)) {
      named <- rc[[group_name]]
      cols <- unname(named[match(lev, names(named))])
    }
    if (anyNA(cols)) {
      fb <- tryCatch(
        cerebro_group_colors(length(lev)),
        error = function(e) cv_colors_for(lev)
      )
      cols[is.na(cols)] <- fb[is.na(cols)]
    }
    cols
  }

  ## Three colouring sources, mirroring the Projection tab's "Color cells by"
  ## (which offers every meta column) while keeping its narrower "Group filters"
  ## (which lists only getGroups()):
  ##   groups    — registered grouping variables: colour AND filter
  ##   cat_extra — other categorical columns: colour only
  ##   fields    — numeric columns (+ Trekker's physical fields): continuous
  group_names <- tryCatch(crb$getGroups(), error = function(e) character(0))
  groups <- cv_build_groups(crb, md, cv_group_colors)
  extra <- cv_build_extra_groups(md, group_names, cv_group_colors)
  cat_extra <- extra$groups
  cat_skipped <- extra$skipped
  fields <- cv_build_fields(md)

  ## spaces: umap (always) + spatial/trekker (if present) + clone (if present)
  projections <- cv_build_projections(crb, cells)
  if (!length(projections)) {
    return(NULL)
  }
  default_projection <- if ("umap" %in% names(projections)) {
    "umap"
  } else {
    names(projections)[1]
  }
  dp <- projections[[default_projection]]
  umap_space <- cv_space(
    "umap",
    paste0(default_projection, " (expression)"),
    dp$x,
    dp$y
  )
  ## A 3-D embedding carries its z into the space too, so the expression panel
  ## starts orbitable rather than only becoming so after a projection switch.
  if (!is.null(dp$z)) {
    umap_space$z <- dp$z
    umap_space$axes <- dp$axes
  }
  spaces <- list(umap_space)

  ## Standard spatial and the Trekker physical mapping are INDEPENDENT spaces:
  ## add each whenever the object carries it. An object with both gets both panels
  ## (the right-panel switch flips between them); neither is dropped.
  sp <- cv_build_spatial(crb, cells)
  if (!is.null(sp)) {
    spaces[[length(spaces) + 1]] <- sp
  }
  trekker_bundle <- NULL
  tk <- cv_build_trekker(crb, cells, md)
  if (!is.null(tk)) {
    spaces[[length(spaces) + 1]] <- tk$space
    trekker_bundle <- tk$bundle
    fields <- c(fields, tk$fields)
  }

  ## immune axis: adds a clone space + a clone_expansion group when receptors
  ## are present.
  clone_bundle <- NULL
  cl <- cv_build_clone(crb, cells, n)
  if (!is.null(cl)) {
    spaces[[length(spaces) + 1]] <- cl$space
    groups[["clone_expansion"]] <- cl$group
    clone_bundle <- cl$bundle
  }

  ## Default colouring: a registered group if there is one (cell_type first, as
  ## before), else any other categorical column, else the first continuous field
  ## expressed as the client's field-mode string. An object with no colourable
  ## column at all still yields a usable bundle — the panels simply draw in one
  ## colour, which is strictly better than a blank tab.
  default_group <- if ("cell_type" %in% names(groups)) {
    "cell_type"
  } else if (length(groups)) {
    names(groups)[1]
  } else if (length(cat_extra)) {
    names(cat_extra)[1]
  } else if (length(fields)) {
    paste0(cv_field_mode, names(fields)[1])
  } else {
    NULL
  }

  list(
    ## Which data set this bundle IS. The client keeps per-image alignment state
    ## across pushes, and a bundle is re-sent for reasons that are not a change
    ## of data set -- returning to the tab, recolouring a group. Without an
    ## identity to compare, "a new bundle" and "a new data set" look the same and
    ## the user's alignment work is thrown away by walking away and back.
    dataset_id = tryCatch(
      {
        if (
          exists("available_crb_files") &&
            !is.null(available_crb_files$selected)
        ) {
          as.character(available_crb_files$selected)
        } else {
          paste0("cells:", n, ":", if (n) cells[1] else "")
        }
      },
      error = function(e) paste0("cells:", n)
    ),
    cells = cells,
    n = n,
    groups = groups,
    cat_extra = cat_extra,
    cat_skipped = cat_skipped,
    fields = fields,
    default_group = default_group,
    projections = projections,
    default_projection = default_projection,
    spaces = spaces,
    clone = clone_bundle,
    trekker = trekker_bundle
  )
}
