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

## Per-cell clone identity (CTstrict) + human label (CTaa) from the IR list,
## aligned to `cells`. NA where a cell carries no receptor.
cv_clone_per_cell <- function(ir, cells) {
  if (is.null(ir) || !length(ir)) {
    return(NULL)
  }
  rows <- do.call(
    rbind,
    lapply(ir, function(df) {
      if (
        is.null(df) ||
          !("barcode" %in% names(df)) ||
          !("CTstrict" %in% names(df))
      ) {
        return(NULL)
      }
      data.frame(
        barcode = as.character(df$barcode),
        CTstrict = as.character(df$CTstrict),
        CTaa = if ("CTaa" %in% names(df)) {
          as.character(df$CTaa)
        } else {
          as.character(df$CTstrict)
        },
        stringsAsFactors = FALSE
      )
    })
  )
  if (is.null(rows) || !nrow(rows)) {
    return(NULL)
  }
  rows <- rows[!is.na(rows$CTstrict) & nzchar(rows$CTstrict), , drop = FALSE]
  rows <- rows[!duplicated(rows$barcode), , drop = FALSE]
  idx <- match(cells, rows$barcode)
  list(ctstrict = rows$CTstrict[idx], ctaa = rows$CTaa[idx])
}

## Resolve an EXTERNAL histology image (Cerebro.options$spatial_images) for the
## CURRENTLY selected dataset, plus its alignment preset, base64-encoded so the
## browser can show it. Returns list(uri, preset) or NULL. The preset offset is
## in DATA units and scale is a unitless multiplier (same contract the Spatial
## page uses), so it transfers to the coordinated-views canvas unchanged.
cv_external_image <- function() {
  if (
    !exists("Cerebro.options") ||
      is.null(Cerebro.options[["spatial_images"]])
  ) {
    return(NULL)
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
    return(NULL)
  }
  path <- si[[nm]][1]
  root <- Cerebro.options[["cerebro_root"]]
  img_path <- if (!is.null(root)) file.path(root, path) else path
  if (!file.exists(img_path)) {
    img_path <- path
  }
  if (
    !file.exists(img_path) || !requireNamespace("base64enc", quietly = TRUE)
  ) {
    return(NULL)
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
  uri <- paste0(
    "data:",
    mime,
    ";base64,",
    base64enc::base64encode(img_path)
  )
  pget <- function(key, d) {
    v <- Cerebro.options[[key]]
    if (is.null(v) || is.null(names(v)) || !(nm %in% names(v))) d else v[[nm]]
  }
  list(
    uri = uri,
    preset = list(
      offsetX = as.numeric(pget("spatial_images_offset_x", 0)),
      offsetY = as.numeric(pget("spatial_images_offset_y", 0)),
      scaleX = as.numeric(pget("spatial_images_scale_x", 1)),
      scaleY = as.numeric(pget("spatial_images_scale_y", 1)),
      flipX = isTRUE(pget("spatial_images_flip_x", FALSE)),
      flipY = isTRUE(pget("spatial_images_flip_y", FALSE)),
      opacity = 0.6
    )
  )
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
cv_field <- function(label, v, min, max, scale = cv_field_scale) {
  list(label = label, v = I(v), min = min, max = max, scale = scale)
}
cv_clone <- function(id, label, size, n_clones, n_receptor) {
  ## id/label/size are arrays; n_clones/n_receptor are true scalars (left bare).
  list(
    id = I(id),
    label = I(label),
    size = I(size),
    n_clones = n_clones,
    n_receptor = n_receptor
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
## `ndim` travels with them: the panels are 2-D, so a 3-D embedding is shown by
## its first two dimensions — but the client must be able to SAY so rather than
## silently flatten it (the Projection tab renders such an object in real 3D).
cv_build_projections <- function(crb, cells) {
  proj_names <- tryCatch(crb$availableProjections(), error = function(e) NULL)
  projections <- list()
  for (pn in proj_names) {
    pj <- tryCatch(crb$getProjection(pn), error = function(e) NULL)
    if (is.null(pj)) {
      next
    }
    pjidx <- match(cells, rownames(pj))
    projections[[pn]] <- list(
      x = round(as.numeric(pj[pjidx, 1]), 4),
      y = round(as.numeric(pj[pjidx, 2]), 4),
      ndim = as.integer(ncol(pj))
    )
  }
  projections
}

## One spatial SAMPLE: its per-cell x/y (aligned to `cells`, NA off-sample) plus
## its histology image. Two image sources, ONE contract: a base64 data URI + the
## data-space bounds it covers + an alignment preset (offset in DATA units, scale
## unitless, flip). The client maps the bounds to screen with the same transform
## as the cells, so the image aligns; preset/user transforms adjust on top.
##   - EMBEDDED (Xenium/MERFISH): image + its own bounds travel in the .crb.
##   - EXTERNAL (Visium H&E): a separate file with a hand-tuned Cerebro.options
##     preset; only offered for the primary sample (allow_external).
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
  image <- NULL
  emb <- sd$histology_image
  if (!is.null(emb) && is.character(emb) && nzchar(emb)) {
    b <- sd$histology_image_bounds
    if (is.null(b)) {
      b <- list(xmin = xr[1], xmax = xr[2], ymin = yr[1], ymax = yr[2])
    }
    image <- list(
      uri = emb,
      bounds = list(
        xmin = as.numeric(b$xmin),
        xmax = as.numeric(b$xmax),
        ymin = as.numeric(b$ymin),
        ymax = as.numeric(b$ymax)
      ),
      preset = identity_preset,
      coord_span = c(diff(xr), diff(yr))
    )
  } else if (allow_external) {
    ext_img <- tryCatch(cv_external_image(), error = function(e) NULL)
    if (!is.null(ext_img)) {
      image <- list(
        uri = ext_img$uri,
        bounds = list(xmin = xr[1], xmax = xr[2], ymin = yr[1], ymax = yr[2]),
        preset = ext_img$preset,
        coord_span = c(diff(xr), diff(yr))
      )
    }
  }
  list(
    name = nm,
    x = round(as.numeric(co[sidx, 1]), 3),
    y = round(as.numeric(co[sidx, 2]), 3),
    image = image
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
    function(i) cv_spatial_one(crb, cells, sp_names[i], allow_external = i == 1)
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
  if (length(built) > 1) {
    space$samples <- lapply(built, function(s) {
      list(
        name = s$name,
        label = paste0(s$name, " (spatial)"),
        x = I(s$x),
        y = I(s$y),
        image = s$image
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
      scale = 255L
    )
  }
  conf_v <- if (!is.null(tk$conf) && !is.null(tk$conf$prop_top)) {
    I(round(as.numeric(tk$conf$prop_top)[tk_idx], 3))
  } else {
    NULL
  }
  ev_flag <- NULL
  if (length(tk$evidence)) {
    ev_bc <- vapply(
      tk$evidence,
      function(e) e$bc %||% "",
      character(1)
    )
    ev_flag <- I(as.integer(cells %in% ev_bc))
  }
  bundle <- list(
    conf = conf_v,
    evidence = ev_flag,
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
  if (is.null(cp) || !any(!is.na(cp$ctstrict))) {
    return(NULL)
  }
  ct <- cp$ctstrict
  tab <- sort(table(ct[!is.na(ct)]), decreasing = TRUE)
  clone_keys <- names(tab)
  clone_size <- as.integer(tab)
  cell_clone <- match(ct, clone_keys) # 1..K, NA if none
  ctaa <- cp$ctaa
  clone_label <- vapply(
    clone_keys,
    function(k) {
      lab <- ctaa[which(ct == k)[1]]
      if (is.na(lab)) k else lab
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
  space <- cv_space(
    "clone",
    "Clonal expansion",
    cx,
    cy
  )
  size_per_cell <- ifelse(
    is.na(cell_clone),
    NA_integer_,
    clone_size[cell_clone]
  )
  lvl <- as.character(cut(
    size_per_cell,
    breaks = c(0, 1, 5, 20, Inf),
    labels = c("Single (1)", "Small (2-5)", "Medium (6-20)", "Large (>20)")
  ))
  lvl[is.na(lvl)] <- "No receptor"
  lev <- c(
    "No receptor",
    "Single (1)",
    "Small (2-5)",
    "Medium (6-20)",
    "Large (>20)"
  )
  group <- cv_group(
    match(lvl, lev) - 1L,
    lev,
    c("#e0e0e0", "#c6dbef", "#6baed6", "#f97316", "#c2410c")
  )
  bundle <- cv_clone(
    ifelse(is.na(cell_clone), -1L, cell_clone - 1L),
    unname(clone_label),
    clone_size,
    K,
    sum(!is.na(cell_clone))
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
  spaces <- list(cv_space(
    "umap",
    paste0(default_projection, " (expression)"),
    dp$x,
    dp$y
  ))

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
