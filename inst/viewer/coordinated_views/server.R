##----------------------------------------------------------------------------##
## Tab: Linked views — server.
##
## Sourced into the main server scope (source(..., local = TRUE)), so
## `input`, `output`, `session` and `data_set` are in scope.
##
## Responsibility: build ONE per-dataset bundle describing the cells, their
## categorical groupings, and every available "space" (a named 2-D layout of the
## SAME cells: umap / spatial / clone), plus per-cell clone identity, and push it
## to www/coordviews.js. All interaction (linked brushing, highlight, readout) is
## then client-side. The engine keys selection on cell index, so a brush in any
## panel highlights the same cells in every other panel — across modalities.
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

## Every projection's coordinates travel in the bundle, keyed by name, so the
## expression panel can switch between UMAP / tSNE / PCA client-side with no
## server round-trip (the "one bundle per dataset, instant" contract).
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
      y = round(as.numeric(pj[pjidx, 2]), 4)
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
  ## physical/meta fields to colour by, per-cell positioning confidence
  ## (dissolve), and a positioning-evidence flag (nuclei markers). All
  ## aligned to `cells`; positioned-only fields are NA where unpositioned.
  flds <- list()
  for (fn in names(tk$fields)) {
    f <- tk$fields[[fn]]
    if (is.null(f$v)) {
      next
    }
    flds[[fn]] <- list(
      label = f$label %||% fn,
      v = I(as.integer(f$v)[tk_idx]),
      min = f$min %||% 0,
      max = f$max %||% 1
    )
  }
  ## numeric, non-constant meta columns (e.g. Myelination) as extra
  ## colour-by options; already in `cells` order, so no re-index needed.
  ## Skip QC/technical columns (nCount_*, nFeature_*, percent.*, log10_*):
  ## they are not analysis variables and only clutter the "Colour by" list.
  qc_col <- "^(nCount|nFeature|percent|log10)"
  for (mc in colnames(md)) {
    v <- md[[mc]]
    if (grepl(qc_col, mc)) {
      next
    }
    if (!is.numeric(v) || length(unique(v[!is.na(v)])) <= 1) {
      next
    }
    rng <- suppressWarnings(range(v, na.rm = TRUE))
    if (!all(is.finite(rng))) {
      next
    }
    flds[[paste0("meta:", mc)]] <- list(
      label = mc,
      v = I(as.integer(round((v - rng[1]) / (rng[2] - rng[1]) * 255))),
      min = round(rng[1], 3),
      max = round(rng[2], 3)
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
    fields = flds,
    conf = conf_v,
    evidence = ev_flag,
    ## Dataset-level (not per-cell, no `tk_idx` re-indexing needed): the
    ## same coordinate-source / QC / Moran's I detail the Trekker page
    ## shows, surfaced here via a modal (see coordviews.js `cv-tk-info-btn`).
    qc = tk$qc,
    moran = tk$moran
  )
  list(space = space, bundle = bundle)
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

  groups <- cv_build_groups(crb, md, cv_group_colors)
  if (!length(groups)) {
    return(NULL)
  }
  default_group <- if ("cell_type" %in% names(groups)) {
    "cell_type"
  } else {
    names(groups)[1]
  }

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

  list(
    cells = cells,
    n = n,
    groups = groups,
    default_group = default_group,
    projections = projections,
    default_projection = default_projection,
    spaces = spaces,
    clone = clone_bundle,
    trekker = trekker_bundle
  )
}

coordviews_bundle <- reactive({
  req(!is.null(data_set()))
  tryCatch(cv_build_bundle(data_set()), error = function(e) NULL)
})

## Push the bundle on (re)connect (coordviews_ready) or data-set change.
observe({
  input[["coordviews_ready"]]
  b <- coordviews_bundle()
  if (is.null(b)) {
    return()
  }
  session$sendCustomMessage("coordviews_data", b)
})

##----------------------------------------------------------------------------##
## Selected-cell detail views — mirror the Overview/Projection tab. When cells
## are selected in ANY linked panel, the client reports their barcodes in
## `coordviews_selection`; we render a "Plot of selected cells" (bar chart for a
## categorical variable / violin for a numeric one, selected vs. rest) and a
## "Table of selected cells" (their meta data), exactly like the Projection tab.
## The client-side composition + top-clonotype readout is unaffected and stays.
##----------------------------------------------------------------------------##
coordviews_selected_barcodes <- reactive({
  sel <- input[["coordviews_selection"]]
  if (is.null(sel) || !length(sel)) {
    return(NULL)
  }
  as.character(sel)
})

## Boxes appear only while a selection exists (same gating as the Overview tab).
## Shiny re-runs this renderUI on every selection change (same as Overview), which
## would reset the "Variable to compare" dropdown each lasso — so seed it from the
## current input via isolate(): the user's choice survives the rebuild, and reading
## it isolated adds no extra dependency (no rebuild when only the variable changes).
output[["coordviews_selected_cells_UI"]] <- renderUI({
  req(coordviews_selected_barcodes())
  meta_cols <- colnames(getMetaData())
  tagList(
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Plot of selected cells"),
          cerebroInfoButton("coordviews_selected_cells_plot_info")
        ),
        tagList(
          selectInput(
            "coordviews_selected_cells_plot_variable",
            label = "Variable to compare:",
            choices = meta_cols[!meta_cols %in% c("cell_barcode")],
            selected = isolate(input[[
              "coordviews_selected_cells_plot_variable"
            ]])
          ),
          plotly::plotlyOutput("coordviews_selected_cells_plot")
        )
      )
    ),
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Table of selected cells"),
          cerebroInfoButton("coordviews_selected_cells_table_info")
        ),
        tagList(
          shinyWidgets::materialSwitch(
            inputId = "coordviews_selected_cells_table_number_formatting",
            label = "Automatically format numbers:",
            value = TRUE,
            status = "primary",
            inline = TRUE
          ),
          shinyWidgets::materialSwitch(
            inputId = "coordviews_selected_cells_table_color_highlighting",
            label = "Highlight values with colors:",
            value = TRUE,
            status = "primary",
            inline = TRUE
          ),
          DT::dataTableOutput("coordviews_selected_cells_table")
        )
      )
    )
  )
})

## Plot: categorical variable -> bar of selected-cell counts per group (coloured
## by the SAME assignColorsToGroups() the Projection tab uses); numeric variable
## -> violin/box of selected vs. not-selected. Barcode-keyed, so it needs no
## projection coordinates.
output[["coordviews_selected_cells_plot"]] <- plotly::renderPlotly({
  sel <- coordviews_selected_barcodes()
  req(sel, input[["coordviews_selected_cells_plot_variable"]])
  cells_df <- getMetaData()
  var <- input[["coordviews_selected_cells_plot_variable"]]
  req(var %in% colnames(cells_df))
  is_selected <- cells_df[["cell_barcode"]] %in% sel
  ## categorical -> bar chart of counts within the selection
  if (is.factor(cells_df[[var]]) || is.character(cells_df[[var]])) {
    sub <- cells_df[is_selected, , drop = FALSE]
    if (nrow(sub) > 0) {
      counts <- sub %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(var))) %>%
        dplyr::tally() %>%
        dplyr::ungroup()
    } else {
      lv <- if (var %in% getGroups()) {
        getGroupLevels(var)
      } else {
        unique(cells_df[[var]])
      }
      counts <- data.frame(x = lv, n = 0L)
      colnames(counts)[1] <- var
    }
    colors_for_groups <- assignColorsToGroups(counts, var)
    x_vals <- as.character(counts[[1]])
    plot <- plotly::plot_ly(
      x = x_vals,
      y = counts[[2]],
      type = "bar",
      color = x_vals,
      colors = colors_for_groups,
      showlegend = FALSE,
      hoverinfo = "y"
    )
    y_axis_title <- "Number of cells"
    ## numeric -> violin/box of selected vs. not selected
  } else if (is.numeric(cells_df[[var]])) {
    grp <- factor(
      ifelse(is_selected, "selected", "not selected"),
      levels = c("selected", "not selected")
    )
    plot <- plotly::plot_ly(
      x = grp,
      y = cells_df[[var]],
      type = "violin",
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      color = grp,
      colors = setNames(
        c("#e74c3c", "#7f8c8d"),
        c("selected", "not selected")
      ),
      showlegend = FALSE,
      hoverinfo = "y",
      marker = list(size = 5)
    )
    y_axis_title <- var
  } else {
    return(NULL)
  }
  plot %>%
    plotly::layout(
      title = "",
      xaxis = list(title = "", mirror = TRUE, showline = TRUE),
      yaxis = list(
        title = y_axis_title,
        tickformat = ",.0f",
        hoverformat = ",.0f",
        mirror = TRUE,
        showline = TRUE
      ),
      hovermode = "compare"
    )
})

## Table: meta data of the selected cells (same prettifyTable options as the
## Projection tab's table). Filtered by barcode; empty skeleton when nothing hit.
output[["coordviews_selected_cells_table"]] <- DT::renderDataTable({
  sel <- coordviews_selected_barcodes()
  if (is.null(sel)) {
    return(getMetaData() %>% dplyr::slice(0) %>% prepareEmptyTable())
  }
  cells_df <- getMetaData() %>%
    dplyr::filter(cell_barcode %in% sel) %>%
    dplyr::select(cell_barcode, dplyr::everything())
  if (nrow(cells_df) == 0) {
    getMetaData() %>% dplyr::slice(0) %>% prepareEmptyTable()
  } else {
    prettifyTable(
      cells_df,
      filter = list(position = "top", clear = TRUE),
      dom = "Brtlip",
      show_buttons = TRUE,
      number_formatting = input[[
        "coordviews_selected_cells_table_number_formatting"
      ]],
      color_highlighting = input[[
        "coordviews_selected_cells_table_color_highlighting"
      ]],
      hide_long_columns = TRUE,
      download_file_name = "linked_views_selected_cells"
    )
  }
})

## Info modals for the two panels.
observeEvent(input[["coordviews_selected_cells_plot_info"]], {
  showModal(modalDialog(
    title = "Plot of selected cells",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    p(
      "Depending on the variable chosen, this plot summarises the cells you ",
      "selected across the linked panels. A categorical variable (e.g. ",
      "'cluster' or 'sample') gives a bar chart of how many selected cells fall ",
      "in each group, coloured exactly as in the panels. A continuous variable ",
      "(e.g. number of transcripts) gives a violin/box plot comparing its ",
      "distribution in the selected vs. non-selected cells."
    )
  ))
})
observeEvent(input[["coordviews_selected_cells_table_info"]], {
  showModal(modalDialog(
    title = "Table of selected cells",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    p(
      "Meta data for the cells selected across the linked panels (some columns ",
      "may be hidden — check the 'Column visibility' button). The table can be ",
      "downloaded as CSV or Excel for further analysis."
    )
  ))
})

##----------------------------------------------------------------------------##
## Gene-expression colouring (the richer "Colour by"). The gene pickers are
## whole-transcriptome server-side searches (same helper the Spatial/Gene tabs
## use); on change the server returns a 0-255 vector aligned to the bundle's
## cell order, and the client colours the points (viridis, or RGB blend).
##----------------------------------------------------------------------------##
cv_has_expression <- function() {
  isTRUE(tryCatch(nrow(data_set()$expression) > 0, error = function(e) FALSE))
}

## Pull one gene, scaled 0-255, aligned to `cells`. Returns NULL if unavailable.
cv_gene_vector <- function(gene, cells) {
  if (is.null(gene) || !nzchar(gene)) {
    return(NULL)
  }
  m <- tryCatch(
    data_set()$getExpressionMatrix(cells = cells, genes = gene),
    error = function(e) NULL
  )
  if (is.null(m)) {
    return(NULL)
  }
  if (is.null(dim(m))) {
    v <- as.numeric(m)
  } else {
    cn <- colnames(m)
    v <- if (!is.null(cn)) {
      as.numeric(m[1, match(cells, cn)])
    } else {
      as.numeric(m[1, ])
    }
  }
  mx <- suppressWarnings(max(v, na.rm = TRUE))
  q <- if (is.finite(mx) && mx > 0) {
    as.integer(round(v / mx * 255))
  } else {
    rep(0L, length(v))
  }
  q[is.na(q)] <- 0L
  list(v = q, max = round(mx, 3))
}

serverSideGeneSelector(
  session,
  "coordviews_gene",
  active = function() cv_has_expression()
)
lapply(
  c("coordviews_gene_r", "coordviews_gene_g", "coordviews_gene_b"),
  function(channel_id) {
    serverSideGeneSelector(session, channel_id, active = cv_has_expression)
  }
)

observeEvent(input[["coordviews_gene"]], {
  b <- coordviews_bundle()
  g <- input[["coordviews_gene"]]
  if (is.null(b) || is.null(g) || !nzchar(g)) {
    return()
  }
  gv <- cv_gene_vector(g, b$cells)
  if (is.null(gv)) {
    session$sendCustomMessage("coordviews_geneval", list(gene = g, ok = FALSE))
    return()
  }
  session$sendCustomMessage(
    "coordviews_geneval",
    list(gene = g, ok = TRUE, v = gv$v, max = gv$max)
  )
})

## RGB co-expression: one gene per channel, each scaled independently; an empty
## channel is all-zero. Recompute whenever any of the three genes changes.
observeEvent(
  list(
    input[["coordviews_gene_r"]],
    input[["coordviews_gene_g"]],
    input[["coordviews_gene_b"]]
  ),
  {
    b <- coordviews_bundle()
    if (is.null(b)) {
      return()
    }
    zero <- rep(0L, b$n)
    chan <- function(id) {
      g <- input[[id]]
      if (is.null(g) || !nzchar(g)) {
        return(list(v = zero, gene = ""))
      }
      gv <- cv_gene_vector(g, b$cells)
      if (is.null(gv)) list(v = zero, gene = "") else list(v = gv$v, gene = g)
    }
    r <- chan("coordviews_gene_r")
    g <- chan("coordviews_gene_g")
    bl <- chan("coordviews_gene_b")
    if (!nzchar(r$gene) && !nzchar(g$gene) && !nzchar(bl$gene)) {
      return()
    }
    session$sendCustomMessage(
      "coordviews_rgbval",
      list(
        ok = TRUE,
        r = r$v,
        g = g$v,
        b = bl$v,
        genes = c(r$gene, g$gene, bl$gene)
      )
    )
  },
  ignoreInit = TRUE
)

##----------------------------------------------------------------------------##
## Spatial histology-image controls — shown only when the current data set's
## spatial entry carries an embedded image. The controls are client-owned
## (cv-img- ids), wired by coordviews.js; adjusting them re-styles the image on
## the canvas instantly and never round-trips to the server.
##----------------------------------------------------------------------------##
output[["coordviews_image_ui"]] <- renderUI({
  b <- coordviews_bundle()
  img <- NULL
  if (!is.null(b)) {
    for (s in b$spaces) {
      if (!is.null(s$image)) {
        img <- s$image
      }
    }
  }
  if (is.null(img)) {
    return(NULL)
  }
  ## Seed the controls from the alignment preset so an external image (Visium
  ## H&E) opens PRE-ALIGNED, exactly as the Spatial tab does. Move sliders are in
  ## DATA units, ranged to the coordinate span so the nudge is meaningful.
  pr <- img$preset
  span <- img$coord_span
  if (is.null(span) || length(span) < 2) {
    span <- c(400, 400)
  }
  rng <- function(id, mn, mx, val, step) {
    tags$input(
      type = "range",
      id = id,
      min = mn,
      max = mx,
      value = val,
      step = step
    )
  }
  chk <- function(id, label, on) {
    tags$label(
      class = "cv-chk",
      if (isTRUE(on)) {
        tags$input(type = "checkbox", id = id, checked = "checked")
      } else {
        tags$input(type = "checkbox", id = id)
      },
      label
    )
  }
  sx <- signif(span[1] * 1.2, 3)
  sy <- signif(span[2] * 1.2, 3)
  div(
    class = "cv-imgbar",
    tags$span(class = "cv-imgbar-title", "Histology image"),
    chk("cv-img-show", "Show", TRUE),
    div(
      class = "cv-img-ctl",
      tags$label("Opacity"),
      rng("cv-img-opacity", 0, 1, pr$opacity %||% 0.6, 0.05)
    ),
    div(
      class = "cv-img-ctl",
      tags$label("Move X"),
      rng("cv-img-offx", -sx, sx, pr$offsetX %||% 0, signif(sx / 200, 2))
    ),
    div(
      class = "cv-img-ctl",
      tags$label("Move Y"),
      rng("cv-img-offy", -sy, sy, pr$offsetY %||% 0, signif(sy / 200, 2))
    ),
    div(
      class = "cv-img-ctl",
      tags$label("Scale"),
      rng("cv-img-scale", 0.3, 3, pr$scaleX %||% 1, 0.02)
    ),
    div(
      class = "cv-img-ctl",
      tags$label("Rotate"),
      rng("cv-img-rotate", -180, 180, 0, 1)
    ),
    chk("cv-img-flipx", "Flip X", isTRUE(pr$flipX)),
    chk("cv-img-flipy", "Flip Y", isTRUE(pr$flipY))
  )
})
outputOptions(output, "coordviews_image_ui", suspendWhenHidden = FALSE)

##----------------------------------------------------------------------------##
## Info modal
##----------------------------------------------------------------------------##
observeEvent(input[["coordinated_views_info"]], {
  showModal(modalDialog(
    title = "Linked views",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    tagList(
      tags$p(
        "Every modality is shown as a 2-D layout of the ",
        tags$b("same cells"),
        ": the ",
        tags$b("UMAP"),
        " (transcriptome), the ",
        tags$b("Spatial"),
        " map (physical positions, when the data set carries them), and ",
        tags$b("Clonal expansion"),
        " (each receptor-bearing cell placed by its clone's rank and size)."
      ),
      tags$p(
        tags$b("Coordinated selection"),
        " — lasso-drag in ",
        tags$i("any"),
        " panel and the same cells highlight in ",
        tags$i("every"),
        " panel, because the selection is keyed on the cell, not on a panel's ",
        "coordinates. Select a UMAP cluster to see where those cells sit in ",
        "tissue and which clonotypes they carry; select an expanded clone to see ",
        "where its cells fall in the UMAP."
      ),
      tags$p(
        tags$b("Readout"),
        " — the selection's cell-type composition and its top clonotypes ",
        "(CDR3, clone size, share of the selection) update live. Click a ",
        "clonotype row to select all of its cells across every panel."
      ),
      tags$p(
        style = "color:#6b6b70;",
        tags$b("Why this is different. "),
        "General coordinated viewers link expression and space, but have no ",
        "concept of a clonotype, so the immune repertoire can never enter their ",
        "linked loop. Here it is a first-class space, and the composition and ",
        "clonotype readouts are computed, not just recoloured."
      )
    )
  ))
})
