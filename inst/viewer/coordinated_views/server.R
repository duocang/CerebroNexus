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

## Per-dataset bundle builders (pure functions; see bundle.R). Sourced first so
## cv_build_bundle() and every cv_build_* / cv_* helper is in scope for the
## reactive below. Kept in a separate file so the builders can be unit-tested
## without a running session (tests/testthat/test-coordinated-views.R).
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/coordinated_views/bundle.R"
  ),
  local = TRUE
)

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
