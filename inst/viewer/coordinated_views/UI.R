##----------------------------------------------------------------------------##
## Tab: Linked views — UI.
##
## The workspace that makes the paper's central claim demonstrable: a selection
## in ANY panel propagates as a coordinated highlight to every other panel,
## across modalities — expression (UMAP), physical space (Spatial), AND the
## immune axis (Clonal) — with a live composition + top-clonotype readout.
##
## Layout is deliberately different from the other tabs: NO left param column.
## A horizontal control bar sits above a full-width panel grid, so the linked
## views get the room the standard param/viz split does not. All controls carry
## `cv-` ids and are wired client-side by www/coordviews.js; the server sends a
## single per-dataset bundle. Custom styles are scoped under `.coordviews-page`.
##----------------------------------------------------------------------------##

## Per-panel hover modebar (plotly-style). `panel` is the JS panel key ("A"/"B").
## Lasso is the default active drag mode. All buttons are wired client-side.
cv_panebar <- function(panel) {
  tbtn <- function(act, tip, ic, active = FALSE) {
    tags$button(
      type = "button",
      class = if (active) "cv-tbtn is-on" else "cv-tbtn",
      `data-act` = act,
      `data-panel` = panel,
      title = tip,
      icon(ic)
    )
  }
  div(
    class = "cv-panebar",
    tbtn("box", "Box select", "vector-square"),
    tbtn("lasso", "Lasso select", "draw-polygon", active = TRUE),
    tbtn("zin", "Zoom in", "search-plus"),
    tbtn("zout", "Zoom out", "search-minus"),
    tbtn("reset", "Reset view", "expand"),
    tbtn("png", "Download PNG", "download")
  )
}

## One panel slot. There are always FOUR in the DOM (A = umap, B/C/D = the data
## set's other spaces); www/coordviews.js assigns a space to each present one and
## hides the rest, and lays them out 1x2 / rotated-品 / 2x2 by how many exist.
## Every head carries a (hidden) Trekker info button, shown by JS on whichever
## panel ends up holding the Trekker space.
cv_pane <- function(key) {
  low <- tolower(key)
  div(
    class = "cv-pane",
    div(
      class = "cv-pane-head",
      tags$span(class = "cv-ptitle", id = paste0("cv-title-", low), "—"),
      tags$button(
        type = "button",
        class = "cv-tbtn",
        id = paste0("cv-tk-info-", low),
        `data-act` = "trekker-info",
        style = "display:none",
        title = "Trekker coordinate source, QC & Moran's I",
        icon("circle-info")
      ),
      cv_panebar(key)
    ),
    tags$canvas(id = paste0("cv-cv-", low)),
    div(class = "cv-tip", id = paste0("cv-tip-", low))
  )
}

## One "value bubble + <input type=range>" control block. Every slider in the
## control bar / More panel shares this markup (only the label, ids, bounds and
## initial display text differ), so they all route through here. `disp` is the
## initial bubble text (e.g. "0.80" for a 0.8 value); `val_id` is the bubble's id
## (kept explicit because it does not always follow input_id, e.g. cv-op-val);
## `wrap_id` sets an id on the outer div when a control needs one (cv-niche-wrap).
cv_range <- function(
  label,
  input_id,
  min,
  max,
  step,
  value,
  disp,
  val_id,
  wrap_id = NULL
) {
  div(
    class = "cv-ctl cv-ctl-range",
    id = wrap_id,
    tags$label(label),
    div(
      class = "cv-range",
      tags$span(class = "cv-ps-val", id = val_id, disp),
      tags$input(
        type = "range",
        id = input_id,
        min = min,
        max = max,
        step = step,
        value = value
      )
    )
  )
}

tab_coordinated_views <- tabItem(
  tabName = "coordinated_views",
  div(
    class = "coordviews-page",
    ## ---- header + info -------------------------------------------------- ##
    div(
      style = "display:flex;align-items:baseline;gap:10px;margin-bottom:2px;",
      tags$h3(
        style = "font-size:18px;font-weight:650;margin:0;",
        "Linked views"
      ),
      cerebroInfoButton("coordinated_views_info")
    ),
    div(
      class = "cv-meta",
      id = "cv-meta",
      "Load a single-cell data set to explore its modalities together."
    ),

    ## ---- horizontal control bar (the layout fix) ------------------------ ##
    div(
      class = "cv-topbar",
      div(
        class = "cv-ctl",
        tags$label("Colour by"),
        tags$select(id = "cv-pick-color")
      ),
      ## Which projection feeds the expression panel. Every projection's coords
      ## travel in the bundle, so switching is client-side and instant. Hidden by
      ## JS when the data set has only one projection.
      div(
        class = "cv-ctl",
        id = "cv-proj-ctl",
        tags$label("Projection"),
        tags$select(id = "cv-pick-proj")
      ),
      ## Spatial-sample picker — shown by JS only when the data set carries MORE
      ## than one spatial section (each its own coordinate system + image). All
      ## samples travel in the bundle, so switching the spatial panel is instant.
      div(
        class = "cv-ctl",
        id = "cv-spatial-ctl",
        style = "display:none",
        tags$label("Spatial data"),
        tags$select(id = "cv-pick-spatial")
      ),
      ## Single-gene expression picker — a real server-side gene search (the
      ## whole transcriptome), shown only in "Gene expression" mode. JS toggles
      ## visibility; the server pushes the 0-255 vector on change.
      div(
        class = "cv-ctl",
        id = "cv-gene-ctl",
        style = "display:none",
        tags$label("Gene"),
        selectizeInput(
          "coordviews_gene",
          label = NULL,
          choices = NULL,
          multiple = FALSE,
          options = list(
            maxOptions = 1000,
            placeholder = "select a gene...",
            create = FALSE,
            loadThrottle = 300
          )
        )
      ),
      ## Three-gene co-expression: one gene per RGB channel, shown only in
      ## "Co-expression (RGB)" mode.
      div(
        class = "cv-ctl cv-rgb-ctl",
        id = "cv-rgb-ctl",
        style = "display:none",
        tags$label("Co-expression (R / G / B)"),
        div(
          class = "cv-rgb-row",
          selectizeInput(
            "coordviews_gene_r",
            label = NULL,
            choices = NULL,
            options = list(
              maxOptions = 1000,
              placeholder = "red gene...",
              create = FALSE,
              loadThrottle = 300
            )
          ),
          selectizeInput(
            "coordviews_gene_g",
            label = NULL,
            choices = NULL,
            options = list(
              maxOptions = 1000,
              placeholder = "green gene...",
              create = FALSE,
              loadThrottle = 300
            )
          ),
          selectizeInput(
            "coordviews_gene_b",
            label = NULL,
            choices = NULL,
            options = list(
              maxOptions = 1000,
              placeholder = "blue gene...",
              create = FALSE,
              loadThrottle = 300
            )
          )
        )
      ),
      cv_range(
        "Point size",
        "cv-ps",
        min = "0.8",
        max = "7",
        step = "0.2",
        value = "3",
        disp = "3.0",
        val_id = "cv-ps-val"
      ),
      ## Clonal-panel layout switch. Shown only when the data set carries an
      ## immune axis (a "clone" space). The clone panel's X/Y are abstract, so
      ## unlike UMAP/Spatial it draws labelled axes and can be re-laid-out
      ## client-side between representations without touching the server.
      div(
        ## cv-collapse (opacity 0) as the initial hidden state so the FIRST reveal
        ## also fades in, matching subsequent switches (revealEl toggles it).
        class = "cv-ctl cv-collapse",
        id = "cv-clone-layout-ctl",
        style = "display:none",
        tags$label("Clonal layout"),
        div(
          class = "cv-seg",
          id = "cv-clone-layout",
          tags$button(
            type = "button",
            class = "cv-seg-btn is-on",
            `data-mode` = "stack",
            "Rank stack"
          ),
          tags$button(
            type = "button",
            class = "cv-seg-btn",
            `data-mode` = "bands",
            "Expansion bands"
          )
        )
      ),
      ## Reveals the "More" panel: point opacity, cell subsampling, group filters.
      tags$button(
        type = "button",
        id = "cv-more-btn",
        class = "cv-morebtn",
        HTML("More &#9662;")
      ),
      ## Right-aligned cluster: the filter/subsample readout + the selection
      ## actions. Both are hidden by default and surface only when relevant.
      div(
        class = "cv-topbar-right",
        ## Live "showing N / M cells" readout — hidden unless a filter/subsample
        ## reduces the view, so filtering is visible even when the panels are
        ## coloured by a different variable than the one being filtered.
        tags$span(class = "cv-shown", id = "cv-shown"),
        ## Selection actions: appear together only when a selection exists. They
        ## stack vertically by default and lay out side by side on narrow widths.
        div(
          class = "cv-selactions cv-collapse",
          id = "cv-selactions",
          style = "display:none",
          tags$button(
            id = "cv-zoom",
            class = "cv-zoombtn",
            "Zoom to selection"
          ),
          tags$button(id = "cv-clear", class = "cv-clearbtn", "Clear selection")
        )
      )
    ),

    ## ---- collapsible "More" panel --------------------------------------- ##
    ## Additional projection parameters (opacity, % of cells) + per-group
    ## filters, mirroring the Overview/Spatial param boxes but kept out of the
    ## way so the top bar stays compact. All client-owned; group-filter chips are
    ## rendered into #cv-filters-row from the bundle's categorical groups.
    div(
      class = "cv-more",
      id = "cv-more",
      style = "display:none",
      cv_range(
        "Point opacity",
        "cv-opacity",
        min = "0.1",
        max = "1",
        step = "0.05",
        value = "0.8",
        disp = "0.80",
        val_id = "cv-op-val"
      ),
      cv_range(
        "Show % of cells",
        "cv-pct",
        min = "5",
        max = "100",
        step = "5",
        value = "100",
        disp = "100",
        val_id = "cv-pct-val"
      ),
      div(
        class = "cv-ctl cv-filters",
        tags$label("Group filters"),
        div(class = "cv-filters-row", id = "cv-filters-row")
      ),
      ## Trekker-only controls (shown by JS when the bundle carries Trekker data):
      ## dissolve least-confident positions + ring nuclei with positioning
      ## evidence. Colour-by physical/meta fields is added to the Colour by list.
      div(
        class = "cv-trekker",
        id = "cv-trekker-ctl",
        style = "display:none",
        cv_range(
          "Dissolve least-confident (%)",
          "cv-dissolve",
          min = "0",
          max = "95",
          step = "5",
          value = "0",
          disp = "0",
          val_id = "cv-dissolve-val"
        ),
        cv_range(
          "Niche radius (µm)",
          "cv-niche",
          min = "50",
          max = "500",
          step = "25",
          value = "250",
          disp = "250",
          val_id = "cv-niche-val",
          wrap_id = "cv-niche-wrap"
        ),
        tags$label(
          class = "cv-chk cv-evidence-chk",
          tags$input(type = "checkbox", id = "cv-evidence"),
          "Mark positioning evidence"
        )
      )
    ),

    ## ---- spatial background-image controls (server-rendered; only when the
    ## current data set carries spatial coordinates with a histology image) --- ##
    uiOutput("coordviews_image_ui"),

    ## ---- selection bar: sits below the params, above the panels --------- ##
    div(
      class = "cv-selbar cv-collapse",
      id = "cv-selbar",
      style = "display:none",
      tags$span(id = "cv-seltext", "—")
    ),

    ## ---- panel grid ----------------------------------------------------- ##
    ## Every space the data set carries gets its OWN panel (no switch): A = UMAP,
    ## then Spatial / Trekker / Clonal in whatever combination exists. coordviews.js
    ## assigns spaces, hides the unused slots, and picks the grid (1x2 / rotated-品
    ## for three / 2x2 for four) to fill the width and height with >=300px squares.
    div(
      class = "cv-panes",
      cv_pane("A"),
      cv_pane("B"),
      cv_pane("C"),
      cv_pane("D")
    ),

    ## ---- legend (categorical) or colourbar (continuous gene) ------------ ##
    div(class = "cv-legend", id = "cv-legend"),
    div(
      class = "cv-cbar",
      id = "cv-cbar",
      style = "display:none",
      tags$span(id = "cv-cb0", "0"),
      div(class = "cv-grad", id = "cv-grad"),
      tags$span(id = "cv-cb1", "1"),
      tags$span(class = "cv-cbar-note", id = "cv-cbar-note", "expression")
    ),

    ## ---- readout: composition + top clonotypes -------------------------- ##
    div(
      style = "margin-top:14px;",
      div(
        class = "cv-readout",
        id = "cv-readout",
        div(
          class = "cv-empty",
          "Lasso-drag in any panel to select cells. The same cells highlight in ",
          "every panel, and their composition and top clonotypes appear here."
        )
      )
    ),

    ## ---- selected-cell detail views (server-rendered) ------------------- ##
    ## Mirrors the Projection tab: a "Plot of selected cells" and a "Table of
    ## selected cells", driven by the barcodes the client reports for the current
    ## selection. Appears only while cells are selected. The client-side
    ## composition/clonotype readout above stays as the quick-look summary.
    div(
      style = "margin-top:6px;",
      uiOutput("coordviews_selected_cells_UI")
    ),

    ## ---- Trekker detail modal --------------------------------------------- ##
    ## Client-driven (no server round-trip): coordviews.js fills these ids from
    ## D.trekker.qc / D.trekker.moran via the shared builders in www/trekker.js
    ## (CerebroTrekker.build*), the same functions the dedicated Trekker page
    ## uses for its permanent "Data and QC" / "Moran's I" boxes.
    tags$dialog(
      id = "cv-tk-modal",
      class = "trekker-page",
      tags$button(
        class = "tk-zoom-x",
        onclick = "document.getElementById('cv-tk-modal').close()",
        "Close"
      ),
      tags$h4(class = "tk-sub-h", "Data and QC"),
      div(class = "tk-grid", id = "cv-tk-stats"),
      div(
        class = "tk-two",
        div(
          tags$h4(class = "tk-sub-h", "Positioning class distribution"),
          tags$table(
            class = "tk-table",
            tags$thead(tags$tr(
              tags$th("Spatial locations"),
              tags$th(class = "num", "Nuclei"),
              tags$th(class = "num", "Share"),
              tags$th("Handling")
            )),
            tags$tbody(id = "cv-tk-postbl")
          ),
          div(class = "tk-flag", id = "cv-tk-salvflag")
        ),
        div(
          tags$h4(class = "tk-sub-h", "Provenance"),
          tags$dl(class = "tk-kv", id = "cv-tk-prov"),
          div(class = "tk-flag", id = "cv-tk-rangeflag")
        )
      ),
      ## Same top gap as "Positioning class distribution" above (that one comes
      ## from .tk-two's own margin-top: 14px; matched here explicitly since this
      ## heading isn't inside a .tk-two).
      tags$h4(
        class = "tk-sub-h",
        style = "margin-top:14px",
        "Spatial autocorrelation — Moran's I (upstream)"
      ),
      tags$table(
        class = "tk-table",
        tags$thead(tags$tr(
          tags$th(class = "num", "#"),
          tags$th("Gene"),
          tags$th(class = "num", "Moran's I")
        )),
        tags$tbody(id = "cv-tk-morantbl")
      )
    )
  )
)
