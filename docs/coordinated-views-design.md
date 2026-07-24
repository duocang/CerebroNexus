# Coordinated cross-modal views — design & Vitessce analysis

> Branch `feat/coordinated-workspace` (off `dev`). Author: build session 2026-07-19.
> Goal: make CerebroVista's central paper claim — *"selections … propagated as
> coordinated highlights across linked visualizations"* — actually true, with the
> **immune axis inside the linked loop**, and fix the layout that leaves no room
> for linked multi-view.

## 1. Vitessce, analysed

[Vitessce (Nature Methods 2024)](https://www.nature.com/articles/s41592-024-02436-x)
is the reference for "coordinated multiple views" of single-cell + spatial data.
Read precisely, not dismissively:

### 1.1 What it actually contributes (its strengths)
- **A real coordination model, not "side-by-side".** Views don't link to each
  other; they link to named **coordination scopes** (`spatialZoom`, `geneSelection`,
  `cellHighlight`, `cellSetSelection`, …) via **coordination types**. A JSON
  *view-config* declares which views share which scopes; interactions cascade
  automatically. (Boukhelifa & Roberts CMV model.) This is genuinely good
  architecture and the part worth learning from.
- **Streaming at scale.** Reads TB-scale imaging directly from cloud **OME-Zarr /
  OME-TIFF**, tiled, lazily — no local copy. Best-in-class for whole-slide imaging.
- **Format breadth as a viewer:** AnnData / MuData / SpatialData / CSV / OME-*.

### 1.2 What it cannot do (its weaknesses — verified from its own docs)
- **Pure viewer. Zero analysis.** It visualises pre-computed results; it computes
  nothing. No differential expression, no composition-of-selection, no stats.
- **Requires data pre-converted** to AnnData-Zarr / OME-Zarr / SpatialData. That
  conversion is a real, technical pre-step.
- **Entirely client-side / static files.** No server-side compute to lean on.
- **No immune axis at all.** Its docs contain no TCR / clonotype / repertoire /
  HLA concept. The adaptive-immune modality is simply outside its data model.
- **Not truly no-code.** Novel views need a JSON view-config or Python/R/JS API.

### 1.3 The opening for CerebroVista
Vitessce coordinates **expression ⇄ space**. It has no notion of a **clonotype**
or an **HLA allele**, so those can never enter its coordinated loop — not a bug,
a boundary of its data model. CerebroVista's thesis is exactly that missing axis:
put **repertoire and HLA into the same coordinated fabric as expression and space**,
and — because there is an R server behind the browser — **compute on the selection**
(composition, top clones, donor HLA) rather than only recolour it.

So we should **not** try to out-Vitessce Vitessce on linked expression⇄space views
(and definitely not on zarr image streaming). We differentiate on the two things it
structurally cannot do: **(a) the immune axis in the coordinated loop**, and
**(b) analysis-on-selection from the R backend.**

## 2. The gap in the current app

- Selection today is **tab-local**: each tab owns its own
  `*_projection_selected_cells` reactive keyed on plot-local `x|y` strings. There is
  **no cross-modal selection bus** (confirmed in code + memory).
- The **Trekker tab** already proves coordinated Spatial⇄UMAP linked brushing on a
  custom canvas — but only for the Trekker slot, and with **no immune axis**.
- The paper's sentence ("tissue sections, UMAPs, **or repertoire views**") is
  therefore **not yet demonstrable** for the general case.
- **Layout:** every module tab is left param-column (width 3) + right viz-column
  (width 9). Two linked panes get squeezed into the 9-col; there is no room for a
  true multi-panel coordinated workspace. The user explicitly flagged this.

## 3. Design — a general "Linked Views" workspace

### 3.1 The unifying idea
**Every modality is just a named 2-D layout of the same cells.**
- UMAP  = the expression embedding coordinates
- Spatial = physical µm coordinates
- Clonal = an abstract expansion layout (see 3.1.1)

All three are `(cellId, x, y)` per **space**. One linked-brushing engine renders N
panels and shares a single **cell-index selection set** across them — identical code
for every modality. This is Vitessce's coordination-scope idea **generalised so the
immune repertoire is a first-class space**, which Vitessce cannot express.

#### 3.1.1 The clonal panel is abstract, so it is drawn differently
UMAP and Spatial are real geometry: aspect-ratio-preserved, axis-free (the
convention that says "these coordinates are a layout, not a measurement"). The
clonal space is **not** geometry — its X and Y *mean* something — so it is the one
panel that:
- **stretches X and Y independently** to fill the panel. Aspect-preserving a
  598-clonotype X against a ~20-cell Y squashed the expansion axis into a one-pixel
  needle; independent normalisation is what makes the panel legible.
- **draws a labelled L-shaped frame** (client-side canvas), so the reader knows
  what the axes are. UMAP/Spatial stay axis-free.
- offers a **layout switch** (client-side, instant — recomputed in JS from
  `clone.id` + `clone.size`, no server round-trip):
  - **Rank stack** — one column per clonotype, ranked largest-at-left, cells
    stacked upward = expansion. Shows the expansion *skyline*.
  - **Expansion bands** — four horizontal tiers (Single / Small / Medium / Large),
    cells packed within each band so **band width ∝ number of cells in that tier**.
    Shows the expansion *distribution* and, under a cell-type colouring, which
    lineages dominate the expanded clones.

No-receptor cells are left out of the clonal panel (they have no clonal identity);
they remain in the UMAP/Spatial panels and the shared selection.

### 3.2 Layout (fixes the space problem)
- **No left param column.** A compact **horizontal control bar** at the top:
  colour-by, projection, point size, clear-selection. Lower-frequency knobs live
  behind a **"More"** disclosure to keep the bar uncluttered.
- **UMAP-family parameters** (mirroring the Overview/Spatial param boxes, but
  client-side and instant):
  - **Projection** — every projection's coordinates travel in the bundle, so the
    expression panel switches UMAP ↔ tSNE ↔ PCA with no server round-trip. Only
    the expression panel re-projects; spatial/clonal are untouched. The control
    hides itself when a dataset offers a single projection.
  - **Point opacity** and **Show % of cells** (a stable, seeded subsample, so
    lowering it never reshuffles the view) — in "More".
  - **Group filters** — one collapsible chip per categorical group; unchecking a
    level drops those cells from *every* panel and the selection (one shared
    active mask), reusing the bundle's group values (no server round-trip).
- **Full-width responsive canvas grid** below — 2 panels side-by-side (each tall),
  extensible to 3 when a dataset carries expression + spatial + repertoire.
- **Live readout strip** (full width, under the panels): n selected, cell-type
  composition of the selection, and — the immune axis — **top clonotypes among the
  selection** (rank · CDR3 · n cells · % of selection); donor HLA context when present.

### 3.3 Coordination = instant, client-side
One R→JS handoff per dataset (`sendCustomMessage("coordviews_data", bundle)`), then
**all interaction is client-side**: brushing in any panel updates one shared
cell-index set; every panel dims non-selected and highlights selected on the same
frame; the readout (composition, top clones) is computed in JS from the bundle. No
server round-trip on brush → the "instant coordinated highlights" the paper claims.
The R backend's role is the *analysis* the bundle encodes (clone sizes, colours,
group levels) and, as a fast-follow, deeper on-selection stats.

### 3.4 Modality-adaptive
- PBMC (default demo): expression + repertoire → links **UMAP ⇄ Clonal** (immune
  axis in the loop — the differentiator).
- Spatial demos: expression + spatial → links **UMAP ⇄ Spatial** (Trekker axis).
- A future dataset with all three links all three.

### 3.5 Data bundle (R → JS)
```
{ cells:[barcodes], n,
  groups:{ <name>:{values:[per-cell level idx], levels:[...], colors:[...]} },
  default_group,
  spaces:[ {id,label,x:[],y:[]} , ... ],          // umap always; +clone / +spatial
  clone:{ id:[per-cell clone idx | -1], label:[CTaa per clone], size:[n cells] } }  // if IR
```
All arrays aligned to the canonical `cells` order (keyed on `cell_barcode`).

## 4. Scope

**v1 (this branch):** the workspace tab; UMAP ⇄ (Clonal | Spatial) linked brushing;
categorical colour (cell type / cluster / sample) + clone-expansion colour; live
composition + top-clonotype readout; compact top-bar full-width layout. Additive —
existing tabs untouched, so zero regression risk to them.

**Deferred:** gene-expression colour (needs matrix request/response, like Trekker);
three-panel simultaneous; bidirectional HLA-allele selection; persisting a selection
into the other tabs.

## 5. Validation — done, against the live app (CDP-driven)

All verified on the running app (headless Chrome over CDP), observing the exact
DOM/canvas the user sees:

- **UMAP → Clonal (immune axis):** lasso the T-cell cluster → 298 cells selected;
  selection bar reports "coordinated across all panels"; the Clonal panel highlights
  the same cells; readout computes composition (T 249 / B 49) and **top clonotypes
  with real CDR3s** (e.g. `CARVCPRCDFKGFDPW_CQQYNNWPYTF`, in-selection count, clone
  size, % of selection). Screenshot: `cv-6-linked`.
- **Clonotype-row click → cells:** clicking a clone row (size 74) selects exactly
  74 cells across every panel. Repertoire selection propagates into the loop.
- **Clonal → UMAP:** brushing the clone skyline highlights those cells in the UMAP.
- **UMAP ⇄ Spatial (Visium demo):** dataset auto-adapts to umap + spatial; brushing a
  tissue region highlights the same cells in the UMAP (scattered across clusters —
  "which identities live in this region"); composition by cluster. Screenshot:
  `cv-8-spatial-brush`.
- **Dataset-switch robustness:** PBMC (744) → Visium (1,491, spatial) → PBMC (744,
  clonal), coordination works after every switch, **zero page errors**. (Fixed a
  duplicate-listener bug: panel objects + listeners are now created once, reused.)
- **CI hygiene:** R files air-formatted; the tab is additive (new conditional tab,
  positive-existence app tests unaffected — no exclusive tab-count or sidebar
  snapshot exists to break).

## 6. Files
- `inst/shiny/v1.4/coordinated_views/UI.R` + `server.R` — tab (bundle builder + handoff)
- `inst/shiny/v1.4/www/coordviews.js` + `coordviews.css` — engine + full-width layout
- `inst/shiny/v1.4/shiny_UI.R`, `shiny_server.R` — wiring (source / placeholder /
  tabItem / asset includes / try_source / insertConditionalTab)

## 7. Enhancements (Spatial parameters ported in)

Driven by the request to move Spatial's settings into the workspace, drop the
redundant panel pickers, and enrich "Colour by".

### 7.1 Panel pickers removed
The Left/Right panel space `<select>`s (and their JS logic) are gone. Panels
auto-assign: left = UMAP (expression), right = the dataset's other space (Spatial
preferred, else Clonal). One fewer decision; the coordination is the point.

### 7.2 Richer "Colour by" (from the Spatial page)
Beyond categorical groups, two continuous modes matching the Spatial page:
- **Gene expression** — a whole-transcriptome server-side gene search
  (`serverSideGeneSelector`, gated by `active=` so its `later::` callbacks don't
  leak into other tabs). The server returns a 0-255 vector aligned to the bundle's
  cell order (`coordviews_geneval`); the client colours by viridis and shows a
  colourbar. Verified: CD3D lights the T-cell cluster in both panels.
- **Co-expression (RGB)** — one gene per channel (`coordviews_gene_r/g/b`); the
  server sends three 0-255 vectors (`coordviews_rgbval`); the client blends them.
  Verified: CD3D/MS4A1/LYZ as R/G/B cleanly separate T/B/Mono.

Both colour modes apply to **every** panel, so a gene lights up in the UMAP and
the Spatial/Clonal panel simultaneously.

### 7.3 Histology background image (from the Spatial page) — both sources
The spatial panel can draw a histology image behind the cells, from **either**
source, on **one** contract: a base64 data URI + the data-space bounds it covers
+ an alignment preset (offset in DATA units, scale unitless, flip). The bounds
map to screen through the **same** transform as the points (`dataToScreen`), so
the image aligns; the data-unit offset is converted to pixels by mapping two
points through that transform (which also handles the y-axis inversion).
- **Embedded** (Xenium/MERFISH): image + its own bounds travel in the .crb;
  identity preset (offset 0, scale 1). Auto-aligns.
- **External** (Visium H&E): a separate PNG named in `Cerebro.options$spatial_images`,
  base64-encoded on the fly; its base bounds are the cells' coordinate range and
  its preset (offset/scale/flip) comes from `spatial_images_*`. Because the
  Spatial page's offset is **already in data units** and scale is unitless (an
  early read of it as "plotly pixels" was wrong), the same preset aligns on this
  canvas unchanged — the H&E opens pre-aligned exactly as on the Spatial tab.

A client-owned control bar — opacity / move X-Y (data units, ranged to the
coordinate span) / scale / rotate / flip X-Y / show — adjusts the image instantly.
The bar appears only when the current dataset has an image, is seeded from the
preset, and resets across switches. Verified on Xenium, MERFISH **and Visium**:
images align under the cells; brushing a tissue region over the image still
coordinates to the UMAP.

Deferred: per-panel independent colour, three-panel simultaneous, HLA-allele
selection, independent X/Y image scale (currently a single scale slider).
