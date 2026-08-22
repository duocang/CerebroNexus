// =============================================================================
// Immune-repertoire Clonal UMAP projection: thin wrapper over the shared
// 2-D Canvas renderer. Only the NON-FACETED
// Clonal UMAP renders through here — the faceted variant (a group_by column is
// chosen) stays on the static ggplot renderPlot, which faceting needs and the
// single-canvas shared renderer cannot express.
//
// The Clonal UMAP is always categorical: a grey "Other cells" background trace
// plus one trace per clonal-expansion level. It is passed as a normal
// categorical render, so it inherits persistent x|y selection,
// zoom-to-selection and unified hover for free. Two IR-specific inputs ride on
// meta: legend_position (IR users pick right/bottom/left/top/none) and per-trace
// hover.hoverinfo (the grey background skips hover, the coloured levels show it)
// — both are honoured by the shared Canvas renderer.
//
// Same wiring as overview/js_projection_update_plot.js: shinyjs delivers the
// positional R args as one array `params`, and this file is prepended into the
// IR extendShinyjs(text=) after projection_layouts.js + projection_scatter.js so
// all three share one global scope.
// =============================================================================

const IR_CLONAL_UMAP_PLOT_ID = 'ir_clonalUMAP_projection';

if (window.cerebroProjection) {
  window.cerebroProjection.registerPlot(IR_CLONAL_UMAP_PLOT_ID);
}

let latestClonalUMAP = null;
let renderedClonalUMAPHost = null;

function drawLatestClonalUMAP(force) {
  const host = document.getElementById(IR_CLONAL_UMAP_PLOT_ID);
  if (
    !latestClonalUMAP ||
    typeof window.cerebroCanvasProjection === 'undefined' ||
    !host ||
    (!force && host === renderedClonalUMAPHost)
  ) {
    return false;
  }
  const [meta, data, hover, group_centers] = latestClonalUMAP;
  window.cerebroCanvasProjection.renderCategorical(
    meta,
    data,
    hover,
    group_centers || null,
    null,
    {}
  );
  renderedClonalUMAPHost = host;
  return true;
}

const clonalUMAPObserver = new MutationObserver(function () {
  drawLatestClonalUMAP(false);
});
clonalUMAPObserver.observe(document.documentElement, { childList: true, subtree: true });

shinyjs.updateClonalUMAP = function (params) {
  const [meta, data, hover, group_centers] = params;
  meta.plot_id = IR_CLONAL_UMAP_PLOT_ID;
  latestClonalUMAP = [meta, data, hover, group_centers];
  drawLatestClonalUMAP(true);
};

shinyjs.irClonalUMAPClearSelection = function () {
  window.cerebroCanvasProjection.clearSelection(IR_CLONAL_UMAP_PLOT_ID);
};

shinyjs.irClonalUMAPZoomToSelection = function () {
  window.cerebroCanvasProjection.toggleZoom(IR_CLONAL_UMAP_PLOT_ID);
};
