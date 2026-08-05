// =============================================================================
// Immune-repertoire Clonal UMAP projection: thin wrapper over the shared
// projection-scatter renderer (www/projection_scatter.js). Only the NON-FACETED
// Clonal UMAP renders through here — the faceted variant (a group_by column is
// chosen) stays on the static ggplot renderPlot, which faceting needs and the
// single-canvas shared renderer cannot express.
//
// The Clonal UMAP is always categorical: a grey "Other cells" background trace
// plus one trace per clonal-expansion level. It is passed as a normal
// categorical render, so it inherits the shared persistent x|y selection,
// zoom-to-selection and unified hover for free. Two IR-specific inputs ride on
// meta: legend_position (IR users pick right/bottom/left/top/none) and per-trace
// hover.hoverinfo (the grey background skips hover, the coloured levels show it)
// — both are honoured by the shared render2DCategorical.
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

shinyjs.updateClonalUMAP = function (params) {
  const [meta, data, hover, group_centers] = params;
  meta.plot_id = IR_CLONAL_UMAP_PLOT_ID;
  // The Clonal UMAP host lives behind a renderUI branch, so an update can arrive
  // before plotly.js has loaded or before the host div exists (a race the always-
  // present hosts of the other tabs never hit). render2DCategorical bails quietly
  // if the div is missing, and Plotly.react needs the global; if either is not
  // ready yet, retry on the next frame with the SAME payload until both are — the
  // R side pushes the data once, so the retry must not be dropped.
  const draw = function (attempt) {
    if (
      typeof Plotly === 'undefined' ||
      !document.getElementById(IR_CLONAL_UMAP_PLOT_ID)
    ) {
      if (attempt < 60) {
        window.requestAnimationFrame(function () {
          draw(attempt + 1);
        });
      }
      return;
    }
    window.cerebroProjection.render2DCategorical(
      meta,
      data,
      hover,
      group_centers || null,
      null,
      {}
    );
  };
  draw(0);
};

shinyjs.irClonalUMAPClearSelection = function () {
  window.cerebroProjection.clearSelection(IR_CLONAL_UMAP_PLOT_ID);
};

shinyjs.irClonalUMAPZoomToSelection = function () {
  window.cerebroProjection.zoomToSelection(IR_CLONAL_UMAP_PLOT_ID);
};
