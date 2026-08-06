// Shared plotly layout factory for projection plots.
//
// Why this file exists:
//   - overview / spatial / gene_expression / module/projection each rendered
//     a UMAP via plotly with a layout object. The four files defined four
//     near-identical ~60-line const layout objects. Color tweaks required
//     editing all four.
//   - This factory is the single source of truth. Each consumer module
//     prepends this file via `paste(shared_layouts_js, module_js)` in its
//     R UI and calls `window.cerebroProjectionLayout.make2D()` /
//     `make3D()` to get a fresh layout object.
//
// Plotly cannot read CSS variables, so the equivalent hex values are inlined
// here. They mirror the --chart-* tokens in custom.css :root (warm neutrals
// that reuse the --c-* app palette) and the R helper cerebro_plotly_theme().
// Those three are the single design source — keep them in sync.

(function () {
  const fontFamily =
    '"Segoe UI Variable", "Segoe UI", -apple-system, BlinkMacSystemFont, ' +
    '"Helvetica Neue", Arial, sans-serif';

  // Mirror of custom.css --chart-* tokens — keep in sync with :root.
  const C = {
    grid:         '#ececec', // --chart-grid  (= --c-border)
    line:         '#e0e0e0', // --chart-axis  (= --c-border-2)
    tick:         '#6b6b70', // --chart-tick  (= --c-text-2)
    title:        '#1c1c1e', // --chart-title (= --c-text)
    signal:       '#2f6fd6', // --chart-signal (= --c-blue)
    accent:       '#f97316', // --chart-accent (= --c-amber)
    hoverBg:      'rgba(255, 255, 255, 0.95)',
    transparent:  'rgba(255, 255, 255, 0)',
  };

  // Exposed so non-projection plotly charts (violin / bar / sankey / IR) and
  // any future consumer can share the exact same axis/hover/font styling
  // instead of re-hardcoding it. The projection factory below uses these too.
  const THEME = {
    font:       fontFamily,
    grid:       C.grid,
    axis:       C.line,
    tick:       C.tick,
    title:      C.title,
    signal:     C.signal,
    accent:     C.accent,
    hoverBg:    C.hoverBg,
    transparent: C.transparent,
  };

  function makeAxis() {
    return {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
      gridcolor: C.grid,
      linecolor: C.line,
      tickfont:  { color: C.tick,  family: fontFamily },
      titlefont: { color: C.title, family: fontFamily },
    };
  }

  function makeHoverLabel() {
    return {
      font: { size: 12, color: C.title, family: fontFamily },
      bgcolor: C.hoverBg,
      bordercolor: C.grid,
      align: 'left',
    };
  }

  /**
   * Build a 2D projection layout.
   * @param {Object} [opts]
   * @param {string} [opts.uirevision]  if provided, sets layout.uirevision
   * @param {boolean} [opts.legend=true]  include legend.itemsizing
   *                                       (gene_expression sets false)
   * @returns {Object} a fresh plotly layout
   */
  function make2D(opts) {
    opts = opts || {};
    const layout = {
      hovermode: 'closest',
      dragmode: 'select',
      // Margins tuned to the actual label extents (measured in-browser), so the
      // plotting area reaches close to the box edges instead of floating in the
      // middle:
      //   l:30 — y-tick labels ("-10".."10") only need ~28px, tight to the edge.
      //   b:22 — x-tick labels sit just under the axis; no dead space above the
      //          "selected cells" footer.
      //   r:12 — no right-hand axis, so keep it minimal.
      //   t:8  — the custom HTML legend already occupies the band above the plot
      //          (Plotly's default t:50 is for a title we never draw).
      margin: { l: 30, r: 12, b: 22, t: 8, pad: 4 },
      xaxis: makeAxis(),
      yaxis: makeAxis(),
      hoverlabel: makeHoverLabel(),
      plot_bgcolor:  C.transparent,
      paper_bgcolor: C.transparent,
    };
    if (opts.legend !== false) layout.legend = { itemsizing: 'constant' };
    if (opts.uirevision != null) layout.uirevision = opts.uirevision;
    return layout;
  }

  /**
   * Build a 3D projection layout. Same options as make2D.
   */
  function make3D(opts) {
    opts = opts || {};
    const layout = {
      hovermode: 'closest',
      margin: { l: 50, r: 50, b: 50, t: 50, pad: 4 },
      scene: { xaxis: makeAxis(), yaxis: makeAxis(), zaxis: makeAxis() },
      hoverlabel: makeHoverLabel(),
      plot_bgcolor:  C.transparent,
      paper_bgcolor: C.transparent,
    };
    if (opts.legend !== false) layout.legend = { itemsizing: 'constant' };
    if (opts.uirevision != null) layout.uirevision = opts.uirevision;
    return layout;
  }

  // Idempotent: prepended into every projection module's extendShinyjs(text=)
  // means this IIFE may run multiple times in the same document; assigning
  // to window is safe.
  window.cerebroProjectionLayout = {
    make2D: make2D,
    make3D: make3D,
    theme: THEME,
  };
})();
