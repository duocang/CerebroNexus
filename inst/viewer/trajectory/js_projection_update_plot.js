// =============================================================================
// Trajectory projection: thin wrappers over the shared 2-D Canvas renderer.
// The trajectory path is passed as line-segment shapes below the cells.
//
// The R observer calls:
//   trajectoryUpdatePlot2DContinuous(meta, data, hover, group_centers, container, shapes)
//   trajectoryUpdatePlot2DCategorical(meta, data, hover, group_centers, container, shapes)
// shinyjs delivers positional args as ONE array `params`.
// =============================================================================

const TRAJECTORY_PLOT_ID = 'trajectory_projection';

if (window.cerebroProjection) {
  window.cerebroProjection.registerPlot(TRAJECTORY_PLOT_ID);
}

shinyjs.trajectoryUpdatePlot2DContinuous = function (params) {
  const [meta, data, hover, group_centers, container, shapes] = params;
  meta.plot_id = TRAJECTORY_PLOT_ID;
  window.cerebroCanvasProjection.renderContinuous(meta, data, hover, group_centers, container, {
    shapes: shapes || [],
  });
};

shinyjs.trajectoryUpdatePlot2DCategorical = function (params) {
  const [meta, data, hover, group_centers, container, shapes] = params;
  meta.plot_id = TRAJECTORY_PLOT_ID;
  window.cerebroCanvasProjection.renderCategorical(meta, data, hover, group_centers, container, {
    shapes: shapes || [],
  });
};

shinyjs.trajectoryClearSelection = function () {
  window.cerebroCanvasProjection.clearSelection(TRAJECTORY_PLOT_ID);
};

shinyjs.trajectoryZoomToSelection = function () {
  window.cerebroCanvasProjection.toggleZoom(TRAJECTORY_PLOT_ID);
};
