(function () {
  "use strict";

  var reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");

  function render(chart) {
    if (!window.Plotly || chart.dataset.rendered === "true") return;
    var labels = JSON.parse(chart.dataset.labels || "[]");
    var values = JSON.parse(chart.dataset.values || "[]");
    var actionColor = getComputedStyle(document.documentElement)
      .getPropertyValue("--builder-action")
      .trim() || "currentColor";
    window.Plotly.newPlot(chart, [{
      type: "bar",
      orientation: "h",
      y: labels,
      x: values,
      hovertemplate: "%{y}: %{x:,}<extra></extra>",
      marker: { color: actionColor }
    }], {
      margin: { l: 90, r: 12, t: 8, b: 30 },
      paper_bgcolor: "rgba(0,0,0,0)",
      plot_bgcolor: "rgba(0,0,0,0)",
      transition: { duration: reducedMotion.matches ? 0 : 180 }
    }, { displayModeBar: false, responsive: true });
    chart.dataset.rendered = "true";
    chart.setAttribute("role", "img");
    chart.setAttribute("aria-label", chart.dataset.label || "Verified statistics chart");
  }

  function scan() {
    document.querySelectorAll("[data-builder-stats-chart]").forEach(render);
  }

  document.addEventListener("shiny:value", function () { setTimeout(scan, 0); });
  document.addEventListener("DOMContentLoaded", scan);
  window.BuilderStats = { scan: scan };
}());
