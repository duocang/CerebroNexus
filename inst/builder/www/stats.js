(function () {
  "use strict";

  var reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  var scanFrame = null;
  var pendingRoots = new Set();

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

  function scan(root) {
    root = root && root.querySelectorAll ? root : document;
    if (root.matches && root.matches("[data-builder-stats-chart]")) render(root);
    root.querySelectorAll("[data-builder-stats-chart]").forEach(render);
  }

  function scheduleScan(root) {
    var candidate = root && root.querySelectorAll ? root : document;
    var covered = Array.from(pendingRoots).some(function (pending) {
      return pending === candidate ||
        (typeof pending.contains === "function" && pending.contains(candidate));
    });
    if (!covered) {
      pendingRoots.forEach(function (pending) {
        if (
          typeof candidate.contains === "function" &&
          candidate.contains(pending)
        ) pendingRoots.delete(pending);
      });
      pendingRoots.add(candidate);
    }
    if (scanFrame !== null) return;
    scanFrame = window.requestAnimationFrame(function () {
      scanFrame = null;
      var roots = Array.from(pendingRoots);
      pendingRoots.clear();
      roots.forEach(scan);
    });
  }

  document.addEventListener("shiny:value", function (event) {
    scheduleScan(event.target);
  });
  document.addEventListener("DOMContentLoaded", function () { scan(document); });
  window.BuilderStats = { scan: scan };
}());
