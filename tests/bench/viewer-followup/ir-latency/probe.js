(() => {
  let pending = null;
  document.addEventListener('click', e => {
    const tab = e.target.closest('a[data-value]');
    if (!tab || !['Abundance', 'Homeostasis'].includes(tab.dataset.value)) return;
    pending = { tab: tab.dataset.value, start: performance.now() };
  }, true);
  const attached = new WeakSet();
  function attach() {
    for (const id of ['ir_plot_clonalAbundance', 'ir_plot_clonalHomeostasis']) {
      const plot = document.getElementById(id);
      if (!plot || typeof plot.on !== 'function' || attached.has(plot)) continue;
      attached.add(plot);
      plot.on('plotly_afterplot', () => {
        const current = pending;
        if (!current || !plot.getClientRects().length ||
            !id.toLowerCase().endsWith(current.tab.toLowerCase())) return;
        pending = null;
        const afterplot = performance.now() - current.start;
        requestAnimationFrame(() => requestAnimationFrame(() => {
          const result = { tab: current.tab, afterplot_ms: afterplot,
            painted_ms: performance.now() - current.start,
            traces: plot.querySelectorAll('.trace').length,
            viewport: [innerWidth, innerHeight], visibility: document.visibilityState };
          document.body.dataset.irLatency = JSON.stringify(result);
          Shiny.setInputValue('ir_latency_evidence', result, { priority: 'event' });
        }));
      });
    }
  }
  new MutationObserver(attach).observe(document.documentElement, { childList: true, subtree: true });
})();
