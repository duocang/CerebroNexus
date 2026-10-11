from pathlib import Path
p=Path(__file__).resolve().parent
(p/'paint').mkdir(exist_ok=True)
s=(p/'perf-server.R').read_text().replace('stability/perf.js','stability/paint.js')
(p/'paint-server.R').write_text(s)
j=(p/'perf.js').read_text().replace('const ops=[],clock=[],memory=[]','const ops=[],clock=[],memory=[],painted=[]')
j=j.replace('{ops,clock,memory,','{ops,clock,memory,painted,')
j=j.replace("  function boot(){", """  window.addEventListener('cerebro:cell-view-ready',e=>{const eventAt=performance.now();queueMicrotask(()=>{
    const host=document.getElementById(e.detail?.id+'_cell_view_host');
    const pane=host?.closest('.tab-pane');
    painted.push({at:performance.now(),eventAt,detail:e.detail,context:CerebroDatasetContext.current(),
      counts:Array.from(host?.querySelectorAll('canvas[data-point-count]')||[]).map(c=>Number(c.dataset.pointCount)),
      loaderVisible:document.getElementById('cerebro-page-loader')?.classList.contains('is-visible'),
      pageReady:pane?.dataset.cerebroPageReady,ariaBusy:pane?.getAttribute('aria-busy'),
      text:host?.innerText,visibility:document.visibilityState});show();
  });});
  function boot(){""")
(p/'paint.js').write_text(j)
