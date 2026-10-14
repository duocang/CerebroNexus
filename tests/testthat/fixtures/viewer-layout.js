(tab) => {
  const root = document.getElementById('shiny-tab-' + tab);
  const visible = el => el.getClientRects().length && getComputedStyle(el).visibility !== 'hidden';
  const rect = el => {
    const r = el.getBoundingClientRect();
    return {left:r.left, right:r.right, top:r.top, bottom:r.bottom, width:r.width, height:r.height};
  };
  const toolbar = root.querySelector('.cerebro-viz-toolbar, .cv-topbar');
  const canvases = Array.from(root.querySelectorAll('.cv-pane:not(.cv-hidden) canvas:not(.cv-mini)')).filter(visible);
  const actions = Array.from(toolbar.querySelectorAll('.cerebro-toolbar-actions button')).filter(visible);
  const legends = Array.from(root.querySelectorAll('.cv-legend, .hla-legend-row')).filter(el => visible(el) && el.textContent.trim());
  const tabs = root.querySelector('.cerebro-analysis-tabs > .nav-tabs');
  const activeTab = tabs && tabs.querySelector('li.active > a');
  const boxes = Array.from(new Set(canvases.map(c => c.closest('.cv-pane')))).map(rect);
  return {
    heading: !!root.querySelector('h1')?.textContent.trim(),
    toolbar:rect(toolbar), canvases:canvases.map(rect),
    accessibleCanvases:canvases.every(el => el.getAttribute('aria-hidden') === 'true' ||
      (el.getAttribute('role') === 'img' &&
      (el.getAttribute('aria-describedby') || '').split(/\s+/).some(id => document.getElementById(id)?.textContent.trim()))) &&
      canvases.some(el => el.getAttribute('role') === 'img'),
    decorativeMinimaps:Array.from(root.querySelectorAll('.cv-mini')).every(el => el.getAttribute('aria-hidden') === 'true'),
    singleGpuLayer:canvases.every(el => el.closest('.cv-pane').querySelectorAll('.cv-gpu-layer').length <= 1),
    paneOverlap:boxes.some((a, i) => boxes.slice(i+1).some(b =>
      Math.min(a.right,b.right)-Math.max(a.left,b.left)>1 &&
      Math.min(a.bottom,b.bottom)-Math.max(a.top,b.top)>1)),
    actions:actions.map(el => ({...rect(el), label:el.textContent.trim(), font:parseFloat(getComputedStyle(el).fontSize)})),
    legends:legends.map(el => ({...rect(el), overflow:el.scrollWidth > el.clientWidth + 1})),
    tabs:tabs ? {rows:new Set(Array.from(tabs.children).map(el=>Math.round(el.getBoundingClientRect().top))).size,
      color:activeTab && getComputedStyle(activeTab).color,
      underline:activeTab && getComputedStyle(activeTab).borderBottomColor} : null,
    documentOverflow:document.documentElement.scrollWidth > innerWidth + 1
  };
}
