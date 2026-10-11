// Small DOM-readable observation panel. No draw/ready wrappers or app mutations.
(() => {
  const events=[], errors=[];
  const panel=document.createElement('pre'); panel.id='tooltip-regression';
  Object.assign(panel.style,{position:'fixed',bottom:0,right:0,zIndex:99999,
    background:'white',maxHeight:'60px',maxWidth:'520px',overflow:'auto',fontSize:'9px'});
  document.body.appendChild(panel);
  function show(){panel.textContent=JSON.stringify({events,errors});}
  window.addEventListener('cerebro:linkedviews-ready',e=>{
    const s=window.cerebroLinkedViewsState?.summary();
    events.push({at:performance.now(),detail:e.detail,transport:s?.transport,
      context:window.CerebroDatasetContext?.current()});show();
  });
  window.addEventListener('error',e=>{errors.push(e.message);show();});
  window.addEventListener('unhandledrejection',e=>{errors.push(String(e.reason));show();});
  show();
})();
