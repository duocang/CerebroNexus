(() => {
  const ops=[],clock=[]; let current=null;
  const panel=document.createElement('pre'); panel.id='remaining-results';
  Object.assign(panel.style,{position:'fixed',bottom:'0',right:'0',maxHeight:'85px',maxWidth:'650px',overflow:'auto',zIndex:99999,background:'#fff',fontSize:'10px'});document.body.appendChild(panel);
  const controls=document.createElement('div');Object.assign(controls.style,{position:'fixed',bottom:'90px',right:'0',zIndex:99999});document.body.appendChild(controls);
  function show(){panel.textContent=JSON.stringify({ops,clock,visibility:document.visibilityState,focus:document.hasFocus()});}
  function button(id,label,fn){const b=document.createElement('button');b.id=id;b.textContent=label;b.onclick=fn;controls.appendChild(b);}
  button('remaining-clock','Clock probe',()=>{const id=performance.now();clock.push({id,start:id});Shiny.setInputValue('remaining_clock',id,{priority:'event'});});
  button('remaining-save','Save measurements',()=>Shiny.setInputValue('remaining_save',{ops,clock,diag:window.linkedDiagEvents,ua:navigator.userAgent,viewport:[innerWidth,innerHeight],resources:performance.getEntriesByType('resource').map(e=>({name:e.name,bytes:e.transferSize,ms:e.duration})),context:CerebroDatasetContext.current(),errors:Array.from(document.querySelectorAll('.shiny-output-error')).filter(e=>e.offsetParent!==null).map(e=>e.textContent)},{priority:'event'}));
  document.addEventListener('click',e=>{const a=e.target.closest('a[href^="#shiny-tab-"]');if(a){current={name:a.getAttribute('href'),start:performance.now(),events:[],heapStart:performance.memory?.usedJSHeapSize};ops.push(current);show();}},true);
  window.addEventListener('cerebro:linkedviews-ready',e=>{if(!current)return; current.events.push({ms:performance.now()-current.start,detail:e.detail,summary:window.cerebroLinkedViewsState?.summary()});show();});
  function boot(){if(!window.Shiny?.shinyapp?.$socket){setTimeout(boot,50);return;}
    Shiny.addCustomMessageHandler('remaining_clock',e=>{Object.assign(clock.find(x=>Math.abs(x.id-e.id)<.01)||{},e,{end:performance.now()});show();});
    Shiny.addCustomMessageHandler('remaining_saved',e=>document.getElementById('remaining-save').textContent='Saved');
    $(document).on('shiny:inputchanged',e=>{if(current&&e.name.startsWith('coordviews')){current.inputs=current.inputs||[];current.inputs.push({name:e.name,value:e.value,ms:performance.now()-current.start});}});
  }boot();
  setInterval(()=>{if(current)current.heapPeak=Math.max(current.heapPeak||0,performance.memory?.usedJSHeapSize||0);show();},250);
})();
