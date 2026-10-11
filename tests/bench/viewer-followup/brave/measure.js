(() => {
  const runs=[]; let current=null, lastMessage=performance.now();
  const panel=document.createElement('pre');panel.id='bench-results';
  Object.assign(panel.style,{position:'fixed',bottom:'0',right:'0',maxHeight:'100px',overflow:'auto',zIndex:99999,background:'#fff',fontSize:'11px'});
  document.body.appendChild(panel);
  const save=document.createElement('button');save.textContent='Save benchmark evidence';save.id='bench-save';
  Object.assign(save.style,{position:'fixed',bottom:'105px',right:'0',zIndex:99999});document.body.appendChild(save);
  save.onclick=()=>Shiny.setInputValue('bench_save',{runs,viewport:[innerWidth,innerHeight],ua:navigator.userAgent,resources:performance.getEntriesByType('resource').map(x=>({name:x.name,bytes:x.transferSize,duration:x.duration})),errors:Array.from(document.querySelectorAll('.shiny-output-error')).filter(x=>x.offsetParent!==null).map(x=>x.textContent)},{priority:'event'});
  function show(){panel.textContent=JSON.stringify({runs,viewport:[innerWidth,innerHeight],ua:navigator.userAgent,visibility:document.visibilityState,quietMs:performance.now()-lastMessage});}
  document.addEventListener('click',e=>{
    const a=e.target.closest('a[href^="#shiny-tab-"]');if(!a)return;
    current={page:a.getAttribute('href'),start:performance.now(),setupQuietMs:performance.now()-lastMessage,visibility:document.visibilityState,events:[],decodes:[],bytes:0,messages:0,heapStart:performance.memory?.usedJSHeapSize};runs.push(current);show();
  },true);
  for(const event of ['cerebro:linkedviews-ready','cerebro:specialist-state'])window.addEventListener(event,e=>{
    if(!current)return;current.events.push({at:performance.now()-current.start,detail:e.detail});
    current.heap=performance.memory?.usedJSHeapSize;
    current.linked=window.cerebroLinkedViewsState?.summary();show();
  });
  function boot(){
    if(!window.Shiny?.shinyapp?.$socket||!window.CBViewWire){setTimeout(boot,50);return;}
    const old=CBViewWire;window.CBViewWire=Object.assign({},old,{unpack:function(x){const start=performance.now(),r=old.unpack(x);if(current)current.decodes.push({start:start-current.start,ms:performance.now()-start,bytes:x.byteLength});return r;}});
    Shiny.shinyapp.$socket.addEventListener('message',e=>{lastMessage=performance.now();if(!current)return;current.messages++;current.bytes+=typeof e.data==='string'?new TextEncoder().encode(e.data).length:e.data.byteLength||e.data.size||0;show();});
    $(document).on('shiny:inputchanged',e=>{if(current && /render_request|coordviews/.test(e.name)){current.requests=current.requests||[];current.requests.push({name:e.name,at:performance.now()-current.start});}});
    Shiny.addCustomMessageHandler('bench_saved',()=>save.textContent='Evidence saved');show();
  }boot();setInterval(()=>{if(current)current.heapPeak=Math.max(current.heapPeak||0,performance.memory?.usedJSHeapSize||0);show();},250);
})();
