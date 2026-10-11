(() => {
  const ops=[],clock=[],memory=[],painted=[];let current=null,last=performance.now();
  const panel=document.createElement('pre');panel.id='remaining-results';
  Object.assign(panel.style,{position:'fixed',bottom:'0',right:'0',maxHeight:'95px',maxWidth:'650px',overflow:'auto',zIndex:99999,background:'#fff',fontSize:'10px'});document.body.appendChild(panel);
  const controls=document.createElement('div');Object.assign(controls.style,{position:'fixed',bottom:'100px',right:'0',zIndex:99999});document.body.appendChild(controls);
  function button(id,label,fn){const b=document.createElement('button');b.id=id;b.textContent=label;b.onclick=fn;controls.appendChild(b);return b;}
  function show(){panel.textContent=JSON.stringify({ops,clock,memory,painted,quietMs:performance.now()-last,visibility:document.visibilityState});}
  function mark(name,value){current={name,value:JSON.parse(JSON.stringify(value)),start:performance.now(),events:[],outputs:[],messages:[],heapStart:performance.memory?.usedJSHeapSize};ops.push(current);show();}
  button('remaining-clock','Clock probe',()=>{const id=performance.now();clock.push({id,start:id});Shiny.setInputValue('remaining_clock',id,{priority:'event'});});
  button('remaining-memory','Memory snapshot',()=>Shiny.setInputValue('remaining_memory',String(ops.length),{priority:'event'}));
  button('remaining-save','Save measurements',()=>Shiny.setInputValue('remaining_save',{ops,clock,memory,painted,ua:navigator.userAgent,viewport:[innerWidth,innerHeight],resources:performance.getEntriesByType('resource').map(e=>({name:e.name,bytes:e.transferSize,ms:e.duration})),context:CerebroDatasetContext.current(),errors:Array.from(document.querySelectorAll('.shiny-output-error')).filter(e=>e.offsetParent!==null).map(e=>e.textContent)},{priority:'event'}));
  document.addEventListener('click',e=>{const a=e.target.closest('a[href^="#shiny-tab-"]');if(a)mark('page',a.getAttribute('href'));},true);
  document.addEventListener('keydown',e=>{if(e.key==='Enter'&&e.target.closest('#expression_projection_input_type')){if(current)current.gesture=performance.now();}},true);
  for(const event of ['cerebro:specialist-state','cerebro:linkedviews-ready'])window.addEventListener(event,e=>{
    if(!current)return;current.events.push({ms:performance.now()-current.start,detail:e.detail,text:document.getElementById('expression_projection_cell_view_host')?.innerText});current.linked=window.cerebroLinkedViewsState?.summary();show();
  });
  window.addEventListener('cerebro:cell-view-ready',e=>{const eventAt=performance.now();queueMicrotask(()=>{
    const host=document.getElementById(e.detail?.id+'_cell_view_host');
    const pane=host?.closest('.tab-pane');
    painted.push({at:performance.now(),eventAt,detail:e.detail,context:CerebroDatasetContext.current(),
      counts:Array.from(host?.querySelectorAll('canvas[data-point-count]')||[]).map(c=>Number(c.dataset.pointCount)),
      loaderVisible:document.getElementById('cerebro-page-loader')?.classList.contains('is-visible'),
      pageReady:pane?.dataset.cerebroPageReady,ariaBusy:pane?.getAttribute('aria-busy'),
      text:host?.innerText,visibility:document.visibilityState});show();
  });});
  function boot(){if(!window.Shiny?.shinyapp?.$socket){setTimeout(boot,50);return;}
    Shiny.addCustomMessageHandler('remaining_clock',e=>{Object.assign(clock.find(x=>Math.abs(x.id-e.id)<.01)||{},e,{end:performance.now()});show();});
    Shiny.addCustomMessageHandler('remaining_memory',e=>{memory.push(e);show();});
    Shiny.addCustomMessageHandler('remaining_saved',e=>document.getElementById('remaining-save').textContent='Saved');
    $(document).on('shiny:inputchanged',e=>{
      if(['expression_genes_input','trajectory_point_color','crb_file_selector'].includes(e.name))mark(e.name,e.value);
      else if(current&&/render_request|rendered_key|aux_request/.test(e.name)){current.inputs=current.inputs||[];current.inputs.push({name:e.name,ms:performance.now()-current.start,value:e.value});}
    });
    $(document).on('shiny:recalculating shiny:recalculated shiny:value',e=>{
      if(!current||!e.target.id?.startsWith('expression_'))return;
      current.outputs.push({kind:e.type,id:e.target.id,ms:performance.now()-current.start,visible:e.target.offsetParent!==null});
      if(e.type==='shiny:value'&&e.target.id==='expression_by_group'){
        const op=current;requestAnimationFrame(()=>requestAnimationFrame(()=>{const el=document.getElementById('expression_by_group');if(el?.querySelector('svg.main-svg .cartesianlayer'))op.groupPaint=performance.now()-op.start;show();}));
      }
    });
    Shiny.shinyapp.$socket.addEventListener('message',e=>{last=performance.now();if(!current)return;current.messages.push({ms:last-current.start,bytes:typeof e.data==='string'?new TextEncoder().encode(e.data).length:e.data.byteLength||e.data.size||0});show();});
  }boot();
  new PerformanceObserver(list=>{if(!current)return;current.longTasks=current.longTasks||[];for(const e of list.getEntries())if(e.startTime>=current.start)current.longTasks.push({ms:e.startTime-current.start,duration:e.duration});}).observe({type:'longtask',buffered:true});
  let lastFrame=performance.now();function frame(now){if(current){current.maxFrameGap=Math.max(current.maxFrameGap||0,now-lastFrame);}lastFrame=now;requestAnimationFrame(frame);}requestAnimationFrame(frame);
  setInterval(()=>{if(current)current.heapPeak=Math.max(current.heapPeak||0,performance.memory?.usedJSHeapSize||0);show();},250);
})();
