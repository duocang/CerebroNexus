(() => {
  const ops=[];let current=null;
  const panel=document.createElement('pre');panel.id='bench-diagnostic';
  Object.assign(panel.style,{position:'fixed',top:'0',right:'0',maxHeight:'100px',maxWidth:'700px',overflow:'auto',zIndex:99999,background:'#fff',fontSize:'10px'});document.body.appendChild(panel);
  const save=document.createElement('button');save.id='bench-diagnostic-save';save.textContent='Save diagnostic';Object.assign(save.style,{position:'fixed',top:'105px',right:'0',zIndex:99999});document.body.appendChild(save);
  function show(){panel.textContent=JSON.stringify(ops);}
  function mark(name,value){current={name,value,start:performance.now(),events:[]};ops.push(current);show();}
  document.addEventListener('click',e=>{const a=e.target.closest('a[href^="#shiny-tab-"]');if(a)mark('page',a.getAttribute('href'));},true);
  window.addEventListener('cerebro:specialist-state',e=>{if(current){current.events.push({ms:performance.now()-current.start,detail:e.detail});show();}});
  function boot(){if(!window.Shiny?.shinyapp?.$socket){setTimeout(boot,50);return;}
    $(document).on('shiny:inputchanged',e=>{if(['trajectory_point_color','expression_genes_input','crb_file_selector'].includes(e.name))mark(e.name,e.value);});
  }boot();
  save.onclick=()=>{
    const context=CerebroDatasetContext.current();
    const past=ops.flatMap(o=>o.events.map(e=>e.detail.dataset_context)).filter(Boolean);
    const old=past.filter(c=>c.epoch!==context.epoch||c.generation!==context.generation||c.dataset_key!==context.dataset_key);
    Shiny.setInputValue('bench_diagnostic',{ops,context,gene:document.getElementById('expression_genes_input')?.selectize?.getValue(),state:cerebroCellViews.summary('expression_projection'),heap:performance.memory?.usedJSHeapSize,staleContexts:old.map(c=>({context:c,accepted:CerebroDatasetContext.accepts({dataset_context:c})})),oldEpochRejected:!CerebroDatasetContext.accepts({dataset_context:{...context,epoch:'test-obsolete-epoch'}})},{priority:'event'});
  };
})();
