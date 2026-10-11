(() => {
 const clock=[];let saved=false;
 const pre=document.createElement('pre');pre.id='gene-diag-results';Object.assign(pre.style,{position:'fixed',bottom:0,right:0,zIndex:99999,maxHeight:'70px',maxWidth:'550px',overflow:'auto',background:'white'});document.body.appendChild(pre);
 const controls=document.createElement('div');Object.assign(controls.style,{position:'fixed',bottom:'75px',right:0,zIndex:99999});document.body.appendChild(controls);
 const button=(id,text,fn)=>{const b=document.createElement('button');b.id=id;b.textContent=text;b.onclick=fn;controls.appendChild(b);};
 button('diag-clock','Clock',()=>{const id=performance.now();clock.push({id,start:id});Shiny.setInputValue('remaining_clock',id,{priority:'event'});});
 button('diag-save','Save diagnosis',()=>{const result={events:window.geneDiagEvents.slice(),clock,viewport:[innerWidth,innerHeight],ua:navigator.userAgent,context:CerebroDatasetContext.current()};Shiny.setInputValue('remaining_save',JSON.parse(JSON.stringify(result)),{priority:'event'});});
 function boot(){if(!window.Shiny?.shinyapp?.$socket){setTimeout(boot,50);return;}
  Shiny.addCustomMessageHandler('remaining_clock',e=>{Object.assign(clock.find(c=>Math.abs(c.id-e.id)<.01)||{},e,{end:performance.now()});});
  Shiny.addCustomMessageHandler('remaining_saved',(_message)=>{saved=true;document.getElementById('diag-save').textContent='Saved';});
  $(document).on('shiny:inputchanged',e=>{if(/expression|sidebar/.test(e.name))geneDiag('input',{name:e.name,value:e.value});});
  $(document).on('shiny:recalculating shiny:recalculated shiny:value',e=>{
    geneDiag('output',{kind:e.type,id:e.target.id,visible:e.target.offsetParent!==null});
    if(e.type==='shiny:value'&&e.target.id==='expression_by_group')requestAnimationFrame(()=>requestAnimationFrame(()=>{
      if(e.target.querySelector('svg.main-svg .cartesianlayer'))geneDiag('complete_group_painted');
    }));
  });
 }boot();
 setInterval(()=>{pre.textContent=JSON.stringify({events:window.geneDiagEvents,clock,saved});},500);
})();
