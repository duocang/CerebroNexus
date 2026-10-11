(() => {
 const events=[];window.geneDiagEvents=events;
 window.geneDiag=(name,detail)=>events.push({name,at:performance.now(),detail});
 const emit=window.geneDiag;
 const Native=window.WebSocket;
 window.WebSocket=new Proxy(Native,{construct(Target,args){
   const ws=new Target(...args);
   ws.addEventListener('message',e=>emit('socket_receive',{bytes:typeof e.data==='string'?e.data.length:e.data.byteLength||e.data.size,type:typeof e.data}));
   const send=ws.send;ws.send=function(data){
     if(typeof data==='string') {try{const p=JSON.parse(data);emit('socket_send',{method:p.method,data:p.data});}catch(_){}}
     return send.apply(this,arguments);
   };return ws;
 }});
 document.addEventListener('visibilitychange',()=>emit('visibility',{state:document.visibilityState}));
 window.addEventListener('focus',()=>emit('focus'));window.addEventListener('blur',()=>emit('blur'));
 document.addEventListener('keydown',e=>{if(e.target.closest('#expression_projection_input_type_UI'))emit('key',{key:e.key});},true);
 document.addEventListener('click',e=>{if(e.target.closest('#expression_projection_input_type_UI'))emit('gene_click',{text:e.target.textContent?.slice(0,80)});},true);
 new PerformanceObserver(list=>list.getEntries().forEach(e=>emit('longtask',{start:e.startTime,duration:e.duration}))).observe({type:'longtask',buffered:true});
 window.addEventListener('cerebro:specialist-state',e=>emit('specialist',e.detail));
 window.addEventListener('cerebro:cell-view-ready',e=>{const eventAt=performance.now();queueMicrotask(()=>{
   const host=document.getElementById(e.detail.id+'_cell_view_host');
   emit('painted',{eventAt,...e.detail,loader:document.getElementById('cerebro-page-loader')?.className,
     pane:host?.closest('.tab-pane')?.dataset.cerebroPageReady,visibility:document.visibilityState,focus:document.hasFocus()});
 });});
 document.addEventListener('DOMContentLoaded',()=>{
   const loader=document.getElementById('cerebro-page-loader');
   if(loader)new MutationObserver(()=>emit('mask',{className:loader.className})).observe(loader,{attributes:true,attributeFilter:['class']});
 });
})();
