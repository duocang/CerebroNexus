(() => {
  const events=[]; window.linkedDiagEvents=events;
  window.linkedDiag=(name,detail)=>events.push({name,at:performance.now(),detail});
  const emit=window.linkedDiag, Native=window.WebSocket;
  window.WebSocket=new Proxy(Native,{construct(Target,args){
    const ws=new Target(...args);
    ws.addEventListener('message',e=>emit('socket_receive',{bytes:typeof e.data==='string'?new TextEncoder().encode(e.data).length:e.data.byteLength||e.data.size,type:typeof e.data}));
    const send=ws.send; ws.send=function(data){
      if(typeof data==='string'){try{const p=JSON.parse(data); if(!p.data?.remaining_save)emit('socket_send',{method:p.method,data:p.data});}catch(_){}}
      return send.apply(this,arguments);
    };return ws;
  }});
  new PerformanceObserver(list=>list.getEntries().forEach(e=>emit('longtask',{start:e.startTime,duration:e.duration}))).observe({type:'longtask',buffered:true});
  window.addEventListener('error',e=>emit('error',{message:e.message}));
  window.addEventListener('unhandledrejection',e=>emit('rejection',{message:String(e.reason)}));
})();
