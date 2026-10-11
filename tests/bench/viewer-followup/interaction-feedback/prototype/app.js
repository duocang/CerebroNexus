/* Isolated UX prototype. All datasets, processing stages and timings are simulated.
   This is not the production Viewer's data, lifecycle or ready implementation. */
(() => {
  'use strict';
  const $ = id => document.getElementById(id);
  const designs = {
    A: ['Minimal / Professional', '更新时保留旧图，用顶部细线与来源标签确认请求已收到；secondary 单独更新。'],
    B: ['Skeleton / Structured', '首次进入用固定结构占位；已有内容更新不回退为 skeleton。观察是否过于像普通网站。'],
    C: ['Progressive Reveal', '先给出图框，再显示可用主图，随后补齐 details、summary 和 table。阶段条不表示虚假的百分比。'],
    D: ['Overlay / Data app', '用局部半透明层强调更新范围。旧图仍可见，但需判断遮罩是否太重。']
  };
  const pages = {
    'Overview': ['Colour by', ['Cell type', 'Cluster', 'Sample'], 'Explore cells and groups in projection space.'],
    'Gene Expression': ['Gene', ['Gad1', 'Gad2', 'Slc17a7'], 'Explore expression across cells in projection space.'],
    'Linked Views': ['Colour by', ['Cell type', 'Cluster', 'Sample'], 'One selection across coordinated views.'],
    'Trajectory': ['Colour by', ['State', 'Pseudotime', 'Sample'], 'Explore cell states along a trajectory.'],
    'Spatial': ['Feature', ['Gad1', 'Gad2', 'Cell type'], 'Explore features in tissue coordinates.'],
    'HLA / TCR Motifs': ['Chain', ['TRB', 'TRA', 'TRG'], 'Explore receptor motifs and network structure.']
  };
  let design = 'A', page = 'Gene Expression', dataset = 'Mouse', phase = 'ready';
  let requested = 'Gad1', displayed = null, operation = 0, timers = [], selected = new Set();
  let primaryReady = true, detailsReady = true, secondaryReady = true, pixels = [], drag = null;
  const visits = new Map();
  const ctx = $('plot').getContext('2d'), selectionCtx = $('selection-layer').getContext('2d');
  function cancel() { operation++; timers.forEach(clearTimeout); timers = []; }
  function later(ms, fn) {
    const serial = operation;
    if (!$('hold').checked) timers.push(setTimeout(() => { if (serial === operation) fn(); }, ms));
  }
  function seedPoints(name, targetPage, value, projection) {
    let seed = name === 'Mouse' ? 31415 : 27182;
    const rand = () => ((seed = (1664525 * seed + 1013904223) >>> 0) / 4294967296);
    const count = targetPage === 'HLA / TCR Motifs' ? 85 : 2200;
    const clusters = [[.22,.4],[.47,.38],[.68,.49],[.42,.69],[.76,.24],[.19,.72]];
    return Array.from({length:count}, (_,i) => {
      const k = i % clusters.length, center = clusters[k];
      const a = rand() * Math.PI * 2, radius = Math.sqrt(rand());
      let x = center[0] + Math.cos(a) * radius * (.08 + k % 3 * .015);
      let y = center[1] + Math.sin(a) * radius * (.095 + k % 2 * .04);
      if (targetPage === 'Trajectory') { x = .12 + i / count * .76; y = .48 + Math.sin(x * 8) * .14 + (rand()-.5)*.09 + (i % 3 - 1) * Math.max(0,x-.5) * .6; }
      if (targetPage === 'Spatial') { x = .15 + i%55/55*.7; y=.13+Math.floor(i/55)/40*.74; }
      if (targetPage === 'HLA / TCR Motifs') { x=.5+Math.cos(a)*radius*.35; y=.48+Math.sin(a)*radius*.34; }
      if (projection === 'tSNE') [x,y] = [.5+(y-.5)*.9,.5+(x-.5)*.85];
      const score = Math.max(0,Math.min(1, (value.includes('2') ? 1-k/6 : k/6) * .65 + rand()*.35));
      return {x,y,score,id:i,k};
    });
  }
  function makeFrame() { return {dataset,page,value:requested,projection:$('projection').value,points:seedPoints(dataset,page,requested,$('projection').value)}; }
  function configurePage() {
    $('page-name').textContent = page;
    $('page-description').textContent = pages[page][2];
    $('parameter-label').textContent = pages[page][0];
    $('parameter').replaceChildren(...pages[page][1].map(value => new Option(value,value)));
    requested = pages[page][1][0];
    $('selection-label').textContent = page === 'HLA / TCR Motifs' ? 'selected nodes' : 'selected cells';
    document.querySelectorAll('[data-page]').forEach(b => { b.classList.toggle('active',b.dataset.page===page); b.setAttribute('aria-current',b.dataset.page===page?'page':'false'); });
  }
  function blankSecondary(target, title, detail, skeleton = false) {
    target.innerHTML = skeleton ? '<div class="skeleton-row"></div><div class="skeleton-row"></div><div class="skeleton-row"></div>' : '';
    if (!skeleton) { const box = document.createElement('div'); box.className='empty-text'; const strong=document.createElement('strong'), small=document.createElement('span'); strong.textContent=title; small.textContent=detail; box.append(strong,small);target.append(box); }
  }
  function panels() {
    const loading = !secondaryReady && phase!=='empty' && phase!=='error';
    $('stats-panel').setAttribute('aria-busy',String(loading));
    $('table-panel').setAttribute('aria-busy',String(loading && selected.size>0));
    $('selected-count').textContent=selected.size.toLocaleString();
    const icon = design==='D' && loading ? '<span class="mini-spinner" aria-hidden="true"></span> ' : '';
    $('stats-status').innerHTML = loading ? icon+'Loading…' : phase==='error'?'Unavailable':phase==='empty'?'No data':'Ready';
    $('table-status').innerHTML = selected.size ? (loading?icon+'Loading…':'Ready') : 'No selection';
    if (phase==='empty'||phase==='error') {
      blankSecondary($('statistics'),'No current result',phase==='empty'?'调整筛选后重试':'主图恢复后再计算');
      blankSecondary($('table-content'),'No selection','没有当前数据集的选择'); return;
    }
    if (loading) blankSecondary($('statistics'),'Loading statistics…',primaryReady?'主图已可用，可继续探索':'等待当前主图结果',design==='B');
    else {
      const mean=displayed?displayed.points.reduce((a,p)=>a+p.score,0)/displayed.points.length:0;
      $('statistics').innerHTML='<div class="metrics"><div><strong>'+mean.toFixed(2)+'</strong><span>Mean · synthetic</span></div><div><strong>'+(displayed?displayed.points.length.toLocaleString():'0')+'</strong><span>Demo '+(page==='HLA / TCR Motifs'?'nodes':'points')+'</span></div></div><p class="stats-note">'+dataset+' / '+requested+' · 示例统计</p>';
    }
    if (!selected.size) blankSecondary($('table-content'),'No selection','在主图框选，或点击 Select example region');
    else if (loading) blankSecondary($('table-content'),'Loading selected cells…',selected.size+' selected · 高亮已生效',design==='B');
    else {
      const rows=[...selected].slice(0,3).map(i=>'<tr><td>demo-'+String(i+1).padStart(5,'0')+'</td><td>Group '+(i%6+1)+'</td><td>'+displayed.points[i].score.toFixed(3)+'</td></tr>').join('');
      $('table-content').innerHTML='<table class="table"><thead><tr><th>Identity</th><th>Group</th><th>Value</th></tr></thead><tbody>'+rows+'</tbody></table><p class="stats-note">前 3 条示例 · '+selected.size+' selected</p>';
    }
  }
  function renderState() {
    document.body.dataset.phase=phase;document.body.dataset.design=design;
    $('dataset-name').textContent=dataset;
    const busy=phase==='loading'||phase==='updating';
    document.querySelector('.progressive-steps').hidden=design!=='C';
    document.querySelector('.legend').hidden=!displayed;
    $('workspace-status').textContent=phase==='error'?'Could not load':phase==='empty'?'No data':busy?(phase==='updating'?'Updating…':'Loading view…'):secondaryReady?'Ready':'Primary ready · secondary loading';
    $('workspace-status').className='status'+(busy?' pending':phase==='error'?' error':'');
    $('selector-pending').hidden=!busy;
    $('apply').textContent=busy?'Update pending…':'Update view';
    $('apply').setAttribute('aria-busy',String(busy));
    $('plot-panel').setAttribute('aria-busy',String(busy));
    $('select').disabled=!primaryReady; $('clear').disabled=!primaryReady||!selected.size;
    $('plot-status').textContent=phase==='loading'?'Preparing primary':phase==='updating'?'Updating to '+requested:phase==='error'?'Error':phase==='empty'?'No data':'Primary ready';
    $('plot-title').textContent=(displayed?displayed.value:requested)+' · '+(displayed?displayed.projection:$('projection').value);
    const axisName=page==='Spatial'?'Tissue':page==='Trajectory'?'DR':$('projection').value;
    document.querySelectorAll('.axis').forEach(axis=>axis.hidden=page==='HLA / TCR Motifs'||page==='Linked Views');
    document.querySelector('.axis-x').textContent=axisName+' 1';document.querySelector('.axis-y').textContent=axisName+' 2';
    $('provenance').textContent=displayed?displayed.dataset+' / '+displayed.value:dataset+' / pending';
    $('primary-note').textContent=primaryReady?(phase==='updating'?'旧图仍可探索 · 结果来源见左侧':detailsReady?'主图可用 · 可 hover / 框选':'主图可用 · cell details 加载中'):'等待当前数据集主图';
    $('placeholder').hidden=!['loading','empty','error'].includes(phase);
    $('placeholder-title').textContent=phase==='error'?'Unable to load this view':phase==='empty'?'No data for this filter':design==='C'?'Plot frame ready':'Preparing '+page;
    $('placeholder-detail').textContent=phase==='error'?'模拟请求失败。保留当前选项，可重试。':phase==='empty'?'这不是加载中。请调整筛选或恢复默认参数。':dataset+' · waiting for the current result';
    $('retry').hidden=phase!=='error'&&phase!=='empty';$('retry').textContent=phase==='empty'?'Reset filter':'Retry';
    $('overlay').hidden=!(design==='D'&&phase==='updating');
    $('overlay-label').textContent='Updating to '+requested+'…';
    $('old-label').hidden=phase!=='updating';
    $('old-label').textContent=displayed?'显示 '+displayed.value+' · 正在请求 '+requested:'';
    const requestedReady=primaryReady&&phase!=='updating';
    for(const [id,done,active] of [['step-frame',true,false],['step-primary',requestedReady,!requestedReady],['step-identity',detailsReady,requestedReady&&!detailsReady],['step-secondary',secondaryReady,requestedReady&&!secondaryReady]]) $(id).className=done?'done':active?'active':'';
    $('event-input').className='done';$('event-primary').className=primaryReady?'done':'active';$('event-secondary').className=secondaryReady?'done':primaryReady?'active':'';
    $('event-input').textContent='Input acknowledged';$('event-primary').textContent=requestedReady?'Primary usable':phase==='updating'?'New primary pending':'Primary pending';$('event-secondary').textContent=secondaryReady?'Secondary complete':'Secondary pending';
    if(phase==='empty'||phase==='error'){ $('event-primary').textContent=phase==='empty'?'No matching data':'Request failed';$('event-secondary').textContent='No current result'; }
    panels();
  }
  function draw() {
    const box=$('plot-stage').getBoundingClientRect(),dpr=Math.min(window.devicePixelRatio||1,2);
    for(const canvas of [$('plot'),$('selection-layer')]){canvas.width=Math.round(box.width*dpr);canvas.height=Math.round(box.height*dpr);canvas.getContext('2d').setTransform(dpr,0,0,dpr,0,0);}
    ctx.clearRect(0,0,box.width,box.height);pixels=[];
    if(!displayed)return;
    const linked=displayed.page==='Linked Views',graph=displayed.page==='HLA / TCR Motifs';
    const panels=linked?4:1;
    for(let panel=0;panel<panels;panel++){
      const width=box.width/(linked?2:1),height=box.height/(linked?2:1),ox=linked?(panel%2)*width:0,oy=linked?Math.floor(panel/2)*height:0;
      if(linked){ctx.strokeStyle='#ededf0';ctx.strokeRect(ox,oy,width,height);ctx.fillStyle='#66666d';ctx.font='10px Segoe UI';ctx.fillText(['UMAP','tSNE','Trajectory','Clonotype'][panel],ox+10,oy+16);}
      const local=displayed.points.map(p=>({x:ox+24+p.x*(width-38),y:oy+20+p.y*(height-44),id:p.id}));
      if(graph){ctx.strokeStyle='#d8dce1';ctx.lineWidth=.7;local.forEach((p,i)=>{const q=local[(i+7)%local.length];if(Math.hypot(p.x-q.x,p.y-q.y)<140){ctx.beginPath();ctx.moveTo(p.x,p.y);ctx.lineTo(q.x,q.y);ctx.stroke();}});}
      displayed.points.forEach((p,i)=>{const xy=local[i];pixels.push(xy);const isSelected=selected.has(p.id);ctx.fillStyle=isSelected?'#2f6fd6':p.score<.22?'#c7cbd1':`hsl(${31-p.score*16} 65% ${75-p.score*32}%)`;ctx.globalAlpha=selected.size&&!isSelected?.32:.84;ctx.beginPath();ctx.arc(xy.x,xy.y,graph?(isSelected?5:3.5):linked?1.05:1.65,0,Math.PI*2);ctx.fill();});
    }
    ctx.globalAlpha=1;
  }
  function readyPrimary() {
    const old=$('transition-frame');
    if(displayed&&!$('motion').checked&&!matchMedia('(prefers-reduced-motion: reduce)').matches){
      old.width=$('plot').width;old.height=$('plot').height;old.getContext('2d').drawImage($('plot'),0,0);
      old.hidden=false;old.classList.remove('crossfade');void old.offsetWidth;old.classList.add('crossfade');
    }
    displayed=makeFrame();primaryReady=true;phase='secondary';
    draw();renderState();
  }
  function begin(kind='first') {
    cancel();$('special').value='normal';$('tooltip').hidden=true;$('transition-frame').hidden=true;drag=null;
    detailsReady=false;secondaryReady=false;
    if(kind==='update'&&displayed){phase='updating';primaryReady=true;}
    else{phase='loading';primaryReady=false;displayed=null;selected.clear();draw();}
    renderState();
    later(1800,readyPrimary);
    later(2500,()=>{detailsReady=true;renderState();});
    later(3600,()=>{secondaryReady=true;detailsReady=true;phase='ready';visits.set(page,{...displayed});renderState();});
  }
  function setSelection(ids) {
    if(!primaryReady||!displayed)return;
    if(phase==='updating'){selected=new Set(ids);draw();renderState();return;}
    cancel();selected=new Set(ids);secondaryReady=false;phase='secondary';requested=displayed.value;$('parameter').value=requested;
    draw();renderState();
    later(900,()=>{secondaryReady=true;detailsReady=true;phase='ready';renderState();});
  }
  function specialState(value) {
    if(value==='normal'){begin('first');return;}
    cancel();$('tooltip').hidden=true;selected.clear();
    if(value==='unselected'){if(!displayed)displayed=makeFrame();requested=displayed.value;$('parameter').value=requested;phase='ready';primaryReady=true;secondaryReady=true;detailsReady=true;draw();}
    else{phase=value;primaryReady=false;secondaryReady=true;detailsReady=false;displayed=null;draw();}
    renderState();
  }
  function setDesign(value) {
    design=value;document.querySelectorAll('.design-picker button').forEach(b=>b.setAttribute('aria-pressed',String(b.dataset.design===design)));
    $('design-title').textContent=design+' · '+designs[design][0];$('design-note').textContent=designs[design][1];
    const url=new URL(location.href);url.searchParams.set('design',design);history.replaceState(null,'',url);renderState();
  }
  document.querySelectorAll('.design-picker button').forEach(b=>b.addEventListener('click',()=>setDesign(b.dataset.design)));
  document.querySelectorAll('[data-page]').forEach(b=>b.addEventListener('click',()=>{
    if(b.dataset.page===page)return;
    if(displayed&&phase==='ready')visits.set(page,{...displayed});
    cancel();page=b.dataset.page;configurePage();$('tooltip').hidden=true;selected.clear();
    const saved=visits.get(page);
    if(saved&&saved.dataset===dataset){displayed=saved;requested=saved.value;$('parameter').value=requested;$('projection').value=saved.projection;primaryReady=detailsReady=secondaryReady=true;phase='ready';draw();renderState();$('workspace-status').textContent='Ready · restored view';}
    else begin('first');
  }));
  $('dataset').addEventListener('change',()=>{dataset=$('dataset').value;visits.clear();begin('first');});
  $('parameter').addEventListener('change',()=>{requested=$('parameter').value;begin('update');});
  $('projection').addEventListener('change',()=>begin('update'));
  $('first').addEventListener('click',()=>begin('first'));
  $('update').addEventListener('click',()=>{if(!displayed)displayed=makeFrame();const values=pages[page][1];requested=values[(values.indexOf(requested)+1)%values.length];$('parameter').value=requested;draw();begin('update');});
  $('apply').addEventListener('click',()=>begin('update'));
  $('secondary').addEventListener('click',()=>{cancel();secondaryReady=detailsReady=false;readyPrimary();selected=new Set(displayed.points.filter(p=>p.k===2).map(p=>p.id));draw();renderState();later(700,()=>{detailsReady=true;renderState();});later(1800,()=>{secondaryReady=true;phase='ready';renderState();});});
  $('revisit').addEventListener('click',()=>{cancel();if(!displayed)displayed=makeFrame();requested=displayed.value;$('parameter').value=requested;$('projection').value=displayed.projection;phase='ready';primaryReady=detailsReady=secondaryReady=true;renderState();draw();$('workspace-status').textContent='Ready · restored view';});
  $('retry').addEventListener('click',()=>begin('first'));
  $('select').addEventListener('click',()=>setSelection(displayed.points.filter(p=>p.k===2).map(p=>p.id)));
  $('clear').addEventListener('click',()=>setSelection([]));
  $('special').addEventListener('change',()=>specialState($('special').value));
  function clearTransition(){ $('transition-frame').hidden=true;$('transition-frame').width=0; }
  $('motion').addEventListener('change',()=>{document.body.classList.toggle('reduced-motion',$('motion').checked);clearTransition();});
  matchMedia('(prefers-reduced-motion: reduce)').addEventListener('change',clearTransition);
  $('hold').addEventListener('change',()=>{if($('hold').checked)cancel();else if(['loading','updating'].includes(phase))begin(phase==='loading'?'first':'update');else if(phase==='secondary')later(1800,()=>{detailsReady=secondaryReady=true;phase='ready';renderState();});});
  const pointer=e=>{const b=$('plot').getBoundingClientRect();return{x:e.clientX-b.left,y:e.clientY-b.top};};
  $('plot').addEventListener('pointerdown',e=>{if(!primaryReady||!displayed)return;drag=pointer(e);$('plot').setPointerCapture(e.pointerId);$('tooltip').hidden=true;});
  $('plot').addEventListener('pointermove',e=>{if(!primaryReady||!displayed)return;const p=pointer(e);
    if(drag){selectionCtx.clearRect(0,0,$('plot').width,$('plot').height);selectionCtx.fillStyle='#2f6fd618';selectionCtx.strokeStyle='#2f6fd6';selectionCtx.fillRect(drag.x,drag.y,p.x-drag.x,p.y-drag.y);selectionCtx.strokeRect(drag.x,drag.y,p.x-drag.x,p.y-drag.y);return;}
    let near=null,d=12;for(const q of pixels){const dist=Math.hypot(q.x-p.x,q.y-p.y);if(dist<d){near=q;d=dist;}}
    $('tooltip').hidden=!near;if(near){$('tooltip').textContent=detailsReady?'demo-'+String(near.id+1).padStart(5,'0')+' · '+displayed.value+' '+displayed.points[near.id].score.toFixed(3):'Cell #'+(near.id+1)+' · details loading…';$('tooltip').style.left=Math.max(4,Math.min(p.x+12,$('plot-stage').clientWidth-190))+'px';$('tooltip').style.top=Math.max(4,p.y-36)+'px';}
  });
  $('plot').addEventListener('pointerup',e=>{if(!drag)return;const p=pointer(e),a=drag;drag=null;selectionCtx.clearRect(0,0,$('plot').width,$('plot').height);setSelection(pixels.filter(q=>q.x>=Math.min(a.x,p.x)&&q.x<=Math.max(a.x,p.x)&&q.y>=Math.min(a.y,p.y)&&q.y<=Math.max(a.y,p.y)).map(q=>q.id));});
  $('plot').addEventListener('pointerleave',()=>{$('tooltip').hidden=true;});
  $('plot').addEventListener('pointercancel',()=>{drag=null;selectionCtx.clearRect(0,0,$('plot').width,$('plot').height);});
  $('transition-frame').addEventListener('animationend',clearTransition);
  new ResizeObserver(draw).observe($('plot-stage'));
  configurePage();displayed=makeFrame();visits.set(page,{...displayed});
  const initial=new URL(location.href).searchParams.get('design');setDesign(designs[initial]?initial:'A');draw();
})();
