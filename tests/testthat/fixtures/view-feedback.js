'use strict';
const fs = require('fs'), vm = require('vm'), assert = require('assert/strict'), path = require('path');
const events = {}, messages = {}, handlers = {}, elements = {}, domEvents = {};
const storage = new Map();
function element(id) {
  const classes = new Set(['active']); let text = '';
  return { id, dataset: {}, get textContent() { return text + this.children.map(c=>c.textContent).join(''); },
    set textContent(v) { text=v; this.children=[]; }, value: '', children: [], tagName: 'INPUT',
    classList: {add(k) {classes.add(k);},toggle(k,on) { if(on) classes.add(k); else classes.delete(k); }, contains(k) {return classes.has(k);}},
    setAttribute() {}, querySelector(selector) {return selector === 'label' ? {textContent:'Colour'} : this.children.find(c=>(c.className || '').split(' ').includes(selector.slice(1))) || null;},
    insertBefore(el) {this.children.push(el);}, appendChild(el) {this.children.push(el);}, closest() {return this;},
    contains(el) {return el === this;} };
}
const ids = {overview_projection:'overview', expression_projection:'geneExpression', trajectory_projection:'trajectory',
  spatial_projection:'spatial', hla_motif_network:'hla_tcr_motifs', linked:'coordinated_views'};
Object.entries(ids).forEach(([id,pane]) => {
  elements['shiny-tab-'+pane] = element(pane);
  elements[id+'_cell_view_host'] = element(id);
});
const document = {addEventListener:(name,fn)=>domEvents[name]=fn,readyState:'complete',activeElement:null,getElementById:id=>elements[id],createElement:()=>element('')};
const jq = {on(names,fn) {names.split(' ').forEach(name=>handlers[name.split('.')[0]]=fn);return jq;}};
let generation = 1, timeoutCalls = 0;
const window = { jQuery:()=>jq, setTimeout:fn=>fn(),
  addEventListener:(name,fn)=>events[name]=fn, dispatchEvent:event=>{if(events[event.type])events[event.type](event);},
  Shiny:{addCustomMessageHandler:(name,fn)=>messages[name]=fn},
  CerebroDatasetContext:{phase:()=> 'ready', accepts:m=>m.dataset_context?.generation===generation}};
vm.runInNewContext(fs.readFileSync(require('path').join(require('path').dirname(process.argv[2] || path.resolve(__dirname, '../../../inst/viewer/www/view-feedback.js')), 'page-lifecycle.js'), 'utf8'), {window});
vm.runInNewContext(fs.readFileSync(process.argv[2] || path.resolve(__dirname,'../../../inst/viewer/www/view-feedback.js'),'utf8'),
  {window,document,setTimeout:fn=>{timeoutCalls++;fn();return 1;},clearTimeout:()=>{},sessionStorage:{getItem:k=>storage.get(k),setItem:(k,v)=>storage.set(k,v),removeItem:k=>storage.delete(k)},CustomEvent:function(type,data){this.type=type;this.detail=data.detail;}});
let count=0;
function check(v){assert.ok(v);count++;}
function header(id){return elements['shiny-tab-'+ids[id]].querySelector('.cerebro-page-feedback');}
function row(id){return elements['shiny-tab-'+ids[id]].querySelector('.cerebro-local-feedback');}
function emit(name,d){events[name]({detail:{dataset_context:{generation},...d}});}
function change(id,key,value){
  const el=elements[key]||(elements[key]=element(key)); el.value=value;document.activeElement=el;
  handlers.change({target:el});
}
function map(id,token,controls,g=generation){messages.cerebro_view_feedback({id,renderToken:token,controls,dataset_context:{generation:g}});}
function paint(id,token,g=generation){emit('cerebro:cell-view-ready',{id,painted:true,renderToken:token,dataset_context:{generation:g}});}
for(const [id,key] of [['overview_projection','overview_projection_point_color'],['expression_projection','expression_projection_to_display'],
  ['trajectory_projection','trajectory_point_color'],['spatial_projection','spatial_projection_point_color'],['hla_motif_network','hla_color_by']]){
  change(id,key,'a');check(row(id).textContent.includes('Updating'));
  map(id,1,{[key]:'a'});check(row(id).textContent.includes('Updating'));
  change(id,key,'b');paint(id,1);check(row(id).textContent.includes('Updating'));
  map(id,2,{[key]:'b'});paint(id,2);check(!row(id).textContent.includes('Updating'));
  change(id,key,'c');map(id,3,{[key]:'c'},0);paint(id,3,0);check(row(id).textContent.includes('Updating'));
  paint(id,4);map(id,4,{[key]:'c'});check(!row(id).textContent.includes('Updating'));
}
emit('cerebro:specialist-state',{viewId:'overview_projection',selectedCells:123,selectionPending:true});
check(row('overview_projection').textContent.includes('Loading identities'));
emit('cerebro:specialist-state',{viewId:'overview_projection',selectedCells:0,selectionPending:false});
check(row('overview_projection').hidden);
check(row('overview_projection').textContent === '');
emit('cerebro:specialist-state',{viewId:'overview_projection',selectedCells:4775,selectionPending:false});
check(row('overview_projection').hidden);
emit('cerebro:linkedviews-ready',{primaryReady:true,painted:true,complete:false});
check(row('linked').textContent.includes('Loading annotations'));
emit('cerebro:linkedviews-ready',{primaryReady:true,painted:true,complete:true});
check(!row('linked').textContent.includes('Loading annotations'));
emit('cerebro:linkedviews-selection',{selectedCells:37});check(row('linked').hidden);
emit('cerebro:view-operation',{id:'spatial_projection',text:'Background unavailable'});
check(row('spatial_projection').textContent.includes('Background unavailable'));
generation=2;emit('cerebro:dataset-context',{changed:true,phase:'ready'});
check(!row('linked').textContent.includes('37 selected'));
paint('overview_projection',4,1);check(row('overview_projection').textContent.includes('Loading'));
const el=elements.overview_projection_point_color;document.activeElement=null;handlers.change({target:el});
check(!row('overview_projection').textContent.includes('Updating'));
paint('hla_motif_network',7);
handlers['shiny:value']({target:{id:'hla_motif_note',querySelector:()=>({textContent:'No motifs',getAttribute:()=> 'empty'})}});
check(row('hla_motif_network').textContent.includes('No motifs'));
check(!row('hla_motif_network').classList.contains('is-error'));
check(window.CerebroPageLifecycle.isTerminal(elements['shiny-tab-hla_tcr_motifs']));
check(header('hla_motif_network').hidden);
paint('hla_motif_network',7);check(row('hla_motif_network').textContent.includes('No motifs'));
change('hla_motif_network','hla_show_isolated',true);
check(row('hla_motif_network').textContent.includes('Updating'));
paint('hla_motif_network',7);check(elements.hla_motif_network_cell_view_host.classList.contains('cerebro-view-terminal'));
map('hla_motif_network',8,{hla_show_isolated:true});paint('hla_motif_network',8);
check(!elements.hla_motif_network_cell_view_host.classList.contains('cerebro-view-terminal'));
check(!window.CerebroPageLifecycle.isTerminal(elements['shiny-tab-hla_tcr_motifs']));
check(!row('hla_motif_network').textContent.includes('Updating'));
change('spatial_projection','spatial_projection_group_labels',true);
emit('cerebro:view-appearance',{id:'spatial_projection',values:{group_labels:true}});
check(!row('spatial_projection').textContent.includes('Updating'));
emit('cerebro:cell-view-ready',{id:'overview_projection',painted:true,renderToken:10,pointCount:0});
check(row('overview_projection').textContent.includes('No cells match'));
emit('cerebro:cell-view-ready',{id:'overview_projection',painted:true,renderToken:11,pointCount:10});
check(!row('overview_projection').textContent.includes('No cells match'));
emit('cerebro:linkedviews-ready',{primaryReady:true,painted:true,complete:true});
elements['cv-pick-color']=element('cv-pick-color');elements['cv-pick-color'].value='__gene__';
change('linked','coordviews_gene','A');
check(row('linked').textContent.includes('Updating expression: A'));
change('linked','coordviews_gene','B');
messages.cerebro_linked_expression_feedback({controls:{coordviews_gene:'A',coordviews_expression_mode:'mean'},ok:true,dataset_context:{generation}});
check(row('linked').textContent.includes('Updating expression: B'));
messages.cerebro_linked_expression_feedback({controls:{coordviews_gene:'B',coordviews_expression_mode:'mean'},ok:true,dataset_context:{generation}});
check(row('linked').hidden);
change('linked','coordviews_gene','missing');
messages.cerebro_linked_expression_feedback({controls:{coordviews_gene:'missing',coordviews_expression_mode:'mean'},ok:false,dataset_context:{generation}});
check(row('linked').textContent.includes('Expression unavailable'));
elements['cv-pick-color'].value='__rgb__';
change('linked','coordviews_gene_r','R');
messages.cerebro_linked_expression_feedback({controls:{coordviews_gene_r:'R',coordviews_gene_g:[],coordviews_gene_b:[]},ok:true,rgb:true,dataset_context:{generation}});
check(row('linked').hidden);
change('linked','cv-pick-color','__gene__');
check(!row('linked').textContent.includes('Updating expression'));
change('linked','coordviews_gene','');
check(row('linked').hidden);
// Layout suggestions share the row, ignore stale contexts, and respect dismissal.
function hasHint(id) {return header(id).children.some(c=>c.className==='cerebro-layout-hint');}
paint('overview_projection', 12);
emit('cerebro:plot-layout',{id:'overview_projection',square:false,wide:true});check(hasHint('overview_projection'));
check(!header('overview_projection').hidden);
emit('cerebro:plot-layout',{id:'overview_projection',square:true,wide:true});check(!hasHint('overview_projection'));
check(row('overview_projection').hidden);
emit('cerebro:plot-layout',{id:'overview_projection',square:false,wide:false});check(!hasHint('overview_projection'));
emit('cerebro:plot-layout',{id:'overview_projection',square:false,wide:true,dataset_context:{generation:0}});check(!hasHint('overview_projection'));
emit('cerebro:plot-layout',{id:'overview_projection',square:false,wide:true});check(hasHint('overview_projection'));
domEvents.click({target:{closest:s=>s==='[data-layout-dismiss]'?{getAttribute:()=> 'overview_projection'}:null}});check(!hasHint('overview_projection'));
emit('cerebro:plot-layout',{id:'overview_projection',square:false,wide:true});check(!hasHint('overview_projection'));
emit('cerebro:linkedviews-ready',{primaryReady:true,painted:true,complete:true});
emit('cerebro:plot-layout',{id:'linked',square:false,wide:true});check(hasHint('linked'));
domEvents.change({target:{id:'cv-square-plots',checked:false},isTrusted:true});check(!hasHint('linked'));
emit('cerebro:plot-layout',{id:'linked',square:false,wide:true});check(hasHint('linked'));
// An explicit uncheck resets dismissal, but only measured wide canvases qualify.
domEvents.change({target:{id:'overview_projection_keep_square',checked:false},isTrusted:true});
const beforeImmediateLayout = timeoutCalls;
emit('cerebro:plot-layout',{id:'overview_projection',square:false,wide:true});check(hasHint('overview_projection'));
check(timeoutCalls === beforeImmediateLayout);
domEvents.change({target:{id:'overview_projection_keep_square',checked:true},isTrusted:true});check(!hasHint('overview_projection'));
domEvents.change({target:{id:'overview_projection_keep_square',checked:false},isTrusted:true});
emit('cerebro:plot-layout',{id:'overview_projection',square:false,wide:false});check(!hasHint('overview_projection'));
console.log(count+' view feedback assertions passed');

// All surfaces suppress routine successful status, without hiding errors/work.
for(const id of Object.keys(ids)){
  if(id==='linked') emit('cerebro:linkedviews-ready',{primaryReady:true,painted:true,complete:true});
  else paint(id,100);
  for(const text of ['Background loaded: arbitrary ROI','No background image','Background images hidden','View ready','No selection']) {
    emit('cerebro:view-operation',{id,text});
    check(!row(id).textContent.includes(text));
  }
  emit('cerebro:view-operation',{id,text:'Loading background: arbitrary ROI…'});
  check(row(id).textContent.includes('Loading background'));
  check(row(id).classList.contains('is-pending'));
  emit('cerebro:view-operation',{id,text:'Background unavailable: arbitrary ROI'});
  check(row(id).classList.contains('is-error'));
  emit('cerebro:view-operation',{id,text:'Background loaded: arbitrary ROI'});
  check(!row(id).classList.contains('is-error'));
}
console.log(count+' feedback assertions passed, including compact local status');

// The hidden header still carries the terminal contract used by both loaders.
handlers['shiny:error']({target:{id:'overview_projection_cell_view_host',
  closest:()=>({id:'shiny-tab-overview'})},error:{message:'Plot failed'}});
check(window.CerebroPageLifecycle.isTerminal(elements['shiny-tab-overview']));
check(header('overview_projection').hidden);
check(row('overview_projection').classList.contains('is-error'));
map('overview_projection',101,{});paint('overview_projection',101);
check(!window.CerebroPageLifecycle.isTerminal(elements['shiny-tab-overview']));
generation++;emit('cerebro:dataset-context',{changed:true,phase:'pending'});
for(const id of Object.keys(ids)) check(!window.CerebroPageLifecycle.isTerminal(elements['shiny-tab-'+ids[id]]));
console.log('PASS terminal marker, hidden header, recovery and dataset reset');

// Clonal and trajectory advice uses the existing drawer controls and never
// leaks onto the other immune analysis tabs or a faceted static plot.
const ir = 'ir_clonalUMAP_projection';
ids[ir] = 'immune_repertoire';
elements['shiny-tab-immune_repertoire'] = element('immune_repertoire');
elements[ir+'_cell_view_host'] = element(ir);
let clonalVisible = true;
elements[ir+'_cell_view_host'].getClientRects = () => clonalVisible ? [{}] : [];
for (const [id, buttonId, inputId, label] of [
  [ir, 'ir_more_button', 'ir_clonalUMAP_keep_square', 'Wide UMAP'],
  ['trajectory_projection', 'trajectory_projection_more_button', 'trajectory_projection_keep_square', 'Wide trajectory']
]) {
  paint(id, 200);
  emit('cerebro:plot-layout', {id, square:false, wide:true});
  check(hasHint(id));
  check(header(id).children.find(c=>c.className==='cerebro-layout-hint').innerHTML.includes(label));
  let opened = 0, focused = 0;
  const labelClasses = {add() {}, remove() {}};
  elements[buttonId] = {getAttribute:()=> 'false', click:()=>opened++};
  elements[inputId] = {getClientRects:()=>[{}], scrollIntoView() {}, focus:()=>focused++,
    closest:()=>({classList:labelClasses})};
  domEvents.click({target:{closest:s=>s==='[data-layout-settings]'?{getAttribute:()=>id}:null}});
  check(opened === 1 && focused === 1);
  domEvents.change({target:{id:inputId,checked:true}}); check(!hasHint(id));
  emit('cerebro:plot-layout', {id, square:true, wide:true}); check(!hasHint(id));
  domEvents.change({target:{id:inputId,checked:false}});
  emit('cerebro:plot-layout', {id, square:false, wide:false}); check(!hasHint(id));
  emit('cerebro:plot-layout', {id, square:false, wide:true, dataset_context:{generation:0}}); check(!hasHint(id));
}
emit('cerebro:plot-layout', {id:ir,square:false,wide:true}); check(hasHint(ir));
handlers.change({name:'ir_tabs',value:'Abundance',target:{}}); check(!hasHint(ir));
clonalVisible = false;
emit('cerebro:plot-layout', {id:ir,square:false,wide:true}); check(!hasHint(ir));
check(row(ir) === null); // Existing immune loading/status remains untouched.
clonalVisible = true;
emit('cerebro:plot-layout', {id:ir,square:false,wide:true}); check(hasHint(ir));
handlers.change({name:'ir_p_umap_group_by',value:'sample',target:{}}); check(!hasHint(ir));

// Exercise the renderer's actual canvas measurement, including a named
// non-UMAP trajectory and the existing 2-D-only/width threshold restrictions.
const renderer = fs.readFileSync(path.join(path.dirname(process.argv[2] ||
  path.resolve(__dirname,'../../../inst/viewer/www/view-feedback.js')), 'cell_views.js'),'utf8');
const start = renderer.indexOf('var hintView = singleActive && singleView(singleActive);');
const end = renderer.indexOf('    return true;', start);
assert(start >= 0 && end > start);
function layout(id, name, width, height, z) {
  let result;
  vm.runInNewContext(renderer.slice(start,end), {
    singleActive:id, singleView:()=>({meta:{space_label:name}}),
    vis:[{spaceId:'test',W:width,H:height}], spaceById:{test:{z}},
    keepPlotsSquare:false, currentDatasetContext:()=>({generation}),
    window:{dispatchEvent:e=>result=e.detail},
    CustomEvent:function(type,data){this.detail=data.detail;}
  });
  return result;
}
check(layout(ir,'Clonal UMAP',1000,500).wide);
check(layout('trajectory_projection','B_cell_maturation',1000,500).wide);
check(!layout('trajectory_projection','B_cell_maturation',800,500).wide);
check(!layout('trajectory_projection','B_cell_maturation',1000,500,[1]).wide);
check(!layout('overview_projection','PCA',1000,500).wide);
console.log(count+' total assertions passed, including specialist layout advice');
