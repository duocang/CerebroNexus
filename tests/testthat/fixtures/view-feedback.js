'use strict';
const fs = require('fs'), vm = require('vm'), assert = require('assert/strict'), path = require('path');
const events = {}, messages = {}, handlers = {}, elements = {}, domEvents = {};
const storage = new Map();
function element(id) {
  const classes = new Set(['active']); let text = '';
  return { id, get textContent() { return text + this.children.map(c=>c.textContent).join(''); },
    set textContent(v) { text=v; this.children=[]; }, value: '', children: [], tagName: 'INPUT',
    classList: {toggle(k,on) { if(on) classes.add(k); else classes.delete(k); }, contains(k) {return classes.has(k);}},
    setAttribute() {}, querySelector(selector) {return selector === 'label' ? {textContent:'Colour'} : this.children[0] || null;},
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
vm.runInNewContext(fs.readFileSync(process.argv[2] || path.resolve(__dirname,'../../../inst/viewer/www/view-feedback.js'),'utf8'),
  {window,document,setTimeout:fn=>{timeoutCalls++;fn();return 1;},clearTimeout:()=>{},sessionStorage:{getItem:k=>storage.get(k),setItem:(k,v)=>storage.set(k,v),removeItem:k=>storage.delete(k)},CustomEvent:function(type,data){this.type=type;this.detail=data.detail;}});
let count=0;
function check(v){assert.ok(v);count++;}
function row(id){return elements['shiny-tab-'+ids[id]].children[0];}
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
paint('hla_motif_network',7);check(row('hla_motif_network').textContent.includes('No motifs'));
change('hla_motif_network','hla_show_isolated',true);
check(row('hla_motif_network').textContent.includes('Updating'));
paint('hla_motif_network',7);check(elements.hla_motif_network_cell_view_host.classList.contains('cerebro-view-terminal'));
map('hla_motif_network',8,{hla_show_isolated:true});paint('hla_motif_network',8);
check(!elements.hla_motif_network_cell_view_host.classList.contains('cerebro-view-terminal'));
check(!row('hla_motif_network').textContent.includes('Updating'));
change('spatial_projection','spatial_projection_group_labels',true);
emit('cerebro:view-appearance',{id:'spatial_projection',values:{group_labels:true}});
check(!row('spatial_projection').textContent.includes('Updating'));
emit('cerebro:cell-view-ready',{id:'overview_projection',painted:true,renderToken:10,pointCount:0});
check(row('overview_projection').textContent.includes('No cells match'));
emit('cerebro:cell-view-ready',{id:'overview_projection',painted:true,renderToken:11,pointCount:10});
check(!row('overview_projection').textContent.includes('No cells match'));
elements['cv-pick-color']=element('cv-pick-color');elements['cv-pick-color'].value='__gene__';
change('linked','coordviews_gene','A');
check(row('linked').textContent.includes('Updating expression: A'));
change('linked','coordviews_gene','B');
messages.cerebro_linked_expression_feedback({controls:{coordviews_gene:'A',coordviews_expression_mode:'mean'},ok:true,dataset_context:{generation}});
check(row('linked').textContent.includes('Updating expression: B'));
messages.cerebro_linked_expression_feedback({controls:{coordviews_gene:'B',coordviews_expression_mode:'mean'},ok:true,dataset_context:{generation}});
check(row('linked').textContent.includes('Expression loaded: B'));
change('linked','coordviews_gene','missing');
messages.cerebro_linked_expression_feedback({controls:{coordviews_gene:'missing',coordviews_expression_mode:'mean'},ok:false,dataset_context:{generation}});
check(row('linked').textContent.includes('Expression unavailable'));
elements['cv-pick-color'].value='__rgb__';
change('linked','coordviews_gene_r','R');
messages.cerebro_linked_expression_feedback({controls:{coordviews_gene_r:'R',coordviews_gene_g:[],coordviews_gene_b:[]},ok:true,rgb:true,dataset_context:{generation}});
check(row('linked').textContent.includes('Expression loaded: R'));
change('linked','cv-pick-color','__gene__');
check(!row('linked').textContent.includes('Updating expression'));
change('linked','coordviews_gene','');
check(row('linked').textContent.includes('No expression selected'));
// Layout suggestions share the row, ignore stale contexts, and respect dismissal.
function hasHint(id) {return row(id).children.some(c=>c.className==='cerebro-layout-hint');}
paint('overview_projection', 12);
emit('cerebro:plot-layout',{id:'overview_projection',square:false,wide:true});check(hasHint('overview_projection'));
check(!row('overview_projection').hidden);
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
