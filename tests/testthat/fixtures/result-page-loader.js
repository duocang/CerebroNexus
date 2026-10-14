'use strict';
const fs = require('fs'), vm = require('vm'), assert = require('assert/strict'), path = require('path');
const source = fs.readFileSync(process.argv[2] || path.resolve(__dirname, '../../../inst/viewer/www/viewer-shell.js'), 'utf8');
const block = require('./js-source.cjs')(source).readyFor('cerebro-page-loader');
let count = 0;
function check(value, message) { assert.ok(value, message); count++; }
function scenario(page = 'markerGenes') {
  const nodes = {}, handlers = {}, events = {}, timers = [], frames = [];
  let active, observer, phase = 'ready';
  class Node {
    constructor(id, classes = '', parent) {
      this.id = id; nodes[id] = this; this.parent = parent; this.children = []; this.attrs = {};
      this.dataset = {}; this.textContent = ''; this.hidden = false; this.visible = true;
      const set = new Set(classes.split(' ').filter(Boolean));
      this.classList = {contains:k=>set.has(k),add:k=>set.add(k),remove:k=>set.delete(k)};
      if (parent) parent.children.push(this);
    }
    getClientRects() { return this.visible && !this.hidden ? [{}] : []; }
    get parentElement() { return this.parent; }
    appendChild(node) {
      if (node.parent) node.parent.children = node.parent.children.filter(n=>n!==node);
      node.parent = this; this.children.push(node); return node;
    }
    setAttribute(k,v) { this.attrs[k] = v; }
    removeAttribute(k) { delete this.attrs[k]; }
    getAttribute(k) { return this.attrs[k] ?? null; }
    contains(n) { return n === this || this.children.some(c=>c.contains(n)); }
    matches(s) {
      if (s.includes(',')) return s.split(',').some(x=>this.matches(x.trim()));
      if (s.startsWith('#')) return this.id === s.slice(1);
      if (s.startsWith('.')) return s.slice(1).split('.').every(c=>this.classList.contains(c));
      return this.tagName === s.toUpperCase();
    }
    querySelectorAll(s) {
      const all = this.children.flatMap(c=>[c,...c.querySelectorAll('*')]);
      return s === '*' ? all : all.filter(n=>s.split(',').some(x=>n.matches(x.trim())));
    }
    querySelector(s) { return this.querySelectorAll(s)[0] || null; }
    closest(s) { return s.startsWith('.tab-pane') ? (this.id.startsWith('shiny-tab-') ? this : this.parent?.closest(s)) :
      this.matches(s) ? this : this.parent?.closest(s); }
  }
  const content = new Node('main-content');
  const announcement = new Node('cerebro-page-loader-announcement', '', content);
  const loader = new Node('cerebro-page-loader', '', content); loader.hidden = true;
  new Node('cerebro-page-loader-detail', '', loader);
  const pane = active = new Node('shiny-tab-'+page, 'tab-pane active', content);
  const controls = new Node(page === 'groups' ? 'groups_controls_UI' : 'controls', 'shiny-bound-output cerebro-result-controls', pane);
  const select = new Node('select', '', controls); select.tagName = 'SELECT';
  const region = new Node('region', 'cerebro-page-loading-region', pane);
  const slot = new Node('slot', 'shiny-bound-output cerebro-result-slot', region);
  const table = new Node('table', 'shiny-bound-output datatables', slot);
  const tableDom = new Node('table-dom', '', table); tableDom.tagName = 'TABLE';
  const jq = {on(names, fn) { names.split(' ').forEach(n=>(handlers[n.split('.')[0]] ||= []).push(fn)); return jq; }};
  const window = {
    jQuery:()=>jq, setTimeout:(fn,ms)=>{ const t={fn,ms};timers.push(t);return t; }, clearTimeout:t=>{if(t)t.cancel=true;},
    requestAnimationFrame:fn=>frames.push(fn), addEventListener:(n,f)=>events[n]=f,
    CerebroDatasetContext:{phase:()=>phase,accepts:()=>false},
    MutationObserver:class { constructor(fn) {observer=fn;} observe() {} }
  };
  const document = {getElementById:id=>nodes[id],querySelector:()=>active,addEventListener:(n,f)=>events[n]=f,
    querySelectorAll:()=>Object.values(nodes).filter(n=>n.id.startsWith('shiny-tab-'))};
  vm.runInNewContext(block,{window,document,ready:fn=>fn(),MutationObserver:window.MutationObserver,Set,WeakSet,Array});
  function emit(name,target=table,extra={}) { for(const f of handlers[name] || [])f({target,...extra}); }
  function flush() { const batch=timers.splice(0);batch.forEach(t=>{if(!t.cancel&&t.ms<1000)t.fn();}); }
  function frame() { frames.splice(0).forEach(f=>f()); }
  function values() { [controls,slot,table].forEach(n=>emit('shiny:value',n));flush(); }
  function start() { emit('shown',pane); }
  function mutation() {observer([{target:tableDom,type:'attributes',attributeName:'style'}]);}
  return {nodes,pane,loader,announcement,region,table,tableDom,controls,slot,emit,flush,frame,values,start,mutation,events,
    phase:v=>phase=v,active:v=>active=v,focus:v=>document.activeElement=v,Node};
}
{
  const s=scenario();s.start();
  check(s.loader.parentElement===s.region,'page card belongs to the result region');
  check(!s.slot.contains(s.loader),'renderUI cannot replace the shared card');
  check(!s.pane.contains(s.announcement) && s.announcement.textContent==='Loading Marker genes…',
    'stage announcement is outside the busy pane');
  const next = new s.Node('shiny-tab-geneExpression', 'tab-pane active', s.pane.parent);
  const nextRegion = new s.Node('next-region', 'cerebro-page-loading-region', next);
  s.active(next);s.emit('shown',next);
  check(s.loader.parentElement===nextRegion && !s.region.contains(s.loader),'navigation moves one card to the next result region');
  check(s.announcement.textContent==='Loading Gene expression…','navigation announces the new page');
}
{
  const s=scenario();s.region.visible=false;s.start();
  check(s.loader.parentElement===s.pane,'pre-show fallback still provides immediate feedback');
  s.region.visible=true;s.start();
  check(s.loader.parentElement===s.region,'shown event uses the real result region, not the previous fallback');
  s.values();s.emit('draw',s.tableDom);s.frame();s.frame();s.flush();
  check(s.announcement.textContent==='','completion clears the announcement');
}
for (const page of ['markerGenes','mostExpressedGenes','enrichedPathways','extra_material']) {
  const s=scenario(page);s.start();s.values();s.frame();s.frame();s.flush();
  check(!s.loader.hidden,page+': values without a DT draw must not finish');
  s.emit('shiny:busy');s.emit('draw',s.tableDom);
  s.frame();s.mutation();s.emit('shiny:idle');s.emit('shiny:busy');
  check(!s.loader.hidden,'keep both paint frames');
  s.frame();s.flush();
  check(s.loader.hidden,page+': independent busy work and style changes must not restart completed-result confirmation');
}
{
  const s=scenario();s.start();s.values();s.emit('draw',s.tableDom);s.frame();
  s.emit('shiny:outputinvalidated',s.table);s.frame();s.flush();
  check(!s.loader.hidden,'new invalidation rejects old paint callback');
  s.emit('shiny:value',s.table);s.emit('draw',s.tableDom);s.flush();s.frame();s.frame();s.flush();
  check(s.loader.hidden,'fresh value and draw settle');
}
{
  const s=scenario();s.start();s.values();s.emit('draw',s.tableDom);s.frame();
  s.focus(s.nodes.select);
  s.emit('shiny:inputchanged',s.nodes.select,{name:'select',el:s.nodes.select});
  s.mutation();s.frame();s.flush();check(!s.loader.hidden,'control change rejects the previous result immediately');
  s.emit('shiny:value',s.table);s.emit('draw',s.tableDom);s.flush();s.frame();s.frame();s.flush();
  check(s.loader.hidden,'edited control completes only with fresh output');
}
{
  const s=scenario();s.start();s.values();s.emit('draw',s.tableDom);s.frame();
  s.phase('loading');s.events['cerebro:dataset-context']({detail:{changed:true,phase:'loading'}});
  s.frame();s.flush();check(!s.loader.hidden,'dataset reset cancels old confirmation');
  s.phase('ready');s.mutation();s.frame();s.frame();s.flush();
  check(!s.loader.hidden,'old DOM is not a new dataset result');
  s.values();s.emit('draw',s.tableDom);s.frame();s.frame();s.flush();check(s.loader.hidden,'fresh dataset completes');
}
{
  const s=scenario();s.start();s.values();s.emit('draw',s.tableDom);s.frame();s.active(null);s.frame();s.flush();
  check(!s.pane.dataset.cerebroPageReady,'navigation cancels completion of inactive page');
}
{
  const s=scenario();s.slot.children=[];s.slot.textContent='No stored results';s.start();s.values();s.frame();s.frame();s.flush();
  check(s.loader.hidden,'explicit empty result completes without a table');
}
{
  const s=scenario();s.start();s.values();
  s.emit('shiny:error',s.table,{error:{message:''}});s.flush();s.frame();s.frame();s.flush();
  check(!s.loader.hidden,'silent req does not stand in for a draw');
  s.table.classList.add('shiny-output-error');s.table.textContent='Failed to load result';
  s.emit('shiny:error',s.table,{error:{message:'Failed to load result'}});s.flush();s.frame();s.frame();s.flush();
  check(s.loader.hidden,'visible terminal error is not an endless loader');
}
{
  const s=scenario();s.start();s.values();s.emit('draw',s.tableDom);s.frame();s.frame();s.flush();
  s.start();s.flush();check(s.loader.hidden,'settled revisit does not reopen loader');
}
{
  const s=scenario();s.start();s.emit('shiny:inputchanged',s.nodes.select,{name:'select',el:s.nodes.select,value:'a'});
  s.values();s.emit('draw',s.tableDom);s.frame();s.focus(s.nodes.select);
  s.emit('shiny:inputchanged',s.nodes.select,{name:'select',el:s.nodes.select,value:'a'});
  s.frame();s.flush();check(s.loader.hidden,'unchanged input rebinding does not invalidate a finished draw');
}
{
  const s=scenario('groups');s.controls.classList.add('cerebro-groups-controls');s.table.visible=false;
  const plot=new s.Node('groups_by_other_group_plot','plotly shiny-bound-output',s.pane);
  s.start();s.values();s.emit('shiny:value',plot);s.flush();s.frame();s.frame();s.flush();
  check(!s.loader.hidden,'Groups waits for afterplot, not only a value');
  s.emit('plotly_afterplot',plot);s.frame();s.mutation();
  s.emit('shiny:inputchanged',s.nodes.select,{name:'select',el:s.nodes.select});
  const lazy = new s.Node('groups_nUMI_plot','shiny-bound-output',s.pane);
  s.emit('shiny:outputinvalidated',lazy);s.emit('shiny:value',lazy);s.frame();s.flush();
  check(s.loader.hidden,'Groups primary draw completes with lazy metrics independent');
}
{
  const s=scenario('extra_material');s.slot.children=[];
  const plot=new s.Node('extra_material_plot_interactive','plotly shiny-bound-output',s.slot);
  s.start();s.values();s.emit('shiny:value',plot);s.flush();s.frame();s.frame();s.flush();
  check(!s.loader.hidden,'Extra plot waits for actual afterplot');
  s.emit('plotly_afterplot',plot);s.frame();s.frame();s.flush();check(s.loader.hidden,'Extra plot confirms two frames after draw');
}
{
  const s=scenario('extra_material');s.slot.children=[];
  const plot=new s.Node('extra_material_plot_plain','shiny-plot-output shiny-bound-output',s.slot);
  const img=new s.Node('png','',plot);img.tagName='IMG';img.complete=false;img.naturalWidth=0;
  s.start();s.values();s.emit('shiny:value',plot);s.flush();s.frame();s.frame();s.flush();
  check(!s.loader.hidden,'Extra PNG waits for image load');
  img.complete=true;img.naturalWidth=500;s.events.load({target:img});s.frame();s.frame();s.flush();
  check(s.loader.hidden,'Extra PNG completes after load and two frames');
}
console.log(count+' result page loader assertions passed');
