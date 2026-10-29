'use strict';
const assert = require('assert/strict'), fs = require('fs'), vm = require('vm');
const events = [], requests = [], listeners = {}, frames = [];
let intersect, resize, disconnected = 0, binding = null;
const gate = {dataset:{}};
function chart(id) {
  const plot = {clientWidth:500, clientHeight:400, innerHTML:''};
  return {id, dataset:{}, offsetParent:{}, clientWidth:640, clientHeight:400, innerHTML:'', plot,
    handlers:{}, addEventListener(name, fn) { this.handlers[name] = fn; },
    querySelector(selector) { return selector === '.cc-plot' ? plot : null; },
    contains() { return true; }};
}
const plot = chart('groups_by_other_group_plot'), violin = chart('groups_nUMI_plot');
const elements = {groups_expression_metrics_gate:gate,
  'shiny-tab-groups':{querySelectorAll:()=>[plot, violin]},
  groups_by_other_group_plot:plot, groups_selected_group:{value:'sample'},
  groups_by_other_group_second_group:{value:'cluster'}, groups_by_other_group_plot_type:{value:'Bar chart'}};
class OutputBinding { renderError(el) { el.innerHTML = 'error'; } }
const scope = {
  document:{getElementById:id=>elements[id], addEventListener:(name,fn)=>listeners[name]=fn},
  Shiny:{setInputValue:(name,value)=>requests.push({name,value}), OutputBinding,
    outputBindings:{register:(b, name)=>{ binding = b; assert.equal(name, 'cerebro.chart'); }}},
  jQuery:Object.assign(() => ({data:()=>null}), {extend:Object.assign}),
  ResizeObserver:class {constructor(fn){resize=fn;} observe(){}},
  IntersectionObserver:class {constructor(fn){intersect=fn;} observe(target){assert.equal(target,gate);} disconnect(){disconnected++;}},
  CustomEvent:class {constructor(type, options){this.type=type;this.detail=options.detail;}},
  requestAnimationFrame:fn=>frames.push(fn),
  // Every chart also sends cerebro:chart-ready; only the Groups event is checked.
  dispatchEvent:event=>{ if (event.type === 'cerebro:groups-primary-ready') { events.push(event); (listeners['window:' + event.type] || (()=>{}))(event); } },
  addEventListener:(name,fn)=>listeners['window:' + name]=fn,
  requestIdleCallback:fn=>fn(),
  cerebroSavedViewDataset:{cell_fingerprint:'A'}, __cerebroPageBenchGeneration:7
};
scope.window = scope;
vm.createContext(scope);
for (const script of JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))) vm.runInContext(script, scope);
vm.runInContext(fs.readFileSync(process.argv[3], 'utf8'), scope);
assert.ok(binding, 'the output binding registers once Shiny is present');

// The metrics gate requests its outputs once, when it scrolls into view.
assert.equal(requests.length,0);
intersect([{isIntersecting:false}]); assert.equal(requests.length,0);
intersect([{isIntersecting:true}]); intersect([{isIntersecting:true}]);
assert.equal(requests.length,1); assert.equal(requests[0].name,'groups_expression_metrics_render_request');
assert.equal(disconnected,1); assert.equal(events.length,0);

const bar = {type:'bar', categories:['A','B'], series:['c1'], colors:['#ff0000'], values:[[3,1234]],
  percent:false, y_title:'Number of cells', legend_title:'cluster'};
binding.renderValue(plot, bar);
assert.deepEqual(JSON.parse(JSON.stringify(events[0].detail)), {
  page:'groups', plotId:plot.id, metric:'composition', traceCount:1,
  datasetFingerprint:'A', selectedGroup:'sample', secondGroup:'cluster',
  plotType:'Bar chart', benchmarkGeneration:7, eventKind:'primary'
});
assert.match(plot.innerHTML, /cc-toolbar/);
assert.match(plot.innerHTML, /data-series="0"/);
assert.equal((plot.plot.innerHTML.match(/fill="#ff0000"/g) || []).length, 2);
assert.match(plot.plot.innerHTML, />1,000</, 'count ticks use thousands separators');

function visit(){
  listeners.click({target:{closest:()=>({getAttribute:()=>'#shiny-tab-groups'})}});
  while(frames.length)frames.shift()();
}
visit(); assert.equal(events.at(-1).detail.eventKind,'cached');
scope.cerebroSavedViewDataset = {cell_fingerprint:'B'};
const before = events.length;
visit(); assert.equal(events.length,before);

// Hidden or empty charts do not report.
plot.offsetParent = null; binding.renderValue(plot, bar); assert.equal(events.length,before);
plot.offsetParent = {}; binding.renderValue(plot, {type:'bar', categories:[], series:[], values:[]});
assert.equal(events.length,before); assert.match(plot.innerHTML, /No cells to display/);

// A zero-width element draws, and reports, once it gets a size.
const twoSeries = Object.assign({}, bar, {series:['c1','c2'], colors:['#ff0000','#00ff00'], values:[[3,1],[2,2]]});
plot.clientWidth = 0;
binding.renderValue(plot, twoSeries);
assert.equal(events.length,before);
elements.groups_selected_group.value = 'condition';
scope.__cerebroPageBenchGeneration = 8;
plot.clientWidth = 640; resize([{target:plot}]);
assert.equal(events.at(-1).detail.eventKind,'primary');
resize([{target:plot}]); assert.equal(events.length,before + 1, 'an unchanged size does not redraw');

// Legend clicks hide a series locally, without another report.
assert.equal(events.at(-1).detail.datasetFingerprint,'B');
assert.equal(events.at(-1).detail.selectedGroup,'condition');
assert.equal(events.at(-1).detail.benchmarkGeneration,8);
assert.equal(events.at(-1).detail.traceCount,2);
const reported = events.length;
const item = {getAttribute:()=> '1', closest(selector) { return selector === '[data-series]' ? item : null; }};
plot.handlers.click({target:item});
assert.equal(events.length, reported);
assert.equal((plot.plot.innerHTML.match(/fill="#00ff00"/g) || []).length, 0);
assert.match(plot.innerHTML, /is-hidden/);

// Violins report their metric and group count.
binding.renderValue(violin, {type:'violin', mode:'percent', y_title:'Percentage of transcripts',
  groups:['A','B'], colors:['#ff0000','#0000ff'], n:[3,1], mean:[.2,.5], q1:[.1,.5], median:[.2,.5],
  q3:[.3,.5], lowerfence:[.1,.5], upperfence:[.3,.5], minimum:[.1,.5], maximum:[.3,.5],
  approximate:[false,false], violins:[{lo:.1, hi:.3, density:[.2,1,.2]}, {lo:.5, hi:.5, density:[]}]});
assert.equal(events.at(-1).detail.metric,'nUMI');
// A drawn metric asks, while idle, for the other metric tabs of its group;
// the composition chart does not.
assert.deepEqual(requests.filter(r => r.name === 'groups_expression_metrics_prefetch'),
  [{name:'groups_expression_metrics_prefetch', value:'condition'}]);
assert.equal(events.at(-1).detail.traceCount,2);
assert.match(violin.plot.innerHTML, />100%</);
assert.match(violin.plot.innerHTML, /<path d="M/);

binding.renderError(violin, {message:'boom'});
assert.equal(violin.innerHTML, 'error');
console.log('groups readiness ok');
