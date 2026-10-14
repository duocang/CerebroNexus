'use strict';
const assert = require('assert/strict'), fs = require('fs'), vm = require('vm');
const events = [], requests = [], listeners = {}, frames = [];
let intersect, mutate, disconnected = 0;
const gate = {dataset:{}};
const plot = {id:'groups_by_other_group_plot', dataset:{}, data:[{type:'bar'}], offsetParent:{},
  classList:{contains:()=>true}, on(name, fn) { listeners[name] = fn; }};
const elements = {groups_expression_metrics_gate:gate, 'shiny-tab-groups':{},
  groups_by_other_group_plot:plot, groups_selected_group:{value:'sample'},
  groups_by_other_group_second_group:{value:'cluster'}, groups_by_other_group_plot_type:{value:'Bar chart'}};
const scope = {
  document:{getElementById:id=>elements[id], addEventListener:(name,fn)=>listeners[name]=fn},
  Shiny:{setInputValue:(name,value)=>requests.push({name,value})},
  IntersectionObserver:class {constructor(fn){intersect=fn;} observe(target){assert.equal(target,gate);} disconnect(){disconnected++;}},
  MutationObserver:class {constructor(fn){mutate=fn;} observe(){}},
  CustomEvent:class {constructor(type, options){this.type=type;this.detail=options.detail;}},
  requestAnimationFrame:fn=>frames.push(fn), dispatchEvent:event=>events.push(event),
  cerebroSavedViewDataset:{cell_fingerprint:'A'}, __cerebroPageBenchGeneration:7
};
scope.window = scope;
vm.createContext(scope);
for (const script of JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))) vm.runInContext(script, scope);
assert.equal(requests.length,0);
intersect([{isIntersecting:false}]); assert.equal(requests.length,0);
intersect([{isIntersecting:true}]); intersect([{isIntersecting:true}]);
assert.equal(requests.length,1); assert.equal(requests[0].name,'groups_expression_metrics_render_request');
assert.equal(disconnected,1); assert.equal(events.length,0);
listeners.plotly_afterplot();
assert.deepEqual(JSON.parse(JSON.stringify(events[0].detail)), {
  page:'groups', plotId:plot.id, metric:'composition', traceCount:1,
  datasetFingerprint:'A', selectedGroup:'sample', secondGroup:'cluster',
  plotType:'Bar chart', benchmarkGeneration:7, eventKind:'primary'
});
function visit(){listeners.click({target:{closest:()=>({})}}); while(frames.length)frames.shift()();}
visit(); assert.equal(events.at(-1).detail.eventKind,'cached');
scope.cerebroSavedViewDataset = {cell_fingerprint:'B'};
const before = events.length;
visit(); assert.equal(events.length,before);
plot.offsetParent = null; listeners.plotly_afterplot(); assert.equal(events.length,before);
plot.offsetParent = {}; plot.data = []; listeners.plotly_afterplot(); assert.equal(events.length,before);
plot.data = [{type:'bar'}, {type:'bar'}];
elements.groups_selected_group.value = 'condition';
scope.__cerebroPageBenchGeneration = 8;
plot.on = function(name, fn) { listeners[name] = fn; };
mutate(); listeners.plotly_afterplot();
assert.equal(events.at(-1).detail.datasetFingerprint,'B');
assert.equal(events.at(-1).detail.selectedGroup,'condition');
assert.equal(events.at(-1).detail.benchmarkGeneration,8);
assert.equal(events.at(-1).detail.traceCount,2);
