'use strict';
const assert = require('assert/strict'), fs = require('fs'), vm = require('vm');
const changes = [], fetches = [], originals = [];
let refreshed = 0;
const selectize = {
  items: ['CD3D'], options: {CD3D: {value:'CD3D',label:'CD3D'}}, isFocused: false,
  settings: {valueField:'value',labelField:'label',load(){}},
  getValue() { return this.items.slice(); },
  clear(silent) { this.items=[]; if(!silent) changes.push([]); },
  clearOptions(silent) {
    for (const key of Object.keys(this.options)) if (!this.items.includes(key)) delete this.options[key];
    this.clear(silent);
  },
  addOption(option) {
    if (Array.isArray(option)) return option.forEach(o=>this.addOption(o));
    if (!this.options[option.value]) this.options[option.value]=option;
  },
  setValue(value,silent) {
    this.clear(true);
    this.items=(Array.isArray(value)?value:value?[value]:[]).filter(v=>this.options[v]);
    if(!silent) changes.push(this.getValue());
  },
  refreshOptions(open) { if (open) refreshed++; }
};
const element={id:'expression_genes_input',selectize};
const binding={name:'shiny.selectInput',receiveMessage(el,data) { originals.push(data); return 'original-result'; }};
const lists={};
function fetch(url) {
  fetches.push(url);
  return new Promise(resolve=>{ lists[url]=genes=>resolve({ok:true,json:()=>Promise.resolve(genes)}); });
}
const jQuery=el=>({on(){},trigger(event){ if(event==='change') changes.push(el.selectize.getValue()); }});
const context={window:{Shiny:{inputBindings:{getBindings:()=>[{binding}]}},fetch,jQuery},
  document:{addEventListener(event,fn){fn();}}};
context.Shiny=context.window.Shiny;
vm.runInNewContext(fs.readFileSync(process.argv[2],'utf8'),context);
const ready=()=>context.window.CerebroGeneSelector.ready(selectize);
const settle=()=>new Promise(resolve=>setTimeout(resolve,0));
(async()=>{
  // Shiny awaits receiveMessage before its next message: answer at once.
  assert.equal(binding.receiveMessage(element,{cerebro_gene_choices:'first',value:['CD3D']}),undefined);
  assert.equal(selectize.settings.load,null,'no server search');
  assert(!ready());
  assert(changes.every(v=>v.join()==='CD3D'),'an update must not publish an empty selection');
  sEdit(['NKG7']); lists.first(['CD3D','MS4A1','NKG7']); await settle();
  assert(ready());
  assert.deepEqual(selectize.items,['NKG7'],'an edit made while the list loads is kept');
  assert(selectize.options.MS4A1,'the whole list is searchable');

  binding.receiveMessage(element,{cerebro_gene_choices:'second'});
  sEdit([]); lists.second(['MS4A1']); await settle();
  assert.deepEqual(selectize.items,[],'a deliberate clear survives a late list');

  binding.receiveMessage(element,{cerebro_gene_choices:'old',value:['CD3D']});
  binding.receiveMessage(element,{cerebro_gene_choices:'new',value:['MS4A1']});
  lists.new(['MS4A1']); lists.old(['OLD_DATASET_GENE']); await settle();
  assert.deepEqual(selectize.items,['MS4A1']);
  assert(!selectize.options.OLD_DATASET_GENE,'a retired list cannot add old genes');

  binding.receiveMessage(element,{cerebro_gene_choices:'canonical',value:['Ms4a1']});
  assert.deepEqual(selectize.items,['Ms4a1']);
  assert.deepEqual(changes.at(-1),['Ms4a1'],'the canonical selection is reported to Shiny');

  // Text typed before the list arrived is searched when it does.
  selectize.isFocused=true; refreshed=0;
  binding.receiveMessage(element,{cerebro_gene_choices:'typed'});
  lists.typed(['CD4']); await settle();
  assert.equal(refreshed,1);
  selectize.isFocused=false;

  // The R/G/B channels share the page's list: one request.
  const before=fetches.length;
  for(const channel of ['r','g','b']) {
    element.id='expression_rgb_gene_'+channel; changes.length=0;
    binding.receiveMessage(element,{cerebro_gene_choices:'rgb',value:'CD3D'});
    assert(changes.every(v=>v.join()==='CD3D'));
  }
  assert.equal(fetches.length-before,1);

  // Other messages keep Shiny's own handling.
  assert.equal(binding.receiveMessage(element,{value:[]}),'original-result');
  assert.deepEqual(originals.at(-1),{value:[]});
  console.log('Gene selector local list assertions passed');
})().catch(e=>{console.error(e);process.exitCode=1;});
function sEdit(values){values.forEach(value=>selectize.addOption({value,label:value}));selectize.setValue(values);}
