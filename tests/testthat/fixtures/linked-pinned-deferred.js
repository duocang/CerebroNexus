const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
const singleActive = null, HOVER_MAX_GROUPS = 12, GENE_MODE = '__gene__';
let colorBy = 'cluster';
let D = {groups: {
  cluster: {levels:['A','B'], values: new Uint8Array([0,1])},
  sample: {levels:['sample-one'], values:null, deferred:true},
  clone_expansion: {levels:['singleton','expanded'], values:[1,0]},
}};
function fieldOf(){return null;}
function cellLabel(i){return 'cell-'+i;}
function esc(x){return String(x).replace(/&/g,'&amp;').replace(/</g,'&lt;');}
eval(source.slice(source.indexOf('  function groupLabel('),source.indexOf('  // The categorical variable')));
eval(source.slice(source.indexOf('  function hoverHtml('),source.indexOf('  // ---- single-cell detail card')));

const before = JSON.stringify(D);
const ordinary = hoverHtml(0,false), pinned = hoverHtml(0,true);
assert(ordinary.includes('<b>A</b>'));
assert(!ordinary.includes('cv-tip-details'));
assert(pinned.includes('<b>A</b>'));
assert(pinned.includes('Clone Expansion:</span> expanded'));
assert(pinned.includes('cv-tip-details'));
assert(!pinned.includes('sample-one'));
assert.strictEqual(JSON.stringify(D),before); // Rendering cannot materialize metadata.
// A column loaded later must become visible, without a stale negative cache.
D.groups.sample.values = new Uint8Array([0,0]);
assert(hoverHtml(1,true).includes('sample:</span> sample-one'));
assert(hoverHtml(1,true).includes('<b>B</b>'));
// A new dataset has its own deferred state and labels.
D = {groups:{cluster:{levels:['new'],values:[0]},sample:{levels:['old'],values:null}}};
assert(hoverHtml(0,true).includes('<b>new</b>'));
assert(!hoverHtml(0,true).includes('sample-one'));
assert(!hoverHtml(0,true).includes('old'));
console.log('linked-pinned-deferred: 12 assertions passed');
