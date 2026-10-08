const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
let linkedHoverOn = true, singleActive = null, D = {n:1462702};
let hoverCell = null, lookups = 0, draws = 0;
const selection = new Set([42]), checkbox = {}, tip = {style:{opacity:0}};
const listeners = {}, spaceById = {main:{}};
const p = {spaceId:'main', tipId:'tip', canvas:{
  addEventListener:(event,fn)=>listeners[event]=fn,
  getBoundingClientRect:()=>({left:0,top:0})
}};
const panels = [p], pinnedTip = {panel:null};
function $(id){return id==='cv-hover'?checkbox:tip;}
function setHoverCell(i){if(i!==hoverCell){hoverCell=i;draws++;}}
function nearest(){lookups++;return 42;}
function singleHoverEnabledAt(){return true;}
function hoverHtml(i){return 'cell '+i;}
function placeTip(){}
eval(source.slice(source.indexOf('  function syncLinkedHoverDefault('),
  source.indexOf('  // ---- brush + pick')));
syncLinkedHoverDefault(true); wireHover(p);
assert.strictEqual(linkedHoverOn,false);
assert.strictEqual(checkbox.checked,false);
listeners.mousemove({clientX:10,clientY:10});
assert.strictEqual(lookups,0);
setLinkedHover(true);
listeners.mousemove({clientX:10,clientY:10});
assert.strictEqual(lookups,1); assert.strictEqual(tip.innerHTML,'cell 42');
syncLinkedHoverDefault(false); assert.strictEqual(linkedHoverOn,true);
setLinkedHover(false);
assert.strictEqual(hoverCell,null); assert.strictEqual(tip.style.opacity,0);
assert.deepStrictEqual([...selection],[42]);
singleActive='expression_projection';
listeners.mousemove({clientX:10,clientY:10});
assert.strictEqual(lookups,2); // Linked preference must not disable Gene hover.
singleActive=null; D={n:1476}; syncLinkedHoverDefault(true);
assert.strictEqual(linkedHoverOn,true);
setLinkedHover(false); syncLinkedHoverDefault(false);
assert.strictEqual(linkedHoverOn,false); // Supplements preserve the choice.
D={n:200000}; syncLinkedHoverDefault(true);
assert.strictEqual(linkedHoverOn,false);
console.log('PASS Linked hover defaults, no picking when disabled, retained selection and specialist hover');
