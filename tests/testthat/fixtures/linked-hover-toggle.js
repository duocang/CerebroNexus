const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
const js = require("./js-source.cjs")(source);
let linkedHoverOn = true, singleActive = null, D = {n:1462702};
let hoverCell = null, lookups = 0, draws = 0;
let sel = new Set([42]);
const checkbox = {}, tip = {style:{opacity:0}};
const listeners = {}, spaceById = {main:{}};
const p = {spaceId:'main', tipId:'tip', canvas:{
  addEventListener:(event,fn)=>listeners[event]=fn,
  getBoundingClientRect:()=>({left:0,top:0})
}};
const panels = [p], pinnedTip = {panel:null};
function $(id){return id==='cv-hover'?checkbox:tip;}
function setHoverCell(i){if(i!==hoverCell){hoverCell=i;draws++;}}
function singleInteractionIsStatic(){return false;}
eval(js.declarations(["interactionIsStatic"]));
function requestSingleAux(){}
function nearest(){lookups++;return 42;}
function singleHoverEnabledAt(){return true;}
let hoverValue = 'cell';
function hoverHtml(i){return hoverValue+' '+i;}
function scheduleSingleMetadata(){}
function singleView(){return null;}
function placeTip(){}
eval(js.declarations(["syncLinkedHoverDefault","setLinkedHover","refreshPanelHover","wireHover"]));
syncLinkedHoverDefault(true); wireHover(p);
assert.strictEqual(linkedHoverOn,true);
assert.strictEqual(checkbox.checked,true);
setLinkedHover(false);
listeners.mousemove({clientX:10,clientY:10});
assert.strictEqual(lookups,0);
setLinkedHover(true);
listeners.mousemove({clientX:10,clientY:10});
assert.strictEqual(lookups,1); assert.strictEqual(tip.innerHTML,'cell 42');
syncLinkedHoverDefault(false); assert.strictEqual(linkedHoverOn,true);
setLinkedHover(false);
assert.strictEqual(hoverCell,null); assert.strictEqual(tip.style.opacity,0);
assert.deepStrictEqual([...sel],[42]);
singleActive='expression_projection';
listeners.mousemove({clientX:10,clientY:10});
assert.strictEqual(lookups,2); // Linked preference must not disable Gene hover.
hoverValue='updated RGB'; refreshPanelHover(p);
assert.strictEqual(hoverCell,42);
assert.strictEqual(tip.innerHTML,'updated RGB 42');
listeners.mouseleave(); refreshPanelHover(p);
assert.strictEqual(hoverCell,null); assert.strictEqual(tip.style.opacity,0);
singleActive=null; D={n:1476}; syncLinkedHoverDefault(true);
assert.strictEqual(linkedHoverOn,true);
setLinkedHover(false); syncLinkedHoverDefault(false);
assert.strictEqual(linkedHoverOn,false); // Supplements preserve the choice.
D={n:200000}; syncLinkedHoverDefault(true);
assert.strictEqual(linkedHoverOn,true);
// The same canvas can return from an HLA network's optional static mode.
let surfaceHome = {}, restored = 0;
function restoreNode(){restored++;}
p.canvas.style={pointerEvents:'none'};
const classes=new Set(['cv-pane-static']);
p.pane={classList:{remove:name=>classes.delete(name)}};
eval(js.declarations(["restoreLinkedSurface"]));
restoreLinkedSurface();
assert.strictEqual(p.canvas.style.pointerEvents,'');
assert.strictEqual(classes.has('cv-pane-static'),false);
assert.strictEqual(restored,4);
console.log('PASS Linked hover defaults, no picking when disabled, retained selection and specialist hover');
