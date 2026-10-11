const fs=require('fs'),assert=require('assert');
const source=fs.readFileSync(process.argv[2],'utf8');
let D,paint=[],fallback=0,reports=0,selection=[1,2],fail=false;
const panels=[{spaceId:'main'},{spaceId:'clone'}];
function shownState(){return {mask:null,count:2};}
function draw(p){paint.push({space:p.spaceId,selection:selection.slice(),progressive:D.progressive});}
function renderShownCount(){}function updateResetButtons(){}
function configFingerprint(){return 'fp';}
function exportWorkspace(){return {selection:selection.slice()};}
function applyData(d){D=d;selection=[];drawAll();drawAll();}
function restoreWorkspace(saved){drawAll();if(fail)throw Error('restore failed');selection=saved.selection;drawAll();}
function reportWorkspaceReady(){reports++;}
function requestWireFallback(){fallback++;}
const context={epoch:'test',dataset_key:'a',generation:1};
const window={CBViewWire:{unpack:x=>x},CerebroDatasetContext:{
  accepts:x=>!!x&&x.dataset_context===context,
  same:(a,b)=>a===b
}};
function datasetMessageFresh(message){return window.CerebroDatasetContext.accepts(message);}
function sameDatasetContext(left,right){return window.CerebroDatasetContext.same(left,right);}
const CBViewState={telemetry:{snapshot:x=>x}};
let transportMetrics={},pendingCloneSupplement=null;
eval(source.slice(source.indexOf('  var drawAllDeferred = false;'),source.indexOf('  // Drop any committed lasso')));
eval(source.slice(source.indexOf('  function applyHydratedSupplement('),source.indexOf('  function onBinarySupplement(')));
function reset(){D={dataset_id:'a',dataset_context:context,progressive_token:4,progressive:true,n:2,cells:['a','b']};selection=[1,2];paint=[];}
const extra={dataset_id:'a',dataset_context:context,dataset_fingerprint:'fp',progressive_token:4,spaces:[{id:'other'}]};
reset();applyHydratedSupplement(extra);assert.equal(paint.length,2);assert(paint.every(p=>!p.progressive));assert.deepStrictEqual(paint[0].selection,[1,2]);assert.equal(reports,1);assert.equal(fallback,0);
reset();applyHydratedSupplement({...extra,dataset_id:'old'});assert.equal(paint.length,0);assert(D.progressive);
reset();fail=true;applyHydratedSupplement(extra);assert.equal(fallback,1);assert.equal(drawAllDeferred,false);drawAll();assert.equal(paint.length,2);
console.log('PASS supplement paints final restored state once, ignores stale datasets, recovers after errors');
