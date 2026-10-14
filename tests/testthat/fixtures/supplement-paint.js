const fs=require('fs'),assert=require('assert');
const source=fs.readFileSync(process.argv[2],'utf8');
let spatialTemplate=null,selectedSpatial=[];
let D,linkedBundle,deferredLinkedSupplements=[],paint=[],fallback=0,reports=0,selection=[1,2],fail=false;
const panels=[{spaceId:'main'},{spaceId:'clone'}];
function shownState(){return {mask:null,count:2};}
function draw(p){paint.push({space:p.spaceId,selection:selection.slice(),progressive:D.progressive});}
function renderShownCount(){}function updateResetButtons(){}
function configFingerprint(){return 'fp';}
function exportWorkspace(){return {selection:selection.slice(),view:{spatial_sections:selectedSpatial.slice()}};}
function exportRuntimeWorkspace(){return exportWorkspace();}
function applyData(d){D=linkedBundle=d;selection=[];spatialTemplate=(d.spaces||[]).find(s=>s.id==='spatial')||null;if(spatialTemplate&&!selectedSpatial.length)selectedSpatial=['fov-a','fov-b'];drawAll();drawAll();}
function restoreWorkspace(saved){drawAll();if(fail)throw Error('restore failed');selection=saved.selection;selectedSpatial=saved.view.spatial_sections;drawAll();}
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
eval(source.slice(source.indexOf('  function linkedSupplementFresh('),source.indexOf('  function onBinarySupplement(')));
function reset(){spatialTemplate=null;selectedSpatial=[];D=linkedBundle={dataset_id:'a',dataset_context:context,progressive_token:4,progressive:true,n:2,cells:['a','b']};selection=[1,2];paint=[];}
const extra={dataset_id:'a',dataset_context:context,dataset_fingerprint:'fp',progressive_token:4,spaces:[{id:'other'}]};
reset();applyHydratedSupplement(extra);assert.equal(paint.length,2);assert(paint.every(p=>!p.progressive));assert.deepStrictEqual(paint[0].selection,[1,2]);assert.equal(reports,1);assert.equal(fallback,0);
reset();applyHydratedSupplement({...extra,dataset_id:'old'});assert.equal(paint.length,0);assert(D.progressive);
reset();applyHydratedSupplement({...extra,spaces:[{id:'spatial'}]});assert.deepStrictEqual(selectedSpatial,['fov-a','fov-b']);assert.deepStrictEqual(selection,[1,2]);
reset();spatialTemplate={id:'spatial'};applyHydratedSupplement({...extra,spaces:[{id:'spatial'}]});assert.deepStrictEqual(selectedSpatial,[]);
reset();fail=true;applyHydratedSupplement(extra);assert.equal(fallback,1);assert.equal(drawAllDeferred,false);drawAll();assert.equal(paint.length,2);
console.log('PASS supplement paints final restored state once, ignores stale datasets, recovers after errors');
