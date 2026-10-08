const fs=require('fs'),assert=require('assert');
const source=fs.readFileSync(process.argv[2],'utf8');
let D={n:1462702},bordersOn=false,revision=0,selection=null;
const GPU_MIN_CELLS=20000,window={devicePixelRatio:1};
let spaceById={clone:{_axisSpec:{},_unit:{}}};
function isSpatialSpace(s){return !!s.spatial;}
function gpuDataState(p){return {data:D,unit:spaceById[p.spaceId]._unit,revision,selection};}
function context(){return {copies:0,globalAlpha:.95,fillStyle:'red',strokeStyle:'black',lineWidth:1,
 save(){this.saved=[this.globalAlpha,this.fillStyle,this.strokeStyle,this.lineWidth];},
 restore(){[this.globalAlpha,this.fillStyle,this.strokeStyle,this.lineWidth]=this.saved;},
 setTransform(){},drawImage(){this.copies++;}};}
const document={createElement(){const ctx=context();return {width:0,height:0,getContext(){return ctx;}};}};
const p={spaceId:'clone',W:400,H:300,canvas:{width:400,height:300},ctx:context(),_renderPointSize:2,view:null};
eval(source.slice(source.indexOf('  function sameGpuDataState('),source.indexOf('  function gpuCandidate(')));
eval(source.slice(source.indexOf('  function cloneBaseState('),source.indexOf('  function draw(p,')));
assert.equal(restoreCloneBase(p,cloneBaseState(p)),false);
rememberCloneBase(p,cloneBaseState(p));const bitmap=p.cloneBase.canvas;
assert(restoreCloneBase(p,cloneBaseState(p)));assert.equal(p.ctx.copies,1);assert.equal(p.ctx.globalAlpha,.95);
for(const change of [()=>{p.view={cx:.5,cy:.5,span:.8};},()=>{p.view.cx=.4;},()=>{p.view.span=.4;},
 ()=>{p.canvas.width++;},()=>{p.W+=.25;},()=>{window.devicePixelRatio=2;},()=>{p._renderPointSize++;},
 ()=>{bordersOn=true;},()=>{spaceById.clone.pointBorder={color:'blue',width:2};},
 ()=>{spaceById.clone._axisSpec={};},()=>{spaceById.clone._unit={};},()=>{D={n:D.n};},
 ()=>{revision++;},()=>{selection=new Set([1]);},()=>{selection=null;}]){
 change();const state=cloneBaseState(p);assert.equal(restoreCloneBase(p,state),false);
 rememberCloneBase(p,state);assert.strictEqual(p.cloneBase.canvas,bitmap);assert(restoreCloneBase(p,cloneBaseState(p)));
}
for(const key of ['spatial','trajectory','hulls']){
 spaceById.clone[key]=key==='hulls'?[{}]:true;assert.equal(cloneBaseState(p),null);assert.equal(p.cloneBase,null);delete spaceById.clone[key];
}
spaceById.clone._unit.nz=[];assert.equal(cloneBaseState(p),null);delete spaceById.clone._unit.nz;
D={n:100};assert.equal(cloneBaseState(p),null);D={n:1462702};
spaceById.other={_unit:{},_axisSpec:{}};p.spaceId='other';assert.equal(cloneBaseState(p),null);
console.log('PASS clonal base reuse and invalidation for view, geometry, dataset, size, color, selection and unsupported scenes');
