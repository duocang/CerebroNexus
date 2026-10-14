const assert = require('assert'), fs = require('fs'), vm = require('vm');
const js = require('./js-source.cjs')(fs.readFileSync(process.argv[2], 'utf8'));
const scope = {
  spaceById: {}, D: {n:3}, deferredUnitOf:()=>null, gpuCandidate:()=>false,
  isSpatialSpace:space=>space.spatial, singleActive:'trekker_projection',
  singleSpaceIds:['transition'], panels:[], draws:0, drawAll(){scope.draws++;}
};
vm.createContext(scope);
vm.runInContext(js.declarations(['OCC','occIdx','occSAT','unitOf','project','transitionUnit','updateTrekkerTransition']),scope);
const space = {x:[0,20,10],y:[0,10,5]};
scope.spaceById.test = space;
for (const [W,H] of [[620,220],[220,620]]) {
  const panel = {spaceId:'test',W,H};
  space.spatial = false;
  scope.project(panel);
  assert.deepEqual(Array.from(panel.sx),[10,W-10,W/2]);
  assert.deepEqual(Array.from(panel.sy),[H-10,10,H/2]);
  space.spatial = true;
  scope.project(panel);
  assert.equal((panel.sx[1]-panel.sx[0])/20,(panel.sy[0]-panel.sy[1])/10,
    'spatial distances must use the same scale on both axes');
  assert.equal(panel.sx[2],W/2); assert.equal(panel.sy[2],H/2);
}
const from={x:[0,20,null],y:[0,10,5]}, to={x:[100,0,50],y:[0,200,50]};
const transition={_transition:{fromX:from.x,fromY:from.y,toX:to.x,toY:to.y,
  fromUnit:scope.unitOf(from),toUnit:scope.unitOf(to)}};
scope.spaceById.transition=transition;
scope.panels=[{spaceId:'transition',W:220,H:220}];
for (const value of [0,0.5,1,2,-1]) {
  scope.updateTrekkerTransition({target:{id:'trekker_morph',value}});
  const t=Math.min(1,Math.max(0,value));
  assert.deepEqual(Array.from(transition.x),[100*t,20*(1-t),null]);
  assert.deepEqual(Array.from(transition._unit.nx),[t,1-t,0]);
  assert.deepEqual(Array.from(transition._unit.ok),[1,1,0]);
  assert.equal(scope.panels[0].sx[0],10+200*t);
}
scope.singleActive='overview_projection';
scope.updateTrekkerTransition({target:{id:'trekker_morph',value:1}});
assert.equal(scope.draws,5,'hidden Trekker must not redraw');

vm.runInContext(js.declarations(['unitToScreen','dataToScreen','drawTrajectory','pointSizeOf','radiusOf']),scope);
space.trajectory=true;
const panel={spaceId:'test',W:620,H:220};
scope.project(panel);
const lines=[];
const pen={save(){},restore(){},beginPath(){},stroke(){},
  moveTo(x,y){lines.push(['move',x,y]);},lineTo(x,y){lines.push(['line',x,y]);}};
for(const edges of [[[0,0,20,10]],{x0:[0],y0:[0],x1:[20],y1:[10]},
  {from:[1,0,9],to:[2,2,1]}]) {
  space.edges=edges;lines.length=0;scope.drawTrajectory(panel,pen);
  assert.deepEqual(lines,[['move',panel.sx[0],panel.sy[0]],['line',panel.sx[1],panel.sy[1]]]);
}
scope.ps=6;
space.pointSizes=[10,0,NaN];
assert.equal(scope.radiusOf(panel,0),5);
assert.equal(scope.radiusOf(panel,1),3);
panel._renderPointSize=8;
assert.equal(scope.radiusOf(panel,2),4);
panel.depth=[0,5,10];panel._dmin=0;panel._dspan=10;
assert.equal(scope.radiusOf(panel,0),2.5);
assert.equal(scope.radiusOf(panel,2),5.4);

const hla = require('./js-source.cjs')(fs.readFileSync(process.argv[3],'utf8'));
const motion={matches:false};
const hlaScope={window:{matchMedia:()=>motion}};
vm.createContext(hlaScope);
vm.runInContext(hla.declarations(['reducedMotion','withMotion']),hlaScope);
assert.equal(hlaScope.withMotion({},300).animation.duration,300);
motion.matches=true;
assert.equal(hlaScope.withMotion({},300).animation,false);
assert.equal(hlaScope.withMotion({},0).animation,false);
