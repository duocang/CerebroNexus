const fs=require('fs'),assert=require('assert'),vm=require('vm');
const context={window:{},Int32Array,Float32Array,Math,isFinite};
let source=fs.readFileSync(process.argv[2],'utf8');
vm.runInNewContext(source,context);const G=context.window.CBGeom;
let seed=17;function random(){seed=(Math.imul(seed,1664525)+1013904223)>>>0;return seed/4294967296;}
const n=20000,unit={nx:new Float32Array(n),ny:new Float32Array(n),ok:new Uint8Array(n)};
for(let i=0;i<n;i++){unit.nx[i]=random()*1.2-.1;unit.ny[i]=random()*1.2-.1;unit.ok[i]=i%37!==0;}
unit.nx[1]=unit.nx[2]=.5;unit.ny[1]=unit.ny[2]=.5;unit.nx[3]=NaN;unit.ny[4]=Infinity;
const grid=G.pointGrid(unit,n);assert.strictEqual(G.pointGrid(unit,n),grid);
let checks=0;
for(const view of [null,{cx:.5,cy:.5,span:.04},{cx:.2,cy:.8,span:.37},{cx:1.1,cy:-.1,span:.9}]) {
 for(const frame of [{x:10,y:10,width:1200,height:480},{x:36,y:10,width:90,height:1600}]) {
  const sx=new Float32Array(n),sy=new Float32Array(n);
  for(let i=0;i<n;i++){let x=unit.nx[i],y=unit.ny[i];if(view){x=(x-view.cx)/view.span+.5;y=(y-view.cy)/view.span+.5;}sx[i]=frame.x+x*frame.width;sy[i]=frame.y+frame.height-y*frame.height;}
  for(let q=0;q<150;q++) {
   const i=Math.floor(random()*n),mx=q%2?sx[i]+random()*20-10:random()*frame.width,my=q%2?sy[i]+random()*20-10:random()*frame.height;
   const accept=i=>i%7!==0;
   let expected=-1,bd=200;
   for(let j=0;j<n;j++){if(!unit.ok[j]||!accept(j))continue;let dx=sx[j]-mx,dy=sy[j]-my,d=dx*dx+dy*dy;if(d<bd){bd=d;expected=j;}}
   assert.equal(G.nearestInGrid(grid,view,frame,mx,my,accept),expected,JSON.stringify({view,frame,mx,my}));checks++;
  }
 }
}
const tiny={nx:new Float32Array([.5,.5]),ny:new Float32Array([.5,.5]),ok:new Uint8Array([1,1])};
let tg=G.pointGrid(tiny,2),frame={x:0,y:0,width:100,height:100};
assert.equal(G.nearestInGrid(tg,null,frame,50,50,()=>true),0);
assert.equal(G.nearestInGrid(tg,null,frame,50,50,i=>i===1),1);
assert.equal(G.nearestInGrid(tg,null,frame,64.2,50,()=>true),-1);
assert.equal(G.nearestInGrid(tg,null,frame,50,50,()=>false),-1);
tiny.nx=new Float32Array([.1,.2]);assert.notStrictEqual(G.pointGrid(tiny,2),tg);
console.log('PASS',checks,'indexed nearest vs full scan, views/aspect/filters/invalid coordinates/ties/cache replacement');
