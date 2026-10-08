const fs=require('fs'),assert=require('assert');
const source=fs.readFileSync(process.argv[2],'utf8');
const n=97,RGB_MODE='rgb';
let D={n},sel=null,nicheSet=null,mode='field',opacity=.73,revision=0;
let unit={nx:new Float32Array(n),ny:new Float32Array(n),ok:new Uint8Array(n)};
for(let i=0;i<n;i++){unit.nx[i]=i/n;unit.ny[i]=1-i/n;unit.ok[i]=i%13!==0;}
let order=new Uint32Array(Array.from({length:n},(_,i)=>n-1-i)),palette=[[12,34,56,255],[200,128,99,255]],single=false;
const shown=new Uint8Array(Array.from({length:n},(_,i)=>i%7!==0)),shownCount=shown.reduce((a,b)=>a+b,0);
function gpuDataState(){return {data:D,unit,mode,opacity,revision,selection:sel,niche:nicheSet,groupValues:single?shown:null,groupColors:single?'flat':null};}
function panelColorMode(){return mode;}function pointOpacityOf(){return opacity;}function paintOrder(){return order;}
function gpuColor(key){return key==='flat'?palette[0]:palette[key];}function colorOf(i){return i%palette.length;}
function rgbGpuColor(r,g,b){return r|(g<<8)|(b<<16)|((r+g+b>0)?0x1000000:0);}
eval(source.slice(source.indexOf('  function sameGpuDataState('),source.indexOf('  function gpuCandidate(')));
eval(source.slice(source.indexOf('  function updateGpuSelection('),source.indexOf('  function drawGpuPoints(')));
function compare(p,mask=shown,count=shownCount){
 const before=p.gpuData;const actual=buildGpuData(p,mask,count),expected=buildGpuData({},mask,count);
 // GPU draws count vertices; unused capacity differs between the flat-fill
 // and filtered paths and must never be rendered. Compare every active byte.
 for(const [key,width] of [['positions',2],['colors',4],['layers',1]])assert.deepStrictEqual(Array.from(actual[key].subarray(0,actual.count*width)),Array.from(expected[key].subarray(0,expected.count*width)),key);
 assert.equal(actual.count,expected.count);assert.equal(actual.foreground,expected.foreground);
 return {before,actual};
}
let checks=0;
for(const alpha of [255,128])for(const sorted of [true,false]){
 palette=[[12,34,56,alpha],[200,128,99,255]];order=sorted?new Uint32Array(Array.from({length:n},(_,i)=>n-1-i)):null;
 sel=null;nicheSet=null;mode='field';const p={};compare(p);
 for(const selected of [new Set([1,2,13,14,55]),new Set([3]),new Set(Array.from({length:n},(_,i)=>i)),null]){
  sel=selected;let {before,actual}=compare(p);if(alpha===255)assert.strictEqual(actual.positions,before.positions);else assert.notStrictEqual(actual.positions,before.positions);checks++;
 }
 nicheSet=new Set([5,10]);compare(p);nicheSet=null;compare(p);
 for(const change of [()=>{opacity=opacity===.5?.73:.5;},()=>{revision++;},()=>{unit={...unit,nx:unit.nx.slice()};},()=>{D={...D};}]){
  change();let {before,actual}=compare(p);assert.notStrictEqual(actual.positions,before.positions);checks++;
 }
}
mode='rgb';D.rgb={r:new Uint8Array(n),g:new Uint8Array(n),b:new Uint8Array(n)};D.rgb.r[2]=255;
sel=null;nicheSet=null;order=null;let p={};compare(p);sel=new Set([2,3]);let result=compare(p);assert.notStrictEqual(result.before.positions,result.actual.positions);
mode='field';palette=[[10,20,30,255]];single=true;sel=null;const full=new Uint8Array(n);full.fill(1);p={};compare(p,full,n);sel=new Set([2,13]);result=compare(p,full,n);assert.strictEqual(result.before.positions,result.actual.positions);
console.log('PASS',checks,'selection-only GPU updates equal full rebuild, filters/invalid cells/order/alpha/clear/niche/RGB/geometry');
