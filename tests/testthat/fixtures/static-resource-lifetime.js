const assert=require('assert'),fs=require('fs');
const source=fs.readFileSync(process.argv[2],'utf8');
const js = require("./js-source.cjs")(source);
const extract = name => js.declarations([name]);
const projectionResourceCache=new Map(),projectionSubsetResourceCache=new Map(),metadataCodesResourceCache=new Map(),categoricalResourceCache=new Map();
let fetches=0;
const window={fetch:async()=>{fetches++;return {ok:true,arrayBuffer:async()=>new ArrayBuffer(8000000)};}};
eval(extract('resourceCacheKey'));eval(extract('fetchCachedResource'));eval(extract('clearStaticResourceCaches'));
let assetCache = {}, pendingAssets = new Set(), spaceById = {}, imgToken = 0;
function resetSingleViews() {}
function markSingleHosts() {}
function showUnavailable() {}
function currentDatasetContext() { return null; }
eval(extract('invalidateDatasetViewState'));
(async()=>{
 const rows=[];
 for(let dataset=0;dataset<10;dataset++){
  invalidateDatasetViewState({phase:'pending'});
  const resource={url:`/dataset-${dataset}/projection.bin`,checksum:'immutable'};
  const a=await fetchCachedResource(resource,projectionResourceCache,'test','',buffer=>new Float32Array(buffer));
  const b=await fetchCachedResource(resource,projectionResourceCache,'test','',()=>{throw Error('duplicate decode');});
  assert.strictEqual(a.value,b.value);assert(b.cacheHit);assert.equal(projectionResourceCache.size,1);
  rows.push({datasets:dataset+1,retainedBytes:a.value.byteLength,entries:projectionResourceCache.size});
 }
 assert.equal(fetches,10);
 for(const map of [projectionSubsetResourceCache,metadataCodesResourceCache,categoricalResourceCache])map.set('old',Promise.resolve({value:new Uint8Array(100)}));
 invalidateDatasetViewState({phase:'pending'});
 for(const map of [projectionResourceCache,projectionSubsetResourceCache,metadataCodesResourceCache,categoricalResourceCache])assert.equal(map.size,0);
 // In-flight failure after invalidation must not remove a new same-key request.
 let rejectOld,resolveNew;
 window.fetch=()=>new Promise((resolve,reject)=>{rejectOld=reject;});
 const resource={url:'/same',checksum:'same'};
 const old=fetchCachedResource(resource,projectionResourceCache,'test','',x=>x).catch(()=>null);
 clearStaticResourceCaches();
 window.fetch=()=>new Promise(resolve=>{resolveNew=resolve;});
 const fresh=fetchCachedResource(resource,projectionResourceCache,'test','',x=>x);
 const saved=projectionResourceCache.get(resourceCacheKey(resource,''));
 rejectOld(Error('obsolete'));await old;
 assert.strictEqual(projectionResourceCache.get(resourceCacheKey(resource,'')),saved);
 resolveNew({ok:true,arrayBuffer:async()=>new ArrayBuffer(8)});await fresh;
 clearStaticResourceCaches();
 console.log(JSON.stringify({pass:true,rows,afterInvalidationEntries:projectionResourceCache.size,lateFailureSafe:true,sameDatasetReuse:true}));
})().catch(e=>{console.error(e);process.exitCode=1;});
