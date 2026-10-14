const assert = require('assert'), fs = require('fs'), vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
const js = require("./js-source.cjs")(source);
const extract = name => js.declarations([name]);
const handlers = {}, requests = [], applied = [];
const sandbox = {
  assetCache: Object.create(null), pendingAssets: new Set(), spaceById: {}, imgToken: 0,
  resetSingleViews() {}, clearStaticResourceCaches() {}, markSingleHosts() {}, showUnavailable() {},
  applyDeferredAsset(key, uri) { applied.push({key, uri}); },
  document: {addEventListener() {}},
  CustomEvent: function(type, options) { this.type = type; this.detail = options.detail; },
  dispatchEvent() {}, setTimeout() {},
  Shiny: {
    addCustomMessageHandler(name, handler) { handlers[name] = handler; },
    setInputValue(name, value) { requests.push({name, value}); }
  }
};
sandbox.window = sandbox;
vm.createContext(sandbox);
vm.runInContext(fs.readFileSync(process.argv[3], 'utf8'), sandbox);
for (const name of ['datasetContextApi', 'currentDatasetContext', 'currentDatasetContextKey',
  'datasetMessageFresh', 'configFingerprint', 'deferredAssetCacheKey',
  'requestDeferredAsset', 'invalidateDatasetViewState']) {
  vm.runInContext(extract(name), sandbox);
}
vm.runInContext(js.handler('coordviews_asset'), sandbox);
const C = sandbox.CerebroDatasetContext;
function activate(dataset, generation) {
  const context = {epoch: 'session', dataset_key: dataset, generation};
  assert(C.receive({phase: 'pending', dataset_context: context}));
  sandbox.invalidateDatasetViewState({phase: 'pending', dataset_context: context});
  assert.strictEqual(Object.keys(sandbox.assetCache).length, 0, 'retired image URI remains owned');
  assert.strictEqual(sandbox.pendingAssets.size, 0, 'retired pending request remains owned');
  assert(C.receive({phase: 'ready', dataset_context: context}));
  sandbox.D = {dataset_context: context, dataset_id: dataset, dataset_fingerprint: 'fp-' + dataset};
  return context;
}
function message(context, uri) {
  return {dataset_context: context, dataset_fingerprint: 'fp-' + context.dataset_key,
    key: 'background', uri};
}
const a1 = activate('A', 1);
sandbox.requestDeferredAsset('background');
sandbox.requestDeferredAsset('background');
assert.strictEqual(requests.length, 1, 'pending request must be deduplicated');
handlers.coordviews_asset(message(a1, 'data:image/png;base64,first'));
assert.strictEqual(sandbox.pendingAssets.size, 0);
sandbox.requestDeferredAsset('background');
assert.strictEqual(requests.length, 1, 'same generation must reuse loaded image');
assert.strictEqual(applied.length, 2);
sandbox.requestDeferredAsset('unfinished');
const b2 = activate('B', 2);
handlers.coordviews_asset(message(a1, 'stale A'));
assert.strictEqual(applied.length, 2);
assert.strictEqual(Object.keys(sandbox.assetCache).length, 0);
sandbox.requestDeferredAsset('background');
handlers.coordviews_asset(message(b2, 'data:image/png;base64,second'));
const a3 = activate('A', 3);
sandbox.requestDeferredAsset('background');
handlers.coordviews_asset(message(a1, 'stale first A'));
handlers.coordviews_asset(message(b2, 'stale B'));
assert.strictEqual(sandbox.pendingAssets.size, 1, 'stale response cannot satisfy current request');
assert.strictEqual(applied.length, 3);
handlers.coordviews_asset(message(a3, 'data:image/png;base64,third'));
assert.strictEqual(sandbox.pendingAssets.size, 0);
assert.strictEqual(Object.keys(sandbox.assetCache).length, 1);
assert.strictEqual(applied.at(-1).uri, 'data:image/png;base64,third');
console.log('PASS deferred image ownership A -> B -> A, same-generation reuse, pending deduplication, stale-response rejection');
