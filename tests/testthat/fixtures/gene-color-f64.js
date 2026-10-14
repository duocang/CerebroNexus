const assert = require('assert');
const fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
const js = require("./js-source.cjs")(source);
eval(js.declarations(["singleColorCache", "SINGLE_COLOR_CACHE_LIMIT"]));
eval(js.declarations(['cacheSingleColor', 'hydrateColorPacket']));

const exact = 1 + Math.pow(2, -40);
const dense = hydrateColorPacket({protocol: 'dense-f64-v1', length: 4,
  color: [exact, null, 0, -0]}, 'dense');
assert(dense instanceof Float64Array);
assert.strictEqual(dense[0], exact);
assert(Number.isNaN(dense[1]));
assert.strictEqual(dense[2], 0);
assert(Object.is(dense[3], -0));
assert.strictEqual(hydrateColorPacket({protocol: 'dense-f64-v1', length: 4,
  color: [9, 9, 9, 9]}, 'dense'), dense);

const sparse = hydrateColorPacket({protocol: 'sparse-f64-v1', length: 5,
  index: [1, 3], color: [exact, null]}, 'sparse');
assert(sparse instanceof Float64Array);
assert.deepStrictEqual(Array.from(sparse.slice(0, 3)), [0, exact, 0]);
assert(Number.isNaN(sparse[3]));
assert.strictEqual(sparse[4], 0);
assert.strictEqual(hydrateColorPacket({protocol: 'sparse-f32-v1',
  length: 1, index: [0], color: [1]}, ''), null);
console.log('PASS full-precision dense/sparse Gene colours, NA, zero and cache');

singleColorCache.clear();
const packet = value => ({protocol:'dense-f64-v1', length:1, color:[value]});
for (const key of ['a','b','c','d']) hydrateColorPacket(packet(1), key);
const kept = hydrateColorPacket(packet(99), 'a');
hydrateColorPacket(packet(2), 'e');
assert.strictEqual(hydrateColorPacket(packet(99), 'a'), kept);
assert.strictEqual(hydrateColorPacket(packet(3), 'b')[0], 3);
assert(singleColorCache.size <= 4);
