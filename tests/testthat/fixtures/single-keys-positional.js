const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
var D = { rows: 'all', cells: ['a', 'b', 'c'] };
eval(source.slice(
  source.indexOf('  function keysAreViewCells('),
  source.indexOf('  function emptyVector(')
));
assert.strictEqual(keysAreViewCells(['a', 'b', 'c']), true);
// A reordered, shorter or foreign key set needs the barcode Map.
assert.strictEqual(keysAreViewCells(['b', 'a', 'c']), false);
assert.strictEqual(keysAreViewCells(['a', 'b']), false);
assert.strictEqual(keysAreViewCells(['a', 'b', 'x']), false);
assert.strictEqual(keysAreViewCells(null), false);
// Cells that are not every canonical cell may repeat: no shortcut.
D.rows = [0, 1, 2];
assert.strictEqual(keysAreViewCells(['a', 'b', 'c']), false);
D.rows = null;
assert.strictEqual(keysAreViewCells(['a', 'b', 'c']), false);
console.log('PASS keys that are the view cells in order skip the barcode Map');
