// The background image must rotate in data space about the origin, the same
// way R rotates the cell coordinates (rotateSpatialCoordinates). Rotating it on
// screen about its own centre leaves it behind when the plot is rotated.
const fs = require('fs'), vm = require('vm'), assert = require('assert');
const source = fs.readFileSync(process.argv[2], 'utf8');
function fn(name) {
  const start = source.indexOf('  function ' + name + '(');
  assert(start >= 0, name);
  const end = source.indexOf('\n  function ', start + 1);
  return source.slice(start, end < 0 ? undefined : end);
}

// Portrait image (like the MERFISH DAPI mosaic) beside a rotated scene.
const bounds = {xmin: 0, xmax: 6, ymin: 0, ymax: 10};
let transform = null, translate = null;
const context = {
  save() {}, restore() {}, drawImage() {},
  translate(x, y) { translate = [x, y]; },
  scale() {}, rotate() { throw new Error('image must not be rotated on screen'); },
  transform(...m) { transform = m; },
  globalAlpha: 1
};
const sandbox = {
  singleActive: 'spatial_projection', linkedBackgroundsOn: false,
  spaceById: {s: {_imgEl: {}, _imgReady: true,
    _imgState: {show: true, opacity: 1, offsetX: 0, offsetY: 0, scaleX: 1,
      scaleY: 1, flipX: false, flipY: false, rotate: 90}}},
  currentImage: () => ({bounds, preset: {}}),
  imageRenderState: (img, state) => state,
  // Screen = data with the y axis flipped, so rotation direction is testable.
  dataToScreen: (p, x, y) => [x, -y]
};
vm.createContext(sandbox);
vm.runInContext(['rotateDataPoint', 'drawImage'].map(fn).join('\n'), sandbox);
sandbox.drawImage({spaceId: 's', ctx: context}, context);

// Where the image centre must land: its data centre rotated 90 degrees about
// the origin, as the cells are.
const centre = [3, 5], theta = Math.PI / 2;
const expected = [centre[0] * Math.cos(theta) - centre[1] * Math.sin(theta),
  centre[0] * Math.sin(theta) + centre[1] * Math.cos(theta)];
assert(Math.abs(translate[0] - expected[0]) < 1e-9, 'x ' + translate[0]);
assert(Math.abs(translate[1] + expected[1]) < 1e-9, 'y ' + translate[1]);
// Width and height axes of the image are rotated along with it.
assert(Math.abs(transform[0]) < 1e-9 && Math.abs(transform[3]) < 1e-9,
  'image axes follow the rotation: ' + transform.join(','));
console.log('PASS background image rotates in data space with the cells');
