const fs = require('fs'), assert = require('assert/strict'), vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');

async function check(backend) {
  const uploads = [];
  const record = array => uploads.push(Buffer.from(array.buffer, array.byteOffset, array.byteLength));
  const gl = {
    createShader: () => ({}), shaderSource() {}, compileShader() {},
    getShaderParameter: () => true, deleteShader() {},
    createProgram: () => ({}), attachShader() {}, linkProgram() {},
    getProgramParameter: () => true, getUniformLocation: () => ({}),
    createBuffer: () => ({}), useProgram() {}, bindBuffer() {},
    bufferData: (_target, array) => record(array), enableVertexAttribArray() {},
    vertexAttribIPointer() {}, vertexAttribPointer() {}, viewport() {},
    getParameter: () => 'test adapter'
  };
  const device = {
    createShaderModule: () => ({}),
    createRenderPipelineAsync: async () => ({getBindGroupLayout: () => ({})}),
    lost: new Promise(() => {}), addEventListener() {},
    createBuffer: () => ({destroy() {}}), createBindGroup: () => ({}),
    queue: {writeBuffer(_buffer, _offset, data, offset, length) {
      uploads.push(Buffer.from(data, offset, length));
    }}
  };
  const gpu = {
    requestAdapter: async () => ({requestDevice: async () => device}),
    getPreferredCanvasFormat: () => 'rgba8unorm'
  };
  const context = {configure() {}};
  const canvas = {
    style: {}, clientWidth: 100, clientHeight: 100, addEventListener() {},
    getContext: type => type === 'webgpu' ? context : gl
  };
  const scope = {
    window: {}, navigator: backend === 'webgpu' ? {gpu} : {}, performance,
    GPUBufferUsage: {UNIFORM: 1, COPY_DST: 2, VERTEX: 4},
    GPUTextureUsage: {RENDER_ATTACHMENT: 1, COPY_SRC: 2}
  };
  // Execute the complete renderer and use its existing public API. Only the
  // hardware boundary is replaced, so no source extraction/parser is needed.
  vm.runInNewContext(source, scope);
  const renderer = scope.window.CerebroPointRenderer.create(canvas);
  await renderer.ready;
  assert.equal(renderer.isReady(), true);
  const positions = new Float32Array([0, 0, 1, 1]);
  const colors = new Uint8Array([1, 2, 3, 255, 4, 5, 6, 255]);
  const layers = new Uint32Array([0, 0]);
  const bytes = array => Buffer.from(array.buffer, array.byteOffset, array.byteLength);
  renderer.setData({positions, colors, layers, count: 2, foreground: false});
  assert.deepEqual(uploads, [bytes(positions), bytes(colors), bytes(layers)]);
  renderer.setData({positions, colors, layers, count: 2, foreground: false});
  assert.equal(uploads.length, 3);
  const updated = colors.slice(); updated[3] = 13;
  renderer.setData({positions, colors: updated, layers, count: 2, foreground: false});
  assert.equal(uploads.length, 4);
  assert.deepEqual(uploads[3], bytes(updated));
  const newLayers = layers.slice(); newLayers[1] = 1;
  renderer.setData({positions, colors: updated, layers: newLayers, count: 2, foreground: true});
  assert.equal(uploads.length, 5);
  assert.deepEqual(uploads[4], bytes(newLayers));
  renderer.setData({positions, colors: updated, layers: newLayers, count: 1, foreground: true});
  assert.equal(uploads.length, 5);
  assert.equal(renderer.stats().pointCount, 1);
  const newPositions = positions.slice(); newPositions[0] = 0.25;
  renderer.setData({positions: newPositions, colors: updated, layers: newLayers, count: 2, foreground: true});
  assert.equal(uploads.length, 6);
  assert.deepEqual(uploads[5], bytes(newPositions));
  assert.equal(renderer.stats().positionUploads, 2);
  assert.throws(() => renderer.setData({positions: new Float32Array(1), colors, layers, count: 2}), /shorter/);
  console.log('PASS', backend, 'complete renderer: exact uploads, unchanged buffers and count-only changes');
}
(async () => {
  await check('webgpu');
  await check('webgl2');
})().catch(error => { console.error(error); process.exitCode = 1; });
