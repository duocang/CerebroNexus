(function (global) {
  'use strict';

  var VERTEX_SOURCE = `#version 300 es
precision highp float;
layout(location = 0) in vec2 a_position;
layout(location = 1) in vec4 a_color;
layout(location = 2) in uint a_layer;
uniform vec4 u_view;
uniform vec4 u_rect;
uniform vec4 u_canvas;
uniform int u_pass;
out vec2 v_offset;
out vec4 v_color;
const vec2 CORNERS[4] = vec2[4](
  vec2(-1.0, -1.0), vec2(1.0, -1.0),
  vec2(-1.0, 1.0), vec2(1.0, 1.0)
);
void main() {
  vec2 corner = CORNERS[gl_VertexID];
  float outerRadius = u_canvas.z + u_canvas.w * 0.5 + 1.0;
  vec2 zoomed = (a_position - u_view.xy) / u_view.z + vec2(0.5);
  vec2 screen = vec2(
    u_rect.x + zoomed.x * u_rect.z,
    u_rect.y + (1.0 - zoomed.y) * u_rect.w
  ) + corner * outerRadius;
  vec2 clip = vec2(
    screen.x / u_canvas.x * 2.0 - 1.0,
    1.0 - screen.y / u_canvas.y * 2.0
  );
  bool hidden = u_pass >= 0 && int(a_layer) != u_pass;
  gl_Position = hidden ? vec4(2.0, 2.0, 0.0, 1.0) : vec4(clip, 0.0, 1.0);
  v_offset = corner * outerRadius;
  v_color = a_color;
}`;

  var FRAGMENT_SOURCE = `#version 300 es
precision highp float;
in vec2 v_offset;
in vec4 v_color;
uniform vec4 u_border;
uniform vec2 u_style;
out vec4 out_color;
void main() {
  float distanceFromCenter = length(v_offset);
  float radius = u_style.x;
  float borderWidth = u_style.y;
  float outerRadius = radius + borderWidth * 0.5;
  float aa = max(fwidth(distanceFromCenter), 0.75);
  float coverage = 1.0 - smoothstep(outerRadius - aa, outerRadius + aa,
    distanceFromCenter);
  if (coverage <= 0.0) discard;
  float borderMix = borderWidth > 0.0
    ? smoothstep(radius - borderWidth * 0.5 - aa,
        radius - borderWidth * 0.5 + aa, distanceFromCenter)
    : 0.0;
  out_color = mix(v_color, u_border, borderMix);
  out_color.a *= coverage;
}`;

  function shader(gl, type, source) {
    var value = gl.createShader(type);
    gl.shaderSource(value, source);
    gl.compileShader(value);
    if (!gl.getShaderParameter(value, gl.COMPILE_STATUS)) {
      var message = gl.getShaderInfoLog(value);
      gl.deleteShader(value);
      throw new Error(message || 'WebGL2 shader compilation failed.');
    }
    return value;
  }

  function program(gl) {
    var vertex = shader(gl, gl.VERTEX_SHADER, VERTEX_SOURCE);
    var fragment = shader(gl, gl.FRAGMENT_SHADER, FRAGMENT_SOURCE);
    var value = gl.createProgram();
    gl.attachShader(value, vertex);
    gl.attachShader(value, fragment);
    gl.linkProgram(value);
    gl.deleteShader(vertex);
    gl.deleteShader(fragment);
    if (!gl.getProgramParameter(value, gl.LINK_STATUS)) {
      var message = gl.getProgramInfoLog(value);
      gl.deleteProgram(value);
      throw new Error(message || 'WebGL2 program link failed.');
    }
    return value;
  }

  function create(canvas) {
    var gl = canvas.getContext('webgl2', {
      alpha: true,
      antialias: false,
      depth: false,
      premultipliedAlpha: true,
      preserveDrawingBuffer: true
    });
    if (!gl) throw new Error('WebGL2 is unavailable.');

    var pipeline = program(gl);
    var vao = gl.createVertexArray();
    var positionBuffer = gl.createBuffer();
    var colorBuffer = gl.createBuffer();
    var layerBuffer = gl.createBuffer();
    var data = null;
    var width = 1;
    var height = 1;
    var debugInfo = gl.getExtension('WEBGL_debug_renderer_info');
    var metrics = {
      backend: 'webgl2',
      ready: true,
      pointCount: 0,
      adapter: debugInfo
        ? gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL)
        : gl.getParameter(gl.RENDERER)
    };
    var locations = {
      view: gl.getUniformLocation(pipeline, 'u_view'),
      rect: gl.getUniformLocation(pipeline, 'u_rect'),
      canvas: gl.getUniformLocation(pipeline, 'u_canvas'),
      pass: gl.getUniformLocation(pipeline, 'u_pass'),
      border: gl.getUniformLocation(pipeline, 'u_border'),
      style: gl.getUniformLocation(pipeline, 'u_style')
    };

    gl.bindVertexArray(vao);
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0);
    gl.vertexAttribDivisor(0, 1);
    gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
    gl.enableVertexAttribArray(1);
    gl.vertexAttribPointer(1, 4, gl.UNSIGNED_BYTE, true, 0, 0);
    gl.vertexAttribDivisor(1, 1);
    gl.bindBuffer(gl.ARRAY_BUFFER, layerBuffer);
    gl.enableVertexAttribArray(2);
    gl.vertexAttribIPointer(2, 1, gl.UNSIGNED_INT, 0, 0);
    gl.vertexAttribDivisor(2, 1);
    gl.bindVertexArray(null);

    gl.disable(gl.DEPTH_TEST);
    gl.disable(gl.CULL_FACE);
    gl.enable(gl.BLEND);
    gl.blendFuncSeparate(
      gl.SRC_ALPHA,
      gl.ONE_MINUS_SRC_ALPHA,
      gl.ONE,
      gl.ONE_MINUS_SRC_ALPHA
    );

    function resize(cssWidth, cssHeight, dpr) {
      width = Math.max(1, Number(cssWidth) || 1);
      height = Math.max(1, Number(cssHeight) || 1);
      dpr = Math.max(1, Number(dpr) || 1);
      var pixelWidth = Math.max(1, Math.round(width * dpr));
      var pixelHeight = Math.max(1, Math.round(height * dpr));
      if (canvas.width !== pixelWidth) canvas.width = pixelWidth;
      if (canvas.height !== pixelHeight) canvas.height = pixelHeight;
      canvas.style.width = width + 'px';
      canvas.style.height = height + 'px';
      gl.viewport(0, 0, pixelWidth, pixelHeight);
    }

    function setData(next) {
      if (!next || !next.positions || !next.colors || !next.layers) {
        throw new Error('WebGL2 point data is incomplete.');
      }
      var count = Number(next.count) || 0;
      if (next.positions.length < count * 2 || next.colors.length < count * 4 ||
          next.layers.length < count) {
        throw new Error('WebGL2 point buffers are shorter than count.');
      }
      if (data && data.positions === next.positions && data.colors === next.colors &&
          data.layers === next.layers && data.count === count &&
          data.foreground === !!next.foreground) return;
      var started = performance.now();
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, next.positions, gl.STATIC_DRAW);
      gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, next.colors, gl.DYNAMIC_DRAW);
      gl.bindBuffer(gl.ARRAY_BUFFER, layerBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, next.layers, gl.STATIC_DRAW);
      data = {
        positions: next.positions,
        colors: next.colors,
        layers: next.layers,
        count: count,
        foreground: !!next.foreground
      };
      metrics.pointCount = count;
      metrics.uploadSubmitMs = performance.now() - started;
    }

    function draw(options) {
      if (!data) return false;
      options = options || {};
      var view = options.view || { cx: 0.5, cy: 0.5, span: 1 };
      var rect = options.rect || { x: 0, y: 0, width: width, height: height };
      var pointSize = Math.max(0, Number(options.pointSize) || 0);
      var border = options.border;
      var borderWidth = border ? Math.max(0, Number(border.width) || 0) : 0;
      var borderColor = border && border.color ? border.color : [0, 0, 0, 0];
      var started = performance.now();

      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
      if (!data.count || !pointSize) return true;
      gl.useProgram(pipeline);
      gl.bindVertexArray(vao);
      gl.uniform4f(locations.view, Number(view.cx), Number(view.cy),
        Math.max(1e-9, Number(view.span)), 0);
      gl.uniform4f(locations.rect, Number(rect.x), Number(rect.y),
        Number(rect.width), Number(rect.height));
      gl.uniform4f(locations.canvas, width, height, pointSize / 2, borderWidth);
      gl.uniform4f(locations.border, borderColor[0] / 255, borderColor[1] / 255,
        borderColor[2] / 255, borderColor[3] / 255);
      gl.uniform2f(locations.style, pointSize / 2, borderWidth);
      if (data.foreground) {
        gl.uniform1i(locations.pass, 0);
        gl.drawArraysInstanced(gl.TRIANGLE_STRIP, 0, 4, data.count);
        gl.uniform1i(locations.pass, 1);
        gl.drawArraysInstanced(gl.TRIANGLE_STRIP, 0, 4, data.count);
      } else {
        gl.uniform1i(locations.pass, -1);
        gl.drawArraysInstanced(gl.TRIANGLE_STRIP, 0, 4, data.count);
      }
      gl.bindVertexArray(null);
      metrics.drawSubmitMs = performance.now() - started;
      return true;
    }

    function clear() {
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
    }

    resize(canvas.clientWidth || 1, canvas.clientHeight || 1,
      global.devicePixelRatio || 1);
    return {
      ready: Promise.resolve(),
      isReady: function () { return true; },
      resize: resize,
      setData: setData,
      draw: draw,
      clear: clear,
      idle: function () {
        var fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
        gl.flush();
        return new Promise(function (resolve, reject) {
          function poll() {
            var status = gl.clientWaitSync(fence, 0, 0);
            if (status === gl.TIMEOUT_EXPIRED) {
              global.setTimeout(poll, 0);
              return;
            }
            gl.deleteSync(fence);
            if (status === gl.WAIT_FAILED) {
              reject(new Error('WebGL2 fence wait failed.'));
            } else {
              resolve();
            }
          }
          poll();
        });
      },
      stats: function () {
        return Object.assign({}, metrics, {
          contextLost: gl.isContextLost(),
          error: gl.getError()
        });
      }
    };
  }

  global.CerebroPointRenderer = { backend: 'webgl2', create: create };
})(window);
