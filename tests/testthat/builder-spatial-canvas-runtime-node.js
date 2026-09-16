"use strict";

const fs = require("fs");
const path = require("path");
const vm = require("vm");

const delegated = {};
const nativeHandlers = {};
const inputs = [];
const custom = {};
const frames = [];
const arcs = [];
let timerId = 0;
const timers = new Map();
const context = new Proxy(
  {
    measureText: function (value) {
      return {width: String(value).length * 6};
    },
    arc: function (x, y) { arcs.push({x: x, y: y}); },
  },
  {
    get: function (target, key) {
      if (!(key in target)) target[key] = function () {};
      return target[key];
    },
    set: function (target, key, value) {
      target[key] = value;
      return true;
    },
  }
);
const canvas = {
  id: "enhance-alignment_spatial_plot",
  clientWidth: 500,
  clientHeight: 400,
  width: 0,
  height: 0,
  dataset: {},
  getContext: function () { return context; },
  addEventListener: function () {},
  getBoundingClientRect: function () { return {left: 0, top: 0}; },
};
const slider = {
  id: "enhance-img_scale",
  type: "text",
  value: "0.001",
  min: "0",
  max: "10",
  step: "0.001",
};
const number = {
  id: "enhance-img_scale_number",
  type: "number",
  value: "0.001",
  min: "0",
  max: "10",
  step: "0.001",
};
const elements = {
  "enhance-alignment_spatial_plot": canvas,
  "enhance-img_scale": slider,
  "enhance-img_scale_number": number,
};

global.document = {
  activeElement: null,
  getElementById: function (id) { return elements[id] || null; },
  addEventListener: function (type, handler) {
    nativeHandlers[type] = nativeHandlers[type] || [];
    nativeHandlers[type].push(handler);
  },
};
global.performance = {now: function () { return 10; }};
global.Image = function () {};
global.window = {
  devicePixelRatio: 1,
  addEventListener: function () {},
  requestAnimationFrame: function (callback) {
    frames.push(callback);
    return frames.length;
  },
  cancelAnimationFrame: function () {},
  setTimeout: function (callback) {
    timerId += 1;
    timers.set(timerId, callback);
    return timerId;
  },
  clearTimeout: function (id) { timers.delete(id); },
};
function jq(node) {
  return {
    on: function (events, selector, handler) {
      if (node === document) {
        events.split(/\s+/).forEach(function (event) {
          delegated[event.split(".")[0]] = handler;
        });
      }
      return this;
    },
    data: function (key, value) {
      node.__data = node.__data || {};
      if (arguments.length > 1) {
        node.__data[key] = value;
        return this;
      }
      return node.__data[key];
    },
  };
}
jq.fn = {};
window.jQuery = jq;
global.jQuery = jq;
slider.__data = {
  ionRangeSlider: {
    update: function (options) {
      if (Number.isFinite(Number(options.step))) {
        slider.step = String(options.step);
      }
      if (Number.isFinite(Number(options.from))) {
        const step = Number(slider.step) || 1;
        const rounded = Math.round(Number(options.from) / step) * step;
        slider.value = String(Number(rounded.toPrecision(15)));
      }
    },
  },
};
global.Shiny = {
  setInputValue: function (id, value) { inputs.push({id: id, value: value}); },
  addCustomMessageHandler: function (id, handler) { custom[id] = handler; },
};
window.Shiny = global.Shiny;

const canvasSource = path.resolve(
  __dirname,
  "..",
  "..",
  "inst",
  "builder",
  "www",
  "builder-spatial-canvas.js"
);
vm.runInThisContext(fs.readFileSync(canvasSource, "utf8"), {
  filename: canvasSource,
});

const controls = {
  coordinateRotation: 0,
  coordinateScale: 2,
  dx: 0,
  dy: 0,
  scale: 0.001,
  rotation: 0,
  flip_x: false,
  flip_y: false,
  image_opacity: 0.8,
  point_opacity: 0.85,
  point_size: 5,
};
const overlay = {
  available: true,
  viewKey: "overlay",
  dataset: "dataset-a",
  snapshotIdentity: "snapshot-a",
  section: "section-a",
  generation: 1,
  resetToken: 0,
  layout: "overlay",
  activeRoi: "",
  activeImage: "A",
  bounds: {xmin: 0, xmax: 10, ymin: 0, ymax: 20},
  image: null,
  roiImages: {},
  points: {
    x: [0, 10],
    y: [0, 20],
    group: ["A", "B"],
    color: ["#111", "#222"],
    count: [1, 1],
  },
  controls: controls,
};
custom.builder_spatial_canvas_scene(overlay);
frames.shift()();
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const overlayViewport = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_viewports";
}).slice(-1)[0].value;

slider.value = "0.01";
let stopped = 0;
delegated.change.call(slider, {
  type: "change",
  currentTarget: slider,
  originalEvent: undefined,
  stopImmediatePropagation: function () { stopped += 1; },
});
delegated.input.call(slider, {
  type: "input",
  currentTarget: slider,
  originalEvent: undefined,
  stopImmediatePropagation: function () { stopped += 1; },
});
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const commits = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
});
const numberValueAfterIon = number.value;

slider.step = "0.01";
number.step = "0.01";
number.value = "0.001";
const numberEvent = {
  target: number,
  stopImmediatePropagation: function () {},
};
nativeHandlers.input.forEach(function (handler) { handler(numberEvent); });
nativeHandlers.change.forEach(function (handler) { handler(numberEvent); });
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const tinyScaleCommits = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).slice(commits.length);
const tinyNumberValueAfterEntry = String(number.value);
const tinySliderValueAfterEntry = String(slider.value);
const tinyStepAfterEntry = String(slider.step);

const separate = {
  available: true,
  viewKey: "separate",
  dataset: "dataset-a",
  snapshotIdentity: "snapshot-a",
  section: "section-a",
  generation: 2,
  resetToken: 0,
  layout: "separate",
  activeRoi: "A",
  activeImage: "A",
  bounds: {xmin: 0, xmax: 30, ymin: 0, ymax: 20},
  image: null,
  roiImages: {},
  roiBounds: {
    A: {xmin: -100, xmax: 10, ymin: 0, ymax: 200},
    B: {xmin: 20, xmax: 30, ymin: 0, ymax: 20},
  },
  roiPointAppearance: {},
  roiCoordinateTransforms: {},
  points: {
    x: [-45, 10, 20, 30],
    y: [100, 20, 0, 20],
    group: ["A", "A", "B", "B"],
    color: ["#111", "#111", "#222", "#222"],
    count: [2, 2, 2, 2],
  },
  controls: Object.assign({}, controls, {coordinateRotation: 90}),
};
slider.step = "0.02";
number.step = "0.02";
arcs.length = 0;
custom.builder_spatial_canvas_scene(separate);
frames.shift()();
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const separateViewport = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_viewports";
}).slice(-1)[0].value;
const separateFirstPoint = arcs[0];
const authoritativeStep = String(slider.step);

const resetRaceCommitStart = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).length;
nativeHandlers.pointerdown.forEach(function (handler) {
  handler({target: slider});
});
slider.value = "0.02";
nativeHandlers.input.forEach(function (handler) {
  handler({
    target: slider,
    stopImmediatePropagation: function () {},
  });
});
custom.builder_spatial_canvas_scene(Object.assign({}, separate, {
  generation: 3,
  resetToken: 1,
  controls: Object.assign({}, separate.controls, {scale: 0.001}),
}));
nativeHandlers.pointerup.forEach(function (handler) { handler({}); });
const resetRaceCommits = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).slice(resetRaceCommitStart);
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});

custom.builder_spatial_canvas_clear({viewKey: "separate", generation: 3});
delegated.input.call(slider, {
  type: "input",
  currentTarget: slider,
  originalEvent: undefined,
  stopImmediatePropagation: function () {},
});
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "null-controls",
  generation: 4,
  resetToken: 1,
  controls: controls,
}));
frames.shift()();
const nullControlsViewport = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_viewports";
}).slice(-1)[0].value;
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});

console.log(JSON.stringify({
  overlay: overlayViewport,
  separate: separateViewport,
  separateFirstPoint: separateFirstPoint,
  nullControlsViewKey: nullControlsViewport.viewKey,
  numberValue: numberValueAfterIon,
  tinyNumberValue: tinyNumberValueAfterEntry,
  tinySliderValue: tinySliderValueAfterEntry,
  tinyStep: tinyStepAfterEntry,
  authoritativeStep: authoritativeStep,
  tinyScaleCommits: tinyScaleCommits,
  commits: commits,
  resetRaceCommits: resetRaceCommits,
  stopped: stopped,
  pendingTimers: timers.size,
}));
