(function () {
  "use strict";

  var state = {
    scene: null,
    generation: -1,
    resetToken: -1,
    viewKey: null,
    controls: null,
    frame: 0,
    image: null,
    imageUri: null,
    dragging: false,
    activeControlId: null,
    releaseGuardId: null,
    controlsHeld: false,
    pendingEventAt: null,
    coordinateSequence: 0,
    colorGroups: {},
  };
  window.__builderSpatialCanvasMetrics = window.__builderSpatialCanvasMetrics || {
    sceneMessages: 0, renders: 0, latestCoordinateRotation: 0,
    eventToRenderMs: [], renderTimes: [], longTasks: 0,
  };
  var controlMap = {
    "enhance-coordinate_rotation": ["coordinateRotation", 1],
    "enhance-img_dx": ["dx", 1],
    "enhance-img_dy": ["dy", 1],
    "enhance-img_scale": ["scale", 1],
    "enhance-img_rotate": ["rotation", 1],
    "enhance-image_flip_x": ["flip_x", 1],
    "enhance-image_flip_y": ["flip_y", 1],
    "enhance-image_opacity": ["image_opacity", 0.01],
    "enhance-point_opacity": ["point_opacity", 0.01],
    "enhance-point_size": ["point_size", 1],
  };

  function canvas() {
    return document.getElementById("enhance-alignment_spatial_plot");
  }
  function finite(value, fallback) {
    value = Number(value);
    return Number.isFinite(value) ? value : fallback;
  }
  function schedule() {
    if (!state.frame) state.frame = window.requestAnimationFrame(draw);
  }
  function pushMetric(name, value) {
    var values = window.__builderSpatialCanvasMetrics[name];
    values.push(value);
    if (values.length > 600) values.splice(0, values.length - 600);
  }
  function groupPointColors(points) {
    var groups = {}, x = points.x || [], colors = points.color || [];
    for (var i = 0; i < x.length; i += 1) {
      var color = colors[i] || "#777";
      if (!groups[color]) groups[color] = [];
      groups[color].push(i);
    }
    return groups;
  }
  function setScene(message) {
    var generation = finite(message.generation, -1);
    if (generation < state.generation) return;
    var viewChanged = state.viewKey !== null && message.viewKey !== state.viewKey;
    state.scene = message;
    state.colorGroups = groupPointColors(message.points || {x: [], color: []});
    window.__builderSpatialCanvasMetrics.sceneMessages += 1;
    state.generation = generation;
    state.viewKey = message.viewKey;
    var resetToken = finite(message.resetToken, 0);
    if (viewChanged || resetToken > state.resetToken || !state.controls) {
      state.resetToken = resetToken;
      state.controls = Object.assign({}, message.controls || {});
    }
    if (viewChanged) state.controlsHeld = false;
    loadImage(message.image && message.image.uri);
    schedule();
  }
  function clear() {
    state.scene = null;
    state.image = null;
    state.imageUri = null;
    state.colorGroups = {};
    state.viewKey = null;
    state.controls = null;
    state.controlsHeld = false;
    var node = canvas();
    if (node) node.getContext("2d").clearRect(0, 0, node.width, node.height);
  }
  function loadImage(uri) {
    if (!uri || uri === state.imageUri) return;
    state.imageUri = uri;
    var next = new Image();
    next.onload = function () {
      if (state.imageUri === uri) state.image = next;
      schedule();
    };
    next.onerror = function () {
      if (state.imageUri === uri) state.image = null;
      schedule();
    };
    next.src = uri;
  }
  function rotated(point, bounds, degrees) {
    var cx = (bounds.xmin + bounds.xmax) / 2;
    var cy = (bounds.ymin + bounds.ymax) / 2;
    var angle = finite(degrees, 0) * Math.PI / 180;
    var x = point.x - cx, y = point.y - cy;
    return {x: cx + x * Math.cos(angle) - y * Math.sin(angle),
      y: cy + x * Math.sin(angle) + y * Math.cos(angle)};
  }
  function viewport(bounds) {
    var cx = (bounds.xmin + bounds.xmax) / 2;
    var cy = (bounds.ymin + bounds.ymax) / 2;
    var side = Math.hypot(bounds.xmax - bounds.xmin, bounds.ymax - bounds.ymin);
    side = Math.max(side, 1) * 1.12;
    return {xmin: cx - side / 2, xmax: cx + side / 2,
      ymin: cy - side / 2, ymax: cy + side / 2};
  }
  function viewportLayout(bounds, width, height, pad) {
    var view = viewport(bounds);
    var viewWidth = view.xmax - view.xmin;
    var viewHeight = view.ymax - view.ymin;
    var scale = Math.min(
      Math.max(width - pad * 2, 1) / viewWidth,
      Math.max(height - pad * 2, 1) / viewHeight
    );
    var plotWidth = viewWidth * scale;
    var plotHeight = viewHeight * scale;
    var offsetX = (width - plotWidth) / 2;
    var offsetY = (height - plotHeight) / 2;
    return {
      view: view,
      scale: scale,
      offsetX: offsetX,
      offsetY: offsetY,
      screen: function (point) {
        return {
          x: offsetX + (point.x - view.xmin) * scale,
          y: offsetY + plotHeight - (point.y - view.ymin) * scale,
        };
      },
    };
  }
  function draw() {
    state.frame = 0;
    var node = canvas(), scene = state.scene;
    if (!node || !scene) return;
    window.__builderSpatialCanvasMetrics.renders += 1;
    var now = performance.now();
    pushMetric("renderTimes", now);
    if (state.pendingEventAt !== null) {
      pushMetric("eventToRenderMs", now - state.pendingEventAt);
      state.pendingEventAt = null;
    }
    window.__builderSpatialCanvasMetrics.latestCoordinateRotation = finite(
      state.controls && state.controls.coordinateRotation, 0
    );
    var cssWidth = Math.max(node.clientWidth, 1);
    var cssHeight = Math.max(node.clientHeight, 1);
    var dpr = Math.min(window.devicePixelRatio || 1, 2);
    var width = Math.round(cssWidth * dpr), height = Math.round(cssHeight * dpr);
    if (node.width !== width || node.height !== height) {
      node.width = width; node.height = height;
    }
    var ctx = node.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, cssWidth, cssHeight);
    ctx.fillStyle = "#fafbfa"; ctx.fillRect(0, 0, cssWidth, cssHeight);
    if (!scene.available || !scene.bounds) return;
    var pad = 28;
    var layout = viewportLayout(scene.bounds, cssWidth, cssHeight, pad);
    var scale = layout.scale, screen = layout.screen;
    window.__builderSpatialCanvasMetrics.latestViewport = {
      centerX: layout.offsetX + (layout.view.xmax - layout.view.xmin) * scale / 2,
      centerY: layout.offsetY + (layout.view.ymax - layout.view.ymin) * scale / 2,
      scale: scale,
    };
    drawGrid(ctx, cssWidth, cssHeight, pad);
    drawImage(ctx, scene, screen, scale);
    drawPoints(ctx, scene, screen);
    drawFrame(ctx, scene.bounds, screen, 0, "#9a958d", [4, 4], 1);
    var angle = finite(state.controls.coordinateRotation, 0);
    drawFrame(ctx, scene.bounds, screen, angle, "#5f5a54", [], 1.5);
    drawReference(ctx, scene.bounds, screen, angle);
    var summary = document.getElementById(node.id + "-summary");
    if (summary) summary.textContent = "Spatial alignment preview with " +
      (scene.points.x || []).length + " sampled points" +
      (scene.capped ? " from a bounded sample." : ".");
  }
  function drawGrid(ctx, width, height, pad) {
    ctx.strokeStyle = "rgba(0,0,0,.08)"; ctx.lineWidth = 1;
    for (var i = 0; i <= 5; i += 1) {
      var x = pad + (width - pad * 2) * i / 5;
      var y = pad + (height - pad * 2) * i / 5;
      ctx.beginPath(); ctx.moveTo(x, pad); ctx.lineTo(x, height - pad); ctx.stroke();
      ctx.beginPath(); ctx.moveTo(pad, y); ctx.lineTo(width - pad, y); ctx.stroke();
    }
  }
  function drawImage(ctx, scene, screen, scale) {
    if (!state.image || !scene.image || !scene.image.baseBounds) return;
    var b = scene.image.baseBounds, c = state.controls;
    var center = screen({x: (b.xmin + b.xmax) / 2 + finite(c.dx, 0),
      y: (b.ymin + b.ymax) / 2 + finite(c.dy, 0)});
    var width = (b.xmax - b.xmin) * scale * finite(c.scale, 1);
    var height = (b.ymax - b.ymin) * scale * finite(c.scale, 1);
    ctx.save(); ctx.globalAlpha = finite(c.image_opacity, .8);
    ctx.translate(center.x, center.y);
    ctx.rotate(-finite(c.rotation, 0) * Math.PI / 180);
    ctx.scale(c.flip_x ? -1 : 1, c.flip_y ? -1 : 1);
    ctx.drawImage(state.image, -width / 2, -height / 2, width, height);
    ctx.restore();
  }
  function drawPoints(ctx, scene, screen) {
    var p = scene.points, c = state.controls;
    ctx.globalAlpha = finite(c.point_opacity, .85);
    var radius = Math.max(1, finite(c.point_size, 5) / 2);
    var bounds = scene.bounds;
    var cx = (bounds.xmin + bounds.xmax) / 2;
    var cy = (bounds.ymin + bounds.ymax) / 2;
    var angle = finite(c.coordinateRotation, 0) * Math.PI / 180;
    var cosine = Math.cos(angle), sine = Math.sin(angle);
    Object.keys(state.colorGroups).forEach(function (color) {
      ctx.beginPath(); ctx.fillStyle = color;
      state.colorGroups[color].forEach(function (index) {
        var x = p.x[index] - cx, y = p.y[index] - cy;
        var at = screen({x: cx + x * cosine - y * sine,
          y: cy + x * sine + y * cosine});
        ctx.moveTo(at.x + radius, at.y);
        ctx.arc(at.x, at.y, radius, 0, Math.PI * 2);
      });
      ctx.fill();
    });
    ctx.globalAlpha = 1;
  }
  function corners(bounds, degrees) {
    return [{x: bounds.xmin, y: bounds.ymin}, {x: bounds.xmax, y: bounds.ymin},
      {x: bounds.xmax, y: bounds.ymax}, {x: bounds.xmin, y: bounds.ymax}]
      .map(function (p) { return rotated(p, bounds, degrees); });
  }
  function path(ctx, points, screen) {
    points.forEach(function (p, i) { p = screen(p); if (i) ctx.lineTo(p.x, p.y);
      else ctx.moveTo(p.x, p.y); });
    var first = screen(points[0]); ctx.lineTo(first.x, first.y);
  }
  function drawFrame(ctx, bounds, screen, degrees, color, dash, width) {
    ctx.save(); ctx.strokeStyle = color; ctx.lineWidth = width; ctx.setLineDash(dash);
    ctx.beginPath(); path(ctx, corners(bounds, degrees), screen); ctx.stroke(); ctx.restore();
  }
  function drawReference(ctx, bounds, screen, degrees) {
    var edge = corners(bounds, degrees).slice(0, 2).map(screen);
    ctx.save(); ctx.strokeStyle = "#d45500"; ctx.fillStyle = "#d45500"; ctx.lineWidth = 3;
    ctx.beginPath(); ctx.moveTo(edge[0].x, edge[0].y); ctx.lineTo(edge[1].x, edge[1].y); ctx.stroke();
    edge.forEach(function (p) { ctx.beginPath(); ctx.arc(p.x, p.y, 3, 0, Math.PI * 2); ctx.fill(); });
    ctx.font = "12px sans-serif"; ctx.textAlign = "center";
    var label = (degrees > 0 ? "+" : "") + finite(degrees, 0).toFixed(1) + "°";
    ctx.fillText(label, (edge[0].x + edge[1].x) / 2, (edge[0].y + edge[1].y) / 2 - 8);
    ctx.restore();
  }
  function controlValue(target, factor) {
    if (target.type === "checkbox") return target.checked;
    return finite(target.value, 0) * factor;
  }
  function consumeControl(target) {
    var spec = controlMap[target.id];
    if (!spec || !state.controls) return false;
    if (state.controlsHeld) return false;
    state.controls[spec[0]] = controlValue(target, spec[1]);
    state.activeControlId = target.id;
    state.pendingEventAt = performance.now();
    schedule();
    if (spec[0] === "coordinateRotation") {
      window.__builderSpatialCanvasMetrics.latestCoordinateRotation = state.controls[spec[0]];
    }
    return true;
  }
  function sendCoordinateDraft(value) {
    var scene = state.scene;
    if (!scene || !scene.dataset || !scene.snapshotIdentity || !scene.section) return;
    state.coordinateSequence += 1;
    Shiny.setInputValue("builder_spatial_coordinate_draft", {
      dataset: scene.dataset,
      snapshotIdentity: scene.snapshotIdentity,
      section: scene.section,
      rotationDegrees: value,
      sequence: state.coordinateSequence,
      generation: scene.generation,
    }, {priority: "event"});
  }
  function finishInteraction() {
    if (!state.dragging) return;
    state.dragging = false;
    var id = state.activeControlId;
    state.activeControlId = null;
    if (!id || !window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var target = document.getElementById(id);
    if (!target) return;
    var value = target.type === "checkbox" ? target.checked : Number(target.value);
    if (target.type !== "checkbox" && !Number.isFinite(value)) return;
    state.releaseGuardId = id;
    window.setTimeout(function () { state.releaseGuardId = null; }, 0);
    if (id === "enhance-coordinate_rotation") {
      sendCoordinateDraft(value);
      return;
    }
    Shiny.setInputValue(id, value, {priority: "event"});
  }
  document.addEventListener("pointerdown", function (event) {
    if (controlMap[event.target.id] && event.target.type !== "checkbox") {
      state.dragging = true;
    }
  }, true);
  document.addEventListener("mousedown", function (event) {
    if (event.target.closest && event.target.closest(".irs")) state.dragging = true;
  }, true);
  document.addEventListener("input", function (event) {
    if (!consumeControl(event.target)) return;
    if (state.dragging || event.target.id === state.releaseGuardId) {
      event.stopImmediatePropagation();
    }
  }, true);
  document.addEventListener("change", function (event) {
    if (event.target.id === "enhance-active_section") {
      state.controlsHeld = true;
      return;
    }
    if (!consumeControl(event.target)) return;
    if (
      event.target.id === "enhance-coordinate_rotation" &&
      !state.dragging &&
      event.target.id !== state.releaseGuardId
    ) {
      sendCoordinateDraft(Number(event.target.value));
    }
    if (state.dragging || event.target.id === state.releaseGuardId) {
      event.stopImmediatePropagation();
    }
  }, true);
  document.addEventListener("pointerup", finishInteraction, true);
  document.addEventListener("pointercancel", finishInteraction, true);
  document.addEventListener("mouseup", finishInteraction, true);
  document.addEventListener("pointermove", function (event) {
    var node = canvas(), scene = state.scene;
    if (!node || !scene || !scene.available) return;
    var tip = document.getElementById(node.id + "-tooltip"); if (!tip) return;
    var rect = node.getBoundingClientRect(), best = -1, bestDistance = 64;
    var pad = 28;
    var layout = viewportLayout(scene.bounds, rect.width, rect.height, pad);
    var view = layout.view, scale = layout.scale;
    for (var i = 0; i < scene.points.x.length; i += 1) {
      var p = rotated({x: scene.points.x[i], y: scene.points.y[i]}, scene.bounds,
        state.controls.coordinateRotation);
      var x = layout.offsetX + (p.x - view.xmin) * scale;
      var y = layout.offsetY + (view.ymax - p.y) * scale;
      var distance = Math.pow(x - (event.clientX - rect.left), 2) +
        Math.pow(y - (event.clientY - rect.top), 2);
      if (distance < bestDistance) { bestDistance = distance; best = i; }
    }
    if (best < 0) { tip.hidden = true; return; }
    tip.textContent = scene.points.barcode[best] + " · " + scene.points.group[best] +
      " · " + scene.points.count[best] + " sampled cells";
    tip.style.left = (event.clientX - rect.left) + "px";
    tip.style.top = (event.clientY - rect.top) + "px"; tip.hidden = false;
  });
  document.addEventListener("pointerleave", function (event) {
    if (!event.target.classList || !event.target.classList.contains("builder-spatial-canvas")) return;
    var tip = document.getElementById(event.target.id + "-tooltip"); if (tip) tip.hidden = true;
  }, true);
  window.addEventListener("resize", schedule);
  document.addEventListener("shiny:connected", schedule);
  if (window.jQuery) {
    window.jQuery(document).on(
      "input.builderSpatialCanvas change.builderSpatialCanvas",
      Object.keys(controlMap).map(function (id) { return "#" + id; }).join(","),
      function (event) {
        if (!consumeControl(event.currentTarget)) return;
        if (state.dragging || event.currentTarget.id === state.releaseGuardId) {
          event.stopImmediatePropagation();
        }
      }
    );
  }
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("builder_spatial_canvas_scene", setScene);
    Shiny.addCustomMessageHandler("builder_spatial_canvas_clear", function (message) {
      clear();
    });
    Shiny.addCustomMessageHandler("builder_spatial_canvas_reset", function (message) {
      if (message.viewKey && message.viewKey !== state.viewKey) clear();
      state.resetToken = finite(message.resetToken, state.resetToken + 1);
      state.controls = Object.assign({}, message.controls || state.controls || {});
      schedule();
    });
  }
  if (window.PerformanceObserver) {
    try {
      new PerformanceObserver(function (list) {
        window.__builderSpatialCanvasMetrics.longTasks += list.getEntries().length;
      }).observe({entryTypes: ["longtask"]});
    } catch (error) { /* long-task observation is optional */ }
  }
}());
