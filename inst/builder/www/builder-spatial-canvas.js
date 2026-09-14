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
    images: {},
    dragging: false,
    activeControlId: null,
    activeTransform: "points",
    releaseGuardId: null,
    controlsHeld: false,
    pendingEventAt: null,
    coordinateSequence: 0,
    colorGroups: {},
    screenPoints: [],
    roiPanels: [],
    hoverFrame: 0,
    hoverPoint: null,
    hoverNode: null,
    resizeObserver: null,
    observedCanvas: null,
    viewportSignature: null,
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
  function observeCanvas(node) {
    if (!window.ResizeObserver || state.observedCanvas === node) return;
    if (state.resizeObserver) state.resizeObserver.disconnect();
    state.resizeObserver = new ResizeObserver(schedule);
    state.resizeObserver.observe(node);
    state.observedCanvas = node;
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
    if (viewChanged) {
      state.controlsHeld = false;
      state.activeTransform = "points";
    }
    loadImage(message.image && message.image.uri);
    loadImages(message.roiImages || {});
    schedule();
  }
  function clear() {
    var node = canvas();
    var tip = node && document.getElementById(node.id + "-tooltip");
    if (tip) tip.hidden = true;
    state.scene = null;
    state.image = null;
    state.imageUri = null;
    state.images = {};
    state.colorGroups = {};
    state.screenPoints = [];
    state.roiPanels = [];
    state.hoverPoint = null;
    state.hoverNode = null;
    state.viewportSignature = null;
    if (state.hoverFrame) window.cancelAnimationFrame(state.hoverFrame);
    state.hoverFrame = 0;
    state.viewKey = null;
    state.controls = null;
    state.controlsHeld = false;
    state.activeTransform = "points";
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
  function loadImages(groups) {
    Object.keys(groups).forEach(function (roi) {
      (groups[roi] || []).forEach(function (image) {
        var uri = image && image.uri;
        if (!uri || Object.prototype.hasOwnProperty.call(state.images, uri)) {
          return;
        }
        state.images[uri] = null;
        var next = new Image();
        next.onload = function () { state.images[uri] = next; schedule(); };
        next.onerror = function () { state.images[uri] = null; schedule(); };
        next.src = uri;
      });
    });
  }
  function rotated(point, bounds, degrees) {
    var cx = (bounds.xmin + bounds.xmax) / 2;
    var cy = (bounds.ymin + bounds.ymax) / 2;
    var angle = finite(degrees, 0) * Math.PI / 180;
    var x = point.x - cx, y = point.y - cy;
    return {x: cx + x * Math.cos(angle) - y * Math.sin(angle),
      y: cy + x * Math.sin(angle) + y * Math.cos(angle)};
  }
  function viewport(bounds, degrees) {
    var cx = (bounds.xmin + bounds.xmax) / 2;
    var cy = (bounds.ymin + bounds.ymax) / 2;
    var frame = corners(bounds, degrees);
    var width = Math.max.apply(null, frame.map(function (p) { return p.x; })) -
      Math.min.apply(null, frame.map(function (p) { return p.x; }));
    var height = Math.max.apply(null, frame.map(function (p) { return p.y; })) -
      Math.min.apply(null, frame.map(function (p) { return p.y; }));
    width = Math.max(width, 1);
    height = Math.max(height, 1);
    return {xmin: cx - width / 2, xmax: cx + width / 2,
      ymin: cy - height / 2, ymax: cy + height / 2};
  }
  function viewportLayout(bounds, degrees, width, height, pad) {
    var view = viewport(bounds, degrees);
    var viewWidth = view.xmax - view.xmin;
    var viewHeight = view.ymax - view.ymin;
    var plotWidth = Math.max(width - pad * 2, 1);
    var plotHeight = Math.max(height - pad * 2, 1);
    var targetAspect = plotWidth / plotHeight;
    var centerX = (view.xmin + view.xmax) / 2;
    var centerY = (view.ymin + view.ymax) / 2;
    if (viewWidth / viewHeight < targetAspect) {
      viewWidth = viewHeight * targetAspect;
      view.xmin = centerX - viewWidth / 2;
      view.xmax = centerX + viewWidth / 2;
    } else {
      viewHeight = viewWidth / targetAspect;
      view.ymin = centerY - viewHeight / 2;
      view.ymax = centerY + viewHeight / 2;
    }
    var scale = plotWidth / viewWidth;
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
  function publishViewports(scene, viewports) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var payload = {
      viewKey: scene.viewKey,
      generation: scene.generation,
      viewports: viewports,
    };
    var signature = JSON.stringify(payload);
    if (signature === state.viewportSignature) return;
    state.viewportSignature = signature;
    Shiny.setInputValue("builder_spatial_viewports", payload, {priority: "event"});
  }
  function draw() {
    state.frame = 0;
    var node = canvas(), scene = state.scene;
    if (!node || !scene) return;
    observeCanvas(node);
    setupCanvasHover(node);
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
    state.screenPoints = [];
    if (!scene.available || !scene.bounds) return;
    if (scene.layout === "separate") {
      drawSeparate(ctx, scene, cssWidth, cssHeight);
      updateSummary(node, scene, " across separate ROI panels.");
      return;
    }
    var pad = 2;
    var angle = finite(state.controls.coordinateRotation, 0);
    var layout = viewportLayout(
      scene.bounds,
      0,
      cssWidth,
      cssHeight,
      pad
    );
    var scale = layout.scale, screen = layout.screen;
    window.__builderSpatialCanvasMetrics.latestViewport = {
      centerX: layout.offsetX + (layout.view.xmax - layout.view.xmin) * scale / 2,
      centerY: layout.offsetY + (layout.view.ymax - layout.view.ymin) * scale / 2,
      scale: scale,
    };
    var viewportKey = scene.activeRoi || "__section__";
    var viewports = {};
    viewports[viewportKey] = layout.view;
    publishViewports(scene, viewports);
    drawGrid(ctx, cssWidth, cssHeight, pad);
    var imageGeometry = drawImage(ctx, scene, screen);
    drawPoints(ctx, scene, screen);
    if (imageGeometry) {
      drawImageFrame(ctx, imageGeometry, state.activeTransform === "image");
    }
    drawFrame(ctx, scene.bounds, screen, 0, "#9a958d", [4, 4], 1);
    drawFrame(ctx, scene.bounds, screen, angle, "#5f5a54", [], 1.5);
    if (state.activeTransform === "points") {
      drawReference(ctx, scene.bounds, screen, angle, "Points");
    }
    updateSummary(node, scene, ".");
  }
  function updateSummary(node, scene, suffix) {
    var summary = document.getElementById(node.id + "-summary");
    if (summary) summary.textContent = "Spatial alignment preview with " +
      (scene.points.x || []).length + " sampled points" +
      (scene.capped ? " from a bounded sample" : "") + suffix;
  }
  function drawSeparate(ctx, scene, width, height) {
    var p = scene.points, groups = [];
    (p.group || []).forEach(function (group) {
      if (groups.indexOf(group) < 0) groups.push(group);
    });
    var columns = Math.ceil(Math.sqrt(groups.length));
    var rows = Math.ceil(groups.length / columns);
    var gap = 10, header = 24;
    var panelWidth = (width - gap * (columns + 1)) / columns;
    var panelHeight = (height - gap * (rows + 1)) / rows;
    var controls = state.controls || {};
    state.screenPoints = new Array(p.x.length);
    state.roiPanels = [];
    var viewports = {};
    groups.forEach(function (group, panelIndex) {
      var indices = [];
      for (var i = 0; i < p.group.length; i += 1) {
        if (p.group[i] === group) indices.push(i);
      }
      var column = panelIndex % columns, row = Math.floor(panelIndex / columns);
      var left = gap + column * (panelWidth + gap);
      var top = gap + row * (panelHeight + gap);
      var plotTop = top + header;
      var plotHeight = Math.max(panelHeight - header, 1);
      var active = group === scene.activeRoi;
      var roiControls = Object.assign({},
        (scene.roiPointAppearance || {})[group] || {},
        (scene.roiCoordinateTransforms || {})[group] || {});
      if (active) roiControls = Object.assign(roiControls, controls);
      var angle = finite(roiControls.coordinateRotation, 0);
      var xs = indices.map(function (index) { return p.x[index]; });
      var ys = indices.map(function (index) { return p.y[index]; });
      var bounds = {
        xmin: Math.min.apply(null, xs), xmax: Math.max.apply(null, xs),
        ymin: Math.min.apply(null, ys), ymax: Math.max.apply(null, ys),
      };
      if (bounds.xmin === bounds.xmax) { bounds.xmin -= .5; bounds.xmax += .5; }
      if (bounds.ymin === bounds.ymax) { bounds.ymin -= .5; bounds.ymax += .5; }
      var local = viewportLayout(bounds, 0, panelWidth, plotHeight, 6);
      viewports[group] = local.view;
      var screen = function (point) {
        var at = local.screen(point);
        return {x: left + at.x, y: plotTop + at.y};
      };
      ctx.fillStyle = "#fafbfa";
      ctx.fillRect(left, top, panelWidth, panelHeight);
      var roiImages = (scene.roiImages || {})[group] || [];
      var activeImageGeometry = null;
      roiImages.forEach(function (roiImage) {
        var imageControls = Object.assign({}, roiImage.controls || {});
        if (active && roiImage.active) {
          imageControls = Object.assign(imageControls, controls);
        }
        var geometry = drawImage(ctx, scene, screen, roiImage, imageControls);
        if (active && roiImage.active) activeImageGeometry = geometry;
      });
      ctx.globalAlpha = finite(roiControls.point_opacity, .85);
      var radius = Math.max(1, finite(roiControls.point_size, 5) / 2);
      var cx = (bounds.xmin + bounds.xmax) / 2;
      var cy = (bounds.ymin + bounds.ymax) / 2;
      var radians = angle * Math.PI / 180;
      var cosine = Math.cos(radians), sine = Math.sin(radians);
      indices.forEach(function (index) {
        var x = p.x[index] - cx, y = p.y[index] - cy;
        var at = screen({x: cx + x * cosine - y * sine,
          y: cy + x * sine + y * cosine});
        state.screenPoints[index] = at;
        ctx.beginPath();
        ctx.fillStyle = p.color[index] || "#777";
        ctx.arc(at.x, at.y, radius, 0, Math.PI * 2);
        ctx.fill();
      });
      ctx.globalAlpha = 1;
      if (active) {
        if (activeImageGeometry) {
          drawImageFrame(
            ctx,
            activeImageGeometry,
            state.activeTransform === "image"
          );
        }
        drawFrame(ctx, bounds, screen, 0, "#9a958d", [4, 4], 1);
        drawFrame(ctx, bounds, screen, angle, "#5f5a54", [], 1.5);
        if (state.activeTransform === "points") {
          drawReference(ctx, bounds, screen, angle, "Points");
        }
      }
      ctx.strokeStyle = active ? "#d45500" : "#9a958d";
      ctx.lineWidth = active ? 3 : 1;
      ctx.strokeRect(left, top, panelWidth, panelHeight);
      ctx.fillStyle = active ? "#d45500" : "#4b4742";
      ctx.font = "600 13px sans-serif";
      ctx.fillText(group, left + 8, top + 16);
      state.roiPanels.push({roi: group, left: left, top: top,
        right: left + panelWidth, bottom: top + panelHeight});
    });
    publishViewports(scene, viewports);
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
  function drawImage(ctx, scene, screen, image, controls) {
    image = image || scene.image;
    var loaded = image === scene.image ? state.image : state.images[image && image.uri];
    if (!loaded || !image || !image.baseBounds) return null;
    var b = image.baseBounds, c = controls || state.controls || {};
    var cx = (b.xmin + b.xmax) / 2, cy = (b.ymin + b.ymax) / 2;
    var center = screen({x: cx + finite(c.dx, 0),
      y: cy + finite(c.dy, 0)});
    var left = screen({x: b.xmin, y: cy});
    var right = screen({x: b.xmax, y: cy});
    var bottom = screen({x: cx, y: b.ymin});
    var top = screen({x: cx, y: b.ymax});
    var width = Math.hypot(right.x - left.x, right.y - left.y) *
      finite(c.scale, 1);
    var height = Math.hypot(top.x - bottom.x, top.y - bottom.y) *
      finite(c.scale, 1);
    ctx.save(); ctx.globalAlpha = finite(c.image_opacity, .8);
    ctx.translate(center.x, center.y);
    ctx.rotate(-finite(c.rotation, 0) * Math.PI / 180);
    ctx.scale(c.flip_x ? -1 : 1, c.flip_y ? -1 : 1);
    ctx.drawImage(loaded, -width / 2, -height / 2, width, height);
    ctx.restore();
    return {
      center: center,
      width: width,
      height: height,
      degrees: finite(c.rotation, 0),
    };
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
    state.screenPoints = new Array(p.x.length);
    Object.keys(state.colorGroups).forEach(function (color) {
      ctx.beginPath(); ctx.fillStyle = color;
      state.colorGroups[color].forEach(function (index) {
        var x = p.x[index] - cx, y = p.y[index] - cy;
        var at = screen({x: cx + x * cosine - y * sine,
          y: cy + x * sine + y * cosine});
        state.screenPoints[index] = at;
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
  function drawAngleReference(ctx, edge, degrees, label) {
    var centerX = (edge[0].x + edge[1].x) / 2;
    var centerY = (edge[0].y + edge[1].y) / 2 - 11;
    var text = label + " " + finite(degrees, 0).toFixed(1) + "°";
    ctx.save();
    ctx.strokeStyle = "#167c78";
    ctx.fillStyle = "#167c78";
    ctx.lineWidth = 3;
    ctx.beginPath();
    ctx.moveTo(edge[0].x, edge[0].y);
    ctx.lineTo(edge[1].x, edge[1].y);
    ctx.stroke();
    edge.forEach(function (point) {
      ctx.beginPath();
      ctx.arc(point.x, point.y, 4, 0, Math.PI * 2);
      ctx.fill();
    });
    ctx.font = "600 12px sans-serif";
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    var labelWidth = ctx.measureText(text).width + 12;
    ctx.fillStyle = "rgba(255,255,255,.96)";
    ctx.fillRect(centerX - labelWidth / 2, centerY - 9, labelWidth, 18);
    ctx.strokeStyle = "#c8ceca";
    ctx.lineWidth = 1;
    ctx.strokeRect(centerX - labelWidth / 2, centerY - 9, labelWidth, 18);
    ctx.fillStyle = "#3f4542";
    ctx.fillText(text, centerX, centerY);
    ctx.restore();
  }
  function drawReference(ctx, bounds, screen, degrees, label) {
    var edge = corners(bounds, degrees).slice(0, 2).map(screen);
    drawAngleReference(ctx, edge, degrees, label);
  }
  function drawImageFrame(ctx, geometry, active) {
    var angle = -geometry.degrees * Math.PI / 180;
    var cosine = Math.cos(angle), sine = Math.sin(angle);
    var point = function (x, y) {
      return {
        x: geometry.center.x + x * cosine - y * sine,
        y: geometry.center.y + x * sine + y * cosine,
      };
    };
    ctx.save();
    ctx.translate(geometry.center.x, geometry.center.y);
    ctx.rotate(angle);
    ctx.strokeStyle = active ? "#5f5a54" : "#9a958d";
    ctx.lineWidth = active ? 1.5 : 1;
    ctx.setLineDash(active ? [] : [4, 4]);
    ctx.strokeRect(
      -geometry.width / 2,
      -geometry.height / 2,
      geometry.width,
      geometry.height
    );
    ctx.restore();
    if (active) {
      drawAngleReference(ctx, [
        point(-geometry.width / 2, geometry.height / 2),
        point(geometry.width / 2, geometry.height / 2),
      ], geometry.degrees, "Image");
    }
  }
  function controlValue(target, factor) {
    if (target.type === "checkbox") return target.checked;
    return finite(target.value, 0) * factor;
  }
  function consumeControl(target) {
    var spec = controlMap[target.id];
    if (!spec || !state.controls) return false;
    if (state.controlsHeld) return false;
    if (spec[0] === "dx" || spec[0] === "dy") {
      target.value = Math.round(finite(target.value, 0));
    }
    state.activeTransform = spec[0] === "coordinateRotation" ||
      spec[0] === "point_opacity" || spec[0] === "point_size" ?
      "points" : "image";
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
      roi: scene.activeRoi || "",
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
  document.addEventListener("focusin", function (event) {
    var spec = controlMap[event.target.id];
    if (!spec) return;
    state.activeTransform = spec[0] === "coordinateRotation" ||
      spec[0] === "point_opacity" || spec[0] === "point_size" ?
      "points" : "image";
    schedule();
  }, true);
  document.addEventListener("change", function (event) {
    if (event.target.id === "enhance-active_image") {
      state.activeTransform = "image";
      schedule();
    }
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
  document.addEventListener("click", function (event) {
    var button = event.target.closest &&
      event.target.closest(".spatial-image-nudge button[data-target]");
    if (!button) return;
    var target = document.getElementById(button.dataset.target);
    if (!target) return;
    target.value = finite(target.value, 0) +
      finite(button.dataset.delta, 0) * finite(target.step, 1);
    target.dispatchEvent(new Event("input", {bubbles: true}));
    target.dispatchEvent(new Event("change", {bubbles: true}));
  }, true);
  document.addEventListener("pointerup", finishInteraction, true);
  document.addEventListener("pointercancel", finishInteraction, true);
  document.addEventListener("mouseup", finishInteraction, true);
  document.addEventListener("toggle", function (event) {
    if (
      event.target.matches &&
      event.target.matches(".builder-viewer-spatial-alignment[open]")
    ) {
      schedule();
    }
  }, true);
  function updateHover() {
    state.hoverFrame = 0;
    var node = canvas(), scene = state.scene, point = state.hoverPoint;
    if (
      !node ||
      node !== state.hoverNode ||
      !scene ||
      !scene.available ||
      !point
    ) return;
    var tip = document.getElementById(node.id + "-tooltip");
    if (!tip) return;
    var rect = node.getBoundingClientRect(), best = -1, bestDistance = 64;
    var pointerX = point.clientX - rect.left;
    var pointerY = point.clientY - rect.top;
    for (var i = 0; i < state.screenPoints.length; i += 1) {
      var at = state.screenPoints[i];
      if (!at) continue;
      var distance = Math.pow(at.x - pointerX, 2) +
        Math.pow(at.y - pointerY, 2);
      if (distance < bestDistance) { bestDistance = distance; best = i; }
    }
    if (best < 0) { tip.hidden = true; return; }
    tip.textContent = scene.points.barcode[best] + " · " + scene.points.group[best] +
      " · " + scene.points.count[best] + " sampled cells";
    tip.style.left = pointerX + "px";
    tip.style.top = pointerY + "px";
    tip.hidden = false;
  }
  function setupCanvasHover(node) {
    if (node.dataset.builderSpatialHover === "true") return;
    node.dataset.builderSpatialHover = "true";
    node.addEventListener("click", function (event) {
      if (!state.scene || state.scene.layout !== "separate") return;
      var rect = node.getBoundingClientRect();
      var x = event.clientX - rect.left, y = event.clientY - rect.top;
      var panel = state.roiPanels.find(function (candidate) {
        return x >= candidate.left && x <= candidate.right &&
          y >= candidate.top && y <= candidate.bottom;
      });
      if (panel && window.Shiny) {
        Shiny.setInputValue("builder_spatial_roi_select", {
          roi: panel.roi, nonce: Date.now(),
        }, {priority: "event"});
      }
    });
    node.addEventListener("pointermove", function (event) {
      state.hoverNode = node;
      state.hoverPoint = {clientX: event.clientX, clientY: event.clientY};
      if (!state.hoverFrame) {
        state.hoverFrame = window.requestAnimationFrame(updateHover);
      }
    });
    node.addEventListener("pointerleave", function () {
      if (state.hoverNode !== node) return;
      state.hoverNode = null;
      state.hoverPoint = null;
      if (state.hoverFrame) window.cancelAnimationFrame(state.hoverFrame);
      state.hoverFrame = 0;
      var tip = document.getElementById(node.id + "-tooltip");
      if (tip) tip.hidden = true;
    });
  }
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
