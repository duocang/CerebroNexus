(function () {
  "use strict";

  var state = {
    scene: null,
    generation: -1,
    retiredGeneration: -1,
    resetToken: -1,
    viewKey: null,
    controls: null,
    frame: 0,
    image: null,
    imageKey: null,
    images: {},
    dragging: false,
    interacting: false,
    activeControlId: null,
    activeTransform: "points",
    releaseGuardId: null,
    controlsHeld: false,
    pendingEventAt: null,
    pendingControlDirty: false,
    pendingControlTimer: 0,
    syntheticFinishTimer: 0,
    pendingAuthoritativeControls: null,
    controlSequence: 0,
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
  var POINT_EDGE_PADDING = 18;
  var IMAGE_EDGE_PADDING = POINT_EDGE_PADDING / 2;
  var ROI_PANEL_GAP = 8;
  var LEGACY_VIEWPORT_PADDING = 2;
  var LEGACY_ROI_PANEL_GAP = 10;
  var LEGACY_ROI_HEADER = 24;
  var LEGACY_ROI_VIEWPORT_PADDING = 6;
  var NONPOSITIVE_SCALE_FALLBACK = 0.02;

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
    if (generation < state.generation || generation <= state.retiredGeneration) return;
    var viewChanged = state.viewKey !== null && message.viewKey !== state.viewKey;
    if (viewChanged) flushControlCommit();
    state.scene = message;
    state.colorGroups = groupPointColors(message.points || {x: [], color: []});
    window.__builderSpatialCanvasMetrics.sceneMessages += 1;
    state.generation = generation;
    state.viewKey = message.viewKey;
    var resetToken = finite(message.resetToken, 0);
    var controlsAdopted = false;
    if (viewChanged || resetToken > state.resetToken || !state.controls) {
      if (state.interacting && !viewChanged) {
        state.pendingAuthoritativeControls = {
          controls: Object.assign({}, message.controls || {}),
          resetToken: resetToken,
          viewKey: message.viewKey,
        };
      } else {
        state.resetToken = resetToken;
        state.controls = Object.assign({}, message.controls || {});
        state.pendingAuthoritativeControls = null;
        controlsAdopted = true;
      }
    }
    if (viewChanged) {
      state.controlsHeld = false;
      state.dragging = false;
      state.interacting = false;
      state.activeControlId = null;
      state.releaseGuardId = null;
      state.activeTransform = "points";
    }
    if (controlsAdopted) syncAuthoritativeScaleControl();
    loadImage(message.image);
    loadImages(message.roiImages || {});
    schedule();
  }
  function clear() {
    flushControlCommit();
    if (state.syntheticFinishTimer) {
      window.clearTimeout(state.syntheticFinishTimer);
      state.syntheticFinishTimer = 0;
    }
    var node = canvas();
    var tip = node && document.getElementById(node.id + "-tooltip");
    if (tip) tip.hidden = true;
    state.scene = null;
    state.image = null;
    state.imageKey = null;
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
    state.dragging = false;
    state.interacting = false;
    state.activeControlId = null;
    state.releaseGuardId = null;
    state.pendingAuthoritativeControls = null;
    state.pendingControlDirty = false;
    state.activeTransform = "points";
    if (node) node.getContext("2d").clearRect(0, 0, node.width, node.height);
  }
  function imageKey(image) {
    return image && (image.sourceKey || image.uri);
  }
  function loadImage(image) {
    var key = imageKey(image), uri = image && image.uri;
    if (!key) {
      state.image = null;
      state.imageKey = null;
      return;
    }
    if (state.imageKey !== key) state.image = null;
    state.imageKey = key;
    if (state.images[key]) {
      state.image = state.images[key];
      return;
    }
    if (!uri || Object.prototype.hasOwnProperty.call(state.images, key)) return;
    state.images[key] = null;
    var next = new Image();
    next.onload = function () {
      state.images[key] = next;
      if (state.imageKey === key) state.image = next;
      schedule();
    };
    next.onerror = function () {
      state.images[key] = null;
      if (state.imageKey === key) state.image = null;
      schedule();
    };
    next.src = uri;
  }
  function loadImages(groups) {
    Object.keys(groups).forEach(function (roi) {
      (groups[roi] || []).forEach(function (image) {
        var key = imageKey(image), uri = image && image.uri;
        if (!key || !uri || Object.prototype.hasOwnProperty.call(state.images, key)) {
          return;
        }
        state.images[key] = null;
        var next = new Image();
        next.onload = function () { state.images[key] = next; schedule(); };
        next.onerror = function () { state.images[key] = null; schedule(); };
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
  function viewportLayout(bounds, degrees, width, height, pad, imagePad) {
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
    imagePad = Math.max(0, Math.min(finite(imagePad, pad), pad));
    var imageExpansion = (pad - imagePad) / scale;
    return {
      view: view,
      imageFitView: {
        xmin: view.xmin - imageExpansion,
        xmax: view.xmax + imageExpansion,
        ymin: view.ymin - imageExpansion,
        ymax: view.ymax + imageExpansion,
      },
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
  function publishViewports(scene, viewports, imageFitViewports) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var payload = {
      viewKey: scene.viewKey,
      generation: scene.generation,
      viewports: viewports,
      imageFitViewports: imageFitViewports,
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
    var pad = POINT_EDGE_PADDING;
    var angle = finite(state.controls && state.controls.coordinateRotation, 0);
    var layout = viewportLayout(
      scene.bounds,
      angle,
      cssWidth,
      cssHeight,
      pad,
      IMAGE_EDGE_PADDING
    );
    var persistedLayout = viewportLayout(
      scene.bounds,
      angle,
      cssWidth,
      cssHeight,
      LEGACY_VIEWPORT_PADDING,
      LEGACY_VIEWPORT_PADDING
    );
    var scale = layout.scale, screen = layout.screen;
    window.__builderSpatialCanvasMetrics.latestViewport = {
      centerX: layout.offsetX + (layout.view.xmax - layout.view.xmin) * scale / 2,
      centerY: layout.offsetY + (layout.view.ymax - layout.view.ymin) * scale / 2,
      scale: scale,
    };
    var viewportKey = scene.activeRoi || "__section__";
    var viewports = {};
    var imageFitViewports = {};
    viewports[viewportKey] = persistedLayout.view;
    imageFitViewports[viewportKey] = layout.imageFitView;
    publishViewports(scene, viewports, imageFitViewports);
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
    if (!groups.length) {
      state.screenPoints = [];
      state.roiPanels = [];
      publishViewports(scene, {}, {});
      return;
    }
    var columns = Math.ceil(Math.sqrt(groups.length));
    var rows = Math.ceil(groups.length / columns);
    var gap = ROI_PANEL_GAP;
    var panelWidth = (width - gap * (columns + 1)) / columns;
    var panelHeight = (height - gap * (rows + 1)) / rows;
    var legacyPanelWidth = (
      width - LEGACY_ROI_PANEL_GAP * (columns + 1)
    ) / columns;
    var legacyPanelHeight = (
      height - LEGACY_ROI_PANEL_GAP * (rows + 1)
    ) / rows;
    var legacyPlotHeight = Math.max(legacyPanelHeight - LEGACY_ROI_HEADER, 1);
    var controls = state.controls || {};
    state.screenPoints = new Array(p.x.length);
    state.roiPanels = [];
    var viewports = {};
    var imageFitViewports = {};
    groups.forEach(function (group, panelIndex) {
      var indices = [];
      for (var i = 0; i < p.group.length; i += 1) {
        if (p.group[i] === group) indices.push(i);
      }
      var column = panelIndex % columns, row = Math.floor(panelIndex / columns);
      var left = gap + column * (panelWidth + gap);
      var top = gap + row * (panelHeight + gap);
      var active = group === scene.activeRoi;
      var roiControls = Object.assign({},
        (scene.roiPointAppearance || {})[group] || {},
        (scene.roiCoordinateTransforms || {})[group] || {});
      if (active) roiControls = Object.assign(roiControls, controls);
      var angle = finite(roiControls.coordinateRotation, 0);
      var xs = indices.map(function (index) { return p.x[index]; });
      var ys = indices.map(function (index) { return p.y[index]; });
      var bounds = (scene.roiBounds || {})[group] || {
        xmin: Math.min.apply(null, xs), xmax: Math.max.apply(null, xs),
        ymin: Math.min.apply(null, ys), ymax: Math.max.apply(null, ys),
      };
      if (bounds.xmin === bounds.xmax) { bounds.xmin -= .5; bounds.xmax += .5; }
      if (bounds.ymin === bounds.ymax) { bounds.ymin -= .5; bounds.ymax += .5; }
      var local = viewportLayout(
        bounds,
        angle,
        panelWidth,
        panelHeight,
        POINT_EDGE_PADDING,
        IMAGE_EDGE_PADDING
      );
      var persisted = viewportLayout(
        bounds,
        angle,
        legacyPanelWidth,
        legacyPlotHeight,
        LEGACY_ROI_VIEWPORT_PADDING,
        LEGACY_ROI_VIEWPORT_PADDING
      );
      viewports[group] = persisted.view;
      imageFitViewports[group] = local.imageFitView;
      var screen = function (point) {
        var at = local.screen(point);
        return {x: left + at.x, y: top + at.y};
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
      state.roiPanels.push({roi: group, left: left, top: top,
        right: left + panelWidth, bottom: top + panelHeight});
    });
    publishViewports(scene, viewports, imageFitViewports);
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
    var loaded = image === scene.image ? state.image : state.images[imageKey(image)];
    if (!loaded || !image || !image.baseBounds) return null;
    var b = image.baseBounds, c = controls || state.controls || {};
    var cx = (b.xmin + b.xmax) / 2, cy = (b.ymin + b.ymax) / 2;
    var center = screen({x: cx + finite(c.dx, 0),
      y: cy + finite(c.dy, 0)});
    var left = screen({x: b.xmin, y: cy});
    var right = screen({x: b.xmax, y: cy});
    var bottom = screen({x: cx, y: b.ymin});
    var top = screen({x: cx, y: b.ymax});
    var imageScale = finite(c.scale, 1);
    if (imageScale <= 0) imageScale = NONPOSITIVE_SCALE_FALLBACK;
    var width = Math.hypot(right.x - left.x, right.y - left.y) * imageScale;
    var height = Math.hypot(top.x - bottom.x, top.y - bottom.y) * imageScale;
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
    var p = scene.points, c = state.controls || {};
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
  function pairedSlider(target) {
    if (!target || !target.id) return null;
    if (controlMap[target.id]) return target;
    if (!/_number$/.test(target.id)) return null;
    var slider = document.getElementById(target.id.replace(/_number$/, ""));
    return slider && controlMap[slider.id] ? slider : null;
  }
  function scaleControlStep(value) {
    var defaultStep = NONPOSITIVE_SCALE_FALLBACK;
    if (!Number.isFinite(value) || value <= 0) return defaultStep;
    var ratio = value / defaultStep;
    if (
      Math.abs(ratio - Math.round(ratio)) <=
        1e-12 * Math.max(1, Math.abs(ratio))
    ) return defaultStep;
    var parts = String(value).toLowerCase().split("e");
    var fraction = (parts[0].split(".")[1] || "").length;
    var exponent = parts.length > 1 ? Number(parts[1]) : 0;
    var digits = Math.max(0, fraction - (Number.isFinite(exponent) ? exponent : 0));
    return Math.min(defaultStep, Math.pow(10, -digits));
  }
  function syncSliderControl(number, slider) {
    var value = finite(number.value, finite(slider.value, 0));
    var minimum = number.min === "" ? NaN : Number(number.min);
    var maximum = number.max === "" ? NaN : Number(number.max);
    if (Number.isFinite(minimum)) value = Math.max(minimum, value);
    if (Number.isFinite(maximum)) value = Math.min(maximum, value);
    var isScale = controlMap[slider.id][0] === "scale";
    if (isScale && value <= 0) {
      value = NONPOSITIVE_SCALE_FALLBACK;
    }
    number.value = value;
    slider.value = value;
    var range = window.jQuery && window.jQuery(slider).data("ionRangeSlider");
    if (isScale) {
      var step = scaleControlStep(value);
      number.step = step;
      slider.step = step;
      if (range) range.update({from: value, step: step});
    } else if (range) {
      range.update({from: value});
    }
  }
  function syncNumberControl(target) {
    var number = document.getElementById(target.id + "_number");
    if (!number || document.activeElement === number) return;
    if (number.value !== target.value) number.value = target.value;
  }
  function resetControl(id, value) {
    var input = document.getElementById(id);
    if (!input) return;
    if (input.type === "checkbox") {
      input.checked = value;
      return;
    }
    input.value = value;
    var number = document.getElementById(id + "_number");
    if (number) number.value = value;
    var slider = window.jQuery && window.jQuery(input).data("ionRangeSlider");
    var update = {from: value};
    if (controlMap[id] && controlMap[id][0] === "scale") {
      var step = scaleControlStep(finite(value, NONPOSITIVE_SCALE_FALLBACK));
      input.step = step;
      if (typeof input.setAttribute === "function") {
        input.setAttribute("data-step", step);
      }
      if (number) number.step = step;
      update.step = step;
    }
    if (slider) slider.update(update);
  }
  function syncAuthoritativeScaleControl() {
    if (!state.controls || state.controls.scale === undefined) return;
    var heldViewKey = state.viewKey;
    state.controlsHeld = true;
    resetControl("enhance-img_scale", state.controls.scale);
    window.setTimeout(function () {
      if (state.viewKey === heldViewKey && !state.interacting) {
        state.controlsHeld = false;
      }
    }, 0);
  }
  function controlPayload() {
    var scene = state.scene;
    if (!scene || !scene.dataset || !scene.snapshotIdentity || !scene.section) return null;
    state.controlSequence += 1;
    return {
      dataset: scene.dataset,
      snapshotIdentity: scene.snapshotIdentity,
      section: scene.section,
      roi: scene.activeRoi || "",
      image: scene.activeImage || "",
      viewKey: scene.viewKey,
      generation: scene.generation,
      sequence: state.controlSequence,
      controls: Object.assign({}, state.controls || {}),
    };
  }
  function flushControlCommit() {
    if (state.pendingControlTimer) {
      window.clearTimeout(state.pendingControlTimer);
      state.pendingControlTimer = 0;
    }
    if (!state.pendingControlDirty || !window.Shiny || typeof Shiny.setInputValue !== "function") {
      return;
    }
    var payload = controlPayload();
    state.pendingControlDirty = false;
    if (payload) {
      Shiny.setInputValue("builder_spatial_alignment_controls", payload,
        {priority: "event"});
    }
  }
  function queueControlCommit() {
    if (state.pendingControlTimer) window.clearTimeout(state.pendingControlTimer);
    state.pendingControlTimer = window.setTimeout(flushControlCommit, 50);
  }
  function sendInteractionState(active) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var scene = state.scene;
    if (!scene || !scene.dataset || !scene.section) return;
    Shiny.setInputValue("builder_spatial_interaction_state", {
      active: active,
      dataset: scene.dataset,
      snapshotIdentity: scene.snapshotIdentity,
      section: scene.section,
      roi: scene.activeRoi || "",
      image: scene.activeImage || "",
      viewKey: scene.viewKey,
      generation: scene.generation,
      nonce: Date.now(),
    }, {priority: "event"});
  }
  function beginInteraction() {
    if (state.interacting) return;
    state.interacting = true;
    sendInteractionState(true);
  }
  function applyPendingAuthoritativeControls() {
    var pending = state.pendingAuthoritativeControls;
    if (!pending || pending.viewKey !== state.viewKey) return;
    state.resetToken = pending.resetToken;
    state.controls = Object.assign({}, pending.controls);
    state.pendingAuthoritativeControls = null;
    state.controlsHeld = true;
    Object.keys(controlMap).forEach(function (id) {
      var spec = controlMap[id], value = state.controls[spec[0]];
      if (value === undefined) return;
      if (spec[0] === "image_opacity" || spec[0] === "point_opacity") value *= 100;
      resetControl(id, value);
    });
    window.setTimeout(function () { state.controlsHeld = false; }, 0);
  }
  function consumeControl(target) {
    var spec = controlMap[target.id];
    if (!spec || !state.controls) return false;
    if (state.controlsHeld) return false;
    if (spec[0] === "dx" || spec[0] === "dy") {
      target.value = Math.round(finite(target.value, 0));
    }
    if (spec[0] === "scale" && finite(target.value, 0) <= 0) {
      target.value = NONPOSITIVE_SCALE_FALLBACK;
      resetControl(target.id, NONPOSITIVE_SCALE_FALLBACK);
    }
    var value = controlValue(target, spec[1]);
    state.activeTransform = spec[0] === "coordinateRotation" ||
      spec[0] === "point_opacity" || spec[0] === "point_size" ?
      "points" : "image";
    syncNumberControl(target);
    state.activeControlId = target.id;
    if (Object.is(state.controls[spec[0]], value)) return true;
    state.controls[spec[0]] = value;
    state.pendingEventAt = performance.now();
    state.pendingControlDirty = true;
    queueControlCommit();
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
    if (!state.dragging && !state.interacting) return;
    state.dragging = false;
    state.interacting = false;
    if (state.syntheticFinishTimer) {
      window.clearTimeout(state.syntheticFinishTimer);
      state.syntheticFinishTimer = 0;
    }
    var id = state.activeControlId;
    state.activeControlId = null;
    flushControlCommit();
    sendInteractionState(false);
    applyPendingAuthoritativeControls();
    if (!id || !window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var target = document.getElementById(id);
    if (!target) return;
    var value = target.type === "checkbox" ? target.checked : Number(target.value);
    if (target.type !== "checkbox" && !Number.isFinite(value)) return;
    state.releaseGuardId = id;
    window.setTimeout(function () { state.releaseGuardId = null; }, 0);
    if (id === "enhance-coordinate_rotation") {
      sendCoordinateDraft(value);
    }
  }
  document.addEventListener("pointerdown", function (event) {
    if (pairedSlider(event.target) && event.target.type !== "checkbox") {
      state.dragging = true;
      beginInteraction();
    }
  }, true);
  document.addEventListener("mousedown", function (event) {
    if (event.target.closest && event.target.closest(".irs")) {
      state.dragging = true;
      beginInteraction();
    }
  }, true);
  document.addEventListener("input", function (event) {
    var target = pairedSlider(event.target);
    if (!target) return;
    if (target.id === state.releaseGuardId) {
      event.stopImmediatePropagation();
      return;
    }
    if (state.controlsHeld) {
      event.stopImmediatePropagation();
      return;
    }
    if (target !== event.target) syncSliderControl(event.target, target);
    beginInteraction();
    if (!consumeControl(target)) return;
    event.stopImmediatePropagation();
  }, true);
  document.addEventListener("focusin", function (event) {
    var target = pairedSlider(event.target);
    var spec = target && controlMap[target.id];
    if (!spec) return;
    beginInteraction();
    state.activeTransform = spec[0] === "coordinateRotation" ||
      spec[0] === "point_opacity" || spec[0] === "point_size" ?
      "points" : "image";
    schedule();
  }, true);
  document.addEventListener("change", function (event) {
    if (
      event.target.id === "enhance-active_image" ||
      event.target.id === "enhance-active_section" ||
      event.target.id === "enhance-active_roi" ||
      event.target.id === "enhance-active_sample"
    ) {
      flushControlCommit();
      finishInteraction();
      state.controlsHeld = true;
      var heldViewKey = state.viewKey;
      window.setTimeout(function () {
        if (state.viewKey === heldViewKey) state.controlsHeld = false;
      }, 1000);
    }
    if (event.target.id === "enhance-active_image") {
      state.activeTransform = "image";
      schedule();
    }
    var target = pairedSlider(event.target);
    if (!target) return;
    if (target.id === state.releaseGuardId) {
      event.stopImmediatePropagation();
      return;
    }
    if (state.controlsHeld) {
      event.stopImmediatePropagation();
      return;
    }
    if (target !== event.target) syncSliderControl(event.target, target);
    beginInteraction();
    if (!consumeControl(target)) return;
    event.stopImmediatePropagation();
    if (!state.dragging) finishInteraction();
  }, true);
  document.addEventListener("click", function (event) {
    if (
      !event.target.closest ||
      !event.target.closest(".spatial-coordinate-control, .spatial-image-nudge")
    ) {
      if (state.interacting || state.dragging) finishInteraction();
      else flushControlCommit();
    }
    if (event.target.closest && event.target.closest("#enhance-reset_align")) {
      flushControlCommit();
      finishInteraction();
      return;
    }
    var button = event.target.closest &&
      event.target.closest(".spatial-image-nudge button[data-target]");
    if (!button) return;
    beginInteraction();
    var target = document.getElementById(button.dataset.target);
    if (!target) return;
    target.value = finite(target.value, 0) +
      finite(button.dataset.delta, 0) * finite(target.step, 1);
    target.dispatchEvent(new Event("input", {bubbles: true}));
    target.dispatchEvent(new Event("change", {bubbles: true}));
    finishInteraction();
  }, true);
  document.addEventListener("pointerup", finishInteraction, true);
  document.addEventListener("pointercancel", finishInteraction, true);
  document.addEventListener("touchcancel", finishInteraction, true);
  document.addEventListener("mouseup", finishInteraction, true);
  document.addEventListener("focusout", function (event) {
    if (!pairedSlider(event.target)) return;
    window.setTimeout(function () {
      if (!pairedSlider(document.activeElement)) finishInteraction();
    }, 0);
  }, true);
  window.addEventListener("blur", finishInteraction);
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
          roi: panel.roi,
          dataset: state.scene.dataset,
          snapshotIdentity: state.scene.snapshotIdentity,
          section: state.scene.section,
          viewKey: state.scene.viewKey,
          generation: state.scene.generation,
          nonce: Date.now(),
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
      "change.builderSpatialCanvas input.builderSpatialCanvas",
      Object.keys(controlMap).map(function (id) { return "#" + id; }).join(","),
      function (event) {
        if (event.originalEvent) return;
        var target = event.currentTarget;
        if (window.jQuery(target).data("immediate") || state.controlsHeld) return;
        beginInteraction();
        if (!consumeControl(target)) return;
        event.stopImmediatePropagation();
        if (state.dragging) return;
        if (state.syntheticFinishTimer) {
          window.clearTimeout(state.syntheticFinishTimer);
          state.syntheticFinishTimer = 0;
        }
        if (event.type === "change") {
          state.syntheticFinishTimer = window.setTimeout(function () {
            state.syntheticFinishTimer = 0;
            finishInteraction();
          }, 0);
        } else {
          finishInteraction();
        }
      }
    );
  }
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("builder_spatial_canvas_scene", setScene);
    Shiny.addCustomMessageHandler("builder_spatial_canvas_clear", function (message) {
      var generation = finite(message && message.generation, -1);
      if (generation < state.generation) return;
      if (
        message && message.viewKey && state.viewKey &&
        message.viewKey !== state.viewKey && generation <= state.generation
      ) return;
      state.retiredGeneration = Math.max(state.retiredGeneration, generation);
      state.generation = Math.max(state.generation, generation);
      clear();
    });
    Shiny.addCustomMessageHandler("builder_spatial_canvas_reset", function (message) {
      if (message.viewKey && message.viewKey !== state.viewKey) return;
      var nextToken = finite(message.resetToken, state.resetToken + 1);
      if (nextToken <= state.resetToken) return;
      if (state.interacting) {
        state.pendingAuthoritativeControls = {
          controls: Object.assign({}, message.controls || state.controls || {}),
          resetToken: nextToken,
          viewKey: state.viewKey,
        };
        return;
      }
      state.resetToken = nextToken;
      state.controls = Object.assign({}, message.controls || state.controls || {});
      syncAuthoritativeScaleControl();
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
