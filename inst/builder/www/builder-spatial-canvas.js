(function () {
  "use strict";

  var latestScene = null;
  var currentInstance = null;
  var handlerRegistered = false;
  var documentHandlersRegistered = false;
  var jqueryHandlersRegistered = false;
  var observerStarted = false;

  function isObject(value) {
    return value !== null && typeof value === "object" && !Array.isArray(value);
  }

  function isFiniteNumber(value) {
    return typeof value === "number" && isFinite(value);
  }

  function clamp(value, minimum, maximum) {
    return Math.min(maximum, Math.max(minimum, value));
  }

  function safeDisplayText(value, fallback, maximumLength) {
    if (typeof value !== "string") return fallback;
    var text = value.replace(/[\u0000-\u0008\u000b\u000c\u000e-\u001f\u007f]/g, " ");
    text = text.replace(/\s+/g, " ").trim();
    if (!text) return fallback;
    return text.slice(0, maximumLength);
  }

  function safeIdentityValue(value) {
    if (value === null || typeof value === "undefined") return "null:";
    if (typeof value === "string" && value.length <= 512 && !/[\u0000-\u001f\u007f]/.test(value)) {
      return "string:" + value;
    }
    if (isFiniteNumber(value)) return "number:" + String(value);
    if (typeof value === "boolean") return "boolean:" + String(value);
    return null;
  }

  function parseIdentity(identity) {
    if (!isObject(identity)) return null;
    var names = [
      "dataset_id",
      "dataset_revision",
      "section_id",
      "section_kind",
      "image_label"
    ];
    var parsed = {};
    var parts = [];
    var index;
    var value;

    for (index = 0; index < names.length; index += 1) {
      if (!Object.prototype.hasOwnProperty.call(identity, names[index])) return null;
      value = safeIdentityValue(identity[names[index]]);
      if (value === null) return null;
      parsed[names[index]] = value;
      parts.push(value);
    }

    return {
      values: parsed,
      viewKey: JSON.stringify(parts)
    };
  }

  function parseResetToken(value) {
    if (typeof value === "string" && value.length <= 256 && !/[\u0000-\u001f\u007f]/.test(value)) {
      return "string:" + value;
    }
    if (isFiniteNumber(value) && value >= 0) return "number:" + String(value);
    return null;
  }

  function parseBounds(value) {
    if (!isObject(value)) return null;
    if (
      !isFiniteNumber(value.xmin) ||
      !isFiniteNumber(value.xmax) ||
      !isFiniteNumber(value.ymin) ||
      !isFiniteNumber(value.ymax) ||
      value.xmax <= value.xmin ||
      value.ymax <= value.ymin
    ) {
      return null;
    }

    var spanX = value.xmax - value.xmin;
    var spanY = value.ymax - value.ymin;
    var centerX = value.xmin / 2 + value.xmax / 2;
    var centerY = value.ymin / 2 + value.ymax / 2;
    if (
      !isFiniteNumber(spanX) ||
      !isFiniteNumber(spanY) ||
      spanX <= 0 ||
      spanY <= 0 ||
      !isFiniteNumber(centerX) ||
      !isFiniteNumber(centerY)
    ) {
      return null;
    }

    return {
      xmin: value.xmin,
      xmax: value.xmax,
      ymin: value.ymin,
      ymax: value.ymax,
      spanX: spanX,
      spanY: spanY,
      centerX: centerX,
      centerY: centerY
    };
  }

  function isSafeColor(value) {
    if (typeof value !== "string" || value.length === 0 || value.length > 64) return false;
    if (/[;{}\u0000-\u001f\u007f]/.test(value)) return false;
    if (/^#(?:[0-9a-f]{3}|[0-9a-f]{4}|[0-9a-f]{6}|[0-9a-f]{8})$/i.test(value)) return true;
    if (/^[a-z]+$/i.test(value)) return true;
    return /^(?:rgb|rgba|hsl|hsla)\([0-9.,%\s+\-/]+\)$/i.test(value);
  }

  function controlLimits(name) {
    switch (name) {
      case "coordinate_rotation":
      case "rotation":
        return { minimum: -3600, maximum: 3600 };
      case "dx":
      case "dy":
        return { minimum: -1000000000, maximum: 1000000000 };
      case "scale":
        return { minimum: 0.01, maximum: 1000 };
      case "image_opacity":
      case "point_opacity":
        return { minimum: 0, maximum: 1 };
      case "point_size":
        return { minimum: 0.5, maximum: 100 };
      default:
        return null;
    }
  }

  function parseSceneControls(value) {
    if (!isObject(value)) return null;
    var numericNames = [
      "coordinate_rotation",
      "dx",
      "dy",
      "scale",
      "rotation",
      "image_opacity",
      "point_opacity",
      "point_size"
    ];
    var controls = {};
    var index;
    var name;
    var limits;

    for (index = 0; index < numericNames.length; index += 1) {
      name = numericNames[index];
      if (!isFiniteNumber(value[name])) return null;
      if (name === "scale" && value[name] <= 0) return null;
      limits = controlLimits(name);
      controls[name] = clamp(value[name], limits.minimum, limits.maximum);
    }

    if (typeof value.flip_x !== "boolean" || typeof value.flip_y !== "boolean") return null;
    controls.flip_x = value.flip_x;
    controls.flip_y = value.flip_y;
    return controls;
  }

  function parseGroups(value) {
    if (!Array.isArray(value) || value.length > 10000) return null;
    var groups = [];
    var index;
    var group;
    var label;

    for (index = 0; index < value.length; index += 1) {
      group = value[index];
      if (!isObject(group)) return null;
      label = safeDisplayText(group.label, "", 512);
      if (!label || !isSafeColor(group.color)) return null;
      if (!isFiniteNumber(group.count) || group.count < 0 || Math.floor(group.count) !== group.count) {
        return null;
      }
      groups.push({
        label: label,
        color: group.color,
        count: group.count
      });
    }

    return groups;
  }

  function parsePoints(value, groups) {
    if (!isObject(value)) return null;
    if (
      !Array.isArray(value.x) ||
      !Array.isArray(value.y) ||
      !Array.isArray(value.barcode) ||
      !Array.isArray(value.group_index)
    ) {
      return null;
    }

    var length = value.x.length;
    if (
      length > 4000 ||
      value.y.length !== length ||
      value.barcode.length !== length ||
      value.group_index.length !== length
    ) {
      return null;
    }

    var points = {
      x: new Array(length),
      y: new Array(length),
      barcode: new Array(length),
      groupIndex: new Array(length)
    };
    var observedCounts = new Array(groups.length);
    var index;
    var groupIndex;

    for (index = 0; index < groups.length; index += 1) observedCounts[index] = 0;

    for (index = 0; index < length; index += 1) {
      if (!isFiniteNumber(value.x[index]) || !isFiniteNumber(value.y[index])) return null;
      if (typeof value.barcode[index] !== "string" || value.barcode[index].length > 4096) return null;
      groupIndex = value.group_index[index];
      if (
        !isFiniteNumber(groupIndex) ||
        Math.floor(groupIndex) !== groupIndex ||
        groupIndex < 0 ||
        groupIndex >= groups.length
      ) {
        return null;
      }

      points.x[index] = value.x[index];
      points.y[index] = value.y[index];
      points.barcode[index] = value.barcode[index];
      points.groupIndex[index] = groupIndex;
      observedCounts[groupIndex] += 1;
    }

    for (index = 0; index < groups.length; index += 1) {
      if (observedCounts[index] !== groups[index].count) return null;
    }

    return points;
  }

  function parseImage(value) {
    if (value === null) return null;
    if (!isObject(value)) return false;
    var key = safeIdentityValue(value.key);
    var bounds = parseBounds(value.base_bounds);
    if (
      key === null ||
      key === "null:" ||
      typeof value.source_uri !== "string" ||
      value.source_uri.length === 0 ||
      !bounds
    ) {
      return false;
    }

    return {
      key: key,
      sourceUri: value.source_uri,
      baseBounds: bounds
    };
  }

  function parseScene(value, minimumGeneration) {
    if (!isObject(value) || value.schema_version !== 1) return null;
    if (
      !isFiniteNumber(value.generation) ||
      Math.floor(value.generation) !== value.generation ||
      value.generation < 0 ||
      value.generation < minimumGeneration
    ) {
      return null;
    }
    if (typeof value.available !== "boolean") return null;

    var identity = parseIdentity(value.identity);
    var resetToken = parseResetToken(value.reset_token);
    if (!identity || resetToken === null) return null;

    var parsed = {
      generation: value.generation,
      resetToken: resetToken,
      identity: identity.values,
      viewKey: identity.viewKey,
      available: value.available,
      message: safeDisplayText(value.message, "Spatial preview is unavailable.", 400)
    };

    if (!value.available) return parsed;
    if (typeof value.capped !== "boolean") return null;

    var groups = parseGroups(value.groups);
    var coordinateFrame = parseBounds(value.coordinate_frame);
    if (!groups || !coordinateFrame) return null;

    var points = parsePoints(value.points, groups);
    var image = parseImage(value.image);
    var controls = parseSceneControls(value.controls);
    if (!points || image === false || !controls) return null;

    parsed.capped = value.capped;
    parsed.coordinateFrame = coordinateFrame;
    parsed.points = points;
    parsed.groups = groups;
    parsed.image = image;
    parsed.controls = controls;
    return parsed;
  }

  function cloneControls(value) {
    return {
      coordinate_rotation: value.coordinate_rotation,
      dx: value.dx,
      dy: value.dy,
      scale: value.scale,
      rotation: value.rotation,
      flip_x: value.flip_x,
      flip_y: value.flip_y,
      image_opacity: value.image_opacity,
      point_opacity: value.point_opacity,
      point_size: value.point_size
    };
  }

  function controlNameForId(id) {
    switch (id) {
      case "enhance-coordinate_rotation": return "coordinate_rotation";
      case "enhance-img_dx": return "dx";
      case "enhance-img_dy": return "dy";
      case "enhance-img_scale": return "scale";
      case "enhance-img_rotate": return "rotation";
      case "enhance-image_flip_x": return "flip_x";
      case "enhance-image_flip_y": return "flip_y";
      case "enhance-image_opacity": return "image_opacity";
      case "enhance-point_opacity": return "point_opacity";
      case "enhance-point_size": return "point_size";
      default: return null;
    }
  }

  function controlIdForName(name) {
    switch (name) {
      case "coordinate_rotation": return "enhance-coordinate_rotation";
      case "dx": return "enhance-img_dx";
      case "dy": return "enhance-img_dy";
      case "scale": return "enhance-img_scale";
      case "rotation": return "enhance-img_rotate";
      case "flip_x": return "enhance-image_flip_x";
      case "flip_y": return "enhance-image_flip_y";
      case "image_opacity": return "enhance-image_opacity";
      case "point_opacity": return "enhance-point_opacity";
      case "point_size": return "enhance-point_size";
      default: return null;
    }
  }

  function eventControlId(event, data) {
    if (event && event.target && controlNameForId(event.target.id)) return event.target.id;
    if (event && event.target && typeof event.target.closest === "function") {
      var container = event.target.closest(".shiny-input-container, .form-group");
      if (container) {
        var candidate = container.querySelector("input[id], select[id]");
        if (candidate && controlNameForId(candidate.id)) return candidate.id;
      }
    }
    if (event && typeof event.name === "string" && controlNameForId(event.name)) return event.name;
    if (event && event.detail && typeof event.detail.name === "string" && controlNameForId(event.detail.name)) {
      return event.detail.name;
    }
    if (data && typeof data.name === "string" && controlNameForId(data.name)) {
      return data.name;
    }
    if (
      event &&
      event.originalEvent &&
      event.originalEvent.target &&
      controlNameForId(event.originalEvent.target.id)
    ) {
      return event.originalEvent.target.id;
    }
    return null;
  }

  function readNumericControl(element, name, fallback) {
    var number = element && isFiniteNumber(element.valueAsNumber)
      ? element.valueAsNumber
      : Number(element ? element.value : NaN);
    if (!isFiniteNumber(number)) return fallback;

    var isPercentage = name === "image_opacity" || name === "point_opacity";
    if (isPercentage) number /= 100;

    var limits = controlLimits(name);
    var minimum = limits.minimum;
    var maximum = limits.maximum;
    var attributeMinimum = element && element.hasAttribute("min")
      ? Number(element.getAttribute("min"))
      : NaN;
    var attributeMaximum = element && element.hasAttribute("max")
      ? Number(element.getAttribute("max"))
      : NaN;

    if (isFiniteNumber(attributeMinimum)) {
      if (isPercentage) attributeMinimum /= 100;
      minimum = Math.max(minimum, attributeMinimum);
    }
    if (isFiniteNumber(attributeMaximum)) {
      if (isPercentage) attributeMaximum /= 100;
      maximum = Math.min(maximum, attributeMaximum);
    }
    if (minimum > maximum) return fallback;
    return clamp(number, minimum, maximum);
  }

  function readControlsFromDom(fallback) {
    var names = [
      "coordinate_rotation",
      "dx",
      "dy",
      "scale",
      "rotation",
      "flip_x",
      "flip_y",
      "image_opacity",
      "point_opacity",
      "point_size"
    ];
    var values = cloneControls(fallback);
    var index;
    var name;
    var element;

    for (index = 0; index < names.length; index += 1) {
      name = names[index];
      element = document.getElementById(controlIdForName(name));
      if (!element) continue;
      if (name === "flip_x" || name === "flip_y") {
        values[name] = Boolean(element.checked);
      } else {
        values[name] = readNumericControl(element, name, values[name]);
      }
    }

    return values;
  }

  function isCanvasConnected(canvas) {
    if (!canvas) return false;
    if (typeof canvas.isConnected === "boolean") return canvas.isConnected;
    return Boolean(document.documentElement && document.documentElement.contains(canvas));
  }

  function buildViewport(frame, image) {
    var halfWidth = frame.spanX / 2;
    var halfHeight = frame.spanY / 2;
    var radius = Math.hypot(halfWidth, halfHeight);
    if (!isFiniteNumber(radius) || radius <= 0) return null;

    var square = parseBounds({
      xmin: frame.centerX - radius,
      xmax: frame.centerX + radius,
      ymin: frame.centerY - radius,
      ymax: frame.centerY + radius
    });
    if (!square) return null;

    var xmin = square.xmin;
    var xmax = square.xmax;
    var ymin = square.ymin;
    var ymax = square.ymax;

    if (image) {
      xmin = Math.min(xmin, image.baseBounds.xmin);
      xmax = Math.max(xmax, image.baseBounds.xmax);
      ymin = Math.min(ymin, image.baseBounds.ymin);
      ymax = Math.max(ymax, image.baseBounds.ymax);
    }

    var union = parseBounds({ xmin: xmin, xmax: xmax, ymin: ymin, ymax: ymax });
    if (!union) return null;
    var paddingX = union.spanX * 0.06;
    var paddingY = union.spanY * 0.06;
    if (
      !isFiniteNumber(paddingX) ||
      !isFiniteNumber(paddingY) ||
      paddingX <= 0 ||
      paddingY <= 0
    ) {
      return null;
    }

    return parseBounds({
      xmin: union.xmin - paddingX,
      xmax: union.xmax + paddingX,
      ymin: union.ymin - paddingY,
      ymax: union.ymax + paddingY
    });
  }

  function plotGeometry(width, height, viewport) {
    var plot = {
      left: 48,
      right: 14,
      top: 14,
      bottom: 34
    };
    plot.width = Math.max(0, width - plot.left - plot.right);
    plot.height = Math.max(0, height - plot.top - plot.bottom);
    plot.xmax = plot.left + plot.width;
    plot.ymax = plot.top + plot.height;
    if (plot.width <= 0 || plot.height <= 0) return null;

    var worldWidth = viewport.spanX;
    var worldHeight = viewport.spanY;
    var scale = Math.min(plot.width / worldWidth, plot.height / worldHeight);
    if (!isFiniteNumber(scale) || scale <= 0) return null;

    return {
      plot: plot,
      centerX: viewport.centerX,
      centerY: viewport.centerY,
      scale: scale,
      visible: {
        xmin: viewport.centerX - plot.width / (2 * scale),
        xmax: viewport.centerX + plot.width / (2 * scale),
        ymin: viewport.centerY - plot.height / (2 * scale),
        ymax: viewport.centerY + plot.height / (2 * scale)
      }
    };
  }

  function worldToScreen(x, y, geometry) {
    return {
      x: geometry.plot.left + geometry.plot.width / 2 + (x - geometry.centerX) * geometry.scale,
      y: geometry.plot.top + geometry.plot.height / 2 - (y - geometry.centerY) * geometry.scale
    };
  }

  function niceStep(span, targetCount) {
    var rough = span / Math.max(2, targetCount);
    var power = Math.pow(10, Math.floor(Math.log(rough) / Math.LN10));
    var fraction = rough / power;
    var niceFraction;

    if (fraction <= 1) niceFraction = 1;
    else if (fraction <= 2) niceFraction = 2;
    else if (fraction <= 5) niceFraction = 5;
    else niceFraction = 10;
    return niceFraction * power;
  }

  function tickValues(minimum, maximum, step) {
    var values = [];
    var first = Math.ceil(minimum / step - 1e-10) * step;
    var value;
    var count = 0;

    for (value = first; value <= maximum + step * 1e-8 && count < 200; value += step) {
      values.push(Math.abs(value) < step * 1e-10 ? 0 : value);
      count += 1;
    }
    return values;
  }

  function formatTick(value, step) {
    var absolute = Math.abs(value);
    if (absolute >= 10000000 || (absolute > 0 && absolute < 0.0001)) return value.toExponential(1);
    var decimals = clamp(Math.max(0, -Math.floor(Math.log(step) / Math.LN10)), 0, 8);
    var rounded = Number(value.toFixed(decimals));
    return String(rounded === 0 ? 0 : rounded);
  }

  function drawAxes(context, geometry) {
    var plot = geometry.plot;
    var visible = geometry.visible;
    var xStep = niceStep(visible.xmax - visible.xmin, Math.max(2, plot.width / 80));
    var yStep = niceStep(visible.ymax - visible.ymin, Math.max(2, plot.height / 60));
    var xTicks = tickValues(visible.xmin, visible.xmax, xStep);
    var yTicks = tickValues(visible.ymin, visible.ymax, yStep);
    var index;
    var screen;

    context.fillStyle = "#fbfcfd";
    context.fillRect(plot.left, plot.top, plot.width, plot.height);
    context.save();
    context.beginPath();
    context.rect(plot.left, plot.top, plot.width, plot.height);
    context.clip();
    context.strokeStyle = "#dce3ea";
    context.lineWidth = 1;
    context.beginPath();
    for (index = 0; index < xTicks.length; index += 1) {
      screen = worldToScreen(xTicks[index], geometry.centerY, geometry);
      context.moveTo(Math.round(screen.x) + 0.5, plot.top);
      context.lineTo(Math.round(screen.x) + 0.5, plot.ymax);
    }
    for (index = 0; index < yTicks.length; index += 1) {
      screen = worldToScreen(geometry.centerX, yTicks[index], geometry);
      context.moveTo(plot.left, Math.round(screen.y) + 0.5);
      context.lineTo(plot.xmax, Math.round(screen.y) + 0.5);
    }
    context.stroke();
    context.restore();

    context.fillStyle = "#64748b";
    context.strokeStyle = "#94a3b8";
    context.font = "11px system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif";
    context.lineWidth = 1;
    context.textAlign = "center";
    context.textBaseline = "top";
    for (index = 0; index < xTicks.length; index += 1) {
      screen = worldToScreen(xTicks[index], geometry.centerY, geometry);
      context.beginPath();
      context.moveTo(screen.x, plot.ymax);
      context.lineTo(screen.x, plot.ymax + 4);
      context.stroke();
      context.fillText(formatTick(xTicks[index], xStep), screen.x, plot.ymax + 6);
    }

    context.textAlign = "right";
    context.textBaseline = "middle";
    for (index = 0; index < yTicks.length; index += 1) {
      screen = worldToScreen(geometry.centerX, yTicks[index], geometry);
      context.beginPath();
      context.moveTo(plot.left - 4, screen.y);
      context.lineTo(plot.left, screen.y);
      context.stroke();
      context.fillText(formatTick(yTicks[index], yStep), plot.left - 7, screen.y);
    }
  }

  function rotatePoint(x, y, pivotX, pivotY, cosine, sine) {
    var dx = x - pivotX;
    var dy = y - pivotY;
    return {
      x: pivotX + dx * cosine - dy * sine,
      y: pivotY + dx * sine + dy * cosine
    };
  }

  function frameCorners(frame) {
    return [
      { x: frame.xmin, y: frame.ymin },
      { x: frame.xmax, y: frame.ymin },
      { x: frame.xmax, y: frame.ymax },
      { x: frame.xmin, y: frame.ymax }
    ];
  }

  function rotateCorners(corners, pivotX, pivotY, cosine, sine) {
    var rotated = new Array(corners.length);
    var index;
    for (index = 0; index < corners.length; index += 1) {
      rotated[index] = rotatePoint(corners[index].x, corners[index].y, pivotX, pivotY, cosine, sine);
    }
    return rotated;
  }

  function screenCorners(corners, geometry) {
    var result = new Array(corners.length);
    var index;
    for (index = 0; index < corners.length; index += 1) {
      result[index] = worldToScreen(corners[index].x, corners[index].y, geometry);
    }
    return result;
  }

  function traceClosedPath(context, corners) {
    var index;
    context.beginPath();
    context.moveTo(corners[0].x, corners[0].y);
    for (index = 1; index < corners.length; index += 1) {
      context.lineTo(corners[index].x, corners[index].y);
    }
    context.closePath();
  }

  function drawImageLayer(context, rendererImage, scene, controls, geometry) {
    if (!rendererImage || !scene.image || controls.image_opacity <= 0) return;
    var bounds = scene.image.baseBounds;
    var center = worldToScreen(
      bounds.centerX + controls.dx,
      bounds.centerY + controls.dy,
      geometry
    );
    var width = bounds.spanX * geometry.scale;
    var height = bounds.spanY * geometry.scale;
    width *= controls.scale;
    height *= controls.scale;
    var radians = controls.rotation * Math.PI / 180;
    if (
      !isFiniteNumber(center.x) ||
      !isFiniteNumber(center.y) ||
      !isFiniteNumber(width) ||
      !isFiniteNumber(height) ||
      !isFiniteNumber(radians) ||
      width <= 0 ||
      height <= 0
    ) {
      return;
    }

    context.save();
    context.globalAlpha = controls.image_opacity;
    context.translate(center.x, center.y);
    context.rotate(-radians);
    context.scale(controls.flip_x ? -1 : 1, controls.flip_y ? -1 : 1);
    context.drawImage(rendererImage, -width / 2, -height / 2, width, height);
    context.restore();
  }

  function layoutPoints(scene, controls, geometry, pivotX, pivotY, cosine, sine) {
    var buckets = new Array(scene.groups.length);
    var screens = new Array(scene.points.x.length);
    var index;
    var rotated;
    var screen;
    var groupIndex;

    for (index = 0; index < buckets.length; index += 1) buckets[index] = [];
    for (index = 0; index < scene.points.x.length; index += 1) {
      rotated = rotatePoint(
        scene.points.x[index],
        scene.points.y[index],
        pivotX,
        pivotY,
        cosine,
        sine
      );
      screen = worldToScreen(rotated.x, rotated.y, geometry);
      groupIndex = scene.points.groupIndex[index];
      screens[index] = { x: screen.x, y: screen.y, pointIndex: index };
      buckets[groupIndex].push(screen);
    }

    return { buckets: buckets, screens: screens };
  }

  function drawPoints(context, scene, controls, layout) {
    if (controls.point_opacity <= 0 || controls.point_size <= 0) return;
    var radius = controls.point_size / 2;
    var groupIndex;
    var pointIndex;
    var bucket;
    var point;

    context.save();
    context.globalAlpha = controls.point_opacity;
    for (groupIndex = 0; groupIndex < layout.buckets.length; groupIndex += 1) {
      bucket = layout.buckets[groupIndex];
      if (bucket.length === 0) continue;
      context.fillStyle = scene.groups[groupIndex].color;
      context.beginPath();
      for (pointIndex = 0; pointIndex < bucket.length; pointIndex += 1) {
        point = bucket[pointIndex];
        context.moveTo(point.x + radius, point.y);
        context.arc(point.x, point.y, radius, 0, Math.PI * 2);
      }
      context.fill();
    }
    context.restore();
  }

  function drawFrames(context, original, transformed, rotation) {
    context.save();
    context.lineJoin = "round";
    context.lineCap = "round";

    context.strokeStyle = "#94a3b8";
    context.lineWidth = 1;
    context.setLineDash([3, 4]);
    traceClosedPath(context, original);
    context.stroke();

    context.strokeStyle = "#334155";
    context.lineWidth = 1.5;
    context.setLineDash([]);
    traceClosedPath(context, transformed);
    context.stroke();

    context.strokeStyle = "#f97316";
    context.fillStyle = "#f97316";
    context.lineWidth = 2;
    context.beginPath();
    context.moveTo(transformed[0].x, transformed[0].y);
    context.lineTo(transformed[1].x, transformed[1].y);
    context.stroke();
    context.beginPath();
    context.arc(transformed[0].x, transformed[0].y, 3, 0, Math.PI * 2);
    context.arc(transformed[1].x, transformed[1].y, 3, 0, Math.PI * 2);
    context.fill();

    var displayedRotation = Math.abs(rotation) < 0.05 ? 0 : rotation;
    var label = (displayedRotation >= 0 ? "+" : "") + displayedRotation.toFixed(1) + "\u00b0";
    var midpointX = (transformed[0].x + transformed[1].x) / 2;
    var midpointY = (transformed[0].y + transformed[1].y) / 2;
    context.font = "600 11px system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif";
    context.textAlign = "center";
    context.textBaseline = "bottom";
    context.fillText(label, midpointX, midpointY - 7);
    context.restore();
  }

  function createRenderer(canvas) {
    var frame = canvas.closest ? canvas.closest(".spatial-alignment-plot-frame") : canvas.parentNode;
    if (!frame) frame = canvas.parentNode;
    var status = frame ? frame.querySelector(".builder-spatial-canvas-status") : null;
    var tooltip = frame ? frame.querySelector(".builder-spatial-canvas-tooltip") : null;
    var summary = frame ? frame.querySelector(".builder-spatial-canvas-summary") : null;
    var context = canvas.getContext("2d");
    var disposed = false;
    var animationFrame = null;
    var resizeObserver = null;
    var usingWindowResize = false;
    var cssWidth = 0;
    var cssHeight = 0;
    var pixelRatio = 1;
    var scene = null;
    var controls = null;
    var viewport = null;
    var viewportViewKey = null;
    var currentGeneration = -1;
    var currentViewKey = null;
    var currentResetToken = null;
    var lastPayload = null;
    var pointer = null;
    var imageSerial = 0;
    var imageState = {
      viewKey: null,
      key: null,
      sourceUri: null,
      image: null,
      state: "none",
      token: null,
      loader: null
    };

    function setStatus(message) {
      if (status) status.textContent = context ? (message || "") : "Spatial preview is unavailable.";
    }

    function hideTooltip() {
      if (!tooltip) return;
      tooltip.hidden = true;
      tooltip.textContent = "";
      tooltip.style.left = "";
      tooltip.style.top = "";
    }

    function updateUnavailableAccessibility(message) {
      canvas.setAttribute("aria-label", "Spatial preview unavailable.");
      if (summary) summary.textContent = message;
    }

    function updateSceneAccessibility(nextScene) {
      var label = "Spatial preview with " + nextScene.groups.length + " groups and " +
        nextScene.points.x.length + " plotted points";
      if (nextScene.capped) label += ", capped to a sample";
      label += ".";
      canvas.setAttribute("aria-label", label);
      if (summary) {
        summary.textContent = label + (nextScene.capped ? " Not all available points are shown." : "");
      }
    }

    function invalidateImage() {
      imageSerial += 1;
      var loader = imageState.loader;
      if (loader) {
        loader.onload = null;
        loader.onerror = null;
        if (imageState.state === "loading") {
          try {
            loader.src = "data:,";
          } catch (error) {
            // Best-effort cancellation; the stale token still rejects completion.
          }
        }
      }
      imageState = {
        viewKey: null,
        key: null,
        sourceUri: null,
        image: null,
        state: "none",
        token: null,
        loader: null
      };
    }

    function imageTokenIsCurrent(token) {
      return !disposed &&
        isCanvasConnected(canvas) &&
        imageState.token === token &&
        token.serial === imageSerial &&
        scene !== null &&
        scene.generation === token.generation &&
        scene.viewKey === token.viewKey &&
        scene.image !== null &&
        scene.image.key === token.imageKey;
    }

    function syncImage(nextScene, forceReset) {
      var descriptor = nextScene.image;
      if (!descriptor) {
        if (imageState.state !== "none") invalidateImage();
        setStatus("");
        return;
      }

      var isSameImage = !forceReset &&
        imageState.viewKey === nextScene.viewKey &&
        imageState.key === descriptor.key &&
        imageState.sourceUri === descriptor.sourceUri;

      if (isSameImage) {
        if (imageState.token) imageState.token.generation = nextScene.generation;
        if (imageState.state === "loading") setStatus("Loading spatial image\u2026");
        else if (imageState.state === "error") setStatus("Spatial image could not be displayed.");
        else setStatus("");
        return;
      }

      invalidateImage();
      var token = {
        generation: nextScene.generation,
        viewKey: nextScene.viewKey,
        imageKey: descriptor.key,
        serial: imageSerial
      };
      var sourceImage = new Image();
      var settled = false;
      imageState.viewKey = nextScene.viewKey;
      imageState.key = descriptor.key;
      imageState.sourceUri = descriptor.sourceUri;
      imageState.state = "loading";
      imageState.token = token;
      imageState.loader = sourceImage;
      setStatus("Loading spatial image\u2026");

      function releaseLoader() {
        sourceImage.onload = null;
        sourceImage.onerror = null;
        if (imageState.loader === sourceImage) imageState.loader = null;
      }

      function acceptImage() {
        if (settled) return;
        if (!sourceImage.naturalWidth || !sourceImage.naturalHeight) {
          rejectImage();
          return;
        }
        settled = true;
        releaseLoader();
        if (!imageTokenIsCurrent(token)) return;
        imageState.image = sourceImage;
        imageState.state = "loaded";
        setStatus("");
        schedule();
      }

      function rejectImage() {
        if (settled) return;
        settled = true;
        releaseLoader();
        if (!imageTokenIsCurrent(token)) return;
        imageState.image = null;
        imageState.state = "error";
        setStatus("Spatial image could not be displayed.");
        schedule();
      }

      sourceImage.onload = acceptImage;
      sourceImage.onerror = rejectImage;
      sourceImage.src = descriptor.sourceUri;
      if (typeof sourceImage.decode === "function") {
        sourceImage.decode().then(acceptImage, function () {
          if (sourceImage.complete && !sourceImage.naturalWidth) rejectImage();
        });
      }
    }

    function resize() {
      if (disposed || !context || !isCanvasConnected(canvas)) return false;
      var rect = canvas.getBoundingClientRect();
      if (rect.width <= 0 || rect.height <= 0) {
        cssWidth = 0;
        cssHeight = 0;
        return false;
      }

      cssWidth = rect.width;
      cssHeight = rect.height;
      pixelRatio = clamp(isFiniteNumber(window.devicePixelRatio) ? window.devicePixelRatio : 1, 1, 2);
      var backingWidth = Math.max(1, Math.round(cssWidth * pixelRatio));
      var backingHeight = Math.max(1, Math.round(cssHeight * pixelRatio));
      if (canvas.width !== backingWidth || canvas.height !== backingHeight) {
        canvas.width = backingWidth;
        canvas.height = backingHeight;
      }
      context.setTransform(pixelRatio, 0, 0, pixelRatio, 0, 0);
      return true;
    }

    function updateTooltip(layout, geometry) {
      if (!tooltip || !pointer || !scene || !controls || controls.point_opacity <= 0) {
        hideTooltip();
        return;
      }

      var radius = Math.max(6, controls.point_size / 2 + 3);
      var radiusSquared = radius * radius;
      var nearest = null;
      var nearestDistance = radiusSquared;
      var plot = geometry.plot;
      var index;
      var screen;
      var dx;
      var dy;
      var distance;

      for (index = 0; index < layout.screens.length; index += 1) {
        screen = layout.screens[index];
        if (
          screen.x < plot.left ||
          screen.x > plot.xmax ||
          screen.y < plot.top ||
          screen.y > plot.ymax
        ) {
          continue;
        }
        dx = pointer.x - screen.x;
        dy = pointer.y - screen.y;
        distance = dx * dx + dy * dy;
        if (distance <= nearestDistance) {
          nearest = screen;
          nearestDistance = distance;
        }
      }

      if (!nearest) {
        hideTooltip();
        return;
      }

      var pointIndex = nearest.pointIndex;
      var group = scene.groups[scene.points.groupIndex[pointIndex]];
      tooltip.textContent = scene.points.barcode[pointIndex] + " \u00b7 " + group.label +
        " \u00b7 " + group.count + " plotted in group";
      tooltip.hidden = false;

      var frameRect = frame.getBoundingClientRect();
      var canvasRect = canvas.getBoundingClientRect();
      var anchorX = canvasRect.left - frameRect.left + pointer.x;
      var anchorY = canvasRect.top - frameRect.top + pointer.y;
      var maximumLeft = Math.max(4, frameRect.width - tooltip.offsetWidth - 4);
      var maximumTop = Math.max(4, frameRect.height - tooltip.offsetHeight - 4);
      tooltip.style.left = clamp(anchorX + 12, 4, maximumLeft) + "px";
      tooltip.style.top = clamp(anchorY + 12, 4, maximumTop) + "px";
    }

    function draw() {
      if (disposed || !resize()) return;
      context.clearRect(0, 0, cssWidth, cssHeight);
      context.fillStyle = "#ffffff";
      context.fillRect(0, 0, cssWidth, cssHeight);
      if (!scene || !controls || !viewport) {
        hideTooltip();
        return;
      }
      var geometry = plotGeometry(cssWidth, cssHeight, viewport);
      if (!geometry) {
        hideTooltip();
        return;
      }

      drawAxes(context, geometry);

      var frameBounds = scene.coordinateFrame;
      var pivotX = frameBounds.centerX;
      var pivotY = frameBounds.centerY;
      var radians = controls.coordinate_rotation * Math.PI / 180;
      var cosine = Math.cos(radians);
      var sine = Math.sin(radians);
      var originalCorners = frameCorners(frameBounds);
      var transformedCorners = rotateCorners(originalCorners, pivotX, pivotY, cosine, sine);
      var layout = layoutPoints(scene, controls, geometry, pivotX, pivotY, cosine, sine);

      context.save();
      context.beginPath();
      context.rect(geometry.plot.left, geometry.plot.top, geometry.plot.width, geometry.plot.height);
      context.clip();
      drawImageLayer(context, imageState.image, scene, controls, geometry);
      drawPoints(context, scene, controls, layout);
      drawFrames(
        context,
        screenCorners(originalCorners, geometry),
        screenCorners(transformedCorners, geometry),
        controls.coordinate_rotation
      );
      context.restore();
      updateTooltip(layout, geometry);
    }

    function schedule() {
      if (disposed || animationFrame !== null) return;
      animationFrame = window.requestAnimationFrame(function () {
        animationFrame = null;
        draw();
      });
    }

    function setScene(payload) {
      if (disposed) return false;
      if (payload === lastPayload) return true;
      var parsed = parseScene(payload, currentGeneration);
      if (!parsed) return false;

      var viewChanged = currentViewKey !== parsed.viewKey;
      var resetChanged = currentResetToken !== parsed.resetToken;
      currentGeneration = parsed.generation;
      currentViewKey = parsed.viewKey;
      currentResetToken = parsed.resetToken;
      lastPayload = payload;

      if (viewChanged) {
        viewport = null;
        viewportViewKey = null;
      }
      if (viewChanged || resetChanged) {
        controls = null;
        invalidateImage();
        pointer = null;
        hideTooltip();
      }

      if (!parsed.available) {
        scene = null;
        if (viewChanged || resetChanged) controls = null;
        invalidateImage();
        setStatus(parsed.message);
        updateUnavailableAccessibility(parsed.message);
        schedule();
        return true;
      }

      if (!viewport || viewportViewKey !== parsed.viewKey) {
        var nextViewport = buildViewport(parsed.coordinateFrame, parsed.image);
        if (!nextViewport) {
          scene = null;
          viewport = null;
          viewportViewKey = null;
          invalidateImage();
          pointer = null;
          hideTooltip();
          setStatus("Spatial preview geometry is unavailable.");
          updateUnavailableAccessibility("Spatial preview geometry is unavailable.");
          schedule();
          return true;
        }
        viewport = nextViewport;
        viewportViewKey = parsed.viewKey;
      }
      scene = parsed;
      if (!controls || viewChanged || resetChanged) controls = cloneControls(parsed.controls);
      syncImage(parsed, viewChanged || resetChanged);
      updateSceneAccessibility(parsed);
      schedule();
      return true;
    }

    function patchControls(values) {
      if (disposed || !scene || !controls || !isObject(values)) return false;
      var next = cloneControls(controls);
      var numericNames = [
        "coordinate_rotation",
        "dx",
        "dy",
        "scale",
        "rotation",
        "image_opacity",
        "point_opacity",
        "point_size"
      ];
      var index;
      var name;
      var limits;

      for (index = 0; index < numericNames.length; index += 1) {
        name = numericNames[index];
        if (!isFiniteNumber(values[name])) continue;
        limits = controlLimits(name);
        next[name] = clamp(values[name], limits.minimum, limits.maximum);
      }
      if (typeof values.flip_x === "boolean") next.flip_x = values.flip_x;
      if (typeof values.flip_y === "boolean") next.flip_y = values.flip_y;
      controls = next;
      schedule();
      return true;
    }

    function getControls() {
      return controls ? cloneControls(controls) : null;
    }

    function onPointerMove(event) {
      if (disposed) return;
      var rect = canvas.getBoundingClientRect();
      pointer = {
        x: event.clientX - rect.left,
        y: event.clientY - rect.top
      };
      schedule();
    }

    function onPointerLeave() {
      pointer = null;
      hideTooltip();
    }

    function dispose() {
      if (disposed) return;
      disposed = true;
      if (animationFrame !== null) {
        window.cancelAnimationFrame(animationFrame);
        animationFrame = null;
      }
      if (resizeObserver) resizeObserver.disconnect();
      if (usingWindowResize) window.removeEventListener("resize", schedule);
      invalidateImage();
      pointer = null;
      hideTooltip();
      canvas.removeEventListener("pointermove", onPointerMove);
      canvas.removeEventListener("pointerleave", onPointerLeave);
    }

    canvas.addEventListener("pointermove", onPointerMove);
    canvas.addEventListener("pointerleave", onPointerLeave);
    if (typeof window.ResizeObserver === "function") {
      resizeObserver = new window.ResizeObserver(schedule);
      resizeObserver.observe(canvas);
    } else {
      usingWindowResize = true;
      window.addEventListener("resize", schedule);
    }
    if (!context) setStatus("Spatial preview is unavailable.");
    schedule();

    return {
      canvas: canvas,
      setScene: setScene,
      patchControls: patchControls,
      getControls: getControls,
      schedule: schedule,
      draw: draw,
      resize: resize,
      dispose: dispose
    };
  }

  function ensureInstance() {
    var canvas = document.querySelector("[data-builder-spatial-canvas='true']");
    if (
      currentInstance &&
      (currentInstance.canvas !== canvas || !isCanvasConnected(currentInstance.canvas))
    ) {
      currentInstance.dispose();
      currentInstance = null;
    }

    if (canvas && !currentInstance) {
      currentInstance = createRenderer(canvas);
      if (latestScene) currentInstance.setScene(latestScene);
    }
  }

  function minimumCachedGeneration() {
    if (
      latestScene &&
      latestScene.schema_version === 1 &&
      isFiniteNumber(latestScene.generation) &&
      Math.floor(latestScene.generation) === latestScene.generation
    ) {
      return latestScene.generation;
    }
    return -1;
  }

  function receiveScene(payload) {
    ensureInstance();
    if (currentInstance) {
      if (currentInstance.setScene(payload)) latestScene = payload;
      return;
    }
    if (parseScene(payload, minimumCachedGeneration())) latestScene = payload;
  }

  function registerHandler() {
    if (
      handlerRegistered ||
      !window.Shiny ||
      typeof window.Shiny.addCustomMessageHandler !== "function"
    ) {
      return;
    }
    window.Shiny.addCustomMessageHandler("builder_spatial_canvas_scene", receiveScene);
    handlerRegistered = true;
  }

  function handleControlEvent(event, data) {
    if (!eventControlId(event, data)) return;
    ensureInstance();
    if (!currentInstance) return;
    var controls = currentInstance.getControls();
    if (!controls) return;
    currentInstance.patchControls(readControlsFromDom(controls));
  }

  function handleShinyLifecycle() {
    registerHandler();
    ensureInstance();
    installJqueryHandlers();
  }

  function installDocumentHandlers() {
    if (documentHandlersRegistered) return;
    document.addEventListener("input", handleControlEvent, true);
    document.addEventListener("change", handleControlEvent, true);
    document.addEventListener("pointermove", handleControlEvent, true);
    document.addEventListener("shiny:inputchanged", handleControlEvent, true);
    document.addEventListener("shiny:connected", handleShinyLifecycle, true);
    document.addEventListener("shiny:sessioninitialized", handleShinyLifecycle, true);
    documentHandlersRegistered = true;
  }

  function installJqueryHandlers() {
    if (jqueryHandlersRegistered || !window.jQuery) return;
    window.jQuery(document).on(
      "shiny:connected.builderSpatialCanvas shiny:sessioninitialized.builderSpatialCanvas",
      handleShinyLifecycle
    );
    window.jQuery(document).on("shiny:inputchanged.builderSpatialCanvas", handleControlEvent);
    jqueryHandlersRegistered = true;
  }

  function startObserver() {
    if (observerStarted || !document.documentElement || typeof window.MutationObserver !== "function") return;
    var observer = new window.MutationObserver(ensureInstance);
    observer.observe(document.documentElement, { childList: true, subtree: true });
    observerStarted = true;
  }

  function bootstrap() {
    installDocumentHandlers();
    installJqueryHandlers();
    registerHandler();
    startObserver();
    ensureInstance();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bootstrap, { once: true });
  }
  window.addEventListener("load", bootstrap, { once: true });
  bootstrap();
})();
