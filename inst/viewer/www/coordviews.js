/* ==========================================================================
   Coordinated cross-modal views — client-side engine.

   Every modality is just a named 2-D layout of the SAME cells:
     umap    = expression embedding
     spatial = physical coordinates
     clone   = (clone expansion-rank, within-clone index)
   The engine renders N panels, each showing one "space", over a single shared
   selection set keyed on CELL INDEX. Because the selection is by cell index,
   brushing in any panel highlights the same cells in every other panel —
   across modalities — with no special-casing. This is the coordination fabric
   the paper claims, generalised so the immune repertoire is a first-class
   space (which a generic viewer cannot express).

   One R->JS handoff per dataset (Shiny message "coordviews_data"); all
   interaction (brush, highlight, readout) is client-side and instant.

   All ids are `cv-`-prefixed; all styles scoped under `.coordviews-page`.
   ========================================================================== */
(function () {
  'use strict';

  var D = null;                 // the data bundle
  var panels = [];              // [{key, canvas, ctx, spaceId, W, H, sx, sy, lasso, drag, moved}]
  var sel = null;               // Set of selected cell indices (null = none)
  var pick = null;              // hovered/clicked cell index
  var zoomed = false;           // is the umap panel currently zoomed to a selection
  var selectMode = 'lasso';     // drag-select mode: 'lasso' (freeform) or 'box'
  // Trekker controls brought into Linked views (only when D.trekker is present):
  var dissolvePct = 0;          // % of least-confident nuclei to dissolve
  var dissolveThresh = null;    // conf value below which cells are hidden
  var evidenceOn = false;       // ring nuclei that carry positioning evidence
  var nicheRadius = 250;        // µm radius for the picked-nucleus niche readout
  var nicheSet = null;          // cell indices inside the picked nucleus's niche
  var colorBy = null;           // group name
  var ps = 3.0;                 // point size
  var pointOpacity = 0.8;       // base draw opacity (no-selection view)
  var hidden = new Set();       // hidden level indices for the active group
  var curProj = null;           // projection name feeding the expression panel
  var pctShow = 100;            // % of cells to render
  var pctMask = null;           // Uint8Array subsample mask, or null (all shown)
  var groupFilter = {};         // groupName -> Set(allowed level idx); absent = all
  var spaceById = {};
  var resizeObserver = null;    // fires when the tab becomes visible / resizes
  var resizeTimer = null;
  // Histology background image (embedded in the spatial entry). Aligned to the
  // cells via the entry's data-space bounds; user transforms adjust on top.
  var imgEl = null, imgReady = false;
  // offsetX/offsetY are in DATA units (converted to screen via dataToScreen), so
  // an external image's data-space preset aligns on this canvas unchanged.
  var imgState = { show: true, opacity: 0.6, offsetX: 0, offsetY: 0,
    scaleX: 1, scaleY: 1, flipX: false, flipY: false, rotate: 0 };

  // Categorical fallback palette (mirrors the app).
  var PAL = ['#636EFA', '#EF553B', '#00CC96', '#AB63FA', '#FFA15A', '#19D3F3',
    '#FF6692', '#B6E880', '#FF97FF', '#FECB52', '#2f6fd6', '#f97316',
    '#16a34a', '#9a5cd0', '#e05780', '#38b2ac', '#d97706', '#7bb0e8'];

  // RGB co-expression: cells whose max channel is <= RGB_MIN form a light-grey
  // substrate; the rest blend FROM that grey toward their full-brightness hue by
  // intensity (max/255), so weak co-expression stays faint (≈grey) and only
  // strong expression is vivid — additive RGB otherwise renders low signal as
  // near-black on white. One grey source, shared by the colour and the layering.
  var RGB_MIN = 28;
  var RGB_GREY_RGB = [217, 219, 222];
  var RGB_GREY = 'rgb(' + RGB_GREY_RGB.join(',') + ')';

  // Viridis anchors for continuous (single-gene) colouring.
  var VIR = [[68, 1, 84], [72, 40, 120], [62, 73, 137], [49, 104, 142],
    [38, 130, 142], [31, 158, 137], [53, 183, 121], [110, 206, 88],
    [181, 222, 43], [253, 231, 37]];
  function viridis(t) {
    t = Math.max(0, Math.min(1, t));
    var s = t * (VIR.length - 1), i = Math.floor(s), f = s - i;
    var a = VIR[i], b = VIR[Math.min(i + 1, VIR.length - 1)];
    return [Math.round(a[0] + (b[0] - a[0]) * f),
      Math.round(a[1] + (b[1] - a[1]) * f),
      Math.round(a[2] + (b[2] - a[2]) * f)];
  }

  function $(id) { return document.getElementById(id); }
  function fmt(n) { return (n == null || isNaN(n)) ? '—' : n.toLocaleString('en-US'); }
  // Human-facing label for a group key. Metadata columns keep their own names;
  // the server-synthesised expansion group gets a readable label.
  function groupLabel(g) { return g === 'clone_expansion' ? 'Clone Expansion' : g; }

  // ---- per-space unit normalisation ---------------------------------------
  // Real geometry (umap, spatial) preserves aspect ratio. An abstract space
  // with independent axes (the clone panel) sets `stretch` so X and Y each
  // fill the box on their own — otherwise a wide clone-rank axis squashes the
  // expansion axis into a needle.
  function unitOf(space) {
    var xs = space.x, ys = space.y, n = xs.length;
    var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
    for (var i = 0; i < n; i++) {
      var xv = xs[i], yv = ys[i];
      if (xv == null || isNaN(xv)) continue;
      if (xv < x0) x0 = xv; if (xv > x1) x1 = xv;
      if (yv < y0) y0 = yv; if (yv > y1) y1 = yv;
    }
    var dw = (x1 - x0) || 1, dh = (y1 - y0) || 1;
    var kx, ky, ox, oy;
    if (space.stretch) {
      kx = 1 / dw; ky = 1 / dh; ox = 0; oy = 0;
    } else {
      var k = 1 / Math.max(dw, dh);
      kx = k; ky = k; ox = (1 - dw * k) / 2; oy = (1 - dh * k) / 2;
    }
    var nx = new Float32Array(n), ny = new Float32Array(n), ok = new Uint8Array(n);
    for (var j = 0; j < n; j++) {
      var a = xs[j], b = ys[j];
      if (a == null || isNaN(a) || b == null || isNaN(b)) { ok[j] = 0; continue; }
      nx[j] = (a - x0) * kx + ox; ny[j] = (b - y0) * ky + oy; ok[j] = 1;
    }
    // x0/y0/k/ox/oy let us map ARBITRARY data coords (e.g. image bounds) to the
    // same unit box the points use, so a background image aligns to the cells.
    // (Aspect-preserving spaces only — kx === ky there; the image never uses a
    // stretched space.)
    return { nx: nx, ny: ny, ok: ok, x0: x0, y0: y0, k: kx, ky: ky, ox: ox, oy: oy };
  }

  // Map a unit-box coord (nx, ny in [0,1]) to this panel's screen pixels,
  // applying the panel's optional zoom view (a centred sub-rectangle of the unit
  // box blown up to fill the panel). view=null is the identity — the default,
  // unchanged path. Used by the point loop, image bounds, and axis ticks alike,
  // so a zoomed panel stays internally consistent.
  function unitToScreen(p, nx, ny) {
    var v = p.view, zx, zy;
    if (v) { zx = (nx - v.cx) / v.span + 0.5; zy = (ny - v.cy) / v.span + 0.5; }
    else { zx = nx; zy = ny; }
    return [p._sox + zx * p._S, p._soy + p._S - zy * p._S];
  }
  // Map a data-space (dx, dy) to this panel's screen pixels, using the same
  // transform the points use. Requires project(p) to have run.
  function dataToScreen(p, dx, dy) {
    var u = spaceById[p.spaceId] && spaceById[p.spaceId]._unit;
    if (!u || p._S == null) return null;
    var nx = (dx - u.x0) * u.k + u.ox;
    var ny = (dy - u.y0) * u.k + u.oy;
    return unitToScreen(p, nx, ny);
  }

  // Per-cell RGB channels (0..255) + their max — the one source for both the
  // colour blend and the expressing test.
  function rgbAt(i) {
    var d = D.rgb, r = d && d.r ? d.r[i] : 0, g = d && d.g ? d.g[i] : 0,
      b = d && d.b ? d.b[i] : 0;
    return { r: r, g: g, b: b, m: Math.max(r, g, b) };
  }
  function rgbExpressing(i) { return rgbAt(i).m > RGB_MIN; }

  // ---- colour of a cell under the active mode -----------------------------
  function colorOf(i) {
    if (colorBy === GENE_MODE) {
      if (!D.gene) return '#dcdcdc';
      var c = viridis(D.gene.v[i] / 255);
      return 'rgb(' + c[0] + ',' + c[1] + ',' + c[2] + ')';
    }
    if (colorBy === RGB_MODE) {
      var e = rgbAt(i);
      if (e.m <= RGB_MIN) return RGB_GREY;            // non-expressing → grey
      // Blend grey -> full-brightness hue by intensity; never near-black.
      var t = e.m / 255, ch = [e.r, e.g, e.b], o = [0, 0, 0];
      for (var k = 0; k < 3; k++) {
        o[k] = Math.round(RGB_GREY_RGB[k] + (ch[k] / e.m * 255 - RGB_GREY_RGB[k]) * t);
      }
      return 'rgb(' + o[0] + ',' + o[1] + ',' + o[2] + ')';
    }
    var fld = fieldOf();
    if (fld) {
      var fv = fld.v[i];
      if (fv == null || isNaN(fv)) return '#e6e7ea';   // unpositioned / NA → faint
      var vc = viridis(fv / 255);
      return 'rgb(' + vc[0] + ',' + vc[1] + ',' + vc[2] + ')';
    }
    var g = D.groups[colorBy];
    if (!g) return '#888';
    var lv = g.values[i];
    if (lv < 0 || lv == null) return '#cccccc';
    return (g.colors && g.colors[lv]) || PAL[lv % PAL.length];
  }
  function visible(i) {
    // Continuous modes never hide points (no categorical legend to toggle).
    if (colorBy === GENE_MODE || colorBy === RGB_MODE || fieldOf()) return true;
    var g = D.groups[colorBy];
    if (!g) return true;
    return !hidden.has(g.values[i]);
  }
  // A cell is "active" when it survives the group filters AND the % subsample.
  // Inactive cells are drawn in no panel and are not selectable, matching the
  // Overview page's group-filter + "show % of cells" behaviour.
  function activeCell(i) {
    if (pctMask && !pctMask[i]) return false;
    // Trekker: dissolve the least-confidently-positioned nuclei.
    if (dissolveThresh != null && D.trekker && D.trekker.conf) {
      var cf = D.trekker.conf[i];
      if (cf != null && cf < dissolveThresh) return false;
    }
    for (var gname in groupFilter) {
      if (!Object.prototype.hasOwnProperty.call(groupFilter, gname)) continue;
      var allowed = groupFilter[gname];
      if (!allowed) continue;
      var g = D.groups[gname];
      if (g && !allowed.has(g.values[i])) return false;
    }
    return true;
  }
  // Threshold for the dissolve slider: the conf value at the dissolvePct-quantile
  // of positioned cells; cells below it are hidden.
  function rebuildDissolve() {
    dissolveThresh = null;
    if (!D || !D.trekker || !D.trekker.conf || dissolvePct <= 0) return;
    var vals = [];
    for (var i = 0; i < D.n; i++) {
      var c = D.trekker.conf[i];
      if (c != null && !isNaN(c)) vals.push(c);
    }
    if (!vals.length) return;
    vals.sort(function (a, b) { return a - b; });
    var k = Math.floor(vals.length * dissolvePct / 100);
    dissolveThresh = vals[Math.min(k, vals.length - 1)];
  }
  // Legend-visible AND filter-active — the single gate every render/hit-test uses.
  function shown(i) { return visible(i) && activeCell(i); }
  // Stable per-cell subsample: the same subset persists across redraws/toggles
  // (deterministic hash), so lowering "% of cells" never reshuffles the view.
  function rebuildPctMask() {
    if (!D || pctShow >= 100) { pctMask = null; return; }
    var n = D.n, thr = pctShow / 100;
    pctMask = new Uint8Array(n);
    for (var i = 0; i < n; i++) {
      var h = (Math.imul(i + 1, 2654435761) >>> 0) / 4294967296;
      pctMask[i] = h < thr ? 1 : 0;
    }
  }
  // After a filter / subsample change: drop now-inactive cells from the current
  // selection, then redraw. Keeps the coordinated selection consistent.
  function applyActiveChange() {
    if (sel) {
      var s = new Set();
      sel.forEach(function (i) { if (activeCell(i)) s.add(i); });
      setSelection(s.size ? s : null);
    } else {
      drawAll();
    }
  }

  // ---- panel geometry + projection ----------------------------------------
  function project(p) {
    var sp = spaceById[p.spaceId];
    if (!sp) { p.sx = null; p.sy = null; return; }
    if (!sp._unit) sp._unit = unitOf(sp);
    var u = sp._unit, n = D.n;
    // Axis'd spaces (the clone panel) reserve room on the left for the y-label
    // and along the bottom for the x-label; geometric spaces keep a tight square.
    var axed = !!sp._axisSpec;
    var padL = axed ? 42 : 16, padB = axed ? 30 : 16, padT = 16, padR = 16;
    var S = Math.min(p.W - padL - padR, p.H - padT - padB);
    var ox = padL + (p.W - padL - padR - S) / 2;
    var oy = padT + (p.H - padT - padB - S) / 2;
    p._S = S; p._sox = ox; p._soy = oy;      // for dataToScreen (image bounds)
    p.sx = new Float32Array(n); p.sy = new Float32Array(n); p.ok = u.ok;
    // Inline the unitToScreen transform (avoids per-cell allocation on big sets).
    var v = p.view;
    for (var i = 0; i < n; i++) {
      var zx, zy;
      if (v) { zx = (u.nx[i] - v.cx) / v.span + 0.5; zy = (u.ny[i] - v.cy) / v.span + 0.5; }
      else { zx = u.nx[i]; zy = u.ny[i]; }
      p.sx[i] = ox + zx * S;
      p.sy[i] = oy + S - zy * S;   // y up
    }
  }
  // Size one panel's canvas to a `side` x `side` square (resizeAll computes the
  // side so all visible panels fit the width AND height in one viewport; floor
  // 300px). Below 420px the header can't hold title + toolbar, so the toolbar
  // floats vertically (see .cv-pane.cv-narrow .cv-panebar).
  function resizePanelSquare(p, side) {
    var dpr = window.devicePixelRatio || 1;
    p.W = side; p.H = side;
    p.canvas.width = side * dpr; p.canvas.height = side * dpr;
    p.canvas.style.width = side + 'px';
    p.canvas.style.height = side + 'px';
    p.ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    if (p.pane) p.pane.classList.toggle('cv-narrow', side < 420);
    project(p);
  }

  // Draw the histology image behind the points of the spatial panel. The image's
  // data-space bounds are mapped to screen with the SAME transform as the cells,
  // so it aligns; opacity/offset/scale/flip/rotate then adjust it on top.
  function drawImage(p) {
    var sp = spaceById[p.spaceId];
    if (!sp || !sp.image || !imgEl || !imgReady || !imgState.show) return;
    var b = sp.image.bounds;
    if (!b) return;
    var tl = dataToScreen(p, b.xmin, b.ymax);   // data ymax = top (y-up)
    var br = dataToScreen(p, b.xmax, b.ymin);
    if (!tl || !br) return;
    var x = Math.min(tl[0], br[0]), y = Math.min(tl[1], br[1]);
    var w = Math.abs(br[0] - tl[0]), h = Math.abs(br[1] - tl[1]);
    if (!(w > 0) || !(h > 0)) return;
    // Convert the DATA-unit offset to screen pixels by mapping two points through
    // the same projection the cells use (handles scale + the y-axis inversion).
    var o0 = dataToScreen(p, b.xmin, b.ymin);
    var o1 = dataToScreen(p, b.xmin + imgState.offsetX, b.ymin + imgState.offsetY);
    var offSX = (o0 && o1) ? (o1[0] - o0[0]) : 0;
    var offSY = (o0 && o1) ? (o1[1] - o0[1]) : 0;
    var c = p.ctx;
    c.save();
    c.globalAlpha = imgState.opacity;
    c.translate(x + w / 2 + offSX, y + h / 2 + offSY);
    if (imgState.rotate) c.rotate(imgState.rotate * Math.PI / 180);
    c.scale(imgState.scaleX * (imgState.flipX ? -1 : 1),
      imgState.scaleY * (imgState.flipY ? -1 : 1));
    c.drawImage(imgEl, -w / 2, -h / 2, w, h);
    c.restore();
    c.globalAlpha = 1;
  }

  // Draw a labelled L-shaped frame for an abstract space (the clone panel).
  // Geometric spaces (umap, spatial) carry no _axisSpec and stay axis-free, so
  // their coordinates read as "layout, not measurement" — the usual convention.
  function drawAxes(p) {
    var sp = spaceById[p.spaceId], spec = sp && sp._axisSpec;
    if (!spec || p._S == null) return;
    var u = sp._unit; if (!u) return;
    var c = p.ctx, S = p._S;
    var x0 = p._sox, yt = p._soy, x1 = p._sox + S, yb = p._soy + S;
    var dataToY = function (dy) {
      var ny = (dy - u.y0) * u.ky + u.oy;
      var zy = p.view ? ((ny - p.view.cy) / p.view.span + 0.5) : ny;
      return p._soy + S - zy * S;
    };
    c.save();
    c.font = "10px -apple-system, 'Segoe UI', Roboto, sans-serif";
    // expansion-tier bands: faint separator + tier label per band
    if (spec.bands) {
      spec.bands.forEach(function (bd, bi) {
        if (bi > 0) {
          c.strokeStyle = '#eef0f3'; c.lineWidth = 1;
          var ys = dataToY(bd.lo);
          c.beginPath(); c.moveTo(x0, ys); c.lineTo(x1, ys); c.stroke();
        }
        // white chip behind the label so it stays legible over dense points
        var ly = dataToY(bd.mid), lw = c.measureText(bd.label).width;
        c.fillStyle = 'rgba(255,255,255,0.82)';
        c.fillRect(x0 + 3, ly - 7, lw + 6, 14);
        c.fillStyle = '#6b7280'; c.textAlign = 'left'; c.textBaseline = 'middle';
        c.fillText(bd.label, x0 + 6, ly);
      });
    }
    // L-shaped frame
    c.strokeStyle = '#d5dae1'; c.lineWidth = 1;
    c.beginPath(); c.moveTo(x0, yt); c.lineTo(x0, yb); c.lineTo(x1, yb); c.stroke();
    // x-axis label under the bottom edge
    c.fillStyle = '#6b7280'; c.textAlign = 'center'; c.textBaseline = 'top';
    c.fillText(spec.xlab, (x0 + x1) / 2, yb + 9);
    // y-axis label rotated up the left edge
    c.save();
    c.translate(x0 - 14, (yt + yb) / 2); c.rotate(-Math.PI / 2);
    c.textAlign = 'center'; c.textBaseline = 'bottom';
    c.fillText(spec.ylab, 0, 0);
    c.restore();
    c.restore();
  }

  // Paint one cell dot at the given alpha, coloured by the active mode.
  function paintCell(p, i, alpha) {
    var c = p.ctx;
    c.globalAlpha = alpha;
    c.fillStyle = colorOf(i);
    c.beginPath(); c.arc(p.sx[i], p.sy[i], ps, 0, 6.2832); c.fill();
  }

  function draw(p) {
    var c = p.ctx; c.clearRect(0, 0, p.W, p.H);
    if (!p.sx) return;
    drawImage(p);
    drawAxes(p);
    // One two-layer pass: background on layer 0, foreground on layer 1 (on top).
    // The foreground is the "meaningful" set — expressing cells in RGB mode,
    // otherwise the selected cells. Alpha: with a selection, selected stay solid
    // and the rest fade; with none, RGB dims its grey substrate, else the opacity
    // slider governs. Both the layering and the paint go through one code path.
    // Highlight set = the lasso selection, or (Trekker) the picked nucleus's
    // niche. Cells in it stay solid; everything else fades.
    var n = D.n, i, rgb = colorBy === RGB_MODE;
    var hiSet = (sel && sel.size) ? sel : nicheSet;
    // shown(i) (= visible + activeCell) is stable across this draw but is tested
    // 2n times below (two layers) plus once in the evidence pass; precompute it
    // once. Uint8 mask, indexed instead of recomputed — halves the per-cell work
    // on the hot lasso-drag redraw path.
    var shownMask = new Uint8Array(n);
    for (i = 0; i < n; i++) shownMask[i] = shown(i) ? 1 : 0;
    for (var layer = 0; layer < 2; layer++) {
      for (i = 0; i < n; i++) {
        if (!p.ok[i] || !shownMask[i]) continue;
        var fg = rgb ? rgbExpressing(i) : !!(hiSet && hiSet.has(i));
        if ((layer === 0) === fg) continue;   // bg on layer 0, fg on layer 1
        var inHi = !hiSet || hiSet.has(i);
        var alpha = hiSet ? (inHi ? 0.95 : 0.05)
          : rgb ? (fg ? 1 : 0.5 * pointOpacity) : pointOpacity;
        paintCell(p, i, alpha);
      }
    }
    // Trekker: ring nuclei that carry positioning evidence.
    if (evidenceOn && D.trekker && D.trekker.evidence) {
      c.globalAlpha = 1; c.strokeStyle = '#1f2937'; c.lineWidth = 1.4;
      for (i = 0; i < n; i++) {
        if (!p.ok[i] || !shownMask[i] || D.trekker.evidence[i] !== 1) continue;
        c.beginPath(); c.arc(p.sx[i], p.sy[i], ps + 2.5, 0, 6.2832); c.stroke();
      }
    }
    // picked cell ring
    if (pick != null && p.ok[pick]) {
      c.globalAlpha = 1; c.strokeStyle = '#f97316'; c.lineWidth = 2.2;
      c.beginPath(); c.arc(p.sx[pick], p.sy[pick], ps + 4, 0, 6.2832); c.stroke();
    }
    // Trekker: dashed niche-radius circle around the picked nucleus (physical
    // panel). Radius µm → screen px via the same data→unit→screen scale.
    if (pick != null && !sel && D.trekker && p.spaceId === 'trekker' && p.ok[pick]) {
      var nu = spaceById['trekker'] && spaceById['trekker']._unit;
      if (nu && p._S != null) {
        var rpx = nicheRadius * nu.k * p._S * (p.view ? 1 / p.view.span : 1);
        c.globalAlpha = 1;
        c.fillStyle = 'rgba(37,99,235,0.10)';
        c.beginPath(); c.arc(p.sx[pick], p.sy[pick], rpx, 0, 6.2832); c.fill();
        c.strokeStyle = '#1d4ed8'; c.lineWidth = 2.5; c.setLineDash([7, 4]);
        c.beginPath(); c.arc(p.sx[pick], p.sy[pick], rpx, 0, 6.2832); c.stroke();
        c.setLineDash([]);
      }
    }
    // lasso: filled while dragging; a dashed outline once committed (kept so the
    // selected region stays visible until the next selection / reproject).
    if (p.lasso && p.lasso.length > 1) {
      c.globalAlpha = 1; c.strokeStyle = '#2f6fd6'; c.lineWidth = 1.5;
      c.beginPath(); c.moveTo(p.lasso[0][0], p.lasso[0][1]);
      for (var q = 1; q < p.lasso.length; q++) c.lineTo(p.lasso[q][0], p.lasso[q][1]);
      c.closePath();
      if (p.drag) {
        c.fillStyle = 'rgba(47,111,214,.08)'; c.fill(); c.stroke();
      } else {
        c.setLineDash([5, 4]); c.stroke(); c.setLineDash([]);
      }
    }
    c.globalAlpha = 1;
  }
  // Live "showing N / M cells" readout — the single feedback that a filter or
  // subsample took effect, regardless of what the panels are coloured by.
  function renderShownCount() {
    var el = $('cv-shown'); if (!el || !D) return;
    var n = 0;
    for (var i = 0; i < D.n; i++) if (shown(i)) n++;
    // Hidden entirely when nothing is filtered out; only surfaces to explain a
    // reduced view (group filter, subsample, or legend-hide).
    if (n >= D.n) { el.style.display = 'none'; return; }
    el.style.display = '';
    el.textContent = 'showing ' + fmt(n) + ' of ' + fmt(D.n) + ' cells';
  }
  function drawAll() { panels.forEach(draw); renderShownCount(); }
  // Drop any committed lasso outlines (their screen coords go stale on reproject,
  // and a new selection supersedes them). Returns true if anything was cleared.
  function clearLassos() {
    var any = false;
    panels.forEach(function (p) { if (p.lasso) { p.lasso = null; any = true; } });
    return any;
  }

  // Centre a slider's value bubble over its thumb. Uses the slider's fixed 150px
  // width as a fallback so it positions correctly even while its panel is hidden
  // (offsetWidth 0), and re-runs on every input.
  function positionRangeVal(sliderId, valId) {
    var s = $(sliderId), v = $(valId);
    if (!s || !v) return;
    var min = parseFloat(s.min), max = parseFloat(s.max), val = parseFloat(s.value);
    var frac = (max > min) ? (val - min) / (max - min) : 0;
    var w = s.offsetWidth || 150, thumb = 16;
    v.style.left = (frac * (w - thumb) + thumb / 2) + 'px';
  }
  function positionAllRangeVals() {
    positionRangeVal('cv-ps', 'cv-ps-val');
    positionRangeVal('cv-opacity', 'cv-op-val');
    positionRangeVal('cv-pct', 'cv-pct-val');
    positionRangeVal('cv-dissolve', 'cv-dissolve-val');
    positionRangeVal('cv-niche', 'cv-niche-val');
  }

  // ---- geometry helpers ----------------------------------------------------
  // inPoly lives in the shared CBGeom module (www/cv-geom.js). `nearest` stays
  // here: its visibility predicate (p.ok + shown) and fixed hit radius are this
  // engine's, not shared.
  function nearest(p, mx, my) {
    var best = -1, bd = 200, n = D.n, i;
    for (i = 0; i < n; i++) {
      if (!p.ok[i] || !shown(i)) continue;
      var dx = p.sx[i] - mx, dy = p.sy[i] - my, d = dx * dx + dy * dy;
      if (d < bd) { bd = d; best = i; }
    }
    return best;
  }

  // ---- selection ----------------------------------------------------------
  function setSelection(s) {
    sel = (s && s.size) ? s : null;
    rebuildNiche();   // a lasso selection supersedes the niche highlight
    // A zoom is tied to a specific selection, so any selection change (new brush
    // or clear) returns to the full view and resets the toggle.
    if (zoomed) { resetZoom(); zoomed = false; }
    updateZoomBtn();
    updateSelActions();
    renderSelbar(); renderReadout(); reportSelection(); drawAll();
  }
  // Subtle reveal/collapse for the selection bar + the Zoom/Clear group, so they
  // fade/slide in and out instead of popping. Elements start with `cv-collapse`
  // (+ display:none). To show: drop display (back into flow), let two frames
  // commit the collapsed state, then remove the class to transition in. To hide:
  // re-add the class to transition out, then set display:none once it settles.
  function revealEl(el, show) {
    if (!el) return;
    if (show) {
      if (el._hideT) { clearTimeout(el._hideT); el._hideT = null; }
      el.style.display = '';
      requestAnimationFrame(function () {
        requestAnimationFrame(function () { el.classList.remove('cv-collapse'); });
      });
    } else {
      if (getComputedStyle(el).display === 'none') return;
      el.classList.add('cv-collapse');
      el._hideT = setTimeout(function () {
        el.style.display = 'none'; el._hideT = null;
      }, 240);
    }
  }
  // The Zoom / Clear buttons live together and appear only with a selection.
  function updateSelActions() {
    var hasSel = !!(sel && sel.size);
    // A Trekker niche pick also gets the (animated) Clear button — but not the
    // "Zoom to selection" button, which is meaningless for a single-cell pick.
    var hasNiche = !hasSel && pick != null && !!nicheSet;
    var show = hasSel || hasNiche;
    revealEl($('cv-selactions'), show);
    if (show) updateSelActionsLayout();
    var zb = $('cv-zoom');
    if (zb) zb.style.display = hasSel ? '' : 'none';
  }
  // Vertical stack while the buttons fit on the controls' row; horizontal once
  // the other controls push them onto their own (full-width) line. Detected by
  // comparing row position against a control that is always on the first row —
  // pure CSS can't tell whether a content-sized flex item has wrapped.
  function updateSelActionsLayout() {
    var box = $('cv-selactions');
    if (!box || getComputedStyle(box).display === 'none') return;
    // "Colour by" is always the first control on row 1; if the buttons start at
    // or below its bottom edge they have wrapped onto their own line.
    var ref = $('cv-pick-color');
    if (!ref) return;
    var a = box.getBoundingClientRect(), b = ref.getBoundingClientRect();
    box.classList.toggle('cv-actions-row', a.top >= b.bottom - 2);
  }
  // Zoom ONLY the expression (umap) panel to the bounding box of its selected
  // cells — the point of the action is to inspect the umap's internal structure
  // for the selection. The other panel (Spatial / Clonal) keeps its full view; a
  // clonal rank layout has no meaningful "zoom", and the spatial context is more
  // useful whole. The umap panel maps the selection's unit-box extent (padded,
  // aspect-preserved, centred) to fill itself.
  function zoomToSelection() {
    if (!sel || !sel.size) return false;
    var did = false;
    panels.forEach(function (p) {
      if (p.spaceId !== 'umap') return;
      var sp = spaceById[p.spaceId], u = sp && sp._unit;
      if (!u) return;
      var nx0 = Infinity, nx1 = -Infinity, ny0 = Infinity, ny1 = -Infinity, any = false;
      sel.forEach(function (i) {
        if (!u.ok[i]) return;
        any = true;
        var nx = u.nx[i], ny = u.ny[i];
        if (nx < nx0) nx0 = nx; if (nx > nx1) nx1 = nx;
        if (ny < ny0) ny0 = ny; if (ny > ny1) ny1 = ny;
      });
      if (!any) return;
      var span = Math.max(nx1 - nx0, ny1 - ny0) * 1.25;
      if (!(span > 0.02)) span = 0.02;   // floor: don't over-zoom a tiny selection
      p.view = { cx: (nx0 + nx1) / 2, cy: (ny0 + ny1) / 2, span: span };
      project(p);
      did = true;
    });
    if (did) drawAll();
    return did;
  }
  function resetZoom() {
    var any = false;
    panels.forEach(function (p) { if (p.view) { p.view = null; project(p); any = true; } });
    if (any) drawAll();
  }
  // The Zoom button is a toggle: zoom in to the selection, or zoom back out. Its
  // label + active style reflect the current state.
  function updateZoomBtn() {
    var b = $('cv-zoom'); if (!b) return;
    b.textContent = zoomed ? 'Zoom back' : 'Zoom to selection';
    b.classList.toggle('is-zoomed', zoomed);
  }
  function toggleZoom() {
    clearLassos();   // reprojecting invalidates the screen-space lasso
    if (zoomed) { resetZoom(); zoomed = false; }
    else { zoomed = zoomToSelection(); }
    updateZoomBtn();
  }
  // Sync the active drag-mode highlight (box vs lasso) across every toolbar.
  function syncModeButtons() {
    ['box', 'lasso'].forEach(function (m) {
      var btns = document.querySelectorAll('.cv-tbtn[data-act="' + m + '"]');
      Array.prototype.forEach.call(btns, function (b) {
        b.classList.toggle('is-on', selectMode === m);
      });
    });
  }
  // Step-zoom a panel about its current view centre. factor<1 zooms in, >1 out;
  // zooming back past the full extent clears the zoom.
  function zoomStep(p, factor) {
    clearLassos();
    var v = p.view || { cx: 0.5, cy: 0.5, span: 1 };
    var span = v.span * factor;
    if (span >= 1) {
      if (p.view) { p.view = null; project(p); }
    } else {
      p.view = { cx: v.cx, cy: v.cy, span: Math.max(0.04, span) };
      project(p);
    }
    // keep the umap "Zoom back" toggle honest when the panel returns to full
    if (p.spaceId === 'umap' && !p.view && zoomed) { zoomed = false; updateZoomBtn(); }
    drawAll();
  }
  function downloadPanelPNG(p) {
    try {
      // Composite onto white first — the canvas itself is transparent, so a raw
      // export would have no background.
      var src = p.canvas, tmp = document.createElement('canvas');
      tmp.width = src.width; tmp.height = src.height;
      var c = tmp.getContext('2d');
      c.fillStyle = '#ffffff'; c.fillRect(0, 0, tmp.width, tmp.height);
      c.drawImage(src, 0, 0);
      var nm = (spaceById[p.spaceId] && spaceById[p.spaceId].label) || p.spaceId || 'panel';
      var a = document.createElement('a');
      a.href = tmp.toDataURL('image/png');
      a.download = 'linked-views-' + nm.replace(/[^\w.-]+/g, '_') + '.png';
      document.body.appendChild(a); a.click(); document.body.removeChild(a);
    } catch (e) { /* toDataURL can throw if the canvas is tainted; ignore */ }
  }
  function reportSelection() {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var arr = null;
    if (sel && sel.size) { arr = []; sel.forEach(function (i) { arr.push(D.cells[i]); }); }
    Shiny.setInputValue('coordviews_selection', arr);
  }

  // ---- readouts (composition + top clonotypes) — fully client-side --------
  function renderSelbar() {
    var bar = $('cv-selbar');
    if (!bar) return;
    if (sel) {
      $('cv-seltext').innerHTML = 'Selected <b>' + fmt(sel.size) + '</b> / ' +
        fmt(D.n) + ' cells &mdash; coordinated across all panels';
    }
    revealEl(bar, !!sel);
  }
  // Trekker: cell-type composition of the picked nucleus's physical neighbours
  // within the niche radius (µm). Uses the physical (spatial) space coords. Shown
  // in place of the empty readout when a single nucleus is picked.
  // Compute the niche: the picked nucleus + every cell within `nicheRadius` µm of
  // it in the Trekker physical space. Doubles as the highlight set — cells in
  // it stay solid, everything else fades. Null unless a single nucleus is picked
  // (and no lasso selection is active). The distance loop is CBGeom.nicheAround
  // (shared with trekker.js). This engine keys on spaceById['trekker'] and passes
  // inclusive=<=, skipNaN=true (unpositioned cells align to NaN here).
  function rebuildNiche() {
    nicheSet = null;
    if (pick == null || (sel && sel.size) || !D.trekker) return;
    var sp = spaceById['trekker'];
    if (!sp || !sp.x) return;
    var px = sp.x[pick], py = sp.y[pick];
    if (px == null || isNaN(px)) return;   // picked cell not positioned
    nicheSet = CBGeom.nicheAround(
      sp.x, sp.y, D.n, pick, px, py,
      nicheRadius * nicheRadius, true, true
    );
  }
  // Composition bar-chart HTML for a {levelIdx: count} map under categorical
  // group g, sorted descending, bars scaled to the top count. Shared by the
  // selection readout and the Trekker niche readout (identical markup).
  function compBars(comp, g) {
    var rows = Object.keys(comp).map(function (k) { return [parseInt(k, 10), comp[k]]; })
      .sort(function (a, b) { return b[1] - a[1]; });
    var mx = rows.length ? rows[0][1] : 1;
    return rows.map(function (r) {
      var col = (g.colors && g.colors[r[0]]) || PAL[r[0] % PAL.length];
      return '<div class="cv-bar"><span class="cv-bar-nm" style="color:' + col + '">' +
        g.levels[r[0]] + '</span><span class="cv-bar-tr"><span class="cv-bar-fl" style="width:' +
        (r[1] / mx * 100).toFixed(1) + '%;background:' + col + '"></span></span>' +
        '<span class="cv-bar-ct">' + r[1] + '</span></div>';
    }).join('');
  }
  function renderNiche(host) {
    if (!nicheSet) return false;
    var compGroup = D.groups['cell_type'] ? 'cell_type'
      : (D.groups[colorBy] ? colorBy : Object.keys(D.groups)[0]);
    var g = D.groups[compGroup]; if (!g) return false;
    var comp = {}, tot = 0;
    nicheSet.forEach(function (i) {
      if (i === pick) return;   // the composition is of the neighbours
      var lv = g.values[i]; comp[lv] = (comp[lv] || 0) + 1; tot++;
    });
    var head = '<div class="cv-readcol"><h4 class="cv-read-h">Niche of picked nucleus ' +
      '<span class="cv-read-sub">' + fmt(tot) + ' neighbours within ' + nicheRadius +
      ' µm · by ' + groupLabel(compGroup) + '</span></h4>';
    if (!tot) {
      host.innerHTML = head + '<div class="cv-empty-sm">No other nuclei within this ' +
        'radius — increase the niche radius.</div></div>';
      return true;
    }
    host.innerHTML = head + '<div class="cv-bars">' + compBars(comp, g) + '</div></div>';
    return true;
  }
  // The niche radius only means something once a single nucleus is picked (and no
  // lasso selection is active), so its slider is disabled — with a hint tooltip —
  // until then.
  function updateNicheEnabled() {
    var nk = $('cv-niche'); if (!nk) return;
    var on = (pick != null && !(sel && sel.size));
    nk.disabled = !on;
    var wrap = $('cv-niche-wrap');
    if (wrap) {
      wrap.classList.toggle('cv-disabled', !on);
      wrap.title = on ? '' : 'Click a nucleus in a panel first to set its niche';
    }
  }
  function renderReadout() {
    var host = $('cv-readout'); if (!host) return;
    updateNicheEnabled();
    if (!sel || !sel.size) {
      if (renderNiche(host)) return;   // Trekker: picked-nucleus niche composition
      host.innerHTML = '<div class="cv-empty">Lasso-drag in any panel to select cells. ' +
        'The same cells highlight in every panel, and their composition and ' +
        'top clonotypes appear here.' +
        (D.trekker ? ' <b>Or click a single nucleus</b> to see its niche — the ' +
          'cell-type composition within the radius (µm).' : '') +
        '</div>';
      return;
    }
    var idxs = []; sel.forEach(function (i) { idxs.push(i); });

    // composition by cell_type if present, else the active categorical group,
    // else the first available group. colorBy may be a gene/RGB pseudo-mode that
    // is NOT a real group — falling back to it would crash on g.values below.
    var compGroup = D.groups['cell_type'] ? 'cell_type'
      : (D.groups[colorBy] ? colorBy : Object.keys(D.groups)[0]);
    var g = D.groups[compGroup];
    var comp = {};
    idxs.forEach(function (i) { var lv = g.values[i]; comp[lv] = (comp[lv] || 0) + 1; });
    var compHtml = compBars(comp, g);

    // top clonotypes among the selection (the immune axis in the loop)
    var cloneHtml = '';
    if (D.clone) {
      var cc = {};
      idxs.forEach(function (i) { var ci = D.clone.id[i]; if (ci >= 0) cc[ci] = (cc[ci] || 0) + 1; });
      var crows = Object.keys(cc).map(function (k) { return [parseInt(k, 10), cc[k]]; })
        .sort(function (a, b) { return b[1] - a[1]; }).slice(0, 8);
      var withRcp = idxs.filter(function (i) { return D.clone.id[i] >= 0; }).length;
      if (crows.length) {
        cloneHtml = '<div class="cv-readcol"><h4 class="cv-read-h">Top clonotypes in selection' +
          ' <span class="cv-read-sub">' + fmt(withRcp) + ' of ' + fmt(sel.size) +
          ' carry a receptor</span></h4><table class="cv-ctable"><thead><tr>' +
          '<th>#</th><th>CDR3 (clonotype)</th><th class="num">in sel.</th>' +
          '<th class="num">clone size</th><th class="num">% of sel.</th></tr></thead><tbody>' +
          crows.map(function (r, k) {
            var lab = D.clone.label[r[0]] || '(clone ' + r[0] + ')';
            if (lab.length > 42) lab = lab.slice(0, 40) + '…';
            return '<tr class="cv-crow" data-clone="' + r[0] + '"><td class="num">' + (k + 1) +
              '</td><td class="cv-cdr3">' + lab + '</td><td class="num">' + r[1] +
              '</td><td class="num">' + D.clone.size[r[0]] + '</td><td class="num">' +
              (r[1] / sel.size * 100).toFixed(1) + '%</td></tr>';
          }).join('') + '</tbody></table>' +
          '<div class="cv-hint">Click a clonotype row to select all its cells across every panel.</div></div>';
      } else {
        cloneHtml = '<div class="cv-readcol"><h4 class="cv-read-h">Clonotypes</h4>' +
          '<div class="cv-empty-sm">No receptor-bearing cells in this selection.</div></div>';
      }
    }

    host.innerHTML =
      '<div class="cv-readcol"><h4 class="cv-read-h">Composition <span class="cv-read-sub">by ' +
      groupLabel(compGroup) + '</span></h4><div class="cv-bars">' + compHtml + '</div></div>' + cloneHtml;

    // wire clonotype-row -> select its cells (repertoire selection into the loop)
    Array.prototype.forEach.call(host.querySelectorAll('.cv-crow'), function (tr) {
      tr.onclick = function () {
        var cid = parseInt(tr.getAttribute('data-clone'), 10);
        var s = new Set();
        for (var i = 0; i < D.n; i++) if (D.clone.id[i] === cid) s.add(i);
        clearLassos();   // this selection didn't come from a lasso
        setSelection(s);
      };
    });
  }

  // ---- legend (categorical) / colourbar (gene) ----------------------------
  function renderColorbar(show, maxVal, label, minVal) {
    var C = $('cv-cbar'); if (!C) return;
    C.style.display = show ? 'flex' : 'none';
    if (!show) return;
    var g = $('cv-grad');
    if (g) {
      var stops = [];
      for (var i = 0; i <= 10; i++) {
        var c = viridis(i / 10);
        stops.push('rgb(' + c[0] + ',' + c[1] + ',' + c[2] + ') ' + (i * 10) + '%');
      }
      g.style.background = 'linear-gradient(90deg,' + stops.join(',') + ')';
    }
    var cb0 = $('cv-cb0'); if (cb0) cb0.textContent = (minVal == null ? '0' : (+minVal).toFixed(1));
    var cb1 = $('cv-cb1'); if (cb1) cb1.textContent = (maxVal == null ? '' : (+maxVal).toFixed(1));
    var note = $('cv-cbar-note'); if (note) note.textContent = label || 'expression';
  }
  function renderLegend() {
    var L = $('cv-legend'); if (!L) return;
    L.innerHTML = '';
    // Continuous modes: colourbar for a single gene; nothing for RGB.
    if (colorBy === GENE_MODE) {
      renderColorbar(!!D.gene, D.gene ? D.gene.max : null,
        D.gene ? (D.gene.gene + ' (log-normalised)') : 'select a gene');
      return;
    }
    var fld = fieldOf();
    if (fld) {   // Trekker physical / meta field → viridis colourbar (min..max)
      renderColorbar(true, fld.max, fld.label, fld.min);
      return;
    }
    if (colorBy === RGB_MODE) {
      // Legend for the three additive channels: a colour swatch + the gene that
      // drives it (or "—" if empty). Updates when a channel gene changes.
      renderColorbar(false);
      var chans = [['R', '#e11d1d'], ['G', '#12a150'], ['B', '#2563eb']];
      var genes = (D.rgb && D.rgb.genes) || ['', '', ''];
      chans.forEach(function (ch, k) {
        var gene = genes[k] || '';
        var d = document.createElement('div');
        d.className = 'cv-lg cv-rgb-lg' + (gene ? '' : ' off');
        d.innerHTML = '<span class="cv-dot" style="background:' + ch[1] + '"></span>' +
          '<b style="color:' + ch[1] + '">' + ch[0] + '</b> ' +
          (gene ? gene : '<span class="cv-rgb-none">not set</span>');
        L.appendChild(d);
      });
      return;
    }
    renderColorbar(false);
    var g = D.groups[colorBy];
    if (!g) return;
    var counts = {};
    for (var i = 0; i < D.n; i++) counts[g.values[i]] = (counts[g.values[i]] || 0) + 1;
    g.levels.forEach(function (nm, li) {
      var col = (g.colors && g.colors[li]) || PAL[li % PAL.length];
      var d = document.createElement('div');
      d.className = 'cv-lg' + (hidden.has(li) ? ' off' : '');
      d.innerHTML = '<span class="cv-dot" style="background:' + col + '"></span>' + nm +
        ' <span class="cv-lg-ct">(' + (counts[li] || 0) + ')</span>';
      d.onclick = function () {
        if (hidden.has(li)) hidden.delete(li); else hidden.add(li);
        renderLegend(); drawAll();
      };
      L.appendChild(d);
    });
  }

  // ---- hover tooltip -------------------------------------------------------
  function wireHover(p) {
    var tip = $(p.tipId);
    p.canvas.addEventListener('mousemove', function (e) {
      var r = p.canvas.getBoundingClientRect();
      var mx = e.clientX - r.left, my = e.clientY - r.top;
      if (p.drag) { tip.style.opacity = 0; return; }
      var i = nearest(p, mx, my);
      if (i < 0) { tip.style.opacity = 0; return; }
      var g = D.groups[colorBy];
      var h = '<b>' + (g ? g.levels[g.values[i]] : D.cells[i]) + '</b>';
      if (D.clone && D.clone.id[i] >= 0) {
        var lab = D.clone.label[D.clone.id[i]] || '';
        if (lab.length > 36) lab = lab.slice(0, 34) + '…';
        h += '<br>clone: ' + lab + ' <span style="opacity:.7">(' +
          D.clone.size[D.clone.id[i]] + ' cells)</span>';
      }
      tip.innerHTML = h; tip.style.opacity = 1;
      var tx = p.sx[i] + 12, ty = p.sy[i] - 8;
      if (tx + tip.offsetWidth > p.W) tx = p.sx[i] - tip.offsetWidth - 12;
      tip.style.left = tx + 'px'; tip.style.top = ty + 'px';
    });
    p.canvas.addEventListener('mouseleave', function () { tip.style.opacity = 0; });
  }

  // ---- brush + pick --------------------------------------------------------
  function wireBrush(p) {
    var pos = function (e) {
      var r = p.canvas.getBoundingClientRect();
      return [e.clientX - r.left, e.clientY - r.top];
    };
    p.canvas.addEventListener('mousedown', function (e) {
      // a fresh brush supersedes any committed lasso (this panel's is replaced,
      // the other panel's is dropped)
      panels.forEach(function (o) { if (o !== p) o.lasso = null; });
      p.drag = true; p.moved = false; p.start = pos(e); p.lasso = [p.start];
    });
    p.canvas.addEventListener('mousemove', function (e) {
      if (!p.drag) return;
      var q = pos(e);
      if (selectMode === 'box') {
        // rectangle from the drag origin to the cursor; inPoly treats it as a
        // 4-point polygon, so mouseup selection is unchanged.
        var a = p.start;
        if (Math.abs(q[0] - a[0]) + Math.abs(q[1] - a[1]) > 3) {
          p.lasso = [[a[0], a[1]], [q[0], a[1]], [q[0], q[1]], [a[0], q[1]]];
          p.moved = true; draw(p);
        }
      } else {
        var last = p.lasso[p.lasso.length - 1];
        if (Math.abs(q[0] - last[0]) + Math.abs(q[1] - last[1]) > 3) {
          p.lasso.push(q); p.moved = true; draw(p);
        }
      }
    });
    window.addEventListener('mouseup', function (e) {
      if (!p.drag) return;
      p.drag = false;
      if (!D || !p.ok || !p.sx) { p.lasso = null; return; }  // not projected yet
      var keep = false;
      if (p.moved && p.lasso && p.lasso.length > 2) {
        var s = new Set();
        for (var i = 0; i < D.n; i++) {
          if (p.ok[i] && shown(i) && CBGeom.inPoly(p.sx[i], p.sy[i], p.lasso)) s.add(i);
        }
        // A real lasso selection supersedes any prior single-cell pick — clear it
        // BEFORE setSelection so its drawAll() drops the stale orange pick ring
        // (that ring is not gated on `!sel`). An empty lasso keeps the pick.
        if (s.size) { pick = null; setSelection(s); keep = true; }  // keep the trace
        else { setSelection(null); }
      } else {
        var m = pos(e), k = nearest(p, m[0], m[1]);
        pick = (k >= 0) ? k : null;
        rebuildNiche();              // Trekker: cells within the picked niche
        updateSelActions();          // niche pick → show the (animated) Clear button
        drawAll();
        if (!sel) renderReadout();   // Trekker niche readout for the picked cell
      }
      if (!keep) p.lasso = null;
      draw(p);
    });
  }

  // ---- build panels from DOM ----------------------------------------------
  // The canvas elements persist across dataset switches, so panel objects and
  // their event listeners are created EXACTLY ONCE. Re-wiring on every onData
  // would stack duplicate listeners whose stale closures fire on unprojected
  // panels. Subsequent onData calls reuse these objects; project() resets their
  // per-dataset arrays.
  function buildPanels() {
    if (panels.length) return;               // already built + wired
    // FOUR slots: A = umap, B/C/D take whatever other spaces the data set has.
    // layoutPanels() assigns spaces + hides the unused ones each onData.
    var defs = [
      { key: 'A', canvasId: 'cv-cv-a', tipId: 'cv-tip-a' },
      { key: 'B', canvasId: 'cv-cv-b', tipId: 'cv-tip-b' },
      { key: 'C', canvasId: 'cv-cv-c', tipId: 'cv-tip-c' },
      { key: 'D', canvasId: 'cv-cv-d', tipId: 'cv-tip-d' }
    ];
    defs.forEach(function (d) {
      var cv = $(d.canvasId); if (!cv) return;
      var p = { key: d.key, canvas: cv, ctx: cv.getContext('2d'), tipId: d.tipId,
        pane: cv.parentElement, spaceId: null, W: 0, H: 0, sx: null, sy: null,
        ok: null, lasso: null, drag: false, moved: false, view: null };
      panels.push(p);
      wireHover(p); wireBrush(p);
    });
    // Re-project + redraw whenever the panes gain/lose size — critically, when
    // the tab flips from display:none to visible (0 -> real width).
    resizeObserver = new ResizeObserver(function () {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(function () { if (D) resizeAll(); }, 30);
    });
    panels.forEach(function (p) {
      if (p.canvas.parentElement) resizeObserver.observe(p.canvas.parentElement);
    });
    // Also re-fit when the CHROME above the panels changes height — the "More"
    // panel expanding/collapsing, or the histology-image / selection bars
    // animating in and out — so the squares always fill the remaining viewport
    // in real time, not only after leaving and re-entering the tab.
    ['cv-more', 'coordviews_image_ui', 'cv-selbar'].forEach(function (id) {
      var el = $(id);
      if (el) resizeObserver.observe(el);
    });
  }

  // Panels: A = umap (expression), always. B = a "right candidate" (any non-umap
  // space). Default right space is SMART: the physical map (spatial/Trekker) if it
  // carries a histology image, else the clonal axis, else whichever single
  // candidate exists. When >1 candidate exists, a segmented switch in panel B's
  // header (renderSpaceSwitch) lets the user flip between them — nothing is ever
  // silently dropped, which the old "spatial > clone > last" priority did.
  function rightCandidates() {
    return D.spaces.filter(function (s) { return s.id !== 'umap'; });
  }
  function defaultSpaceFor(key) {
    if (key === 'A') return 'umap';
    var cands = rightCandidates();
    if (!cands.length) return null;
    var phys = cands.filter(function (s) { return s.id === 'spatial'; })[0];
    if (phys && phys.image) return 'spatial';   // tissue image is the compelling view
    var clone = cands.filter(function (s) { return s.id === 'clone'; })[0];
    if (clone) return 'clone';                  // else the differentiating immune axis
    return cands[0].id;
  }
  var panelB = function () {
    var b = null;
    panels.forEach(function (p) { if (p.key === 'B') b = p; });
    return b;
  };
  // Short label for the switch button (the panel title carries the full label).
  function spaceSwitchLabel(s) {
    if (s.id === 'spatial') return s.image ? 'Spatial' : 'Physical';
    if (s.id === 'trekker') return 'Trekker';
    if (s.id === 'clone') return 'Clonal';
    return s.label || s.id;
  }
  // Render the right-panel space switch — one segmented button per candidate,
  // shown only when there are ≥2 (with one, the panel just shows it, as before).
  function renderSpaceSwitch() {
    var host = $('cv-space-switch'); if (!host) return;
    var cands = rightCandidates(), pB = panelB();
    if (cands.length < 2 || !pB) { host.style.display = 'none'; host.innerHTML = ''; return; }
    host.style.display = '';
    host.innerHTML = cands.map(function (s) {
      return '<button type="button" class="cv-seg-btn' +
        (s.id === pB.spaceId ? ' is-on' : '') + '" data-space="' + s.id + '">' +
        spaceSwitchLabel(s) + '</button>';
    }).join('');
  }
  // Coordinate-source / QC / positioning / Moran's I detail for a Trekker data
  // set — same content as the dedicated Trekker page's "Data and QC" +
  // "Moran's I" boxes, built by the shared functions in www/trekker.js
  // (loaded before this file) so the two pages never drift apart.
  function openTrekkerModal() {
    var dlg = $('cv-tk-modal');
    var CT = window.CerebroTrekker;
    if (!dlg || !CT || !D.trekker || !D.trekker.qc) return;
    var q = D.trekker.qc;
    $('cv-tk-stats').innerHTML = CT.buildStatsGrid(q);
    $('cv-tk-postbl').innerHTML = CT.buildPositionTable(q);
    $('cv-tk-salvflag').innerHTML = CT.buildSalvFlag(q);
    $('cv-tk-prov').innerHTML = CT.buildProvenanceDl(q);
    $('cv-tk-rangeflag').innerHTML = CT.buildRangeFlag(q);
    $('cv-tk-morantbl').innerHTML = D.trekker.moran ? CT.buildMoranRows(D.trekker.moran, false) : '';
    dlg.showModal();
  }
  // Flip the right panel to another space. The selection/pick/coordination are
  // keyed on cell index, so they are untouched — the highlighted cells simply
  // re-render in the newly shown space. Lassos are screen-space, so they go stale.
  function setPanelBSpace(id) {
    var pB = panelB();
    if (!pB || pB.spaceId === id || !spaceById[id]) return;
    clearLassos();
    pB.spaceId = id; pB.view = null;
    project(pB);
    var t = $('cv-title-b'), sp = spaceById[id];
    if (t) t.textContent = sp ? sp.label : id;
    updateSpaceScopedControls();
    renderSpaceSwitch();
    drawAll();
  }
  // Controls that belong to a specific right-panel space (the clonal-layout switch,
  // the histology-image bar) are shown only while that space is the one on screen —
  // they used to key off "the dataset HAS this space", which is wrong now that the
  // right panel can switch away from it.
  function updateSpaceScopedControls() {
    if (!D || !D.spaces) return;
    // Every space now has its own always-on panel, so these controls show whenever
    // the data set HAS the space they act on: the clonal-layout switch when a
    // clone panel exists, the histology-image bar when a spatial panel carries an
    // image. revealEl fades them in/out (see .cv-collapse CSS).
    var hasClone = !!spaceById['clone'];
    var hasImg = D.spaces.some(function (s) { return s.image && s.image.uri; });
    revealEl($('cv-clone-layout-ctl'), hasClone);
    revealEl($('coordviews_image_ui'), hasImg);
  }

  // ---- clonal-panel layout (client-side, instant) -------------------------
  // The clone space has abstract axes, so it is re-laid-out here between
  // representations without a server round-trip, using only D.clone (per-cell
  // clone id + per-clone size). No-receptor cells are left NA (not plotted):
  // they have no clonal identity, so they belong in the UMAP/Spatial panels.
  var CLONE_MODE = 'stack';
  var TIER_LAB = ['Single (1)', 'Small (2–5)', 'Medium (6–20)', 'Large (>20)'];
  function tierOf(sz) { return sz > 20 ? 3 : sz > 5 ? 2 : sz > 1 ? 1 : 0; }

  function applyCloneLayout(mode) {
    var sp = spaceById['clone'];
    if (!sp || !D || !D.clone) return;
    CLONE_MODE = mode;
    var n = D.n, id = D.clone.id, size = D.clone.size, i, ci;
    var x = new Float32Array(n), y = new Float32Array(n);
    for (i = 0; i < n; i++) { x[i] = NaN; y[i] = NaN; }

    if (mode === 'bands') {
      // Horizontal bands by expansion tier; band height fixed, band width grows
      // with the number of cells in that tier (so the eye reads tier size).
      var present = {};
      for (i = 0; i < n; i++) { ci = id[i]; if (ci >= 0) present[tierOf(size[ci])] = 1; }
      var tiers = Object.keys(present).map(Number).sort(function (a, b) { return a - b; });
      var band = {}; tiers.forEach(function (t, bi) { band[t] = bi; });
      var count = {};
      for (i = 0; i < n; i++) {
        ci = id[i]; if (ci < 0) continue;
        var b = band[tierOf(size[ci])];
        count[b] = (count[b] || 0) + 1;
        x[i] = count[b] - 1;
        // stable per-cell jitter fills the band strip (0.1 .. 0.9 within band b)
        var h = (Math.imul(i + 1, 2654435761) >>> 0) / 4294967296;
        y[i] = b + 0.1 + h * 0.8;
      }
      sp._axisSpec = {
        xlab: 'cells in tier  (width ∝ count) →',
        ylab: 'clonal expansion tier ↑',
        bands: tiers.map(function (t, bi) {
          return { label: TIER_LAB[t], lo: bi, mid: bi + 0.5 };
        })
      };
    } else { // 'stack' — one column per clonotype, cells stacked within it
      var seen = {};
      for (i = 0; i < n; i++) {
        ci = id[i]; if (ci < 0) continue;
        seen[ci] = (seen[ci] || 0) + 1;
        x[i] = ci;               // clone id is already the size rank (0 = largest)
        y[i] = seen[ci] - 1;
      }
      sp._axisSpec = {
        xlab: 'clonotypes, ranked  (largest at left)',
        ylab: 'cells stacked in clone  (expansion ↑)'
      };
    }
    sp.x = x; sp.y = y; sp.stretch = true; sp._unit = null;
  }

  function setSegOn(mode) {
    var host = $('cv-clone-layout'); if (!host) return;
    Array.prototype.forEach.call(host.querySelectorAll('.cv-seg-btn'), function (b) {
      b.classList.toggle('is-on', b.getAttribute('data-mode') === mode);
    });
  }

  // Colour picker: categorical groups + two special continuous modes. Selecting
  // a gene mode reveals its picker(s); the server answers with the value vector.
  var GENE_MODE = '__gene__', RGB_MODE = '__rgb__', FIELD_PREFIX = '__field__';
  // Trekker continuous field currently selected in "Colour by", or null.
  // Memoised on (D, colorBy): fieldOf() is called per cell inside colorOf()/
  // visible() on the draw hot path, but its result only changes when the dataset
  // or the colour mode changes — so the string work runs once, not ~3n/draw.
  var _fldCacheD = null, _fldCacheKey = null, _fldCacheVal = null;
  function fieldOf() {
    if (_fldCacheD === D && _fldCacheKey === colorBy) return _fldCacheVal;
    var v = null;
    if (D && D.trekker && D.trekker.fields && colorBy &&
      colorBy.indexOf(FIELD_PREFIX) === 0) {
      v = D.trekker.fields[colorBy.slice(FIELD_PREFIX.length)] || null;
    }
    _fldCacheD = D; _fldCacheKey = colorBy; _fldCacheVal = v;
    return v;
  }
  function fillColorPicker() {
    var sel = $('cv-pick-color'); if (!sel) return;
    var opts = Object.keys(D.groups).map(function (g) {
      return '<option value="' + g + '">' + groupLabel(g) + '</option>';
    });
    // Trekker physical / meta fields — continuous colourings of the physical map.
    if (D.trekker && D.trekker.fields) {
      Object.keys(D.trekker.fields).forEach(function (k) {
        opts.push('<option value="' + FIELD_PREFIX + k + '">' +
          D.trekker.fields[k].label + '</option>');
      });
    }
    opts.push('<option value="' + GENE_MODE + '">Gene expression</option>');
    opts.push('<option value="' + RGB_MODE + '">Co-expression (RGB)</option>');
    sel.innerHTML = opts.join('');
    sel.value = colorBy;
    sel.onchange = function () { setColorBy(sel.value); };
  }
  function setColorBy(mode) {
    colorBy = mode; hidden = new Set();
    var geneCtl = $('cv-gene-ctl'), rgbCtl = $('cv-rgb-ctl');
    if (geneCtl) geneCtl.style.display = (mode === GENE_MODE) ? '' : 'none';
    if (rgbCtl) rgbCtl.style.display = (mode === RGB_MODE) ? '' : 'none';
    renderLegend(); drawAll(); renderReadout();
    // RGB mode widens the bar and may push the action buttons onto their own line
    updateSelActionsLayout();
  }

  // ---- projection picker (expression panel) -------------------------------
  // Every projection's coords are in the bundle, so switching is client-side:
  // swap the umap space's x/y, re-normalise, reproject the expression panel(s).
  // The control hides itself when the data set offers only one projection.
  function fillProjPicker() {
    var selEl = $('cv-pick-proj'); if (!selEl) return;
    var names = D.projections ? Object.keys(D.projections) : [];
    var ctl = $('cv-proj-ctl');
    if (ctl) ctl.style.display = (names.length > 1) ? '' : 'none';
    if (!names.length) return;
    selEl.innerHTML = names.map(function (nm) {
      return '<option value="' + nm + '">' + nm + '</option>';
    }).join('');
    selEl.value = curProj || D.default_projection || names[0];
    selEl.onchange = function () { setProjection(selEl.value); };
  }
  function setProjection(name) {
    if (!D || !D.projections || !D.projections[name]) return;
    curProj = name;
    var sp = spaceById['umap']; if (!sp) return;
    var pj = D.projections[name];
    sp.x = pj.x; sp.y = pj.y; sp._unit = null;
    sp.label = name + ' (expression)';
    panels.forEach(function (p) {
      if (p.spaceId !== 'umap') return;
      project(p);
      var t = $('cv-title-' + p.key.toLowerCase());
      if (t) t.textContent = sp.label;
    });
    drawAll();
  }

  // Preload a space's histology image + seed the transform state from its preset.
  // Reused by onData and by the Spatial-sample switch below.
  function loadSpaceImage(space) {
    imgEl = null; imgReady = false;
    if (!space || !space.image || !space.image.uri) return;
    var pr = space.image.preset || {};
    imgState = {
      show: true,
      opacity: (pr.opacity != null ? pr.opacity : 0.6),
      offsetX: (pr.offsetX != null ? pr.offsetX : 0),
      offsetY: (pr.offsetY != null ? pr.offsetY : 0),
      scaleX: (pr.scaleX != null ? pr.scaleX : 1),
      scaleY: (pr.scaleY != null ? pr.scaleY : 1),
      flipX: !!pr.flipX, flipY: !!pr.flipY, rotate: 0
    };
    var im = new Image();
    im.onload = function () { imgReady = true; drawAll(); };
    im.src = space.image.uri;
    imgEl = im;
  }
  // Spatial-sample picker — shown only when the data set has >1 spatial section.
  // Each sample is its own coordinate system + image; all travel in the bundle
  // (spaceById['spatial'].samples), so switching is client-side and instant.
  function fillSpatialPicker() {
    var selEl = $('cv-pick-spatial'), ctl = $('cv-spatial-ctl');
    if (!selEl || !ctl) return;
    var sp = spaceById['spatial'], samples = sp && sp.samples;
    if (!samples || samples.length < 2) {
      ctl.style.display = 'none'; selEl.innerHTML = ''; return;
    }
    ctl.style.display = '';
    selEl.innerHTML = samples.map(function (s) {
      return '<option value="' + s.name + '">' + s.name + '</option>';
    }).join('');
    selEl.value = sp._sampleName || samples[0].name;
    selEl.onchange = function () { setSpatialSample(selEl.value); };
  }
  function setSpatialSample(name) {
    var sp = spaceById['spatial']; if (!sp || !sp.samples) return;
    var s = sp.samples.filter(function (x) { return x.name === name; })[0];
    if (!s) return;
    sp._sampleName = name;
    sp.x = s.x; sp.y = s.y; sp.label = s.label; sp.image = s.image || null;
    sp._unit = null;
    loadSpaceImage(sp);            // the section's own histology image (or none)
    updateSpaceScopedControls();   // image bar visibility follows the new sample
    panels.forEach(function (p) {
      if (p.spaceId !== 'spatial') return;
      project(p);
      var t = $('cv-title-' + p.key.toLowerCase());
      if (t) t.textContent = sp.label;
    });
    drawAll();
  }

  // ---- group filters (client-side cell subsetting) ------------------------
  // One collapsible chip per categorical group; unchecking a level drops those
  // cells from every panel and the selection. Reuses the bundle's group values
  // (no server round-trip), the same set the Overview page filters on.
  function renderGroupFilters() {
    var host = $('cv-filters-row'); if (!host) return;
    host.innerHTML = '';
    Object.keys(D.groups).forEach(function (gname) {
      var g = D.groups[gname];
      if (!g || !g.levels || !g.levels.length) return;
      var allowed = groupFilter[gname];
      var nsel = allowed ? allowed.size : g.levels.length;
      var items = g.levels.map(function (nm, li) {
        var on = !allowed || allowed.has(li);
        var col = (g.colors && g.colors[li]) || PAL[li % PAL.length];
        return '<label class="cv-filt-item"><input type="checkbox" data-lv="' + li +
          '"' + (on ? ' checked' : '') + '><span class="cv-dot" style="background:' +
          col + '"></span>' + nm + '</label>';
      }).join('');
      var wrap = document.createElement('div');
      wrap.className = 'cv-filt';
      wrap.setAttribute('data-group', gname);
      wrap.innerHTML = '<button type="button" class="cv-filt-btn">' + groupLabel(gname) +
        ' <span class="cv-filt-ct">' + nsel + '/' + g.levels.length +
        '</span></button><div class="cv-filt-menu" style="display:none">' +
        '<div class="cv-filt-acts"><button type="button" data-act="all">All</button>' +
        '<button type="button" data-act="none">None</button></div>' + items + '</div>';
      host.appendChild(wrap);
    });
  }
  function readFilter(wrap) {
    var gname = wrap.getAttribute('data-group');
    var boxes = wrap.querySelectorAll('.cv-filt-menu input[type=checkbox]');
    var chosen = new Set(), total = boxes.length, k = 0;
    Array.prototype.forEach.call(boxes, function (b) {
      if (b.checked) { chosen.add(parseInt(b.getAttribute('data-lv'), 10)); k++; }
    });
    if (k >= total) delete groupFilter[gname];   // all levels on = no filter
    else groupFilter[gname] = chosen;
    var ct = wrap.querySelector('.cv-filt-ct');
    if (ct) ct.textContent = (groupFilter[gname] ? chosen.size : total) + '/' + total;
    applyActiveChange();
  }

  // Read the client-owned histology-image controls into imgState. Missing
  // controls (before render / no image) leave the current value unchanged.
  function syncImgControls() {
    var num = function (id, cur) { var el = $(id); return el ? parseFloat(el.value) : cur; };
    var chk = function (id, cur) { var el = $(id); return el ? el.checked : cur; };
    imgState.show = chk('cv-img-show', imgState.show);
    imgState.opacity = num('cv-img-opacity', imgState.opacity);
    imgState.offsetX = num('cv-img-offx', imgState.offsetX);   // data units
    imgState.offsetY = num('cv-img-offy', imgState.offsetY);   // data units
    var sc = num('cv-img-scale', imgState.scaleX);
    imgState.scaleX = sc; imgState.scaleY = sc;
    imgState.rotate = num('cv-img-rotate', imgState.rotate);
    imgState.flipX = chk('cv-img-flipx', imgState.flipX);
    imgState.flipY = chk('cv-img-flipy', imgState.flipY);
  }

  // Spaces in panel order: umap first, then spatial / trekker / clone (present
  // ones), then anything else. Panel A gets order[0] (umap), B/C/D the rest.
  function orderedSpaces() {
    var pref = ['umap', 'spatial', 'trekker', 'clone'], out = [];
    pref.forEach(function (id) { if (spaceById[id]) out.push(id); });
    D.spaces.forEach(function (s) { if (out.indexOf(s.id) < 0) out.push(s.id); });
    return out;
  }
  // Assign each present space to a panel; hide the unused slots; surface the
  // Trekker info button on whichever panel holds the Trekker space.
  // Fade a pane in from transparent (the .cv-pane opacity transition does the
  // rest) when it appears on a data-set switch, so new panels don't just pop.
  function fadeInPane(el) {
    el.style.opacity = '0';
    requestAnimationFrame(function () {
      requestAnimationFrame(function () { el.style.opacity = ''; });
    });
  }
  function layoutPanels() {
    var order = orderedSpaces();
    panels.forEach(function (p, i) {
      if (i < order.length) {
        var reappearing = p.pane && p.pane.classList.contains('cv-hidden');
        p.spaceId = order[i]; p.view = null; p.lasso = null;
        if (p.pane) {
          p.pane.classList.remove('cv-hidden');
          if (reappearing) fadeInPane(p.pane);
        }
      } else {
        p.spaceId = null;
        if (p.pane) p.pane.classList.add('cv-hidden');
      }
    });
    // Trekker info button lives on the Trekker panel only.
    var showTk = !!(D.trekker && D.trekker.qc);
    panels.forEach(function (p) {
      var btn = $('cv-tk-info-' + p.key.toLowerCase());
      if (btn) btn.style.display = (showTk && p.spaceId === 'trekker') ? '' : 'none';
    });
  }
  // Size ALL visible panels to equal squares that fill the width AND height in a
  // single viewport (so every linked panel is on-screen at once), never below
  // 300px. The grid is 1x2 (two spaces), a rotated "品" (three: one centred square
  // left + two stacked right) or 2x2 (four). Falls back to a single column when
  // two >=300 columns can't fit the width.
  function resizeAll() {
    if (!D || !panels.length) return;
    var panes = panels[0].pane && panels[0].pane.parentElement;
    if (!panes) return;
    var vis = panels.filter(function (p) { return p.spaceId; });
    var k = vis.length;
    if (!k) return;
    var availW = panes.clientWidth;
    if (availW < 20) return;                 // tab still hidden; observer re-runs
    var gap = 14;
    // Per-pane non-canvas overhead (title row + the pane's padding/border/margin),
    // measured so the square maths accounts for it rather than guessing.
    var headEl = vis[0].pane.querySelector('.cv-pane-head');
    var overhead = (headEl ? headEl.offsetHeight : 26) + 34;
    // Available height: from the panels' top to the viewport bottom, minus the
    // legend (or colourbar) so it is never pushed off-screen, minus a small gap.
    var top = panes.getBoundingClientRect().top;
    var legend = $('cv-legend'), cbar = $('cv-cbar');
    var legendH = (legend ? legend.offsetHeight : 0);
    if (cbar && cbar.style.display !== 'none') legendH += cbar.offsetHeight;
    var availH = window.innerHeight - top - legendH - 20;
    // Each pane is border-box with 12px padding + 1px border, so its CONTENT (the
    // square canvas) is 26px narrower than its column track. Subtract that so the
    // canvas fits its pane exactly (never overflows), and make the track = square
    // + chrome. `overhead` already carries the vertical equivalent.
    var chromeX = 26;
    // Layout: two columns unless a single column is forced by a narrow viewport.
    var single = availW < ((300 + chromeX) * 2 + gap);
    var cols = single ? 1 : 2;
    var rows = single ? k : (k <= 2 ? 1 : 2);
    var colW = (availW - (cols - 1) * gap) / cols;
    var rowCanvasH = (availH - rows * overhead - (rows - 1) * gap) / rows;
    var side = Math.max(300, Math.floor(Math.min(colW - chromeX, rowCanvasH)));
    // Explicit column tracks (px) so cells hug the squares and the grid centres in
    // availW; rows stay auto (each pane = head + square), so the "品" span works.
    panes.classList.remove('cv-n2', 'cv-n3', 'cv-n4', 'cv-single');
    panes.classList.add(single ? 'cv-single' : ('cv-n' + k));
    var col = []; for (var c = 0; c < cols; c++) col.push((side + chromeX) + 'px');
    panes.style.gridTemplateColumns = col.join(' ');
    panes.style.gridTemplateRows = '';
    vis.forEach(function (p) { resizePanelSquare(p, side); });
    drawAll();
  }

  // ---- receive the bundle --------------------------------------------------
  function onData(bundle) {
    D = bundle;
    if (!D || !D.spaces || !D.spaces.length) return;
    spaceById = {}; D.spaces.forEach(function (s) { s._unit = null; spaceById[s.id] = s; });
    colorBy = D.default_group || Object.keys(D.groups)[0];
    hidden = new Set(); sel = null; pick = null;
    // Reset the additional-parameter state to defaults for the new dataset.
    curProj = D.default_projection ||
      (D.projections ? Object.keys(D.projections)[0] : null);
    pctShow = 100; pctMask = null; groupFilter = {}; pointOpacity = 0.8;
    var opEl = $('cv-opacity'); if (opEl) opEl.value = '0.8';
    var opLbl = $('cv-op-val'); if (opLbl) opLbl.textContent = '0.80';
    var pctEl = $('cv-pct'); if (pctEl) pctEl.value = '100';
    var pctLbl = $('cv-pct-val'); if (pctLbl) pctLbl.textContent = '100';
    var moreP = $('cv-more'); if (moreP) moreP.style.display = 'none';
    // Trekker controls reset
    dissolvePct = 0; dissolveThresh = null; evidenceOn = false; nicheRadius = 250;
    nicheSet = null;
    var dsEl = $('cv-dissolve'); if (dsEl) dsEl.value = '0';
    var dsLbl = $('cv-dissolve-val'); if (dsLbl) dsLbl.textContent = '0';
    var evEl = $('cv-evidence'); if (evEl) evEl.checked = false;
    var nkEl = $('cv-niche'); if (nkEl) nkEl.value = '250';
    var nkLbl = $('cv-niche-val'); if (nkLbl) nkLbl.textContent = '250';

    // Preload the histology image, if any spatial space carries one, and seed
    // the transform state from its preset (e.g. flipY for some platforms).
    var withImg = null;
    D.spaces.forEach(function (s) { if (s.image && s.image.uri) withImg = s; });
    loadSpaceImage(withImg);

    var meta = $('cv-meta');
    if (meta) {
      var spaceLabels = D.spaces.map(function (s) { return s.label; }).join(' · ');
      meta.innerHTML = fmt(D.n) + ' cells · ' + D.spaces.length +
        ' linked spaces (' + spaceLabels + ')' +
        (D.clone ? ' · ' + fmt(D.clone.n_receptor) + ' receptor-bearing cells, ' +
          fmt(D.clone.n_clones) + ' clonotypes' : '');
    }

    buildPanels();
    // Give every present space its own panel and hide the unused slots (this also
    // places the Trekker info button on the Trekker panel).
    layoutPanels();
    zoomed = false; updateZoomBtn(); syncModeButtons();
    updateSelActions();   // hide the Zoom / Clear group until a selection exists
    // Immune axis present → reveal the clonal-layout switch and lay out the
    // clone space (client-owned) before the first projection runs.
    var hasClone = !!spaceById['clone'];
    if (hasClone) { setSegOn('stack'); applyCloneLayout('stack'); }
    // Show the space-scoped controls that match the spaces now on screen.
    updateSpaceScopedControls();
    // Trekker controls (dissolve + evidence) appear only when the bundle has them
    var hasTk = !!(D.trekker && (D.trekker.conf || D.trekker.evidence));
    var tkc = $('cv-trekker-ctl');
    if (tkc) tkc.style.display = hasTk ? '' : 'none';
    // positioning-evidence markers default ON when the data set carries them
    evidenceOn = !!(D.trekker && D.trekker.evidence);
    var evChk = $('cv-evidence'); if (evChk) evChk.checked = evidenceOn;
    fillColorPicker();
    fillProjPicker();
    fillSpatialPicker();
    renderGroupFilters();
    // hide the gene/RGB pickers on a fresh dataset (starts in a categorical mode)
    var geneCtl = $('cv-gene-ctl'), rgbCtl = $('cv-rgb-ctl');
    if (geneCtl) geneCtl.style.display = 'none';
    if (rgbCtl) rgbCtl.style.display = 'none';
    panels.forEach(function (p) {
      var t = $('cv-title-' + p.key.toLowerCase());
      if (t) { var sp = spaceById[p.spaceId]; t.textContent = sp ? sp.label : p.spaceId; }
    });
    renderLegend();
    resizeAll();
    renderSelbar(); renderReadout();
    // Clear the server-side selection too: sel was reset to null above, but the
    // server input still holds the PREVIOUS dataset's barcodes until we push. The
    // "selected cells" plot/table (server-rendered) key off it, so without this
    // they'd show stale/wrong cells after a dataset switch.
    reportSelection();
    positionAllRangeVals();
  }

  // ---- boot ----------------------------------------------------------------
  // All controls are client-owned (cv- ids), so no shiny:inputchanged is needed.
  // The only server touchpoints: receive the bundle, and signal readiness once
  // the session is actually connected (setInputValue is not callable before).
  function boot() {
    if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) return;
    Shiny.addCustomMessageHandler('coordviews_data', onData);

    // Single-gene expression vector (0-255) for the current gene.
    Shiny.addCustomMessageHandler('coordviews_geneval', function (m) {
      if (!D || !m || !m.ok) return;
      D.gene = { gene: m.gene, v: m.v, max: m.max };
      if (colorBy === GENE_MODE) { renderLegend(); drawAll(); }
    });
    // Three 0-255 channels for RGB co-expression.
    Shiny.addCustomMessageHandler('coordviews_rgbval', function (m) {
      if (!D || !m || !m.ok) return;
      D.rgb = { r: m.r, g: m.g, b: m.b, genes: m.genes };
      if (colorBy === RGB_MODE) { renderLegend(); drawAll(); }
    });

    var ping = function () {
      if (Shiny.setInputValue) {
        Shiny.setInputValue('coordviews_ready', Date.now(), { priority: 'event' });
      }
    };
    var jq = window.jQuery;
    if (jq) { jq(document).on('shiny:connected', ping); }
    else { document.addEventListener('shiny:connected', ping); }

    // clear button + point-size slider live in the top bar (client-owned)
    document.addEventListener('click', function (e) {
      var t = e.target;
      // per-panel modebar: select mode (box/lasso), zoom in/out, reset, PNG
      var tb = t && t.closest && t.closest('.cv-tbtn');
      if (tb) {
        var act = tb.getAttribute('data-act'), key = tb.getAttribute('data-panel');
        var pp = null;
        panels.forEach(function (p) { if (p.key === key) pp = p; });
        if (act === 'box' || act === 'lasso') { selectMode = act; syncModeButtons(); return; }
        if (act === 'trekker-info') { openTrekkerModal(); return; }
        if (pp) {
          if (act === 'png') { downloadPanelPNG(pp); }
          else if (act === 'zin') { zoomStep(pp, 0.8); }
          else if (act === 'zout') { zoomStep(pp, 1.25); }
          else if (act === 'reset') {
            if (pp.view) { pp.view = null; project(pp); }
            if (pp.spaceId === 'umap') { zoomed = false; updateZoomBtn(); }
            clearLassos(); drawAll();
          }
        }
        return;
      }
      if (t && t.id === 'cv-zoom') { toggleZoom(); return; }
      if (t && t.id === 'cv-clear') { pick = null; clearLassos(); setSelection(null); return; }
      // "More" panel toggle
      if (t && t.id === 'cv-more-btn') {
        var mp = $('cv-more');
        if (mp) mp.style.display = (mp.style.display === 'none') ? '' : 'none';
        return;
      }
      // clonal-layout segmented toggle: recompute the clone space + reproject
      var seg = t && t.closest && t.closest('#cv-clone-layout .cv-seg-btn');
      if (seg) {
        var mode = seg.getAttribute('data-mode');
        if (mode === CLONE_MODE) return;
        setSegOn(mode); applyCloneLayout(mode);
        panels.forEach(function (p) { if (p.spaceId === 'clone') project(p); });
        drawAll();
        return;
      }
      // right-panel space switch: flip panel B between physical / clonal
      var ssw = t && t.closest && t.closest('#cv-space-switch .cv-seg-btn');
      if (ssw) { setPanelBSpace(ssw.getAttribute('data-space')); return; }
      // group-filter chip: open/close its level menu
      var fbtn = t && t.closest && t.closest('.cv-filt-btn');
      if (fbtn) {
        var menu = fbtn.parentElement.querySelector('.cv-filt-menu');
        if (menu) menu.style.display = (menu.style.display === 'none') ? '' : 'none';
        return;
      }
      // group-filter All / None
      var act = t && t.closest && t.closest('.cv-filt-acts button');
      if (act) {
        var wrap = act.closest('.cv-filt'), on = act.getAttribute('data-act') === 'all';
        Array.prototype.forEach.call(
          wrap.querySelectorAll('.cv-filt-menu input[type=checkbox]'),
          function (b) { b.checked = on; }
        );
        readFilter(wrap);
        return;
      }
    });
    document.addEventListener('input', function (e) {
      var id = e.target && e.target.id;
      if (id === 'cv-ps') {
        ps = +e.target.value;
        var lbl = $('cv-ps-val'); if (lbl) lbl.textContent = (+e.target.value).toFixed(1);
        positionRangeVal('cv-ps', 'cv-ps-val');
        drawAll();
      } else if (id === 'cv-opacity') {
        pointOpacity = +e.target.value;
        var ol = $('cv-op-val'); if (ol) ol.textContent = pointOpacity.toFixed(2);
        positionRangeVal('cv-opacity', 'cv-op-val');
        drawAll();
      } else if (id === 'cv-pct') {
        pctShow = +e.target.value;
        var pl = $('cv-pct-val'); if (pl) pl.textContent = pctShow;
        positionRangeVal('cv-pct', 'cv-pct-val');
        rebuildPctMask(); applyActiveChange();
      } else if (id === 'cv-dissolve') {
        dissolvePct = +e.target.value;
        var dl = $('cv-dissolve-val'); if (dl) dl.textContent = dissolvePct;
        positionRangeVal('cv-dissolve', 'cv-dissolve-val');
        rebuildDissolve(); applyActiveChange();
      } else if (id === 'cv-niche') {
        nicheRadius = +e.target.value;
        var nl = $('cv-niche-val'); if (nl) nl.textContent = nicheRadius;
        positionRangeVal('cv-niche', 'cv-niche-val');
        rebuildNiche();               // resize the highlighted niche + circle
        drawAll();
        if (!sel) renderReadout();     // recompute the niche of the picked cell
      } else if (id && id.indexOf('cv-img-') === 0) {
        syncImgControls(); drawAll();
      }
    });
    document.addEventListener('change', function (e) {
      var id = e.target && e.target.id;
      if (id && id.indexOf('cv-img-') === 0) { syncImgControls(); drawAll(); return; }
      if (id === 'cv-evidence') { evidenceOn = e.target.checked; drawAll(); return; }
      // a group-filter level checkbox toggled
      var fwrap = e.target && e.target.closest && e.target.closest('.cv-filt');
      if (fwrap && e.target.matches && e.target.matches('.cv-filt-menu input[type=checkbox]')) {
        readFilter(fwrap);
      }
    });
    window.addEventListener('resize', function () {
      clearLassos();   // screen-space lasso no longer matches the reprojected points
      if (D) resizeAll();
      updateSelActionsLayout();
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else { boot(); }
})();
