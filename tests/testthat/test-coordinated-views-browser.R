# test-coordinated-views-browser.R — interaction regressions for Linked views.
#
# test-coordinated-views.R covers the bundle builders. Everything here exists
# only in the browser: viewport state, popover dismissal, escaping, the reveal
# of conditional UI. Every case below is a regression that actually shipped and
# that the ~8,000 unit assertions could not see, because none of it is reachable
# without a DOM.
#
# Most cases drive a SYNTHETIC bundle pushed straight at the client instead of a
# demo .crb. That is deliberate: it pins the exact geometry a case needs — two
# projections whose points do not overlap, a column with more levels than a
# legend can hold, a name containing markup — rather than hoping a demo happens
# to contain it. `Shiny.shinyapp.dispatchMessage` is Shiny's own inbound-message
# entry point; it is internal API, so if a Shiny upgrade ever breaks these, the
# fix is here and not in the app.

library(shinytest2)

inst_dir <- system.file(package = "CerebroNexus")
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

## Open the app on the Linked views tab, ready for a bundle.
cv_app <- function(name) {
  app <- AppDriver$new(
    inst_dir,
    name = name,
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]') !== null",
    timeout = 30000
  )
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_idle(timeout = 20000)
  app
}

## JS that builds a synthetic bundle and hands it to the client. `extra` is
## merged over the defaults, so each test states only what it cares about.
cv_bundle_js <- function(extra = "{}", n = 800) {
  paste0(
    "(function () {\n",
    "  var n = ",
    n,
    ";\n",
    "  var blob = function (c) { var a = new Array(n);\n",
    "    for (var i = 0; i < n; i++) a[i] = c + (Math.random() - 0.5) * 2;\n",
    "    return a; };\n",
    "  var cells = [], vals = [];\n",
    "  for (var i = 0; i < n; i++) { cells.push('c' + i); vals.push(i % 3); }\n",
    "  var b = {\n",
    "    cells: cells, n: n,\n",
    "    groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],\n",
    "      colors: ['#636EFA', '#EF553B', '#00CC96'] } },\n",
    "    cat_extra: {}, cat_skipped: {}, fields: {},\n",
    "    default_group: 'cluster',\n",
    "    projections: { umap: { x: blob(0), y: blob(0), ndim: 2 } },\n",
    "    default_projection: 'umap',\n",
    "    spaces: [{ id: 'umap', label: 'umap (expression)',\n",
    "      x: blob(0), y: blob(0) }],\n",
    "    clone: null, trekker: null\n",
    "  };\n",
    "  var extra = ",
    extra,
    ";\n",
    "  for (var k in extra) b[k] = extra[k];\n",
    "  Shiny.shinyapp.dispatchMessage(JSON.stringify(\n",
    "    { custom: { coordviews_data: b } }));\n",
    "})();"
  )
}

## Fraction of the canvas that has something drawn on it, 0-100. The measure the
## "view left on empty space" regressions are about: a panel can be perfectly
## valid and still show nothing.
cv_ink_js <- function(canvas_id = "cv-cv-a") {
  paste0(
    "(function () {\n",
    "  var cv = document.getElementById('",
    canvas_id,
    "');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var k = 0, s = 0;\n",
    "  for (var i = 0; i < d.length; i += 4 * 17) {\n",
    "    s++;\n",
    "    if (d[i + 3] > 0 && (d[i] < 240 || d[i+1] < 240 || d[i+2] < 240)) k++;\n",
    "  }\n",
    "  return Math.round(k / s * 1000) / 10;\n",
    "})();"
  )
}


test_that("the tab renders a pushed bundle and offers every meta column", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_render")

  # A data set whose meta data spans all three colouring sources: registered
  # groups, an unregistered categorical column, a numeric column, and one with
  # too many levels to colour by.
  app$run_js(cv_bundle_js(
    paste0(
      "{ cat_extra: { orig_ident: { values: vals.map(function (v) ",
      "{ return v % 2; }), levels: ['s1', 's2'], ",
      "colors: ['#111111', '#222222'] } },",
      " cat_skipped: { barcode_like: 791 },",
      " fields: { 'meta:nUMI': { label: 'nUMI', v: vals.map(function (v) ",
      "{ return v * 400; }), min: 100, max: 9000, scale: 1000 } } }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('800 cells') >= 0",
    timeout = 15000
  )

  # every source reaches the picker, and the uncolourable column is listed
  # (disabled) rather than silently dropped
  opts <- app$get_js(
    paste0(
      "Array.from(document.getElementById('cv-pick-color').options)",
      ".map(function (o) { return (o.disabled ? 'x:' : '') + o.textContent; });"
    )
  )
  expect_true(any(opts == "cluster"))
  expect_true(any(opts == "orig_ident"))
  expect_true(any(opts == "nUMI"))
  expect_true(any(grepl("^x:barcode_like", opts)))
  expect_true(any(opts == "Gene expression"))

  # the panel actually drew something
  expect_gt(app$get_js(cv_ink_js()), 1)

  app$stop()
})


test_that("switching projection resets the viewport and the lasso", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_projection_reset")

  # Two projections whose points do not overlap AT ALL: a viewport kept from one
  # necessarily lands on empty space in the other, which is exactly the failure.
  app$run_js(cv_bundle_js(
    paste0(
      "{ projections: { umap: { x: blob(-9), y: blob(-9), ndim: 2 },",
      " tsne: { x: blob(9), y: blob(9), ndim: 2 } },",
      " spaces: [{ id: 'umap', label: 'umap (expression)',",
      " x: blob(-9), y: blob(-9) }] }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-pick-proj').options.length === 2",
    timeout = 15000
  )

  # zoom deep into a corner, and leave a committed lasso behind
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  for (var k = 0; k < 8; k++) cv.dispatchEvent(new WheelEvent('wheel',\n",
      "    { clientX: r.left + 110, clientY: r.top + 380, deltaY: -100,\n",
      "      bubbles: true, cancelable: true }));\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 60, clientY: r.top + 60, bubbles: true }));\n",
      "  for (var s = 40; s <= 200; s += 40)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 60 + s, clientY: r.top + 60 + s,\n",
      "        bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  # the minimap only shows while a view is set, so it is the state's readout
  app$wait_for_js(
    "document.getElementById('cv-mini-a').classList.contains('is-on')",
    timeout = 10000
  )

  app$run_js(
    paste0(
      "(function () { var s = document.getElementById('cv-pick-proj');\n",
      "  s.value = 'tsne'; s.dispatchEvent(new Event('change')); })();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-title-a').textContent.indexOf('tsne') >= 0",
    timeout = 10000
  )

  # view cleared -> minimap gone, and the panel is full of data again rather
  # than showing the old viewport's (now empty) corner
  expect_false(
    app$get_js(
      "document.getElementById('cv-mini-a').classList.contains('is-on');"
    )
  )
  expect_gt(app$get_js(cv_ink_js()), 5)

  app$stop()
})


test_that("group-filter menus open, exclude each other, and dismiss", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_filter_menus")

  app$run_js(cv_bundle_js(
    paste0(
      "{ groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],",
      " colors: ['#636EFA', '#EF553B', '#00CC96'] },",
      " sample: { values: vals.map(function (v) { return (v + 1) % 3; }),",
      " levels: ['s1', 's2', 's3'],",
      " colors: ['#111111', '#222222', '#333333'] } } }"
    )
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-filt-btn').length === 2",
    timeout = 15000
  )

  # The row folds away when closed, so its overflow clip used to swallow these
  # menus entirely: they opened, painted outside the clip, and could not be hit.
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(
    "document.querySelector('.cv-more-clip').classList.contains('is-clear')",
    timeout = 10000
  )

  # Expressions, not statements: these get wrapped in `(...) === n` for
  # wait_for_js as well as read directly, and a trailing `;` breaks the wrap.
  open_count <- paste0(
    "Array.from(document.querySelectorAll('.cv-filt-menu'))",
    ".filter(function (m) { return getComputedStyle(m).display !== 'none'; })",
    ".length"
  )
  lit_count <- "document.querySelectorAll('.cv-filt-btn.is-open').length"
  # is the menu genuinely hit-testable, or merely display:block somewhere off
  # in a clipped region? elementFromPoint is the only honest answer.
  hittable <- paste0(
    "(function () {\n",
    "  var m = document.querySelector('.cv-filt-menu');\n",
    "  if (!m || getComputedStyle(m).display === 'none') return false;\n",
    "  var r = m.getBoundingClientRect();\n",
    "  var el = document.elementFromPoint(r.left + 20, r.top + 12);\n",
    "  return !!(el && m.contains(el));\n",
    "})();"
  )

  app$run_js("document.querySelectorAll('.cv-filt-btn')[0].click();")
  app$wait_for_js(paste0("(", open_count, ") === 1"), timeout = 8000)
  expect_true(app$get_js(hittable))
  expect_equal(app$get_js(lit_count), 1)

  # opening the second must REPLACE the first, not add to it
  app$run_js("document.querySelectorAll('.cv-filt-btn')[1].click();")
  app$wait_for_js(
    paste0(
      "document.querySelectorAll('.cv-filt-btn')[1]",
      ".classList.contains('is-open')"
    ),
    timeout = 8000
  )
  expect_equal(app$get_js(open_count), 1)
  expect_equal(app$get_js(lit_count), 1)

  # ticking a level inside the menu must not close it, and must filter
  app$run_js(
    paste0(
      "document.querySelectorAll('.cv-filt-menu')[1]",
      ".querySelectorAll('.cv-filt-item')[0].click();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-shown').textContent.indexOf('showing') >= 0",
    timeout = 8000
  )
  expect_equal(app$get_js(open_count), 1)

  # a click outside dismisses it — closing only via the chip made it a trap
  app$run_js("document.querySelector('.cv-meta').click();")
  app$wait_for_js(paste0("(", open_count, ") === 0"), timeout = 8000)
  expect_equal(app$get_js(lit_count), 0)

  app$stop()
})


test_that("values from the data set cannot inject markup", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_escaping")

  # The payload increments a counter, so "did it run" is a number rather than a
  # judgement about what the DOM looks like.
  app$run_js("window.__cvXss = 0;")
  app$run_js(cv_bundle_js(
    paste0(
      "(function () {\n",
      "  var evil = '<img src=x onerror=\"window.__cvXss=",
      "(window.__cvXss||0)+1\">';\n",
      "  var g = {}; g['grp' + evil] = { values: vals,\n",
      "    levels: ['lvl' + evil, 'ok', 'fine'],\n",
      "    colors: ['#636EFA', '#EF553B', '#00CC96'] };\n",
      "  var sk = {}; sk['ident' + evil] = 1203;\n",
      "  var pr = {}; pr['proj' + evil] = { x: blob(0), y: blob(0), ndim: 2 };\n",
      "  return { groups: g, cat_skipped: sk, projections: pr,\n",
      "    default_group: 'grp' + evil, default_projection: 'proj' + evil };\n",
      "})()"
    )
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-filt-btn').length === 1",
    timeout = 15000
  )
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(
    "document.querySelector('.cv-more-clip').classList.contains('is-clear')",
    timeout = 10000
  )
  app$run_js("document.querySelectorAll('.cv-filt-btn')[0].click();")
  app$wait_for_js(
    "document.querySelectorAll('.cv-filt-item').length === 3",
    timeout = 8000
  )

  expect_equal(app$get_js("window.__cvXss;"), 0)
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.coordviews-page img[src=\"x\"]').length;"
    ),
    0
  )
  # shown literally, tags and all
  expect_true(app$get_js(
    paste0(
      "document.querySelector('.cv-filt-item').textContent",
      ".indexOf('<img') >= 0;"
    )
  ))

  app$stop()
})


test_that("a 3-D embedding can be rotated, and a 2-D one cannot", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_three_d")

  # Sparse on purpose: 60 points per blob, spread so they do not overlap. A
  # dense cloud hides the depth cue — points grow into each other and the pixel
  # count stops tracking their area, which is what made an early measurement of
  # this read 1.03x when the sizes really did differ.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var per = 60, n = per * 3;\n",
      "  var x = [], y = [], z = [], cells = [], vals = [];\n",
      "  for (var k = 0; k < 3; k++)\n",
      "    for (var j = 0; j < per; j++) {\n",
      "      x.push((k - 1) * 9 + (j % 8) * 0.9);\n",
      "      y.push(Math.floor(j / 8) * 1.2 - 4);\n",
      "      z.push((k - 1) * 6);\n",
      "      cells.push('c' + (k * per + j)); vals.push(k);\n",
      "    }\n",
      "  var flat = x.map(function (v) { return v; });\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],\n",
      "        colors: ['#636EFA', '#EF553B', '#00CC96'] } },\n",
      "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap_3D: { x: x, y: y, z: z, ndim: 3 },\n",
      "        umap_2D: { x: flat, y: y, ndim: 2 } },\n",
      "      default_projection: 'umap_3D',\n",
      "      spaces: [{ id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
      "        x: x, y: y, z: z }],\n",
      "      clone: null, trekker: null\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-pick-proj').options.length === 2",
    timeout = 15000
  )

  # the third dimension is announced, and the tool to use it is offered
  expect_match(
    app$get_js("document.getElementById('cv-title-a').textContent"),
    "3-D"
  )
  # Exactly three components reads as "3-D"; more than three has to say that
  # only the first three are drawn, or a 50-component PCA would claim a view
  # nothing here provides.
  expect_equal(
    app$get_js(
      paste0(
        "Array.from(document.getElementById('cv-pick-proj').options)",
        ".map(function (o) { return o.textContent; })"
      )
    ),
    list("umap_3D (3-D)", "umap_2D")
  )
  orbit_shown <- paste0(
    "getComputedStyle(document.querySelector(",
    "'.cv-pane:first-child .cv-orbit-btn')).display !== 'none'"
  )
  expect_true(app$get_js(orbit_shown))

  # Depth reads as size: same number of points per blob, so pixels per colour
  # measures how large each is drawn. Unrotated, z is the depth directly.
  blob_px <- paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var blue = 0, teal = 0;\n",
    "  for (var i = 0; i < d.length; i += 4) {\n",
    "    if (d[i + 3] === 0) continue;\n",
    "    var R = d[i], G = d[i+1], B = d[i+2];\n",
    "    if (B > 180 && R < 140 && G < 140) blue++;\n",
    "    else if (G > 170 && R < 110 && B > 110 && B < 200) teal++;\n",
    "  }\n",
    "  return [blue, teal];\n",
    "})()"
  )
  px <- app$get_js(blob_px)
  expect_gt(max(unlist(px)) / min(unlist(px)), 1.6)

  # dragging with the rotate tool must actually turn it
  centroid <- paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var sx = 0, sy = 0, k = 0;\n",
    "  for (var yy = 0; yy < cv.height; yy += 4)\n",
    "    for (var xx = 0; xx < cv.width; xx += 4) {\n",
    "      var i = (yy * cv.width + xx) * 4;\n",
    "      if (d[i] < 100 && d[i+1] > 180 && d[i+2] > 130) {\n",
    "        sx += xx; sy += yy; k++; }\n",
    "    }\n",
    "  return k ? [Math.round(sx / k), Math.round(sy / k)] : null;\n",
    "})()"
  )
  before <- unlist(app$get_js(centroid))
  app$run_js(
    paste0(
      "(function () {\n",
      "  document.querySelector(",
      "'.cv-tbtn[data-act=\"orbit\"][data-panel=\"A\"]').click();\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 260, clientY: r.top + 260, bubbles: true }));\n",
      "  for (var s = 20; s <= 160; s += 20)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 260 + s, clientY: r.top + 260,\n",
      "        bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  after <- unlist(app$get_js(centroid))
  expect_gt(sqrt(sum((after - before)^2)), 20)

  # reset returns it to the starting angle. Compared with a tolerance because
  # the centroid is sampled off the canvas on a 4px lattice, so it carries a
  # pixel of rounding — this is "back where it was", not "bit-identical".
  app$run_js(
    "document.querySelector('.cv-tbtn[data-act=\"reset\"][data-panel=\"A\"]').click();"
  )
  app$wait_for_idle(timeout = 5000)
  back <- unlist(app$get_js(centroid))
  expect_lt(sqrt(sum((back - before)^2)), 5)

  # switching to the flat projection retires the tool — it would have nothing
  # to turn, and leaving it offered implies a dimension that is not there
  app$run_js(
    paste0(
      "(function () { var s = document.getElementById('cv-pick-proj');\n",
      "  s.value = 'umap_2D'; s.dispatchEvent(new Event('change')); })();"
    )
  )
  app$wait_for_js(paste0("!(", orbit_shown, ")"), timeout = 8000)
  expect_false(app$get_js(orbit_shown))

  app$stop()
})


test_that("a 3-D panel navigates but cannot be selected on", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_three_d_navigate_only")

  # A 3-D expression space AND a flat clonal one. The pairing is the point: it
  # lets a selection mode be armed on the flat panel and then carried onto the
  # 3-D one, which is the case hiding the buttons cannot cover on its own.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var n = 1200;\n",
      "  var x = [], y = [], z = [], cx = [], cy = [];\n",
      "  var cells = [], vals = [], cid = [];\n",
      "  for (var i = 0; i < n; i++) {\n",
      "    var k = i % 3;\n",
      "    x.push((k - 1) * 4 + Math.random());\n",
      "    y.push(Math.random() * 2);\n",
      "    z.push((k - 1) * 5 + Math.random());\n",
      "    cx.push(i % 40); cy.push(Math.floor(i / 40) % 30);\n",
      "    cells.push('c' + i); vals.push(k); cid.push(i % 20);\n",
      "  }\n",
      "  var lab = [], sz = [];\n",
      "  for (var q = 0; q < 20; q++) { lab.push('CASS' + q); sz.push(60 - q); }\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],\n",
      "        colors: ['#636EFA', '#EF553B', '#00CC96'] } },\n",
      "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap_3D: { x: x, y: y, z: z, ndim: 3 } },\n",
      "      default_projection: 'umap_3D',\n",
      "      spaces: [\n",
      "        { id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
      "          x: x, y: y, z: z },\n",
      "        { id: 'clone', label: 'Clonal expansion', x: cx, y: cy }],\n",
      "      clone: { id: cid, label: lab, size: sz,\n",
      "        n_clones: 20, n_receptor: n },\n",
      "      trekker: null\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-title-b').textContent.indexOf('Clonal') >= 0",
    timeout = 15000
  )

  vis <- function(key, act) {
    paste0(
      "getComputedStyle(document.getElementById('cv-cv-",
      key,
      "')",
      ".closest('.cv-pane').querySelector('.cv-tbtn[data-act=\"",
      act,
      "\"]')).display !== 'none'"
    )
  }
  # navigation stays, selection goes
  expect_false(app$get_js(vis("a", "lasso")))
  expect_false(app$get_js(vis("a", "box")))
  expect_true(app$get_js(vis("a", "orbit")))
  expect_true(app$get_js(vis("a", "pan")))
  expect_true(app$get_js(vis("a", "reset")))
  expect_true(app$get_js(vis("a", "png")))
  # and the flat panel is the mirror image
  expect_true(app$get_js(vis("b", "lasso")))
  expect_false(app$get_js(vis("b", "orbit")))

  # THE case buttons cannot cover: arm lasso on the flat panel, then drag on
  # the 3-D one. selectMode is global, so without an override at the event this
  # still draws a selection nobody could verify.
  app$run_js(
    "document.querySelector('.cv-tbtn[data-act=\"lasso\"][data-panel=\"B\"]').click();"
  )
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 120, clientY: r.top + 120, bubbles: true }));\n",
      "  for (var s = 30; s <= 210; s += 30)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 120 + s, clientY: r.top + 120 + s * 0.7,\n",
      "        bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  expect_false(
    app$get_js(
      "getComputedStyle(document.getElementById('cv-selbar')).display !== 'none'"
    )
  )
  # the drag was not swallowed either — it turned the cloud
  expect_true(app$get_js("document.getElementById('cv-mini-a') !== null"))

  # clicking a single cell there must not pick one, for the same reason
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 260, clientY: r.top + 260, bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup',\n",
      "    { clientX: r.left + 260, clientY: r.top + 260, bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  expect_false(
    app$get_js(
      "document.getElementById('cv-card').classList.contains('is-open')"
    )
  )

  # selecting on the FLAT panel still works, and reaches the 3-D one
  app$run_js(
    paste0(
      "(function () {\n",
      "  document.querySelector(",
      "'.cv-tbtn[data-act=\"box\"][data-panel=\"B\"]').click();\n",
      "  var cv = document.getElementById('cv-cv-b');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 8, clientY: r.top + 8, bubbles: true }));\n",
      "  for (var s = 40; s <= r.width - 20; s += 40)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 8 + s,\n",
      "        clientY: r.top + 8 + s * (r.height / r.width), bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup',\n",
      "    { clientX: r.right - 10, clientY: r.bottom - 10, bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-seltext').textContent.indexOf('Selected') >= 0",
    timeout = 10000
  )
  expect_match(
    app$get_js("document.getElementById('cv-seltext').textContent"),
    "coordinated across all panels"
  )

  app$stop()
})


test_that("an all-3-D data set says where selection has gone", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_three_d_only")

  app$run_js(cv_bundle_js(
    paste0(
      "(function () {\n",
      "  var zz = blob(0);\n",
      "  return { projections: { umap_3D: { x: blob(0), y: blob(0), z: zz,\n",
      "      ndim: 3 } },\n",
      "    default_projection: 'umap_3D',\n",
      "    spaces: [{ id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
      "      x: blob(0), y: blob(0), z: zz }] };\n",
      "})()"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-title-a').textContent.indexOf('3-D') >= 0",
    timeout = 15000
  )

  # The empty readout normally says "lasso-drag in any panel". With nothing but
  # rotatable panels that instruction is false, so it must not be the one shown.
  readout <- app$get_js("document.getElementById('cv-readout').textContent")
  expect_false(grepl("Lasso-drag in any panel", readout, fixed = TRUE))
  expect_match(readout, "turning and looking")
  expect_match(readout, "Projection")

  # and the toolbar cannot be sitting in a mode no panel offers
  expect_true(
    app$get_js(
      "document.querySelector('.cv-pane:first-child .cv-tbtn.is-on')
       .getAttribute('data-act') === 'orbit'"
    )
  )

  app$stop()
})


test_that("a data set with no linked views blanks the workspace", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_unavailable")

  app$run_js(cv_bundle_js())
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('800 cells') >= 0",
    timeout = 15000
  )

  # Staying silent here used to leave the PREVIOUS data set's panels on screen,
  # presenting one data set's cells as another's.
  app$run_js(
    paste0(
      "Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "  coordviews_data: { error: 'TEST: nothing to link on.' } } }));"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('TEST:') >= 0",
    timeout = 10000
  )

  expect_equal(
    app$get_js(
      paste0(
        "Array.from(document.querySelectorAll('.cv-pane'))",
        ".filter(function (p) ",
        "{ return !p.classList.contains('cv-hidden'); }).length;"
      )
    ),
    0
  )
  expect_equal(
    app$get_js("document.getElementById('cv-legend').innerHTML;"),
    ""
  )

  app$stop()
})
