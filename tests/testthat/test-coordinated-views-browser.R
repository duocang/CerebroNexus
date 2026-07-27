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

  # zoom in hard with the toolbar (the wheel no longer zooms), and leave a
  # committed lasso behind
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  var zin = document.querySelector(\n",
      "    '.cv-tbtn[data-act=\"zin\"][data-panel=\"A\"]');\n",
      "  for (var k = 0; k < 8; k++) zin.click();\n",
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
  overlay_before <- unlist(app$get_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var d = cv.getContext('2d')\n",
      "    .getImageData(0, 0, cv.width, cv.height).data;\n",
      "  var sx = 0, sy = 0, k = 0;\n",
      "  for (var yy = 0; yy < cv.height; yy += 2)\n",
      "    for (var xx = 0; xx < cv.width; xx += 2) {\n",
      "      var i = (yy * cv.width + xx) * 4;\n",
      "      if (d[i] < 60 && d[i+1] < 60 && d[i+2] < 70 && d[i+3] > 0) {\n",
      "        sx += xx; sy += yy; k++; }\n",
      "    }\n",
      "  return k ? [Math.round(sx / k), Math.round(sy / k)] : null;\n",
      "})()"
    )
  ))
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

  # Overlays must turn WITH the cloud. The group labels were cached at their
  # unrotated position, so they sat still while the cells moved out from under
  # them — pinned to where each group used to be. Dark pixels pick up both the
  # label chips and the axis names, which is exactly the set that has to follow.
  dark_centroid <- paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var sx = 0, sy = 0, k = 0;\n",
    "  for (var yy = 0; yy < cv.height; yy += 2)\n",
    "    for (var xx = 0; xx < cv.width; xx += 2) {\n",
    "      var i = (yy * cv.width + xx) * 4;\n",
    "      if (d[i] < 60 && d[i+1] < 60 && d[i+2] < 70 && d[i+3] > 0) {\n",
    "        sx += xx; sy += yy; k++; }\n",
    "    }\n",
    "  return k ? [Math.round(sx / k), Math.round(sy / k)] : null;\n",
    "})()"
  )
  overlay_after <- unlist(app$get_js(dark_centroid))
  expect_false(is.null(overlay_after))
  expect_gt(sqrt(sum((overlay_after - overlay_before)^2)), 10)

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
  # A pick reveals the selection actions (that is what makes it clearable), so
  # that bar is the observable. The card used to be the observable here, which
  # stopped meaning anything once a click no longer opens one -- the assertion
  # would have passed against a 3-D panel that picked freely.
  expect_false(
    app$get_js(
      "getComputedStyle(document.getElementById('cv-selbar')).display !== 'none'"
    )
  )
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
  # This data set carries no flat embedding at all, so there is no advice to
  # give. Sending the user to the Projection tab was the wrong answer: its 3-D
  # scatter is no more lassoable than these panels are.
  expect_match(readout, "does not carry")
  expect_false(grepl("Projection", readout, fixed = TRUE))

  # and the toolbar cannot be sitting in a mode no panel offers
  expect_true(
    app$get_js(
      "document.querySelector('.cv-pane:first-child .cv-tbtn.is-on')
       .getAttribute('data-act') === 'orbit'"
    )
  )

  # When the data set DOES carry a flat embedding and is merely showing the 3-D
  # one, there is something to say: the picker is the way back to selecting.
  app$run_js(cv_bundle_js(
    paste0(
      "(function () {\n",
      "  var zz = blob(0);\n",
      "  return { projections: { umap_3D: { x: blob(0), y: blob(0), z: zz,\n",
      "      ndim: 3 }, umap_2D: { x: blob(0), y: blob(0), ndim: 2 } },\n",
      "    default_projection: 'umap_3D',\n",
      "    spaces: [{ id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
      "      x: blob(0), y: blob(0), z: zz }] };\n",
      "})()"
    )
  ))
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-readout').textContent",
      ".indexOf('turning and looking') >= 0"
    ),
    timeout = 15000
  )
  readout <- app$get_js("document.getElementById('cv-readout').textContent")
  expect_match(readout, "pick a 2-D projection above")
  expect_false(grepl("does not carry", readout, fixed = TRUE))
  ## Both halves of the sentence describe the same data set, so the opening
  ## claim has to change with the advice: "the only embedding is 3-D" followed
  ## by "pick a 2-D one" contradicts itself, and a reader acting on the first
  ## half gives up on a data set that can in fact be selected in.
  expect_false(grepl("only embedding is 3-D", readout, fixed = TRUE))
  expect_match(readout, "Every panel is showing a 3-D embedding")

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

## Every string in the bundle originates in a `.crb` the user opened, so each one
## interpolated into innerHTML is an injection point, and there are more of them
## than any one screen shows: the meta line, the legend (categorical and RGB),
## the composition header, the clonotype table, the group filters. A test that
## checks one sink says nothing about the others, so each is driven and asserted
## here -- the composition and clonotype paths went unescaped precisely because
## the earlier version of this test never made a selection and so never rendered
## them.
##
## Colours are the second kind. They land in a `style` attribute, where escaping
## buys nothing: `red;position:fixed;inset:0` never leaves the attribute, it just
## appends declarations. Those are validated against the browser's own colour
## parser instead, once, as the bundle lands.
test_that("data-set strings cannot inject markup into the workspace", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_escaping")

  ## No quotes in the payload, so it survives the R -> JS -> JSON trip unaltered
  ## and any difference on screen is the app's doing rather than the harness's.
  payload <- "<img src=x onerror=window.__cvXss=1>"
  ## Needs no quote to escape: it stays inside the attribute and adds its own
  ## declarations, which is why esc() is not the tool for a colour.
  css_payload <- "red;position:fixed;inset:0;z-index:9999"
  ## Four bad colours of three different kinds, because each kind gets past a
  ## different validator: the CSS injection defeats escaping, `notacolor` defeats
  ## a permissive hand-written grammar, and `inherit` / `var(--x)` defeat
  ## CSS.supports('color', ...) -- legal CSS values that a canvas will not paint.
  bad_colors <- "'{CSS}', 'notacolor', 'inherit', 'var(--cv-x)'"

  app$run_js(cv_bundle_js(
    extra = paste0(
      "{ groups: { '",
      payload,
      "': { values: cells.map(function (c, i) ",
      "{ return i % 4; }),\n",
      "  levels: ['",
      payload,
      "', 'b', 'c', 'd'],\n",
      "  colors: [",
      sub("{CSS}", css_payload, bad_colors, fixed = TRUE),
      "] } },\n",
      "  default_group: '",
      payload,
      "',\n",
      "  spaces: [{ id: 'umap', label: '",
      payload,
      "',\n",
      "    x: blob(0), y: blob(0) }],\n",
      "  rgb: { genes: ['",
      payload,
      "', 'GeneB', 'GeneC'] },\n",
      "  clone: { id: vals.map(function (v) { return v % 2; }),\n",
      "    label: ['",
      payload,
      "', 'CASSIRSSYEQYF'],\n",
      "    size: [400, 400], n_receptor: 800, n_clones: 2 } }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.length > 0",
    timeout = 15000
  )

  ## Select everything, so the composition and clonotype readouts render.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 2, clientY: r.top + 2, bubbles: true }));\n",
      "  var pts = [[r.width - 2, 2], [r.width - 2, r.height - 2],\n",
      "    [2, r.height - 2], [2, 2]];\n",
      "  pts.forEach(function (q) {\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + q[0], clientY: r.top + q[1], bubbles: true }));\n",
      "  });\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-readout').textContent",
      ".indexOf('Composition') >= 0"
    ),
    timeout = 10000
  )

  ## Each sink separately: the payload has to be ON SCREEN as text. Asserting
  ## only "no <img> element" would pass against a build that renders nothing.
  sinks <- list(
    meta = "cv-meta",
    legend = "cv-legend",
    readout = "cv-readout"
  )
  for (nm in names(sinks)) {
    expect_true(
      app$get_js(paste0(
        "document.getElementById('",
        sinks[[nm]],
        "')",
        ".textContent.indexOf('<img') >= 0;"
      )),
      info = nm
    )
  }
  ## The composition header interpolates the grouping COLUMN NAME, which the
  ## level names inside the same readout would otherwise mask: those are escaped
  ## already, so `#cv-readout` keeps showing the payload as text even when the
  ## header has turned it into an element. Assert on the header itself.
  expect_true(app$get_js(
    paste0(
      "(function () { var e = document.querySelector('.cv-read-sub');\n",
      "  return !!e && e.textContent.indexOf('<img') >= 0; })();"
    )
  ))
  ## The clonotype table is its own builder inside the readout.
  expect_true(app$get_js(
    paste0(
      "(function () { var e = document.querySelector('.cv-cdr3');\n",
      "  return !!e && e.textContent.indexOf('<img') >= 0; })();"
    )
  ))
  ## ... as is the group-filter list, which renders whether or not it is open.
  expect_true(app$get_js(
    paste0(
      "(function () { var e = document.querySelector('.cv-filt-item');\n",
      "  return !!e && e.textContent.indexOf('<img') >= 0; })();"
    )
  ))

  ## Colours, while the CATEGORICAL legend is still up. Checking after the switch
  ## to RGB inspects that legend's hard-coded channel swatches instead, which are
  ## a colour whatever the group colours did -- an assertion that cannot fail.
  expect_equal(
    app$get_js(
      paste0(
        "Array.from(document.querySelectorAll('.cv-dot, .cv-bar-fl'))\n",
        "  .map(function (e) { return e.getAttribute('style') || ''; })\n",
        "  .filter(function (s) { return /position:fixed|notacolor|inherit/\n",
        "    .test(s) || s.indexOf('var(') >= 0; }).length;"
      )
    ),
    0
  )
  ## An invalid colour is REPLACED, not merely dropped: a canvas keeps its
  ## PREVIOUS fillStyle on a bad assignment, so a point whose colour silently
  ## went missing inherits its neighbour's rather than showing as unpainted.
  ## Every swatch has to be a colour the canvas itself accepted.
  expect_true(app$get_js(
    paste0(
      "(function () {\n",
      "  var ctx = document.createElement('canvas').getContext('2d');\n",
      "  var sw = Array.from(document.querySelectorAll(\n",
      "    '#cv-legend .cv-dot, .cv-filt-item .cv-dot, .cv-bar-fl'));\n",
      "  if (!sw.length) return false;\n",
      "  return sw.every(function (e) {\n",
      "    var c = (e.getAttribute('style') || '').split(':').pop();\n",
      "    ctx.fillStyle = '#000000'; ctx.fillStyle = c;\n",
      "    var a = ctx.fillStyle;\n",
      "    ctx.fillStyle = '#ffffff'; ctx.fillStyle = c;\n",
      "    return a === ctx.fillStyle;\n",
      "  });\n",
      "})();"
    )
  ))

  ## The RGB channel legend is a separate builder again.
  app$run_js(paste0(
    "(function () { var s = document.getElementById('cv-pick-color');\n",
    "  s.value = '__rgb__'; s.onchange(); })();"
  ))
  expect_true(app$get_js(
    "document.getElementById('cv-legend').textContent.indexOf('<img') >= 0;"
  ))

  ## Nowhere did any of it become an element, and no handler ran.
  expect_null(app$get_js("window.__cvXss || null;"))
  expect_equal(
    app$get_js("document.querySelectorAll('img[src=\"x\"]').length;"),
    0
  )

  app$stop()
})

## A panel remembers how it is looking at its space: the view rectangle, the
## lasso, and -- once a 3-D embedding exists -- the rotation, the depth buffer and
## the cached minimap. Handing a panel a different data set dropped the first two
## and kept the rest, so switching between two 3-D data sets opened the new one at
## the old one's angle, with a depth buffer computed for cells that were gone.
test_that("a data-set switch does not carry the rotation over", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_switch_rot")

  ## Deterministic coordinates, so the same bundle pushed twice must draw the
  ## same pixels. With Math.random() the comparison could not tell a carried-over
  ## rotation from a different cloud.
  push_3d <- paste0(
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
    "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
    "    coordviews_data: {\n",
    "      cells: cells, n: n,\n",
    "      groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],\n",
    "        colors: ['#636EFA', '#EF553B', '#00CC96'] } },\n",
    "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
    "      default_group: 'cluster',\n",
    "      projections: { umap_3D: { x: x, y: y, z: z, ndim: 3 } },\n",
    "      default_projection: 'umap_3D',\n",
    "      spaces: [{ id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
    "        x: x, y: y, z: z }],\n",
    "      clone: null, trekker: null\n",
    "    } } }));\n",
    "})();"
  )
  centroid <- paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var sx = 0, sy = 0, k = 0;\n",
    "  for (var yy = 0; yy < cv.height; yy += 2)\n",
    "    for (var xx = 0; xx < cv.width; xx += 2) {\n",
    "      var i = (yy * cv.width + xx) * 4;\n",
    "      if (d[i + 3] > 0 && d[i] > 60 && d[i] < 140 &&\n",
    "          d[i + 2] > 200) { sx += xx; sy += yy; k++; }\n",
    "    }\n",
    "  return k ? [Math.round(sx / k), Math.round(sy / k)] : null;\n",
    "})()"
  )

  app$run_js(push_3d)
  app$wait_for_js(
    "document.getElementById('cv-title-a').textContent.indexOf('3-D') >= 0",
    timeout = 15000
  )
  app$wait_for_idle(timeout = 5000)
  before <- unlist(app$get_js(centroid))
  expect_false(is.null(before))

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
  rotated <- unlist(app$get_js(centroid))
  expect_gt(sqrt(sum((rotated - before)^2)), 20)

  ## The switch. Identical coordinates, so anything but the starting angle is
  ## state the previous data set left behind.
  app$run_js(push_3d)
  app$wait_for_idle(timeout = 5000)
  after <- unlist(app$get_js(centroid))
  expect_lt(sqrt(sum((after - before)^2)), 5)

  app$stop()
})

## Linked views is one tab among eighteen, and building its bundle means walking
## every cell of the loaded object -- reductions, spatial coordinates, the immune
## repertoire -- into ~156 KB of payload. That used to happen on connect, for
## every session, whether or not anyone opened the tab; worse, the bundle reads
## the reactive colour map, so recolouring a group on another tab rebuilt and
## re-sent all of it while the tab sat hidden. Both halves are asserted here: the
## first version of this test only covered "never opened", and a sticky
## opened-once flag passed it while still rebuilding on every later colour change.
test_that("the bundle is built only while the workspace is on screen", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "cv_browser_lazy",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]') !== null",
    timeout = 30000
  )

  ## The tab exists and its data set is loaded -- and still nothing has been
  ## built. Waiting for the link first matters: asserting before the conditional
  ## tabs are inserted would pass against an eager build that had not run yet.
  expect_equal(app$get_value(export = "coordviews_bundles_built"), 0)

  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_idle(timeout = 20000)

  ## Opening it builds it once, and the workspace really is populated -- a gate
  ## that never opens would also report "0 before, 1 after" if the assertion
  ## stopped at the counter.
  expect_equal(app$get_value(export = "coordviews_bundles_built"), 1)
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.length > 0",
    timeout = 20000
  )
  expect_gt(app$get_js(cv_ink_js()), 1)

  ## Leave for Color management and change a group colour. This invalidates the
  ## bundle, and the workspace is not on screen to receive it.
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-color_management\"]').click();"
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').offsetParent === null",
    timeout = 10000
  )
  ## Wait for the SERVER to know it is hidden, not just the DOM. The client
  ## reports on the way out and again on a poll; asserting before that lands
  ## would be asserting against a window in which the server still believes the
  ## workspace is on screen.
  app$wait_for_value(
    input = "coordviews_visible",
    ignore = list(TRUE, NULL),
    timeout = 10000
  )
  app$wait_for_idle(timeout = 10000)
  app$wait_for_js(
    "document.querySelector('[id^=\"color_\"]') !== null",
    timeout = 20000
  )
  app$run_js(
    paste0(
      "(function () {\n",
      "  var el = document.querySelector('[id^=\"color_\"]');\n",
      "  Shiny.setInputValue(el.id, '#123456');\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 15000)
  expect_equal(app$get_value(export = "coordviews_bundles_built"), 1)

  ## Coming back does pick the change up -- the gate defers the work, it does
  ## not drop it.
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').offsetParent !== null",
    timeout = 10000
  )
  app$wait_for_idle(timeout = 20000)
  expect_equal(app$get_value(export = "coordviews_bundles_built"), 2)

  app$stop()
})

## "Every linked panel on screen at once" is the whole premise of the layout: a
## panel below the fold is one the user cannot compare against. The squares were
## floored at the size below which a panel stops being COMFORTABLE, which on a
## 1366x768 screen is more height than three or four panels have -- so the last
## row fell past the bottom and the page scrolled, on exactly the layouts that
## most need to be seen together.
test_that("three and four panels fit one viewport on a small screen", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_viewport_fit")
  ## A 1366x768 laptop, the smallest screen this is expected to work on.
  app$set_window_size(width = 1366, height = 768)
  app$wait_for_idle(timeout = 10000)

  spaces <- function(n) {
    ids <- c("umap", "spatial", "trekker", "clone")[seq_len(n)]
    paste0(
      "[",
      paste(
        vapply(
          ids,
          function(id) {
            paste0(
              "{ id: '",
              id,
              "', label: '",
              id,
              "',",
              " x: blob(0), y: blob(0) }"
            )
          },
          character(1)
        ),
        collapse = ", "
      ),
      "]"
    )
  }
  ## The panels' own bottom edge, not documentElement.scrollHeight: the grid is
  ## clipped rather than allowed to extend the document, so a row past the fold
  ## does not lengthen the page -- it simply cannot be reached. scrollHeight
  ## reads 768 either way, which is why an earlier version of this test passed
  ## against the very layout it was written to catch.
  overflow <- paste0(
    "(function () {\n",
    "  var panes = document.querySelector('.cv-panes');\n",
    "  return Math.round(panes.getBoundingClientRect().bottom) -\n",
    "    window.innerHeight;\n",
    "})();"
  )

  for (n in c(3, 4)) {
    app$run_js(cv_bundle_js(paste0("{ spaces: ", spaces(n), " }")))
    app$wait_for_js(
      paste0(
        "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length === ",
        n
      ),
      timeout = 15000
    )
    app$wait_for_idle(timeout = 10000)

    ## Every panel drew, so this is not "fits because nothing is there".
    expect_equal(
      app$get_js(
        paste0(
          "Array.from(document.querySelectorAll('.cv-pane:not(.cv-hidden) canvas'))",
          ".filter(function (c) { return c.width > 0 && c.height > 0; }).length"
        )
      ),
      n * 2, # each pane carries its canvas and its minimap
      info = paste(n, "panels")
    )
    ## ... and the page does not scroll to show them.
    expect_lte(app$get_js(overflow), 0)
    ## The squares stay usable rather than collapsing to fit.
    expect_gte(
      app$get_js("document.getElementById('cv-cv-a').clientWidth"),
      150
    )
  }

  app$stop()
})

## Clicking a cell used to throw the full detail card over the middle of the
## workspace. On a Trekker data set that is the same click that picks a nucleus
## to read its niche, so the answer arrived buried under a card covering the
## panels it was about.
##
## Three depths now, each entered deliberately: hover gives a short read that
## follows the cursor; a click PINS that tooltip, which is what makes its buttons
## clickable at all -- one that tracks the pointer moves out from under any
## attempt to press it -- and adds Details and Close; Details opens the card.
test_that("a click pins the tooltip, and the card opens only on request", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_detail_button")

  ## A regular grid, so a probe lands unambiguously on one cell.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var n = 81;\n",
      "  var x = [], y = [], cells = [], vals = [];\n",
      "  for (var j = 0; j < n; j++) {\n",
      "    x.push((j % 9) - 4);\n",
      "    y.push(Math.floor(j / 9) - 4);\n",
      "    cells.push('c' + j); vals.push(j % 2);\n",
      "  }\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a', 'b'],\n",
      "        colors: ['#636EFA', '#EF553B'] },\n",
      "        sample: { values: vals, levels: ['s1', 's2'],\n",
      "          colors: ['#111111', '#222222'] } },\n",
      "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap: { x: x, y: y, ndim: 2 } },\n",
      "      default_projection: 'umap',\n",
      "      spaces: [{ id: 'umap', label: 'umap (expression)', x: x, y: y },\n",
      "        { id: 'trekker', label: 'Trekker (physical)',\n",
      "          x: x.map(function (v) { return v * 60; }),\n",
      "          y: y.map(function (v) { return v * 60; }), unit: 'um' }],\n",
      "      clone: null, trekker: { qc: null }\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('81 cells') >= 0",
    timeout = 15000
  )

  card_open <- "document.getElementById('cv-card').classList.contains('is-open')"
  tip_txt <- "document.getElementById('cv-tip-a').textContent"
  hover <- function() {
    app$run_js(paste0(
      "(function () { var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "    { clientX: r.left + r.width / 2, clientY: r.top + r.height / 2,\n",
      "      bubbles: true })); })();"
    ))
  }

  ## Hover: a short read, and no controls -- they would be unreachable anyway,
  ## since the tooltip follows the cursor.
  hover()
  app$wait_for_js(
    "getComputedStyle(document.getElementById('cv-tip-a')).opacity === '1'",
    timeout = 5000
  )
  expect_null(app$get_js(
    "document.querySelector('#cv-tip-a .cv-tip-btn') || null"
  ))
  hover_text <- app$get_js(tip_txt)
  ## The grouping variables are not part of the glance.
  expect_false(grepl("sample", hover_text, fixed = TRUE))

  ## Click: the tooltip pins, gains both actions, and no card appears.
  app$run_js(paste0(
    "(function () { var cv = document.getElementById('cv-cv-a');\n",
    "  var r = cv.getBoundingClientRect();\n",
    "  var x = r.left + r.width / 2, y = r.top + r.height / 2;\n",
    "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
    "    { clientX: x, clientY: y, bubbles: true }));\n",
    "  window.dispatchEvent(new MouseEvent('mouseup',\n",
    "    { clientX: x, clientY: y, bubbles: true })); })();"
  ))
  app$wait_for_js(
    "document.querySelector('#cv-tip-a .cv-tip-details') !== null",
    timeout = 10000
  )
  expect_false(app$get_js(card_open))
  expect_true(app$get_js(
    "document.querySelector('#cv-tip-a .cv-tip-close') !== null"
  ))
  expect_true(app$get_js(
    "document.getElementById('cv-tip-a').classList.contains('cv-tip-pinned')"
  ))
  ## Pinned, it says more than the glance did.
  expect_match(app$get_js(tip_txt), "sample")
  ## The pick happened: on a Trekker data set that is what the click is for.
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-readout').textContent",
      ".indexOf('Niche of picked nucleus') >= 0"
    ),
    timeout = 10000
  )

  ## It stays put when the pointer moves away -- otherwise the buttons could not
  ## be reached, which is the whole reason for pinning.
  app$run_js(paste0(
    "(function () { var cv = document.getElementById('cv-cv-a');\n",
    "  cv.dispatchEvent(new MouseEvent('mouseleave', { bubbles: true })); })();"
  ))
  app$wait_for_idle(timeout = 5000)
  expect_equal(
    app$get_js("getComputedStyle(document.getElementById('cv-tip-a')).opacity"),
    "1"
  )
  ## The tooltip must not become a hit target itself, or it would block hovering
  ## whatever sits under it; only the buttons are clickable.
  expect_equal(
    app$get_js(
      "getComputedStyle(document.getElementById('cv-tip-a')).pointerEvents"
    ),
    "none"
  )
  expect_equal(
    app$get_js(
      paste0(
        "getComputedStyle(document.querySelector('#cv-tip-a .cv-tip-details'))",
        ".pointerEvents"
      )
    ),
    "auto"
  )

  ## Details opens the card.
  app$run_js("document.querySelector('#cv-tip-a .cv-tip-details').click();")
  app$wait_for_js(card_open, timeout = 10000)
  expect_gt(
    app$get_js("document.getElementById('cv-card-body').textContent.length"),
    0
  )

  ## Close dismisses the tooltip.
  app$run_js("document.querySelector('#cv-tip-a .cv-tip-close').click();")
  app$wait_for_idle(timeout = 5000)
  expect_equal(
    app$get_js("getComputedStyle(document.getElementById('cv-tip-a')).opacity"),
    "0"
  )
  expect_false(app$get_js(
    "document.getElementById('cv-tip-a').classList.contains('cv-tip-pinned')"
  ))
  ## ... but NOT the pick behind it. The ring and, on a Trekker data set, the
  ## niche readout are what the cell was clicked for; putting the tooltip away
  ## is not a reason to give them up. Clicking the cell again drops the pick.
  expect_true(app$get_js(
    paste0(
      "document.getElementById('cv-readout').textContent",
      ".indexOf('Niche of picked nucleus') >= 0"
    )
  ))

  app$stop()
})
