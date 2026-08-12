# Viewer Responsive Drawers Implementation Plan

> **For AI agent workers:** Required sub-skill: use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task by task. Track progress with the checkboxes below.

**Goal:** Replace Viewer mobile navigation and Linked views More settings with accessible, mutually exclusive responsive drawers without changing any Viewer data or plotting behavior.

**Architecture:** The Viewer shell gets a small `viewer-shell.js` controller around AdminLTE's existing push-menu plus an explicit scrim and close control. Linked views keeps its existing `setMoreOpen()` state entry point but removes drag/floating state and presents the same control DOM as a fixed desktop drawer or full-screen narrow drawer. The two controllers coordinate through one `cerebro:overlay-opening` DOM event carrying an owner name.

**Tech stack:** R/Shiny, shinydashboard/AdminLTE 2, vanilla JavaScript, CSS, testthat, shinytest2/Chromote.

---

## File responsibilities

- Create `inst/viewer/www/viewer-shell.js`: mobile navigation state, dismissal,
  focus restoration, viewport cleanup, and cross-overlay event coordination.
- Create `tests/testthat/test-viewer-responsive-shell-browser.R`: real-browser
  mobile shell behavior at 390 px.
- Modify `inst/viewer/shiny_UI.R`: navigation close control, scrim, and loading
  the shell controller.
- Modify `inst/viewer/www/custom.css`: visible hamburger, modal mobile sidebar,
  navigation scrim, motion, and reduced-motion rules.
- Modify `inst/viewer/coordinated_views/UI.R`: semantic drawer shell and removal
  of drag-specific markup.
- Modify `inst/viewer/www/coordviews.js`: simplified drawer state, focus,
  Escape, responsive semantics, and overlay coordination.
- Modify `inst/viewer/www/coordviews.css`: desktop right drawer, narrow
  full-screen drawer, internal scrolling, and reduced-motion rules.
- Modify `tests/testthat/test-coordinated-views.R`: fast source/UI/style contracts.
- Modify `tests/testthat/test-coordinated-views-browser.R`: More settings geometry,
  dismissal, focus, resize, and mutual-exclusion browser regressions.

### Task 1: Lock the More settings drawer contract

**Files:**
- Modify: `tests/testthat/test-coordinated-views.R`
- Modify: `inst/viewer/coordinated_views/UI.R`

- [ ] **Step 1: Add a failing structural test**

Add a test beside the existing Linked views UI/JS/CSS contract tests:

```r
test_that("More settings exposes one non-draggable drawer shell", {
  ui_file <- file.path(dirname(bundle_file), "UI.R")
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  css_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.css")
  skip_if_not(all(file.exists(c(ui_file, js_file, css_file))))

  ui <- paste(readLines(ui_file, warn = FALSE), collapse = "\n")
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
  css <- paste(readLines(css_file, warn = FALSE), collapse = "\n")

  expect_match(ui, 'id = "cv-more"', fixed = TRUE)
  expect_match(ui, '`role` = "dialog"', fixed = TRUE)
  expect_match(ui, '`aria-hidden` = "true"', fixed = TRUE)
  expect_match(ui, 'id = "cv-more-close"', fixed = TRUE)
  expect_no_match(ui, "data-cv-more-drag-handle", fixed = TRUE)
  expect_no_match(ui, "Drag to move", fixed = TRUE)
  expect_no_match(js, "beginMoreDrag", fixed = TRUE)
  expect_no_match(js, "moreFloating", fixed = TRUE)
  expect_match(css, "position: fixed", fixed = TRUE)
})
```

- [ ] **Step 2: Run the contract test and confirm the intended failure**

Run:

```bash
NOT_CRAN=true Rscript -e 'devtools::test(filter = "coordinated-views$", reporter = "summary")'
```

Expected: FAIL because the UI still contains the drag handle/hint and the JS
still defines floating/drag behavior.

- [ ] **Step 3: Make the minimal UI semantic change**

In `inst/viewer/coordinated_views/UI.R`, change the More shell to the following
shape while leaving `.cv-more-clip`, `.cv-more-inner`, and every existing control
unchanged:

```r
div(
  class = "cv-more",
  id = "cv-more",
  `role` = "dialog",
  `aria-modal` = "false",
  `aria-hidden` = "true",
  `aria-labelledby` = "cv-more-title",
  div(
    class = "cv-more-titlebar",
    tags$span(id = "cv-more-title", class = "cv-more-title", "More settings"),
    tags$button(
      type = "button",
      id = "cv-more-close",
      class = "cv-more-close",
      `aria-label` = "Close More settings",
      HTML("&times;")
    )
  ),
  div(class = "cv-more-clip", div(class = "cv-more-inner", ...))
)
```

- [ ] **Step 4: Remove only the obsolete drag/floating JavaScript**

Delete `moreFloating`, `moreDrag`, `MORE_VISIBLE_EDGE`,
`resetMorePosition()`, `clampMorePosition()`, `bringMoreToFront()`,
`beginMoreDrag()`, `moveMoreDrag()`, `endMoreDrag()`, and their pointer event
listeners. Make the More button always call:

```js
setMoreOpen(!isMoreOpen());
```

Remove the `resetMorePosition()` close call. Do not alter range, background,
filter, selection, or rendering functions.

- [ ] **Step 5: Run the contract test and confirm it passes**

Run the same `devtools::test(filter = "coordinated-views$")` command.
Expected: PASS.

- [ ] **Step 6: Commit the structural boundary**

```bash
git add tests/testthat/test-coordinated-views.R \
  inst/viewer/coordinated_views/UI.R inst/viewer/www/coordviews.js
git commit -m "refactor(viewer): define settings drawer shell"
```

### Task 2: Implement the responsive More settings drawer with TDD

**Files:**
- Modify: `tests/testthat/test-coordinated-views-browser.R`
- Modify: `inst/viewer/www/coordviews.css`
- Modify: `inst/viewer/www/coordviews.js`

- [ ] **Step 1: Replace the obsolete drag regression with desktop drawer geometry**

Replace `More settings becomes a recoverable floating window after dragging`
with a test that records linked-panel geometry before and after opening:

```r
test_that("More settings opens as a stable desktop drawer", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_more_drawer_desktop")
  on.exit(app$stop(), add = TRUE)
  app$run_js(cv_bundle_js())
  app$wait_for_js("document.querySelector('.cv-pane:not(.cv-hidden)') !== null")

  before <- app$get_js(
    "document.querySelector('.cv-pane:not(.cv-hidden)').getBoundingClientRect().width"
  )
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js("document.getElementById('cv-more').classList.contains('is-open')")

  expect_equal(
    app$get_js("getComputedStyle(document.getElementById('cv-more')).position"),
    "fixed"
  )
  expect_lte(abs(app$get_js(
    "document.querySelector('.cv-pane:not(.cv-hidden)').getBoundingClientRect().width"
  ) - before), 1)
  expect_true(app$get_js(
    "document.getElementById('cv-more-close').getClientRects().length > 0"
  ))
  expect_false(app$get_js(
    "document.querySelector('[data-cv-more-drag-handle]') !== null"
  ))
})
```

- [ ] **Step 2: Add narrow geometry and control-preservation assertions**

Add a separate test that sets 768x900 and 390x844 viewports and asserts:

```r
app$get_chromote_session()$set_viewport_size(width = 390, height = 844)
app$wait_for_js("innerWidth === 390 && innerHeight === 844")
app$run_js("document.getElementById('cv-more-btn').click();")

drawer <- app$get_js(paste0(
  "(function(){var r=document.getElementById('cv-more').getBoundingClientRect();",
  "return {left:r.left,top:r.top,right:r.right,bottom:r.bottom,",
  "scroll:document.querySelector('#cv-more .cv-more-clip').scrollHeight,",
  "client:document.querySelector('#cv-more .cv-more-clip').clientHeight};})()"
))
expect_lte(abs(drawer$left), 1)
expect_lte(abs(drawer$top), 1)
expect_lte(abs(drawer$right - 390), 1)
expect_lte(abs(drawer$bottom - 844), 1)
expect_true(app$get_js("document.getElementById('cv-ps') !== null"))
expect_true(app$get_js("document.querySelector('[data-cv-bg-mode]') !== null"))
expect_true(app$get_js("document.documentElement.scrollWidth <= innerWidth + 1"))
```

- [ ] **Step 3: Run the browser file and verify red**

```bash
NOT_CRAN=true Rscript -e 'devtools::test(filter = "coordinated-views-browser", reporter = "summary")'
```

Expected: the new drawer geometry tests FAIL because More is still an anchored
absolute popover and the narrow surface does not fill the viewport.

- [ ] **Step 4: Implement desktop and narrow drawer CSS**

Replace the current popover/floating rules with token-driven fixed geometry:

```css
.coordviews-page .cv-more {
  display: none;
  position: fixed;
  z-index: 1600;
  inset: 0 0 0 auto;
  width: min(42rem, calc(100vw - 260px));
  min-width: 34rem;
  grid-template-rows: auto minmax(0, 1fr);
  background: var(--cv-surface);
  border-left: 1px solid var(--cv-border);
  box-shadow: -8px 0 24px rgba(17, 17, 26, .14);
  opacity: 0;
  transform: translateX(1rem);
  transition: opacity .18s var(--ease), transform .2s var(--ease);
}
.coordviews-page .cv-more.is-mounted { display: grid; }
.coordviews-page .cv-more.is-open { opacity: 1; transform: none; }
.coordviews-page .cv-more-clip { min-height: 0; overflow: auto; }
.coordviews-page .cv-more-close { display: block; margin-left: auto; }

@media (max-width: 900px) {
  .coordviews-page .cv-more {
    inset: 0;
    width: 100vw;
    min-width: 0;
    height: 100vh;
    height: 100dvh;
    border-left: 0;
    transform: translateX(100%);
  }
}
```

Keep Background and Points sections readable by switching their internal grids
to one column below 760 px. Remove the old `.is-floating`, grid-row collapse,
grip, and drag-hint styles.

- [ ] **Step 5: Simplify More open/close timing and accessibility state**

In `setMoreOpen(open)`:

```js
if (open) {
  document.dispatchEvent(new CustomEvent('cerebro:overlay-opening', {
    detail: { owner: 'more-settings' }
  }));
  mp.classList.add('is-mounted');
  void mp.offsetWidth;
}
mp.classList.toggle('is-open', open);
mp.setAttribute('aria-hidden', open ? 'false' : 'true');
mp.setAttribute('aria-modal', window.matchMedia('(max-width: 900px)').matches && open
  ? 'true' : 'false');
if (btn) btn.setAttribute('aria-expanded', open ? 'true' : 'false');
```

Use a 220 ms unmount timer after close. Close filter menus on dismissal. Do not
call `resizeAll()`.

- [ ] **Step 6: Run the browser file and verify green**

Run the same browser test command. Expected: PASS, with no new browser console
errors.

- [ ] **Step 7: Commit the responsive drawer**

```bash
git add tests/testthat/test-coordinated-views-browser.R \
  inst/viewer/www/coordviews.css inst/viewer/www/coordviews.js
git commit -m "feat(viewer): add responsive settings drawer"
```

### Task 3: Add accessible More settings dismissal and focus behavior

**Files:**
- Modify: `tests/testthat/test-coordinated-views-browser.R`
- Modify: `inst/viewer/www/coordviews.js`

- [ ] **Step 1: Add failing focus, Escape, resize, and idempotence tests**

Add assertions that:

```r
app$run_js("document.getElementById('cv-more-btn').focus(); document.getElementById('cv-more-btn').click();")
app$wait_for_js("document.activeElement.id === 'cv-more-close'")
app$run_js("document.dispatchEvent(new KeyboardEvent('keydown',{key:'Escape',bubbles:true}));")
app$wait_for_js("!document.getElementById('cv-more').classList.contains('is-open')")
expect_identical(app$get_js("document.activeElement.id"), "cv-more-btn")
expect_identical(app$get_js("document.getElementById('cv-more').getAttribute('aria-hidden')"), "true")
```

Open the drawer, resize from 390 to 1619, and assert that it remains open, its
control values remain unchanged, and `aria-modal` changes from `true` to
`false`.

- [ ] **Step 2: Run the browser test and verify red**

Expected: FAIL because focus and Escape are not yet owned by `setMoreOpen()`.

- [ ] **Step 3: Implement focus and Escape at the existing state boundary**

Track only the More trigger as the restore target. On open, focus
`#cv-more-close` after mounting. On close, focus `#cv-more-btn` unless close was
requested because another overlay is opening. Add one keydown handler:

```js
document.addEventListener('keydown', function (e) {
  if (e.key === 'Escape' && isMoreOpen()) {
    e.preventDefault();
    setMoreOpen(false);
  }
});
```

Listen to media-query `change` and update only `aria-modal`; do not close or
rebuild controls. Repeated calls with the current state return without moving
focus.

- [ ] **Step 4: Verify green and commit**

Run the coordinated browser test file, then commit:

```bash
git add tests/testthat/test-coordinated-views-browser.R inst/viewer/www/coordviews.js
git commit -m "fix(viewer): make settings drawer keyboard safe"
```

### Task 4: Build the mobile Viewer navigation shell with TDD

**Files:**
- Create: `tests/testthat/test-viewer-responsive-shell-browser.R`
- Create: `inst/viewer/www/viewer-shell.js`
- Modify: `inst/viewer/shiny_UI.R`
- Modify: `inst/viewer/www/custom.css`

- [ ] **Step 1: Write the failing mobile-shell browser test**

Create a focused shinytest2 file with a 390x844 `AppDriver`. Assert the shell
contract and all dismiss paths:

```r
test_that("mobile Viewer navigation is a dismissible modal drawer", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "viewer_mobile_nav", width = 390, height = 844)
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js("innerWidth === 390 && document.querySelector('.sidebar-toggle') !== null")

  expect_true(app$get_js(
    "document.querySelector('.sidebar-toggle .cerebro-menu-icon') !== null"
  ))
  app$run_js("document.querySelector('.sidebar-toggle').click();")
  app$wait_for_js("document.body.classList.contains('sidebar-open')")
  expect_true(app$get_js(
    "document.querySelector('.cerebro-nav-scrim').classList.contains('is-open')"
  ))
  expect_identical(app$get_js(
    "document.querySelector('.sidebar-toggle').getAttribute('aria-expanded')"
  ), "true")

  app$run_js("document.querySelector('.cerebro-nav-scrim').click();")
  app$wait_for_js("!document.body.classList.contains('sidebar-open')")
  expect_identical(app$get_js("document.activeElement.className"), "sidebar-toggle")
})
```

Add separate cases for explicit close, Escape, and clicking the Linked views
link. After link selection, assert the active tab changed and the drawer closed.

- [ ] **Step 2: Run the new file and confirm red**

```bash
NOT_CRAN=true Rscript -e 'devtools::test(filter = "viewer-responsive-shell-browser", reporter = "summary")'
```

Expected: FAIL because the explicit menu icon, close button, scrim, and shell
controller do not exist.

- [ ] **Step 3: Add the stable shell DOM and asset**

In `dashboardSidebar()`, add:

```r
tags$button(
  type = "button",
  class = "cerebro-nav-close",
  `aria-label` = "Close navigation",
  HTML("&times;")
)
```

In `dashboardBody()`, before the tab items, add:

```r
tags$button(
  type = "button",
  class = "cerebro-nav-scrim",
  tabindex = "-1",
  `aria-hidden` = "true",
  `aria-label` = "Close navigation"
)
```

Load `cerebro_js("viewer-shell.js", defer = TRUE)` after `custom.css` and before
the page-specific controllers.

- [ ] **Step 4: Implement the minimal shell controller**

`viewer-shell.js` is one IIFE. On ready it replaces the generated hamburger
contents with a CSS-drawn icon span and synchronizes `aria-controls`,
`aria-expanded`, scrim state, and close-button focus. It adapts AdminLTE by
clicking the existing toggle only when a requested state differs:

```js
function setNavigationOpen(open, options) {
  if (!mobileQuery.matches) return;
  var current = document.body.classList.contains('sidebar-open');
  if (open && !current) toggle.click();
  if (!open && current) toggle.click();
  syncNavigationState(open);
  if (open) {
    document.dispatchEvent(new CustomEvent('cerebro:overlay-opening', {
      detail: { owner: 'mobile-navigation' }
    }));
    closeButton.focus();
  } else if (!options || options.restoreFocus !== false) {
    toggle.focus();
  }
}
```

Listen for close-button, scrim, Escape, and `.sidebar-menu a[href^="#shiny-tab-"]`
clicks. On crossing to desktop, remove stale scrim/ARIA state without toggling
the desktop sidebar.

- [ ] **Step 5: Add mobile shell CSS**

At `max-width: 767px`, show a 40x40 hamburger with a three-line icon, position
the sidebar as a 280 px modal drawer, show a close control, and provide a fixed
scrim between content and sidebar. Desktop keeps the existing fixed sidebar and
hides close/scrim. Add reduced-motion rules for sidebar and scrim.

- [ ] **Step 6: Run the new browser test and verify green**

Run the same `viewer-responsive-shell-browser` command. Expected: PASS and no
horizontal overflow at 390 px.

- [ ] **Step 7: Commit the mobile shell**

```bash
git add tests/testthat/test-viewer-responsive-shell-browser.R \
  inst/viewer/www/viewer-shell.js inst/viewer/shiny_UI.R \
  inst/viewer/www/custom.css
git commit -m "feat(viewer): add modal mobile navigation"
```

### Task 5: Coordinate overlays and lock reduced-motion behavior

**Files:**
- Modify: `tests/testthat/test-viewer-responsive-shell-browser.R`
- Modify: `tests/testthat/test-coordinated-views-browser.R`
- Modify: `tests/testthat/test-coordinated-views.R`
- Modify: `inst/viewer/www/viewer-shell.js`
- Modify: `inst/viewer/www/coordviews.js`
- Modify: `inst/viewer/www/custom.css`
- Modify: `inst/viewer/www/coordviews.css`

- [ ] **Step 1: Add a failing mutual-exclusion browser case**

At 390 px, open More settings, then request mobile navigation and assert More
closed. Reverse the order and assert navigation and its scrim closed before More
opened. Also assert there is never more than one open overlay:

```r
expect_lte(app$get_js(paste0(
  "Number(document.body.classList.contains('sidebar-open')) + ",
  "Number(document.getElementById('cv-more').classList.contains('is-open'))"
)), 1)
```

- [ ] **Step 2: Add failing source/style reduced-motion contracts**

Assert that both CSS files contain their drawer selectors inside
`prefers-reduced-motion: reduce`, and both JS controllers reference the shared
`cerebro:overlay-opening` event with distinct owner names.

- [ ] **Step 3: Run focused tests and confirm red**

Run both browser filters and the coordinated unit filter. Expected: mutual
exclusion or reduced-motion contract FAILS until both controllers participate.

- [ ] **Step 4: Implement the shared event handshake**

Before opening, each controller dispatches `cerebro:overlay-opening`. Each
listens for that event and closes itself when `event.detail.owner` names the
other controller. Event-driven closes use `restoreFocus: false` so focus moves
directly into the newly opened surface.

- [ ] **Step 5: Finish reduced-motion and rerun focused tests**

Ensure both drawer transitions are disabled under reduced motion. Run:

```bash
NOT_CRAN=true Rscript -e 'devtools::test(filter = "viewer-responsive-shell-browser|coordinated-views-browser|coordinated-views$", reporter = "summary")'
```

Expected: PASS.

- [ ] **Step 6: Commit overlay coordination**

```bash
git add tests/testthat/test-viewer-responsive-shell-browser.R \
  tests/testthat/test-coordinated-views-browser.R \
  tests/testthat/test-coordinated-views.R inst/viewer/www/viewer-shell.js \
  inst/viewer/www/coordviews.js inst/viewer/www/custom.css \
  inst/viewer/www/coordviews.css
git commit -m "fix(viewer): coordinate responsive overlays"
```

### Task 6: Final verification and live visual regression

**Files:**
- Modify only if a verified regression requires a focused correction.

- [ ] **Step 1: Run JavaScript syntax and whitespace checks**

```bash
node --check inst/viewer/www/viewer-shell.js
node --check inst/viewer/www/coordviews.js
git diff --check HEAD~4..HEAD
```

Expected: all commands exit 0.

- [ ] **Step 2: Run the focused Viewer test set**

```bash
NOT_CRAN=true Rscript -e 'devtools::test(filter = "viewer-responsive-shell-browser|coordinated-views", reporter = "summary")'
```

Expected: zero failures, errors, and warnings apart from existing explicitly
documented environment skips.

- [ ] **Step 3: Run the project's final gate once**

```bash
scripts/precheck.sh
```

Expected: PASS. If an unrelated pre-existing gate remains, record the exact
failure and prove the focused Viewer tests still pass; do not modify unrelated
code.

- [ ] **Step 4: Inspect a real generated Omnibus Viewer**

Open the existing generated Viewer or start `inst/` with the Omnibus fixture.
Verify at 1619x950, 768x900, and 390x844:

- menu icon, scrim, close paths, and active-page navigation;
- desktop/right and narrow/full-screen More settings presentations;
- complete Background and Points controls;
- stable linked plot geometry and state;
- no horizontal overflow;
- mutual exclusion; and
- reduced-motion emulation.

- [ ] **Step 5: Review the final diff against the specification**

Confirm every specification requirement has code and test evidence, no Builder
or integration files changed, and no old drag/floating code remains.

- [ ] **Step 6: Commit any final focused correction, then confirm clean status**

Use a scoped Conventional Commit only if Step 4 found a real regression. Finish
with:

```bash
git status --short --branch
git log -6 --oneline
```

Expected: clean `feat/coordinated-views-nexus` worktree with the design, plan,
implementation, and verification commits only.
