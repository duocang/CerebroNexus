# Builder fluid-width layout implementation plan

> **For AI agents:** Required sub-skill: use superpowers:executing-plans to implement this plan task by task. Track each step with the checkboxes below.

**Goal:** Let the desktop Builder shell and action bar use the full viewport width while retaining the fixed dataset rail, readable inner form widths, and existing responsive behavior.

**Architecture:** Change only the outer layout contract in `builder.css`. A shared desktop gutter token replaces the centered `82.5rem` ceiling; existing component-level width limits and media queries continue to govern readable content and mobile behavior.

**Tech stack:** CSS Grid, Shiny static assets, testthat source contracts, shinytest2/Chromote.

---

### Task 1: Make the Builder outer frame fluid

**Files:**
- Modify: `tests/testthat/test-builder-ui-contract.R`
- Modify: `tests/testthat/test-builder-loading-browser.R`
- Modify: `inst/builder/www/builder.css`

- [ ] **Step 1: Write the failing CSS contract**

Replace the old `max-width: 82.5rem` expectation with assertions that the CSS
defines `--builder-page-gutter: 26px`, contains no `82.5rem` ceiling, and gives
both `.builder-shell` and `.actionbar` fluid outer geometry.

- [ ] **Step 2: Run the contract and verify RED**

Run:

```sh
R -q -e 'devtools::test(".", filter="builder-ui-contract", reporter="summary", stop_on_failure=TRUE)'
```

Expected: FAIL because the old `82.5rem` shell/action-bar limits still exist.

- [ ] **Step 3: Implement the minimal fluid CSS**

Add the gutter token and change the outer rules to the following behavior:

```css
:root { --builder-page-gutter: 26px; }
.builder-shell { width: 100%; max-width: none; }
.actionbar {
  max-width: none;
  margin-inline: var(--builder-page-gutter);
}
.actionbar .inner { max-width: none; margin-inline: 0; }
```

Keep the fixed rail column, component-level form limits, and existing tablet and
mobile rules unchanged.

- [ ] **Step 4: Verify GREEN and focused responsive regression**

Run:

```sh
R -q -e 'devtools::test(".", filter="builder-ui-contract", reporter="summary", stop_on_failure=TRUE)'
```

Expected: PASS.

- [ ] **Step 5: Add and run wide-browser geometry coverage**

Use the existing loading browser regression at `1920px` and assert that the
shell is inset by approximately `26px` on both sides, the main pane is wider
than `1200px`, and document width does not exceed viewport width.

Run:

```sh
CEREBRO_RUN_BROWSER_TESTS=true R -q -e 'devtools::test(".", filter="builder-loading-browser", reporter="summary", stop_on_failure=TRUE)'
```

Expected: PASS with no browser console failures.

- [ ] **Step 6: Final checks and commit**

Run:

```sh
node --check inst/builder/www/builder.js
git diff --check
```

Commit only the CSS and focused tests:

```sh
git add inst/builder/www/builder.css tests/testthat/test-builder-ui-contract.R tests/testthat/test-builder-loading-browser.R
git commit -m "style: let Builder use the viewport width"
```
