# Builder Workspace Visual System Implementation Plan

> **For AI agent workers:** Required sub-skill: use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task by task. Track progress with the checkboxes below.

**Goal:** Unify Data setup, Review, and Build as professional-tool pages with shared lightweight section panels and footer semantics while preserving all Builder workflow behavior.

**Architecture:** Add a small set of stage-layout primitives in `ui/workflow.R`, then migrate each public stage to those primitives without changing input IDs or server transitions. Replace the generic outer stage card with a flat shell, compact summaries, consistent lightweight white section panels, and one in-flow footer; retain smaller bounded panels only for independent objects and conditional/error states.

**Plan revision:** During browser review, the user supplied the Viewer at `http://127.0.0.1:5873/` as the visual reference and requested visible grouping boundaries. The final section treatment therefore uses the reference page's 1px quiet border, 14px desktop radius, and `shadow-1` on a gray workspace. Any later task wording that says an ordinary section is “flat” means the stage shell is flat and sections are not nested cards; it does not override this lightweight panel treatment.

**Tech stack:** R, Shiny/htmltools, CSS custom properties, testthat, existing Builder browser contract helpers.

---

## File responsibilities

- `inst/builder/ui/workflow.R`: shared stage header, section, summary, and footer helpers; Data setup/Build composition helpers.
- `inst/builder/ui/review_stage.R`: Review content composition and Review footer actions.
- `inst/builder/ui/inspect_stage.R`: flat Import & Inspect section markup.
- `inst/builder/ui/core_stage/stage.R`: flat Core content section boundary; preserve disclosure cards inside it.
- `inst/builder/ui/enhance_stage.R`: flat Optional content section boundary; preserve real attachment objects.
- `inst/builder/ui/build_status.R`: Build readiness/status projection placed into the shared Build footer without changing state models or input IDs.
- `inst/builder/server/review.R`: compose Data setup and Review with their footers inside each workflow-stage root.
- `inst/builder/www/builder.layout.css`: flat stage shell, responsive workspace measure, and removal of active-stage card elevation.
- `inst/builder/www/builder.components.css`: reusable stage header/section/summary/footer/object/state-panel rules.
- `inst/builder/www/builder.features.css`: Review, Data setup, and Build exceptions reduced to semantic object/state styling.
- `tests/testthat/test-builder-stage-server.R`: helper markup and action-ID contracts.
- `tests/testthat/test-builder-stage-review.R`: Review/Build containment, labels, and non-editable-plan contracts.
- `tests/testthat/test-builder-ui-contract.R`: source/CSS architecture contracts.
- `tests/testthat/test-builder-responsive-browser.R`: responsive footer and fixed-navigation non-overlap checks if the current browser harness supports the required stage fixture.

### Task 1: Add shared stage-layout primitives

**Files:**
- Modify: `tests/testthat/test-builder-stage-server.R`
- Modify: `tests/testthat/test-builder-ui-contract.R`
- Modify: `inst/builder/ui/workflow.R`
- Modify: `inst/builder/www/builder.components.css`
- Modify: `inst/builder/www/builder.layout.css`

- [ ] **Step 1: Write failing helper and CSS contract tests**

Add a test that renders the new helpers and checks their semantic classes:

```r
header <- app_env$builder_stage_header_ui(
  "Data setup",
  "Choose data to include",
  "Define the content saved to each CRB file."
)
section <- app_env$builder_stage_section_ui(
  "Core content",
  p("Required content")
)
footer <- app_env$builder_stage_footer_ui(
  "1 dataset ready",
  actionButton("continue_to_review", "Continue")
)
html <- htmltools::renderTags(tagList(header, section, footer))$html
expect_match(html, "builder-stage-header", fixed = TRUE)
expect_match(html, "builder-stage-section", fixed = TRUE)
expect_match(html, "builder-stage-footer-status", fixed = TRUE)
expect_match(html, "builder-stage-footer-actions", fixed = TRUE)
```

Update the CSS contract to require `.builder-stage-shell`, `.builder-stage-summary`, `.builder-stage-footer`, and the mobile footer rule, and to reject a border/shadow declaration on `.builder-stage-shell`.

- [ ] **Step 2: Run the focused tests and confirm failure**

Run:

```bash
Rscript -e "testthat::test_file('tests/testthat/test-builder-stage-server.R'); testthat::test_file('tests/testthat/test-builder-ui-contract.R')"
```

Expected: failures report missing `builder_stage_header_ui()` and missing shared stage CSS selectors.

- [ ] **Step 3: Implement the minimal shared helpers**

Add helpers in `inst/builder/ui/workflow.R` with these interfaces:

```r
builder_stage_header_ui <- function(stage, title, intro) {
  tags$header(
    class = "builder-stage-header",
    p(class = "builder-stage-eyebrow", stage),
    h2(title),
    p(class = "stage-intro", intro)
  )
}

builder_stage_section_ui <- function(title, ..., description = NULL,
                                     class = NULL) {
  tags$section(
    class = paste("builder-stage-section", class),
    div(
      class = "builder-stage-section-head",
      h3(title),
      if (!is.null(description)) p(description)
    ),
    ...
  )
}

builder_stage_footer_ui <- function(status, ...) {
  tags$footer(
    class = "builder-stage-footer",
    p(class = "builder-stage-footer-status", status),
    div(class = "builder-stage-footer-actions", ...)
  )
}
```

Add `.builder-stage-shell`, header, summary, section, footer, object, and state-panel styles. Use existing spacing/color tokens, with 1.5rem header separation, 2rem section rhythm, 2.5rem before the footer, and a single footer divider. At `max-width: 40rem`, stack the footer and make only footer action buttons full width.

Override/remove the generic active-stage border and shadow for `.builder-stage-shell`; do not remove generic `.builder-stage` styling until all nested legacy stages have migrated.

- [ ] **Step 4: Run the focused tests and confirm pass**

Run the Step 2 command. Expected: both files pass with no new warnings.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/ui/workflow.R inst/builder/www/builder.components.css inst/builder/www/builder.layout.css tests/testthat/test-builder-stage-server.R tests/testthat/test-builder-ui-contract.R
git commit -m "style(builder): add shared stage layout primitives"
```

### Task 2: Contain Review content and confirmation in one stage shell

**Files:**
- Modify: `tests/testthat/test-builder-stage-review.R`
- Modify: `tests/testthat/test-builder-stage-server.R`
- Modify: `inst/builder/ui/review_stage.R`
- Modify: `inst/builder/server/review.R`
- Modify: `inst/builder/www/builder.features.css`

- [ ] **Step 1: Write failing Review containment tests**

Render the Review stage with its footer and assert:

```r
html <- builder_stage_html(builder_review_stage_ui(
  "review",
  builder_review_model(builder_stage_frozen_plan()),
  footer = builder_review_confirmation_ui()
))
expect_match(html, "builder-stage-shell", fixed = TRUE)
expect_match(html, "builder-stage-footer", fixed = TRUE)
expect_false(grepl("Ready to continue?", html, fixed = TRUE))
expect_match(html, "CRB plan ready", fixed = TRUE)
expect_match(html, "Back to Data setup", fixed = TRUE)
expect_match(html, "Continue to Build", fixed = TRUE)
expect_match(html, 'id="confirm_review"', fixed = TRUE)
expect_match(html, 'id="back_to_settings"', fixed = TRUE)
```

Use `xml2::read_html()` if already available in the test environment to assert that `.builder-stage-footer` is a descendant of `[data-workflow-stage='review']`; otherwise assert containment from the rendered root produced by the helper rather than brittle string offsets.

- [ ] **Step 2: Run focused Review tests and confirm failure**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-builder-stage-review.R'); testthat::test_file('tests/testthat/test-builder-stage-server.R')"
```

Expected: `builder_review_stage_ui()` rejects `footer`, old confirmation copy is present, or the footer is not contained by the Review root.

- [ ] **Step 3: Implement Review shell and footer composition**

Change `builder_review_stage_ui()` to accept `footer = NULL`. Use `builder_stage_header_ui()` and classes `builder-stage builder-stage-shell builder-stage-review`. Keep the compact frozen-plan facts in `builder-stage-summary review-summary-strip`. Append `footer` as the final child.

Replace `builder_review_confirmation_ui()` with:

```r
builder_stage_footer_ui(
  "CRB plan ready",
  actionButton("back_to_settings", "Back to Data setup", class = "btn"),
  actionButton(
    "confirm_review",
    "Continue to Build",
    class = "btn btn-action"
  )
)
```

In `render_review_workbench()`, return one stage:

```r
tagAppendAttributes(
  builder_review_stage_ui(
    "review",
    builder_review_model(plan),
    footer = builder_review_confirmation_ui()
  ),
  `data-workflow-stage` = "review"
)
```

For a single dataset, add an `is-single-dataset` class to the dataset grid and remove the dataset card shadow/border through CSS. Keep light `.builder-object` boundaries for multiple datasets. Flatten nested Review content summary items to rows with dividers; retain warning/error panels.

- [ ] **Step 4: Run focused Review tests and confirm pass**

Run the Step 2 command. Expected: all Review/server tests pass.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/ui/review_stage.R inst/builder/server/review.R inst/builder/www/builder.features.css tests/testthat/test-builder-stage-review.R tests/testthat/test-builder-stage-server.R
git commit -m "style(builder): unify review stage structure"
```

### Task 3: Flatten Data setup and its ordinary sections

**Files:**
- Modify: `tests/testthat/test-builder-stage-server.R`
- Modify: `tests/testthat/test-builder-ui-contract.R`
- Modify: `inst/builder/server/review.R`
- Modify: `inst/builder/ui/inspect_stage.R`
- Modify: `inst/builder/ui/core_stage/stage.R`
- Modify: `inst/builder/ui/enhance_stage.R`
- Modify: `inst/builder/ui/workflow.R`
- Modify: `inst/builder/www/builder.features.css`

- [ ] **Step 1: Write failing Data setup structure tests**

Update helper/source contracts to assert that the Data setup root contains `builder-stage-shell`, uses the title `Choose data to include`, and that Inspect/Core/Enhance roots use `builder-stage-section` without `builder-card`:

```r
expect_match(inspect_html, "builder-stage-section", fixed = TRUE)
expect_false(grepl("builder-card", inspect_html, fixed = TRUE))
expect_match(core_html, "builder-stage-section", fixed = TRUE)
expect_false(grepl("builder-card", core_html, fixed = TRUE))
expect_match(enhance_html, "builder-stage-section", fixed = TRUE)
expect_false(grepl("builder-card", enhance_html, fixed = TRUE))
```

Change the configure-action expectation to `1 dataset ready`, `builder-stage-footer`, and the unchanged `continue_to_review` ID.

- [ ] **Step 2: Run focused stage tests and confirm failure**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-builder-stage-server.R'); testthat::test_file('tests/testthat/test-builder-stage-inspect-core.R'); testthat::test_file('tests/testthat/test-builder-stage-enhance.R'); testthat::test_file('tests/testthat/test-builder-ui-contract.R')"
```

Expected: the legacy child roots still include `builder-card builder-section` and readiness copy still says `Ready to review`.

- [ ] **Step 3: Implement the flat Data setup composition**

In `render_configure_workbench()`, use:

```r
div(
  class = "builder-stage builder-stage-shell builder-stage-configure",
  `data-workflow-stage` = "configure",
  builder_stage_header_ui(
    "Data setup",
    "Choose data to include",
    "Define the content saved to each CRB file."
  ),
  uiOutput("dataset_context"),
  uiOutput("inspect_stage"),
  builder_core_stage_ui("core", core_model),
  builder_enhance_stage_ui(...),
  uiOutput("configure_actions")
)
```

Change `builder_configure_actions_ui()` to return `builder_stage_footer_ui()` and change the ready message in `configure_readiness()` to `N dataset ready` / `N datasets ready`.

Change only the outer classes/headings of Inspect, Core, and Enhance to flat sections. Preserve `builder-viewer-card` disclosures, attachment subcards, spatial alignment plot objects, notices, input IDs, and module output IDs. The three outer roots become `builder-stage-section builder-stage-inspect`, `builder-stage-section builder-stage-core`, and `builder-stage-section builder-stage-enhance`.

Normalize their outer spacing in `builder.features.css`; remove any margins/borders/shadows that recreate cards at the section boundary.

- [ ] **Step 4: Run focused Data setup tests and confirm pass**

Run the Step 2 command. Expected: all four files pass.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/server/review.R inst/builder/ui/inspect_stage.R inst/builder/ui/core_stage/stage.R inst/builder/ui/enhance_stage.R inst/builder/ui/workflow.R inst/builder/www/builder.features.css tests/testthat/test-builder-stage-server.R tests/testthat/test-builder-ui-contract.R
git commit -m "style(builder): flatten data setup sections"
```

### Task 4: Unify Build sections, conditional settings, and final actions

**Files:**
- Modify: `tests/testthat/test-builder-stage-review.R`
- Modify: `tests/testthat/test-builder-ui-contract.R`
- Modify: `inst/builder/ui/workflow.R`
- Modify: `inst/builder/ui/build_status.R`
- Modify: `inst/builder/www/builder.features.css`
- Modify: `inst/builder/server/workflow.R` only if output placement must change; keep observers intact.

- [ ] **Step 1: Write failing Build structure tests**

Assert that `builder_build_workbench_ui()` renders a flat shell, a reviewed-plan summary, three semantic sections, and one footer, while keeping input IDs:

```r
html <- builder_stage_html(builder_build_workbench_ui(model))
expect_match(html, "builder-stage-shell", fixed = TRUE)
expect_false(grepl("builder-stage-build builder-card", html, fixed = TRUE))
expect_match(html, "builder-stage-summary", fixed = TRUE)
expect_match(html, "Build outputs", fixed = TRUE)
```

Render Viewer App options and assert `builder-state-panel` is present and `builder-app-settings builder-card` is absent. Preserve `build_output_mode`, `build_welcome_message`, `build_host`, `build_port`, `build_require_login`, and account-action contracts.

Add a footer/action contract ensuring `back_to_review` and `build` remain unique and Build locking still disables the same controls.

- [ ] **Step 2: Run focused Build tests and confirm failure**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-builder-stage-review.R'); testthat::test_file('tests/testthat/test-builder-ui-contract.R'); testthat::test_file('tests/testthat/test-builder-loading-ui.R')"
```

Expected: Build still uses the generic card root and Viewer App settings still use `builder-card builder-section`.

- [ ] **Step 3: Implement the Build shell and semantic panels**

Change the Build root to `builder-stage builder-stage-shell builder-stage-build`. Use the shared header and summary. Wrap `build_output_options`, destination controls, and status in `builder-stage-section` boundaries without moving their input IDs.

Change Viewer App settings from `builder-card builder-section` to `builder-state-panel builder-app-settings`.

Compose one `builder-stage-footer` as the final Build child. Its status region owns the current readiness/progress/result projection; its action region contains the existing `back_to_review`, folder selection, and `build` controls in their valid states. If splitting `builder_build_stage_status_ui()` is required, keep `builder_build_stage_status_model()` unchanged and add a presentation-only helper that returns status content and action content separately. Do not duplicate `build`, `back_to_review`, or `choose_output_folder` in the DOM.

During queued/building/cancelling states, retain the fieldset lock and existing server-side mutation rejection. Result and recovery actions remain in the status section rather than being forced into the footer.

- [ ] **Step 4: Run focused Build tests and confirm pass**

Run the Step 2 command. Expected: all three files pass.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/ui/workflow.R inst/builder/ui/build_status.R inst/builder/www/builder.features.css inst/builder/server/workflow.R tests/testthat/test-builder-stage-review.R tests/testthat/test-builder-ui-contract.R tests/testthat/test-builder-loading-ui.R
git commit -m "style(builder): unify build stage structure"
```

### Task 5: Remove obsolete styling and verify the complete workflow

**Files:**
- Modify: `tests/testthat/test-builder-ui-contract.R`
- Modify: `tests/testthat/test-builder-responsive-browser.R` if a stable staged fixture exists
- Modify: `inst/builder/www/builder.layout.css`
- Modify: `inst/builder/www/builder.components.css`
- Modify: `inst/builder/www/builder.features.css`

- [ ] **Step 1: Add final architecture and responsive regression tests**

Require exactly one shared footer for each public configurable stage fixture and reject obsolete `.builder-review-confirmation`/`.builder-configure-actions` layout selectors. Add a responsive browser assertion only where the current fixture can navigate through the staged flow reliably:

```js
const footer = document.querySelector('[data-workflow-stage] .builder-stage-footer');
const progress = document.querySelector('.builder-workflow-progress');
const footerBox = footer.getBoundingClientRect();
const progressBox = progress.getBoundingClientRect();
return footerBox.bottom <= progressBox.top || footerBox.top < progressBox.top;
```

Also assert no horizontal overflow at approximately 390px.

- [ ] **Step 2: Run the architecture tests and confirm any obsolete rules fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-builder-ui-contract.R'); testthat::test_file('tests/testthat/test-builder-responsive-browser.R')"
```

Expected before cleanup: obsolete action/card selectors or responsive assumptions fail the new contract.

- [ ] **Step 3: Remove obsolete CSS and normalize final rhythm**

Delete stage-specific margin/action rules superseded by the shared primitives. Keep generic `.builder-card` for legitimate objects outside the public stage shells. Ensure `#builder-workspace` bottom padding accounts for the fixed progress bar and mobile safe area. Keep focus-visible, reduced-motion, disabled, warning, error, and build-lock styles unchanged.

- [ ] **Step 4: Run focused and full verification**

Run:

```bash
Rscript -e "testthat::test_file('tests/testthat/test-builder-stage-server.R'); testthat::test_file('tests/testthat/test-builder-stage-review.R'); testthat::test_file('tests/testthat/test-builder-stage-inspect-core.R'); testthat::test_file('tests/testthat/test-builder-stage-enhance.R'); testthat::test_file('tests/testthat/test-builder-loading-ui.R'); testthat::test_file('tests/testthat/test-builder-ui-contract.R')"
Rscript -e "testthat::test_dir('tests/testthat', filter = 'builder')"
```

Expected: focused files pass. The full Builder run passes except for any already documented baseline failure; compare any failure to the pre-change baseline before attributing it to this work.

- [ ] **Step 5: Perform browser visual QA**

Run the Builder and inspect Upload, Data setup, Review, and Build at desktop, tablet, and approximately 390px. Verify:

- one visual page surface rather than nested page cards;
- consistent header/section/footer rhythm;
- Review actions contained inside Review;
- Viewer App settings are the only conditional tinted Build panel;
- keyboard focus remains visible;
- long paths and dataset names do not widen the viewport;
- no footer or content is covered by the fixed progress navigation;
- build locking, folder selection, authentication dialog, and result/recovery states still work.

- [ ] **Step 6: Commit final cleanup**

```bash
git add inst/builder/www/builder.layout.css inst/builder/www/builder.components.css inst/builder/www/builder.features.css tests/testthat/test-builder-ui-contract.R tests/testthat/test-builder-responsive-browser.R
git commit -m "style(builder): finish workspace visual system"
```
