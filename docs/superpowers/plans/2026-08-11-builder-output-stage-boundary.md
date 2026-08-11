# Builder Output-Stage Boundary Implementation Plan

> **For AI agents:** Required sub-skill: use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task by task. Track progress with the checkboxes below.

**Goal:** Move optional Viewer App selection and configuration from Data setup into Build while making the restrained bottom stage labels safely navigable.

**Architecture:** Separate the frozen CRB review identity from a later immutable build request. The workflow reducer continues to own legal public-stage transitions; a Build-stage draft owns output mode and Viewer App fields until queueing freezes them into the existing BuildPlan/request pipeline. Server-rendered navigation consumes an explicit availability model and emits navigation intents rather than inferring state from the DOM.

**Technical stack:** R, Shiny, htmltools, testthat, shinytest2/chromote, existing Builder reducer and build protocol.

---

## File structure

- Modify `inst/builder/workflow.R`: CRB-only confirmation identity, legal navigation events, and stage-availability projection.
- Modify `inst/builder/ui/workflow.R`: restrained typographic navigation and Data setup footer without App controls.
- Modify `inst/builder/server/workflow.R`: handle stage-navigation intents and enforce execution locks.
- Modify `inst/builder/server/review.R`: freeze/review CRB plans only and remove App controls from Data setup.
- Modify `inst/builder/ui/review_stage.R`: remove App fields from Review and retain CRB-only summary.
- Modify `inst/builder/ui/build_status.R`: render output-mode and expanded Viewer App settings ahead of destination/build controls.
- Modify `inst/builder/server/build.R`: own Build-stage output/App draft, validate it, freeze the final executable plan, and preserve it across navigation.
- Modify `inst/builder/plan/freeze.R`: support a CRB review plan that is finalized with output settings only at Build.
- Modify `inst/builder/www/builder.features.css`: pure-type baseline navigation and Build settings layout.
- Modify `inst/builder/www/builder.js`: focus/navigation behavior only where server-rendered Shiny controls need client support.
- Modify focused tests under `tests/testthat/`: reducer, stage contracts, server flow, App/auth propagation, responsive browser behavior, and style contracts.

### Task 1: Split CRB review identity from final output identity

**Files:**
- Modify: `inst/builder/workflow.R`
- Modify: `inst/builder/plan/freeze.R`
- Test: `tests/testthat/test-builder-workflow.R`
- Test: `tests/testthat/test-builder-plan-core.R`

- [ ] **Step 1: Write failing identity tests**

Add tests proving that App-only changes do not change the review identity, while CRB content changes do:

```r
test_that("review identity contains CRB intent but excludes output intent", {
  plan <- builder_workflow_test_plan()
  identity <- builder_review_plan_identity(plan)

  app_changed <- plan
  app_changed$make_app <- FALSE
  app_changed$app_options$welcome_message <- "Different"
  app_changed$app_auth <- list(enabled = TRUE, account_count = 2L)
  expect_identical(identity, builder_review_plan_identity(app_changed))

  crb_changed <- plan
  crb_changed$items[[1L]]$included_groups <- "cell_type"
  expect_false(identical(identity, builder_review_plan_identity(crb_changed)))
})
```

- [ ] **Step 2: Run the focused tests and confirm RED**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R")'
```

Expected: failure because `.builder_workflow_identity_fields` still includes `make_app`, `app_options`, and `app_auth`.

- [ ] **Step 3: Implement the CRB identity**

Replace the identity field list with CRB-relevant fields only and expose a separate final-request identity helper:

```r
.builder_review_identity_fields <- c(
  "revision", "dataset_order", "items", "manifest", "acknowledgements"
)

builder_final_build_identity <- function(plan) {
  list(
    review = builder_review_plan_identity(plan),
    output = plan[c("make_app", "app_contract_version", "app_options", "app_auth")]
  )
}
```

Keep output paths and overwrite/conflict state out of both intent identities.

- [ ] **Step 4: Run focused tests and confirm GREEN**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R"); testthat::test_file("tests/testthat/test-builder-plan-core.R")'
```

Expected: all assertions pass.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/workflow.R inst/builder/plan/freeze.R tests/testthat/test-builder-workflow.R tests/testthat/test-builder-plan-core.R
git commit -m "refactor(builder): split review identity"
```

### Task 2: Make Data setup and Review CRB-only

**Files:**
- Modify: `inst/builder/server/review.R`
- Modify: `inst/builder/ui/review_stage.R`
- Modify: `inst/builder/ui/workflow.R`
- Test: `tests/testthat/test-builder-stage-review.R`
- Test: `tests/testthat/test-builder-stage-server.R`
- Test: `tests/testthat/test-builder-ui-contract.R`

- [ ] **Step 1: Write failing stage-boundary tests**

Assert the public label and absence of App controls:

```r
test_that("Data setup contains only CRB configuration", {
  html <- as.character(builder_configure_actions_ui("Ready to review", TRUE))
  expect_match(html, "Ready to review", fixed = TRUE)
  expect_false(grepl("Create a Viewer app", html, fixed = TRUE))
  expect_false(grepl("make_app", html, fixed = TRUE))
})

test_that("Review does not project optional output settings", {
  html <- as.character(builder_review_stage_ui(
    "review", builder_review_model(builder_stage_review_plan())
  ))
  expect_false(grepl("Welcome message|Login|Viewer App", html))
})
```

- [ ] **Step 2: Run the focused tests and confirm RED**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-stage-review.R"); testthat::test_file("tests/testthat/test-builder-ui-contract.R")'
```

Expected: current Configure output still contains the App checkbox/options and Review still projects App experience.

- [ ] **Step 3: Remove App controls from Data setup**

Change `builder_configure_actions_ui()` to accept only readiness and Continue state:

```r
builder_configure_actions_ui <- function(message, can_continue) {
  div(
    class = "builder-stage-actions builder-configure-actions",
    p(class = "builder-configure-readiness", message),
    actionButton("continue_to_review", "Continue", class = "btn btn-action",
      disabled = !can_continue)
  )
}
```

Remove `review_app_options`, its conditional panel, and App/auth validation from `render_configure_workbench()`. Freeze the Review plan with `make_app = FALSE`, default output-neutral App fields, and no authentication material.

- [ ] **Step 4: Make Review CRB-only and rename the stage**

Change the visible label from Configure to Data setup in progress/navigation and workspace headings. Remove `model$app` and all App experience/output-mode claims from `builder_review_model()` and `builder_review_stage_ui()`.

- [ ] **Step 5: Run focused tests and confirm GREEN**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-stage-review.R"); testthat::test_file("tests/testthat/test-builder-stage-server.R"); testthat::test_file("tests/testthat/test-builder-ui-contract.R")'
```

Expected: all focused stage-boundary tests pass.

- [ ] **Step 6: Commit**

```bash
git add inst/builder/server/review.R inst/builder/ui/review_stage.R inst/builder/ui/workflow.R tests/testthat/test-builder-stage-review.R tests/testthat/test-builder-stage-server.R tests/testthat/test-builder-ui-contract.R
git commit -m "refactor(builder): make review CRB-only"
```

### Task 3: Add Build-stage output and Viewer App configuration

**Files:**
- Modify: `inst/builder/ui/build_status.R`
- Modify: `inst/builder/server/build.R`
- Modify: `inst/builder/ui/review_stage.R` (reuse typed App-option constructors)
- Modify: `inst/builder/coordinator.R`
- Test: `tests/testthat/test-builder-worker-app.R`
- Test: `tests/testthat/test-builder-app-bundle.R`
- Test: `tests/testthat/test-builder-auth.R`

- [ ] **Step 1: Write failing Build UI/model tests**

Cover the default CRB-only mode and expanded App settings:

```r
test_that("Build owns output mode and expanded App settings", {
  crb <- builder_build_options_ui(builder_build_options())
  expect_match(as.character(crb), "CRB files only", fixed = TRUE)
  expect_false(grepl("Welcome message", as.character(crb), fixed = TRUE))

  app <- builder_build_options_ui(builder_build_options(make_app = TRUE))
  html <- as.character(app)
  expect_match(html, "CRB files + Viewer App", fixed = TRUE)
  expect_match(html, "Welcome message", fixed = TRUE)
  expect_match(html, "Port", fixed = TRUE)
  expect_match(html, "Require login", fixed = TRUE)
})
```

- [ ] **Step 2: Run the focused tests and confirm RED**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-worker-app.R")'
```

Expected: missing `builder_build_options()` / `builder_build_options_ui()`.

- [ ] **Step 3: Implement a typed Build draft and UI**

Create a typed value with explicit defaults:

```r
builder_build_options <- function(
  make_app = FALSE,
  welcome_message = "Welcome to CerebroNexus!",
  host = "127.0.0.1",
  port = 8080L,
  launch_browser = TRUE,
  show_upload_ui = FALSE,
  initial_page = "data_info",
  initial_dataset = NULL
) {
  structure(list(
    make_app = make_app,
    welcome_message = welcome_message,
    host = host,
    port = as.integer(port),
    launch_browser = launch_browser,
    show_upload_ui = show_upload_ui,
    initial_page = initial_page,
    initial_dataset = initial_dataset
  ), class = c("builder_build_options", "list"))
}
```

Render radio choices first. Render the complete App field group directly below only when `make_app` is true; do not use `<details>` or a nested modal for ordinary App fields. Keep account editing in the existing secure dialog.

- [ ] **Step 4: Finalize the executable plan at Build**

In `server/build.R`, preserve a `reactiveVal(builder_build_options())`, validate each accepted edit, and combine it with the confirmed CRB plan immediately before folder preflight/queueing. Populate `make_app`, `app_options`, and the safe `app_auth` summary there. Ensure `builder_final_build_identity()` is checked before enqueue.

- [ ] **Step 5: Verify exact App propagation**

Extend bundle tests so welcome, host, port, launch behavior, upload policy, starting page/dataset, and auth summary reach `createShinyApp()` unchanged. Reuse credential sentinels only in in-memory test fixtures and retain secret-leak assertions.

- [ ] **Step 6: Run focused tests and confirm GREEN**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-worker-app.R"); testthat::test_file("tests/testthat/test-builder-app-bundle.R"); testthat::test_file("tests/testthat/test-builder-auth.R")'
```

Expected: all assertions pass.

- [ ] **Step 7: Commit**

```bash
git add inst/builder/ui/build_status.R inst/builder/server/build.R inst/builder/ui/review_stage.R inst/builder/coordinator.R tests/testthat/test-builder-worker-app.R tests/testthat/test-builder-app-bundle.R tests/testthat/test-builder-auth.R
git commit -m "feat(builder): configure outputs in Build"
```

### Task 4: Make stage labels safely navigable

**Files:**
- Modify: `inst/builder/workflow.R`
- Modify: `inst/builder/ui/workflow.R`
- Modify: `inst/builder/server/workflow.R`
- Test: `tests/testthat/test-builder-workflow.R`
- Test: `tests/testthat/test-builder-stage-server.R`

- [ ] **Step 1: Write failing availability and navigation tests**

```r
test_that("stage navigation exposes only legal destinations", {
  availability <- builder_workflow_availability(
    stage = "review", datasets_ready = TRUE,
    review_ready = TRUE, confirmed = FALSE, locked = FALSE
  )
  expect_identical(availability, c(
    upload = TRUE, configure = TRUE, review = TRUE, build = FALSE
  ))
})

test_that("navigation preserves confirmation without mutation", {
  state <- builder_confirmed_workflow_fixture()
  review <- builder_reduce_workflow(state, list(type = "navigate", stage = "review"))
  expect_true(builder_workflow_confirmation_matches(review, state$review_plan))
})
```

- [ ] **Step 2: Run and confirm RED**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R")'
```

Expected: availability function and general navigation event do not exist.

- [ ] **Step 3: Implement reducer and server navigation**

Add a pure availability projection and a `navigate` event that accepts only stages authorized by the supplied availability. Render available stages as Shiny action links/buttons and disabled stages as non-interactive text. Ignore navigation intents while `builder_mutations_locked()` is true.

- [ ] **Step 4: Run focused tests and confirm GREEN**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R"); testthat::test_file("tests/testthat/test-builder-stage-server.R")'
```

Expected: legal navigation passes, gated/locked transitions remain rejected, and no-op navigation preserves confirmation.

- [ ] **Step 5: Commit**

```bash
git add inst/builder/workflow.R inst/builder/ui/workflow.R inst/builder/server/workflow.R tests/testthat/test-builder-workflow.R tests/testthat/test-builder-stage-server.R
git commit -m "feat(builder): navigate workflow stages"
```

### Task 5: Apply the restrained typographic stage style

**Files:**
- Modify: `inst/builder/www/builder.features.css`
- Modify: `inst/builder/www/builder.js`
- Test: `tests/testthat/test-builder-style-contract.R`
- Test: `tests/testthat/test-builder-responsive-browser.R`
- Test: `tests/testthat/test-builder-staged-workflow-browser.R`

- [ ] **Step 1: Write failing visual-contract tests**

Assert that navigation has no generated icons/numbers/status labels, the current stage has `aria-current`, unavailable stages are disabled/non-interactive, and CSS uses the shared secondary text scale rather than an enlarged stage-specific font size.

- [ ] **Step 2: Run and confirm RED**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-style-contract.R"); testthat::test_file("tests/testthat/test-builder-staged-workflow-browser.R")'
```

Expected: current ordered-list styling and non-clickable labels fail the new contract.

- [ ] **Step 3: Implement the baseline style**

Use neutral dark completed labels, semantic orange current text with a short bottom rule, clearly light-gray unavailable text, and one quiet baseline. Keep the label font on the existing secondary UI scale and use modest weight changes only. Remove circular steps, checkmarks, numerical markers, pills, uppercase state copy, and connector graphics.

- [ ] **Step 4: Implement responsive/focus behavior**

At narrow widths reduce gaps or wrap labels without replacing text with icons. Preserve visible keyboard focus, `aria-current`, native disabled behavior, reduced motion, safe-area spacing, and non-overlap with Data setup/Build controls.

- [ ] **Step 5: Run browser tests and confirm GREEN**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-style-contract.R"); testthat::test_file("tests/testthat/test-builder-responsive-browser.R"); testthat::test_file("tests/testthat/test-builder-staged-workflow-browser.R")'
```

Expected: desktop, tablet, and phone assertions pass.

- [ ] **Step 6: Commit**

```bash
git add inst/builder/www/builder.features.css inst/builder/www/builder.js tests/testthat/test-builder-style-contract.R tests/testthat/test-builder-responsive-browser.R tests/testthat/test-builder-staged-workflow-browser.R
git commit -m "style(builder): refine stage navigation"
```

### Task 6: End-to-end regression and final verification

**Files:**
- Modify: `tests/testthat/test-builder-end-to-end.R`
- Modify: `tests/testthat/test-builder-auth-browser.R`
- Modify: `tests/testthat/test-builder-build-folder-browser.R`
- Modify: `tests/testthat/test-builder-ui-contract.R`

- [ ] **Step 1: Add end-to-end paths**

Cover:

```text
Upload -> Data setup -> Review -> Build -> CRB only
Upload -> Data setup -> Review -> Build -> Viewer App -> auth accounts -> build
Build -> Review -> Build, with Build settings preserved
Build -> Data setup -> accepted CRB edit, with confirmation invalidated
```

- [ ] **Step 2: Run the complete focused Builder suite**

Run:

```bash
Rscript -e 'testthat::test_dir("tests/testthat", filter = "builder", reporter = "summary")'
```

Expected: zero failures.

- [ ] **Step 3: Run source and whitespace checks**

Run:

```bash
node --check inst/builder/www/builder.js
git diff --check
```

Expected: both commands exit successfully with no output.

- [ ] **Step 4: Run the repository precheck once**

Run:

```bash
scripts/precheck.sh
```

Expected: success, or only the already documented unrelated repository blockers. Record exact failures rather than claiming a clean precheck.

- [ ] **Step 5: Perform specification and quality review**

Compare the committed implementation against every section of
`docs/superpowers/specs/2026-08-11-builder-output-stage-boundary-design.md`.
Check for stale user-facing `Configure`, App controls outside Build, duplicated
state, secret exposure, inaccessible navigation, and untested error paths.

- [ ] **Step 6: Commit test completion**

```bash
git add tests/testthat/test-builder-end-to-end.R tests/testthat/test-builder-auth-browser.R tests/testthat/test-builder-build-folder-browser.R tests/testthat/test-builder-ui-contract.R
git commit -m "test(builder): cover output-stage flow"
```
