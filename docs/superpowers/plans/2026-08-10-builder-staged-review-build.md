# Builder Staged Review and Build Implementation Plan

> **For the implementing AI agent:** Required sub-skill: use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to execute this plan task by task. Track every
> step with the checkboxes below.

**Goal:** Replace the always-present Builder action bar and per-dataset review
buttons with a server-owned Upload -> Configure -> Review -> Build workflow in
which one frozen project revision is confirmed before any Build UI exists.

**Architecture:** Add one pure workflow model beside the existing BuildPlan
model, then project it through a small workflow UI module and a dedicated server
orchestrator. Preserve the current worker, release coordinator, output picker,
authentication boundary, and typed result models. The workbench renders one
stage at a time; confirmation stores an inert identity of the exact frozen
BuildPlan, while output-path-specific state remains in the existing build
protocol.

**Technical stack:** R, Shiny, htmltools, testthat, shinytest2, existing
Builder five-layer CSS manifest, vanilla JavaScript, Air, package precheck.

---

## File structure

### Create

- `inst/builder/workflow.R` — pure public-stage state, frozen-plan identity,
  confirmation matching, and reducer transitions.
- `inst/builder/ui/workflow.R` — semantic four-step progress indicator and
  contained Configure, Review-confirmation, and Build-stage shells.
- `inst/builder/server/workflow.R` — Shiny reactives and observers that connect
  imports, configuration readiness, Review confirmation, and the workbench.
- `tests/testthat/test-builder-workflow.R` — pure workflow and identity tests.
- `tests/testthat/test-builder-staged-workflow-browser.R` — real-browser
  loading, configuration, Review, confirmation, invalidation, and Build-stage
  regression.

### Modify

- `inst/builder/app.R` — source the new modules, render the workflow progress,
  remove the global action bar/result placement, and source the workflow server.
- `inst/builder/server/foundation.R` — initialize workflow, selected output,
  and confirmation state; invalidate confirmation through accepted state writes.
- `inst/builder/server/review.R` — keep plan freezing and Review projection,
  remove the combined workbench/actionbar and per-dataset confirmation observers.
- `inst/builder/server/build.R` — require a matching global confirmation,
  separate folder selection from queueing, retain conflict handling, and feed
  the Build-stage status host.
- `inst/builder/server/imports.R` — stop creating and routing through
  `reviewed_revision`; invalidate project confirmation when imports change.
- `inst/builder/server/enhancements.R` — keep result actions but move the result
  projection into the Build-stage host.
- `inst/builder/ui/dataset_rail.R` — replace Reviewing/Reviewed semantics with
  Loading/Needs attention/Ready and remove compact per-dataset review navigation.
- `inst/builder/ui/review_stage.R` — retain the bounded project projection,
  make the page explicitly read-only, and add edit routes plus output summary.
- `inst/builder/ui/build_status.R` — add the stable Build-stage status model and
  host while reusing typed result actions.
- `inst/builder/www/builder.layout.css` — remove global actionbar geometry and
  add stage-shell layout.
- `inst/builder/www/builder.components.css` — style the semantic stepper and
  contained stage actions.
- `inst/builder/www/builder.features.css` — style Review and Build-specific
  summaries, progress, results, and responsive variants; remove compact-review
  navigation styles.
- `inst/builder/www/builder.js` — replace Review/Build-specific scrolling and
  compact navigator code with one server-authorized stage focus handler.
- `tests/testthat/helper-builder-app-source.R` — source the workflow module in
  state-only server tests.
- `tests/testthat/test-builder-stage-server.R` — assert one-stage workbench and
  confirmation invalidation.
- `tests/testthat/test-builder-stage-review.R` — assert the read-only global
  Review hierarchy and single confirmation action.
- `tests/testthat/test-builder-worker-app.R` — replace actionbar and
  `reviewed_revision` source contracts with confirmed-plan Build contracts.
- `tests/testthat/test-builder-ui-contract.R` — replace actionbar/compact-review
  style and JavaScript assertions with staged workflow contracts.
- `tests/testthat/test-builder-rail.R` — replace per-dataset review progress
  assertions with readiness labels.
- `tests/testthat/test-builder-import-queue.R` — assert pending imports expose
  no review or build actions.
- `tests/testthat/test-builder-loading-browser.R` — remove obsolete actionbar
  geometry expectations; keep loading geometry and visibility coverage.
- `tests/testthat/test-builder-build-folder-browser.R` — enter Review, confirm
  once, choose a folder, and build through the new stage controls.

The existing dirty Seurat omnibus fixture work is unrelated. Preserve it
exactly, use path-limited staging for every commit, and do not reformat or
restore its files.

---

### Task 1: Add the pure workflow and confirmation identity

**Files:**

- Create: `inst/builder/workflow.R`
- Create: `tests/testthat/test-builder-workflow.R`
- Modify: `inst/builder/app.R:85-110`
- Modify: `tests/testthat/helper-builder-app-source.R`

- [ ] **Step 1: Write failing workflow-state tests**

Create `tests/testthat/test-builder-workflow.R` with a minimal ready BuildPlan
and these exact behavioral assertions:

```r
builder_repo_source("workflow.R")

workflow_plan <- function(welcome = "Welcome", out_dir = "/tmp/review") {
  structure(
    list(
      error = NULL,
      readiness = "ready",
      revision = 4L,
      dataset_order = "dataset-a",
      out_dir = out_dir,
      make_app = TRUE,
      app_contract_version = 4L,
      overwrite = FALSE,
      items = list(list(
        id = "dataset-a",
        filename = "dataset-a.crb",
        source_snapshot_identity = list(object_md5 = strrep("a", 32L)),
        default_group = "cell_type",
        default_projection = "umap"
      )),
      targets = file.path(out_dir, "cerebro_app"),
      existing_targets = character(),
      manifest = list(expression = "included"),
      app_options = list(welcome_message = welcome),
      app_auth = list(enabled = FALSE, account_count = 0L),
      acknowledgements = list()
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("review identity excludes output placement but includes Viewer state", {
  base <- workflow_plan()
  moved <- workflow_plan(out_dir = "/tmp/another-output")
  moved$targets <- file.path(moved$out_dir, "cerebro_app")
  changed <- workflow_plan(welcome = "Changed")

  expect_identical(
    builder_review_plan_identity(base),
    builder_review_plan_identity(moved)
  )
  expect_false(identical(
    builder_review_plan_identity(base),
    builder_review_plan_identity(changed)
  ))
})

test_that("workflow requires Review before Build", {
  plan <- workflow_plan()
  state <- builder_workflow_state()
  state <- builder_reduce_workflow(state, list(type = "datasets_ready"))
  expect_identical(state$stage, "configure")

  expect_error(
    builder_reduce_workflow(state, list(type = "confirm_review", plan = plan)),
    "Review must be open"
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "open_review", plan = plan)
  )
  expect_identical(state$stage, "review")
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", plan = plan)
  )
  expect_identical(state$stage, "build")
  expect_true(builder_workflow_confirmation_matches(state, plan))

  changed <- workflow_plan(welcome = "Changed")
  expect_false(builder_workflow_confirmation_matches(state, changed))
  invalidated <- builder_reduce_workflow(
    state,
    list(type = "invalidate", stage = "configure")
  )
  expect_identical(invalidated$stage, "configure")
  expect_null(invalidated$confirmation)
})
```

- [ ] **Step 2: Run the tests and verify the missing API failure**

Run:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R")'
```

Expected: FAIL because `builder_review_plan_identity()` and
`builder_workflow_state()` do not exist.

- [ ] **Step 3: Implement the minimum pure model**

Create `inst/builder/workflow.R` with these public fields and transitions:

```r
builder_review_plan_identity <- function(plan) {
  if (
    !inherits(plan, "builder_build_plan") ||
      !is.list(plan) ||
      !identical(plan$readiness, "ready")
  ) {
    stop("A ready frozen BuildPlan is required.", call. = FALSE)
  }
  fields <- c(
    "revision",
    "dataset_order",
    "make_app",
    "app_contract_version",
    "items",
    "manifest",
    "app_options",
    "app_auth",
    "acknowledgements"
  )
  identity <- unclass(plan[fields])
  unserialize(serialize(identity, NULL, version = 3L))
}

builder_workflow_state <- function() {
  structure(
    list(
      stage = "upload",
      review_plan = NULL,
      confirmation = NULL,
      revision = 0L
    ),
    class = c("builder_workflow_state", "list")
  )
}

builder_workflow_confirmation_matches <- function(state, plan) {
  inherits(state, "builder_workflow_state") &&
    is.list(state$confirmation) &&
    identical(
      state$confirmation$identity,
      builder_review_plan_identity(plan)
    )
}

builder_reduce_workflow <- function(state, event) {
  if (!inherits(state, "builder_workflow_state") || !is.list(event)) {
    stop("A valid Builder workflow event is required.", call. = FALSE)
  }
  type <- event$type
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    stop("A valid Builder workflow event is required.", call. = FALSE)
  }
  if (identical(type, "empty")) {
    state$stage <- "upload"
    state$review_plan <- NULL
    state$confirmation <- NULL
  } else if (identical(type, "datasets_ready")) {
    state$stage <- "configure"
  } else if (identical(type, "open_review")) {
    state$review_plan <- unserialize(serialize(event$plan, NULL, version = 3L))
    state$stage <- "review"
  } else if (identical(type, "confirm_review")) {
    if (!identical(state$stage, "review")) {
      stop("Review must be open before it can be confirmed.", call. = FALSE)
    }
    identity <- builder_review_plan_identity(event$plan)
    if (!identical(identity, builder_review_plan_identity(state$review_plan))) {
      stop("The reviewed BuildPlan changed before confirmation.", call. = FALSE)
    }
    state$review_plan <- unserialize(serialize(event$plan, NULL, version = 3L))
    state$confirmation <- list(
      identity = identity,
      plan_revision = event$plan$revision
    )
    state$stage <- "build"
  } else if (identical(type, "back_to_review")) {
    if (is.null(state$review_plan)) {
      stop("No reviewed BuildPlan is available.", call. = FALSE)
    }
    state$stage <- "review"
  } else if (identical(type, "invalidate")) {
    state$stage <- event$stage %||% "configure"
    state$review_plan <- NULL
    state$confirmation <- NULL
  } else {
    stop("The Builder workflow event is not supported.", call. = FALSE)
  }
  state$revision <- as.integer(state$revision) + 1L
  structure(state, class = c("builder_workflow_state", "list"))
}
```

Source `workflow.R` in `inst/builder/app.R` immediately after `state.R`, and add
it to the state-only source helper.

- [ ] **Step 4: Run focused tests and Air**

Run:

```bash
air format inst/builder/workflow.R tests/testthat/test-builder-workflow.R
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R")'
```

Expected: PASS with no warnings.

- [ ] **Step 5: Commit only Task 1 paths**

```bash
git add inst/builder/workflow.R inst/builder/app.R \
  tests/testthat/helper-builder-app-source.R \
  tests/testthat/test-builder-workflow.R
git commit -m "feat(builder): add staged workflow state"
```

---

### Task 2: Split Upload and Configure and remove early Build UI

**Files:**

- Create: `inst/builder/ui/workflow.R`
- Create: `inst/builder/server/workflow.R`
- Modify: `inst/builder/app.R:96-110,300-438`
- Modify: `inst/builder/server/foundation.R:6-117,204-246`
- Modify: `inst/builder/server/review.R:397-551,669-758`
- Modify: `inst/builder/ui/review_stage.R:106-157`
- Modify: `tests/testthat/test-builder-stage-server.R`
- Modify: `tests/testthat/test-builder-import-queue.R`
- Modify: `tests/testthat/test-builder-loading-browser.R`

- [ ] **Step 1: Write failing source and server tests for the loading gate**

Add assertions that the static app shell has no `uiOutput("actionbar")`, that
the loading stage contains no `#build`, `#make_app`, Review action, or output
copy, and that Configure contains the App artifact choice plus one Continue
action:

```r
test_that("loading owns no review or build controls", {
  app <- builder_app_source_text()
  expect_false(grepl('uiOutput("actionbar")', app, fixed = TRUE))
  expect_match(app, 'uiOutput("workflow_progress")', fixed = TRUE)
  expect_match(app, 'source(file.path("ui", "workflow.R")', fixed = TRUE)
})
```

In `test-builder-loading-browser.R`, replace the old disabled-Build assertion
with:

```r
app$wait_for_js(
  paste0(
    "document.querySelector('.builder-loading-stage') !== null && ",
    "document.querySelector('.actionbar') === null && ",
    "document.getElementById('build') === null && ",
    "document.getElementById('make_app') === null && ",
    "document.getElementById('continue_to_review') === null"
  ),
  timeout = 10000
)
```

- [ ] **Step 2: Run the focused tests and verify they fail on the action bar**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-stage-server.R"); testthat::test_file("tests/testthat/test-builder-import-queue.R")'
```

Expected: FAIL because `actionbar`, `build`, and `make_app` still render while
imports are pending.

- [ ] **Step 3: Add stage UI and server orchestration**

Create `builder_workflow_progress_ui()` and a contained Configure footer in
`inst/builder/ui/workflow.R`:

```r
builder_workflow_progress_ui <- function(stage) {
  stages <- c(upload = "Upload", configure = "Configure", review = "Review", build = "Build")
  stopifnot(stage %in% names(stages))
  tags$nav(
    class = "builder-workflow-progress",
    `aria-label` = "Builder progress",
    tags$ol(lapply(names(stages), function(id) {
      tags$li(
        class = paste0("builder-workflow-step is-", if (identical(id, stage)) "current" else "idle"),
        `aria-current` = if (identical(id, stage)) "step" else NULL,
        span(class = "builder-workflow-step-index", match(id, names(stages))),
        span(stages[[id]])
      )
    }))
  )
}

builder_configure_actions_ui <- function(message, can_continue, app_control) {
  div(
    class = "builder-stage-actions builder-configure-actions",
    div(class = "builder-stage-action-copy", message),
    app_control,
    actionButton(
      "continue_to_review",
      "Continue",
      class = "btn btn-action",
      disabled = !isTRUE(can_continue)
    )
  )
}
```

Initialize `workflow <- reactiveVal(builder_workflow_state())` in foundation.
Extract the existing lines that build Import & Inspect, Core, Enhance, and App
options from `output$workbench` into `render_configure_workbench()`. Do not copy
the Review UI or `dataset_review_footer` into that function.

Drive the automatic Upload-to-Configure transition and Configure readiness with
server-owned reactives:

```r
observe({
  entries <- sets()
  pending <- imports()$entries
  current_workflow <- isolate(workflow())
  if (!length(entries) && !length(pending)) {
    if (!identical(current_workflow$stage, "upload")) {
      workflow(builder_reduce_workflow(current_workflow, list(type = "empty")))
    }
  } else if (
    length(entries) &&
      !length(pending) &&
      identical(current_workflow$stage, "upload")
  ) {
    workflow(builder_reduce_workflow(
      current_workflow,
      list(type = "datasets_ready")
    ))
  }
})

configure_readiness <- reactive({
  if (length(imports()$entries)) {
    return(list(
      can_continue = FALSE,
      message = "Wait for all datasets to finish loading."
    ))
  }
  plan <- frozen_review_plan()
  list(
    can_continue = builder_review_can_build(plan),
    message = if (builder_review_can_build(plan)) {
      paste(length(plan$items), "dataset(s) ready to review.")
    } else {
      plan$error %||% "Resolve the highlighted settings."
    }
  )
})
```

Finish `render_configure_workbench()` by appending the contained action region:

```r
configure_content <- tagList(
  uiOutput("dataset_context"),
  uiOutput("inspect_stage"),
  builder_core_stage_ui("core", core_model),
  builder_enhance_stage_ui(
    "enhance",
    builder_enhance_model(
      id = entry$id,
      profile = entry$profile,
      state = if (inherits(state, "try-error")) list() else state,
      settings = entry$settings,
      modules = list()
    ),
    dynamic_modules = TRUE
  ),
  conditionalPanel(
    condition = "input.make_app === true",
    uiOutput("review_app_options")
  )
)
readiness <- configure_readiness()
tagList(
  configure_content,
  builder_configure_actions_ui(
    message = readiness$message,
    can_continue = readiness$can_continue,
    app_control = builder_app_control(
      app_capability,
      current_value = isolate(input$make_app)
    )
  )
)
```

This content must not contain Review or Build markup.

Create `inst/builder/server/workflow.R` with one workbench switch:

```r
output$workflow_progress <- renderUI({
  builder_workflow_progress_ui(workflow()$stage)
})

output$workbench <- renderUI({
  stage <- workflow()$stage
  loading_id <- active_import_id()
  loading_entry <- if (is.null(loading_id)) NULL else builder_import_find(imports(), loading_id)
  if (!is.null(loading_entry)) {
    return(builder_loading_workbench_ui(loading_entry))
  }
  if (identical(stage, "upload")) {
    return(builder_empty_workbench_ui())
  }
  if (identical(stage, "configure")) {
    return(render_configure_workbench())
  }
  if (identical(stage, "review")) {
    return(render_review_workbench())
  }
  render_build_workbench()
})
```

Move `builder_app_control()` beside `review_app_options` in Configure. Remove
`uiOutput("actionbar")` and the separate `uiOutput("result_card")` from the app
shell. Remove `output$actionbar`, `output$review_action_summary`, and
`output$build_actions`; Build controls return in Task 4.

Update the first-run copy to `Build your first Viewer in four steps`, with
Upload, Configure, Review, and Build as separate ordered-list items.

Source the UI module after `ui/review_stage.R`. Source the server module after
`server/review.R` and before `server/build.R`.

- [ ] **Step 4: Run unit tests and the loading browser regression**

```bash
air format inst/builder/app.R inst/builder/ui/workflow.R \
  inst/builder/server/workflow.R inst/builder/server/foundation.R \
  inst/builder/server/review.R inst/builder/ui/review_stage.R \
  tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-import-queue.R \
  tests/testthat/test-builder-loading-browser.R
Rscript -e 'testthat::test_file("tests/testthat/test-builder-stage-server.R"); testthat::test_file("tests/testthat/test-builder-import-queue.R")'
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-browser.R")'
```

Expected: loading DOM contains no Review or Build controls; Configure still
renders after the example finishes loading.

- [ ] **Step 5: Commit only Task 2 paths**

```bash
git add inst/builder/app.R inst/builder/ui/workflow.R \
  inst/builder/server/workflow.R inst/builder/server/foundation.R \
  inst/builder/server/review.R inst/builder/ui/review_stage.R \
  tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-import-queue.R \
  tests/testthat/test-builder-loading-browser.R
git commit -m "feat(builder): separate upload and configure"
```

---

### Task 3: Replace per-dataset Review with one global frozen Review

**Files:**

- Modify: `inst/builder/server/workflow.R`
- Modify: `inst/builder/server/foundation.R:204-246`
- Modify: `inst/builder/server/review.R:397-667`
- Modify: `inst/builder/server/imports.R:880-1055`
- Modify: `inst/builder/ui/dataset_rail.R:143-351,826-866`
- Modify: `inst/builder/ui/review_stage.R:1007-end`
- Modify: `inst/builder/ui/workflow.R`
- Modify: `tests/testthat/test-builder-workflow.R`
- Modify: `tests/testthat/test-builder-stage-review.R`
- Modify: `tests/testthat/test-builder-stage-server.R`
- Modify: `tests/testthat/test-builder-rail.R`
- Modify: `tests/testthat/test-builder-worker-app.R:331-355`

- [ ] **Step 1: Write failing global Review tests**

Add reducer and server assertions for these contracts:

```r
test_that("navigation does not invalidate confirmation but accepted edits do", {
  plan <- workflow_plan()
  state <- builder_reduce_workflow(
    builder_reduce_workflow(builder_workflow_state(), list(type = "datasets_ready")),
    list(type = "open_review", plan = plan)
  )
  state <- builder_reduce_workflow(state, list(type = "confirm_review", plan = plan))
  review_again <- builder_reduce_workflow(state, list(type = "back_to_review"))
  expect_true(builder_workflow_confirmation_matches(review_again, plan))
  expect_null(builder_reduce_workflow(
    review_again,
    list(type = "invalidate", stage = "configure")
  )$confirmation)
})
```

Update Review UI tests to assert exactly one
`Looks good — continue to build`, one `Back to settings`, read-only dataset/App
summaries, and no `review_current_dataset`. Update rail tests to allow only
Ready, Needs attention, Loading, Blocked, and Reload required labels.

- [ ] **Step 2: Run focused tests and verify old per-dataset semantics fail**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R"); testthat::test_file("tests/testthat/test-builder-stage-review.R"); testthat::test_file("tests/testthat/test-builder-rail.R")'
```

Expected: FAIL on `reviewed_revision`, Reviewing/Reviewed labels, and missing
global confirmation actions.

- [ ] **Step 3: Implement Review entry, confirmation, and invalidation**

Add these observers to `server/workflow.R`:

```r
observeEvent(input$continue_to_review, {
  plan <- isolate(frozen_review_plan())
  req(builder_review_can_build(plan))
  workflow(builder_reduce_workflow(
    isolate(workflow()),
    list(type = "open_review", plan = plan)
  ))
  session$sendCustomMessage("builder_focus_stage", list(id = "review"))
})

observeEvent(input$back_to_settings, {
  current <- isolate(workflow())
  current$stage <- "configure"
  current$revision <- current$revision + 1L
  workflow(structure(current, class = c("builder_workflow_state", "list")))
  session$sendCustomMessage("builder_focus_stage", list(id = "configure"))
})

observeEvent(input$confirm_review, {
  plan <- isolate(frozen_review_plan())
  workflow(builder_reduce_workflow(
    isolate(workflow()),
    list(type = "confirm_review", plan = plan)
  ))
  session$sendCustomMessage("builder_focus_stage", list(id = "build"))
})
```

Make `render_review_workbench()` consume only `workflow()$review_plan`, project
it through `builder_review_model()`, and append:

```r
builder_review_confirmation_ui <- function() {
  div(
    class = "builder-stage-actions builder-review-confirmation",
    div(
      class = "builder-stage-action-copy",
      strong("Ready to continue?"),
      span("Confirm this frozen revision to open the Build step.")
    ),
    actionButton("back_to_settings", "Back to settings", class = "btn"),
    actionButton(
      "confirm_review",
      "Looks good — continue to build",
      class = "btn btn-action"
    )
  )
}
```

Delete `builder_dataset_is_reviewed()`, `builder_review_progress()`,
`builder_next_unreviewed()`, `builder_compact_dataset_review_ui()`,
`dataset_review_footer`, and all Review previous/next observers. Map rail status
directly from `builder_dataset_state(entry)$readiness` through
`.builder_rail_readiness()`.

Remove `reviewed_revision = NULL` from new import entries. Whenever
`replace_entry()` accepts a changed settings object, or import add/remove/reorder
changes the dataset set, compare a fresh Review identity with the stored
confirmation. If they differ, reduce `invalidate` to Configure. Merely selecting
a dataset or opening Configure must not clear confirmation.

- [ ] **Step 4: Run global Review, rail, and server tests**

```bash
air format inst/builder/server/workflow.R inst/builder/server/foundation.R \
  inst/builder/server/review.R inst/builder/server/imports.R \
  inst/builder/ui/dataset_rail.R inst/builder/ui/review_stage.R \
  inst/builder/ui/workflow.R tests/testthat/test-builder-workflow.R \
  tests/testthat/test-builder-stage-review.R \
  tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-rail.R \
  tests/testthat/test-builder-worker-app.R
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R"); testthat::test_file("tests/testthat/test-builder-stage-review.R"); testthat::test_file("tests/testthat/test-builder-stage-server.R"); testthat::test_file("tests/testthat/test-builder-rail.R"); testthat::test_file("tests/testthat/test-builder-worker-app.R")'
```

Expected: PASS; repository search finds no production
`reviewed_revision`, `review_current_dataset`, or `Review datasets`.

- [ ] **Step 5: Commit only Task 3 paths**

```bash
git add inst/builder/server/workflow.R inst/builder/server/foundation.R \
  inst/builder/server/review.R inst/builder/server/imports.R \
  inst/builder/ui/dataset_rail.R inst/builder/ui/review_stage.R \
  inst/builder/ui/workflow.R tests/testthat/test-builder-workflow.R \
  tests/testthat/test-builder-stage-review.R \
  tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-rail.R \
  tests/testthat/test-builder-worker-app.R
git commit -m "feat(builder): add project-wide review"
```

---

### Task 4: Gate Build behind confirmation and separate folder selection

**Files:**

- Modify: `inst/builder/server/foundation.R:91-117`
- Modify: `inst/builder/server/workflow.R`
- Modify: `inst/builder/server/build.R:9-232`
- Modify: `inst/builder/ui/workflow.R`
- Modify: `tests/testthat/test-builder-workflow.R`
- Modify: `tests/testthat/test-builder-worker-app.R:282-355`
- Modify: `tests/testthat/test-builder-build-folder-browser.R`

- [ ] **Step 1: Write failing confirmed-Build tests**

Extend source tests to require `selected_output <- reactiveVal(NULL)`, remove
the multi-dataset confirmation dialog, and reject build queueing unless
`builder_workflow_confirmation_matches(workflow(), frozen_review_plan())` is
true.

Rewrite the browser setup sequence as:

```r
app$click("continue_to_review")
app$wait_for_js("document.getElementById('confirm_review') !== null", timeout = 30000)
app$click("confirm_review")
app$wait_for_js(
  paste0(
    "document.querySelector('[data-workflow-stage=build]') !== null && ",
    "document.getElementById('choose_output_folder') !== null && ",
    "document.getElementById('build').disabled === true"
  ),
  timeout = 30000
)
```

After the fake native picker returns a path, assert that `#build` becomes
enabled but no work is queued until it is clicked.

- [ ] **Step 2: Run the folder and source tests and verify failure**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-worker-app.R")'
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-build-folder-browser.R")'
```

Expected: FAIL because the current Build button chooses a folder and queues
immediately, and because it still checks per-dataset review revisions.

- [ ] **Step 3: Implement confirmed Build and selected output state**

Initialize `selected_output <- reactiveVal(NULL)` in foundation. Add the Build
shell to `ui/workflow.R`:

```r
builder_build_stage_ui <- function(model, output_path = NULL) {
  div(
    class = "builder-stage builder-stage-build",
    `data-workflow-stage` = "build",
    h2("Build your Viewer"),
    p(class = "stage-intro", "Your reviewed configuration is frozen and ready."),
    builder_review_output_summary_ui(model),
    div(
      class = "builder-build-folder",
      div(
        class = "builder-build-path",
        if (is.null(output_path)) "No output folder selected" else output_path
      ),
      actionButton("choose_output_folder", "Choose folder…", class = "btn")
    ),
    uiOutput("build_stage_status")
  )
}

builder_review_output_summary_ui <- function(model) {
  div(
    class = "builder-reviewed-output",
    div(
      class = "builder-reviewed-output-item",
      span("Datasets"),
      strong(model$dataset_count)
    ),
    div(
      class = "builder-reviewed-output-item",
      span("Output"),
      strong(model$output_label)
    )
  )
}
```

Render the Build workbench only from the confirmed frozen plan:

```r
render_build_workbench <- function() {
  current_workflow <- workflow()
  req(
    identical(current_workflow$stage, "build"),
    inherits(current_workflow$review_plan, "builder_build_plan")
  )
  model <- builder_review_model(current_workflow$review_plan, result())
  builder_build_stage_ui(model, isolate(selected_output()))
}
```

Change folder selection so it sets `selected_output(choice$path)` and returns
without calling `prepare_selected_output()`. Replace the old `input$build`
observer with:

```r
observeEvent(input$build, {
  req(identical(isolate(workflow())$stage, "build"))
  review_plan <- isolate(frozen_review_plan())
  req(builder_workflow_confirmation_matches(isolate(workflow()), review_plan))
  path <- isolate(selected_output())
  req(builder_stage_has_text(path %||% ""))
  prepare_selected_output(path)
})
```

Before `prepare_selected_output()` enqueues an actual output plan, compare the
current Review identity again. On mismatch, clear `selected_output`, invalidate
the workflow to Configure, and announce that settings changed.

Retain only conflict actions in `builder_build_dialog`; remove
`review_required`, `attention_required`, and the second multi-dataset
confirmation. Add `back_to_review` handling that preserves confirmation.

- [ ] **Step 4: Run focused Build tests**

```bash
air format inst/builder/server/foundation.R inst/builder/server/workflow.R \
  inst/builder/server/build.R inst/builder/ui/workflow.R \
  tests/testthat/test-builder-workflow.R \
  tests/testthat/test-builder-worker-app.R \
  tests/testthat/test-builder-build-folder-browser.R
Rscript -e 'testthat::test_file("tests/testthat/test-builder-workflow.R"); testthat::test_file("tests/testthat/test-builder-worker-app.R")'
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-build-folder-browser.R")'
```

Expected: folder selection and Build are two distinct accepted actions; a
matching confirmation is required at both entry points.

- [ ] **Step 5: Commit only Task 4 paths**

```bash
git add inst/builder/server/foundation.R inst/builder/server/workflow.R \
  inst/builder/server/build.R inst/builder/ui/workflow.R \
  tests/testthat/test-builder-workflow.R \
  tests/testthat/test-builder-worker-app.R \
  tests/testthat/test-builder-build-folder-browser.R
git commit -m "feat(builder): gate build by confirmed plan"
```

---

### Task 5: Move progress, errors, and results into one Build-stage host

**Files:**

- Modify: `inst/builder/ui/build_status.R:373-531`
- Modify: `inst/builder/ui/workflow.R`
- Modify: `inst/builder/server/build.R:301-320`
- Modify: `inst/builder/server/enhancements.R:347-420`
- Modify: `inst/builder/app.R:314-397`
- Modify: `tests/testthat/test-builder-stage-server.R`
- Modify: `tests/testthat/test-builder-worker-app.R`
- Modify: `tests/testthat/test-builder-build-folder-browser.R`

- [ ] **Step 1: Write failing stable-host tests**

Assert that the Build page contains exactly one
`id="build-stage-status"`, that queued/running/result rendering happens inside
it, and that `result_card` is absent from the global pane. Add model tests for
idle, queued, running, success, needs-decision, failure, and recovery-required
states.

```r
test_that("Build status projection keeps one stable host", {
  idle <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = list(build_status = "idle"),
    note = NULL,
    result = NULL,
    output_selected = TRUE
  )
  expect_identical(idle$state, "ready")
  expect_true(idle$can_build)

  running <- builder_build_stage_status_model(
    flow = list(stage = "building"),
    protocol = list(build_status = "running"),
    note = "Building 3 datasets…",
    result = NULL,
    output_selected = TRUE
  )
  expect_identical(running$state, "building")
  expect_false(running$can_build)
})
```

- [ ] **Step 2: Run status tests and verify the missing model failure**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-worker-app.R"); testthat::test_file("tests/testthat/test-builder-stage-server.R")'
```

Expected: FAIL because build state is split among topbar `busy`, action button,
and global `result_card`.

- [ ] **Step 3: Implement the stable Build status model and host**

Add `builder_build_stage_status_model()` to `ui/build_status.R`. Its returned
fields are exactly `state`, `message`, `pipeline_state`, `can_build`, and
`result_model`. Map protocol states as follows:

```r
builder_build_stage_status_model <- function(
  flow,
  protocol,
  note,
  result,
  output_selected
) {
  build_status <- protocol$build_status %||% "idle"
  state <- if (!is.null(result)) {
    "result"
  } else if (build_status %in% c("queued", "running", "cancelling")) {
    if (identical(build_status, "queued")) "queued" else "building"
  } else if (identical(flow$stage, "choosing_folder")) {
    "choosing_folder"
  } else {
    "ready"
  }
  list(
    state = state,
    message = note,
    pipeline_state = switch(state, queued = "queued", building = "building", NULL),
    can_build = identical(state, "ready") && isTRUE(output_selected),
    result_model = if (is.null(result)) NULL else builder_build_status_model(result)
  )
}
```

Render every state through one outer host:

```r
builder_build_stage_status_ui <- function(model) {
  stopifnot(is.list(model), builder_stage_has_text(model$state %||% ""))
  content <- switch(
    model$state,
    ready = actionButton(
      "build",
      "Build Viewer",
      class = "btn btn-action",
      disabled = !isTRUE(model$can_build)
    ),
    choosing_folder = div(
      class = "builder-build-waiting",
      span(class = "spinner"),
      span("Choosing output folder…")
    ),
    queued = tagList(
      builder_build_pipeline_ui("queued"),
      p(model$message %||% "Build queued…")
    ),
    building = tagList(
      builder_build_pipeline_ui("building"),
      p(model$message %||% "Building Viewer…")
    ),
    result = builder_build_status_ui(model$result_model),
    stop("The Build-stage status is unsupported.", call. = FALSE)
  )
  div(
    id = "build-stage-status",
    class = paste("builder-build-stage-status", paste0("is-", model$state)),
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true",
    content
  )
}
```

Connect the host to existing protocol and result state in `server/build.R`:

```r
output$build_stage_status <- renderUI({
  req(identical(workflow()$stage, "build"))
  model <- builder_build_stage_status_model(
    flow = build_flow(),
    protocol = protocol() %||% list(build_status = "idle"),
    note = busy_note(),
    result = result(),
    output_selected = builder_stage_has_text(selected_output() %||% "")
  )
  builder_build_stage_status_ui(model)
})
```

Ready shows `Build Viewer`; queued/building shows the existing server-driven
pipeline and note; result shows `builder_build_status_ui(model$result_model)`.
Preserve existing Open App, Reveal Folder, Copy Path, report, retry, worker
restart, and manual recovery actions.

Remove global `uiOutput("result_card")`. Keep the topbar `busy` output only for
non-build import/inspection work; when the workflow stage is Build, the stage
host owns the pipeline and live status.

- [ ] **Step 4: Run status, result-action, and folder browser tests**

```bash
air format inst/builder/ui/build_status.R inst/builder/ui/workflow.R \
  inst/builder/server/build.R inst/builder/server/enhancements.R \
  inst/builder/app.R tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-worker-app.R \
  tests/testthat/test-builder-build-folder-browser.R
Rscript -e 'testthat::test_file("tests/testthat/test-builder-worker-app.R"); testthat::test_file("tests/testthat/test-builder-stage-server.R")'
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-build-folder-browser.R")'
```

Expected: one stable Build host progresses from ready to building; terminal
results and their typed actions remain reachable inside that host.

- [ ] **Step 5: Commit only Task 5 paths**

```bash
git add inst/builder/ui/build_status.R inst/builder/ui/workflow.R \
  inst/builder/server/build.R inst/builder/server/enhancements.R \
  inst/builder/app.R tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-worker-app.R \
  tests/testthat/test-builder-build-folder-browser.R
git commit -m "feat(builder): stabilize build status host"
```

---

### Task 6: Finish responsive styling, focus, and real-browser flow

**Files:**

- Modify: `inst/builder/www/builder.layout.css:319-440`
- Modify: `inst/builder/www/builder.components.css`
- Modify: `inst/builder/www/builder.features.css`
- Modify: `inst/builder/www/builder.js:900-950,1900-2180`
- Modify: `tests/testthat/test-builder-ui-contract.R:494-594`
- Create: `tests/testthat/test-builder-staged-workflow-browser.R`

- [ ] **Step 1: Write failing CSS, focus, and end-to-end browser tests**

Replace compact per-dataset Review assertions with these contracts:

```r
test_that("staged workflow is semantic, contained, and reduced-motion safe", {
  layout <- builder_asset_text("www", "builder.layout.css")
  components <- builder_asset_text("www", "builder.components.css")
  features <- builder_asset_text("www", "builder.features.css")
  js <- builder_asset_text("www", "builder.js")

  expect_false(grepl(".actionbar", layout, fixed = TRUE))
  expect_match(components, ".builder-workflow-progress", fixed = TRUE)
  expect_match(components, ".builder-stage-actions", fixed = TRUE)
  expect_match(features, ".builder-stage-review", fixed = TRUE)
  expect_match(features, ".builder-stage-build", fixed = TRUE)
  expect_match(js, 'addCustomMessageHandler("builder_focus_stage"', fixed = TRUE)
  expect_false(grepl("setupCompactReviewNavigator", js, fixed = TRUE))
  expect_match(features, "prefers-reduced-motion: reduce", fixed = TRUE)
})
```

Create one shinytest2 flow that, at 1920, 768, and 390 CSS pixels, verifies:

1. loading has no future-stage controls;
2. Configure has one Continue action;
3. Review has exactly one confirmation action and no editable form controls;
4. confirmation reveals Build;
5. returning to settings and changing the dataset name clears confirmation and
   hides Build;
6. repeating Review restores Build;
7. focus lands on the active stage heading and horizontal overflow is absent.

- [ ] **Step 2: Run UI contracts and verify obsolete styles/handlers fail**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R")'
```

Expected: FAIL on `.actionbar`, compact review navigator CSS/JavaScript, and
missing stage focus handler.

- [ ] **Step 3: Implement the five-layer styles and one focus handler**

Remove `.actionbar` geometry from `builder.layout.css`. Place only shell and
responsive stage layout there. Put reusable progress/actions in
`builder.components.css` and Review/Build-specific cards in
`builder.features.css`.

Replace `builder_focus_review`, `builder_focus_build`, compact Review
measurement, IntersectionObserver, and Review segment click code with:

```js
window.Shiny.addCustomMessageHandler("builder_focus_stage", function (message) {
  var id = message && message.id ? String(message.id) : "";
  var stage = document.querySelector('[data-workflow-stage="' + id + '"]');
  if (!stage) return;
  var heading = stage.querySelector("h2");
  if (!heading) return;
  if (!heading.hasAttribute("tabindex")) heading.setAttribute("tabindex", "-1");
  heading.scrollIntoView({
    block: "start",
    behavior: reducedMotion.matches ? "auto" : "smooth",
  });
  heading.focus({ preventScroll: true });
  scheduleStatusAnnouncement("Opened " + id + " step.");
});
```

At `max-width: 40rem`, stack action buttons full width and show the progress
indicator as the current ordinal plus label. Under reduced motion, set stage,
status, and focus-related transition durations to zero.

- [ ] **Step 4: Run style, responsive, and end-to-end browser regressions**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R")'
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-staged-workflow-browser.R")'
node --check inst/builder/www/builder.js
git diff --check
```

Expected: all tests PASS; no console warnings/errors and no horizontal overflow
at 1920, 768, or 390 CSS pixels.

- [ ] **Step 5: Commit only Task 6 paths**

```bash
git add inst/builder/www/builder.layout.css \
  inst/builder/www/builder.components.css \
  inst/builder/www/builder.features.css inst/builder/www/builder.js \
  tests/testthat/test-builder-ui-contract.R \
  tests/testthat/test-builder-staged-workflow-browser.R
git commit -m "feat(builder): polish staged build flow"
```

---

### Task 7: Run specification and repository verification

**Files:**

- Verify all files changed in Tasks 1-6
- Do not modify unrelated Seurat omnibus fixture files

- [ ] **Step 1: Scan for obsolete production contracts**

```bash
rg -n 'reviewed_revision|review_current_dataset|Review datasets|builder_focus_review|builder_focus_build|setupCompactReviewNavigator|class = "actionbar"' inst/builder
```

Expected: no matches. Test descriptions may mention the removed behavior only
when asserting its absence.

- [ ] **Step 2: Run the focused non-browser suite**

```bash
Rscript -e 'files <- c(
  "tests/testthat/test-builder-workflow.R",
  "tests/testthat/test-builder-stage-server.R",
  "tests/testthat/test-builder-stage-review.R",
  "tests/testthat/test-builder-worker-app.R",
  "tests/testthat/test-builder-rail.R",
  "tests/testthat/test-builder-import-queue.R",
  "tests/testthat/test-builder-ui-contract.R"
); invisible(lapply(files, testthat::test_file))'
```

Expected: all focused tests PASS with no failures.

- [ ] **Step 3: Run the three real-browser regressions**

```bash
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'files <- c(
  "tests/testthat/test-builder-loading-browser.R",
  "tests/testthat/test-builder-build-folder-browser.R",
  "tests/testthat/test-builder-staged-workflow-browser.R"
); invisible(lapply(files, testthat::test_file))'
```

Expected: all browser tests PASS; Chrome logs contain no warning, error,
assertion, or thrown exception.

- [ ] **Step 4: Run repository-required formatting and full precheck**

```bash
scripts/precheck.sh
```

Expected: Air, testthat, R CMD check, and pkgdown complete successfully. If the
known local Chrome debug-port startup issue blocks an unrelated broad browser
suite, preserve the failure output and still require all three focused browser
tests from Step 3 to pass.

- [ ] **Step 5: Review the final diff and commit only verification fixes**

```bash
git diff --check
git status --short
git diff -- inst/builder tests/testthat
```

Confirm that every changed production/test path belongs to this plan and that
the pre-existing fixture changes are byte-for-byte untouched. If verification
required a scoped fix, commit only those paths:

```bash
git add inst/builder/workflow.R inst/builder/app.R \
  inst/builder/server/foundation.R inst/builder/server/workflow.R \
  inst/builder/server/review.R inst/builder/server/build.R \
  inst/builder/server/imports.R inst/builder/server/enhancements.R \
  inst/builder/ui/workflow.R inst/builder/ui/dataset_rail.R \
  inst/builder/ui/review_stage.R inst/builder/ui/build_status.R \
  inst/builder/www/builder.layout.css \
  inst/builder/www/builder.components.css \
  inst/builder/www/builder.features.css inst/builder/www/builder.js \
  tests/testthat/helper-builder-app-source.R \
  tests/testthat/test-builder-workflow.R \
  tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-stage-review.R \
  tests/testthat/test-builder-worker-app.R \
  tests/testthat/test-builder-rail.R \
  tests/testthat/test-builder-import-queue.R \
  tests/testthat/test-builder-ui-contract.R \
  tests/testthat/test-builder-loading-browser.R \
  tests/testthat/test-builder-build-folder-browser.R \
  tests/testthat/test-builder-staged-workflow-browser.R
git commit -m "test(builder): verify staged build flow"
```

If no fix was required, do not create an empty commit.
