# Enhance Analysis Card Interaction Implementation Plan

> **For AI agents:** Required sub-skill: use superpowers:executing-plans to implement this plan task by task. Track progress with the checkboxes below.

**Goal:** Turn each optional-analysis card into a polished amber selection control and restore detailed metadata through a compact accessible Info dialog.

**Architecture:** Keep the existing Shiny checkbox and server input IDs as the source of truth. Render the checkbox inside a full-card label, place a separate Info button above it, use CSS `:has(input:checked)` for visual selection state, and use the existing Builder dialog utilities in `builder.js` to create one transient details dialog from `data-*` attributes.

**Technical stack:** R/Shiny `htmltools`, Builder CSS, vanilla JavaScript, `testthat` static/UI contracts.

---

## File structure

- Modify `inst/builder/ui/enhance_stage.R`: render selectable card labels, visually hidden checkbox inputs, and Info metadata attributes.
- Modify `inst/builder/www/builder.css`: define default, hover/focus, selected, blocked, dialog, and reduced-motion styles.
- Modify `inst/builder/www/builder.js`: open/close the reusable accessible Info dialog without toggling card selection.
- Modify `tests/testthat/test-builder-stages.R`: verify card markup, retained checkbox IDs, metadata, and blocked semantics.
- Modify `tests/testthat/test-builder-ui-contract.R`: verify CSS and client interaction/accessibility contracts.

### Task 1: Selectable analysis-card markup

**Files:**
- Modify: `tests/testthat/test-builder-stages.R`
- Modify: `inst/builder/ui/enhance_stage.R`

- [ ] **Step 1: Write a failing UI test**

Assert that rendered modules include `.enhance-module-select`, `.enhance-info-button`, the unchanged `enhance-analysis_<id>` checkbox ID, a checked state for selected modules, disabled state for blocked modules, and `data-*` values for pages, cost, network, prerequisite, replacement, and skip behaviour.

- [ ] **Step 2: Run the focused test and verify RED**

Run:

```bash
R -q -e 'devtools::test(".", filter="builder-stages", reporter="summary")'
```

Expected: failure because the new card label, Info button, and metadata attributes do not exist.

- [ ] **Step 3: Implement the minimum markup**

Render each module as:

```r
div(
  class = paste("enhance-module", blocked_class),
  tags$label(
    class = "enhance-module-select",
    tags$input(id = ns(paste0("analysis_", module$id)), type = "checkbox", ...),
    tags$span(class = "enhance-module-title", module$label),
    p(class = "consequence", module$consequence %||% "")
  ),
  tags$button(
    type = "button",
    class = "enhance-info-button",
    `aria-label` = paste("Information about", module$label),
    `data-title` = module$label,
    `data-description` = module$consequence %||% "",
    `data-pages` = paste(module$enabled_pages %||% character(), collapse = ", "),
    `data-cost` = module$cost %||% "Not applicable",
    `data-network` = module$network %||% "Not required",
    `data-prerequisite` = module$prerequisite %||% "None",
    `data-replacement` = module$replacement_policy %||% "",
    `data-skip` = module$skip_consequence %||% "",
    "i"
  )
)
```

- [ ] **Step 4: Run the focused stage test and verify GREEN**

Run the command from Step 2. Expected: PASS.

### Task 2: Amber interaction states

**Files:**
- Modify: `tests/testthat/test-builder-ui-contract.R`
- Modify: `inst/builder/www/builder.css`

- [ ] **Step 1: Write a failing CSS contract test**

Assert presence of selectors for `.enhance-module:hover`, `.enhance-module:has(input:checked)`, `.enhance-module:focus-within`, `.enhance-module.is-blocked`, `.enhance-module-checkbox`, `.enhance-info-button`, and the reduced-motion media query. Assert selected-state use of `var(--c-amber)` and hover use of `var(--c-amber-50)`.

- [ ] **Step 2: Run the focused UI contract test and verify RED**

```bash
R -q -e 'devtools::test(".", filter="builder-ui-contract", reporter="summary")'
```

Expected: failure on missing interaction selectors.

- [ ] **Step 3: Implement card styles**

Use a neutral default card, a pale amber hover/focus surface with restrained shadow, and a full amber selected surface with white title and warm pale description. Visually hide the checkbox using the existing accessible `.visually-hidden` technique. Keep blocked cards neutral, dimmed, and non-interactive. Style the Info control as a small circular button whose contrast adapts in selected cards.

- [ ] **Step 4: Run the focused UI contract test and verify GREEN**

Run the command from Step 2. Expected: PASS.

### Task 3: Accessible Info dialog

**Files:**
- Modify: `tests/testthat/test-builder-ui-contract.R`
- Modify: `inst/builder/www/builder.js`
- Modify: `inst/builder/www/builder.css`

- [ ] **Step 1: Write a failing client contract test**

Assert that `builder.js` contains `.enhance-info-button`, `showAnalysisInfo`, `prepareDialog`, `builder-analysis-info-dialog`, `aria-labelledby`, backdrop close, and focus restoration. Assert the dialog labels `Available in`, `Typical time`, `Requires`, `Network`, `If already present`, and `If skipped`.

- [ ] **Step 2: Run the focused UI contract test and verify RED**

Run the Task 2 test command. Expected: failure on missing Info dialog client behavior.

- [ ] **Step 3: Implement the transient dialog**

Add delegated click handling for `.enhance-info-button`. Create a backdrop and one dialog, populate escaped text using `textContent`, render the six labelled facts, call the existing `prepareDialog()`, close on the close button or backdrop, let the existing trap close on Escape, remove the nodes, update the body lock, and restore focus to the Info button. Stop propagation and prevent default so Info never toggles the checkbox.

- [ ] **Step 4: Add dialog styles**

Add compact title/summary layout, a responsive two-column fact grid, neutral fact tiles, and a full-width row for replacement and skip behaviour.

- [ ] **Step 5: Run the focused UI contract test and verify GREEN**

Run the Task 2 test command. Expected: PASS.

### Task 4: Focused regression and commit

**Files:** all files above.

- [ ] **Step 1: Run focused Builder regression**

```bash
R -q -e 'devtools::test(".", filter="builder-(rail|stages|ui-contract)", reporter="summary")'
```

Expected: all focused tests pass.

- [ ] **Step 2: Check patch hygiene**

```bash
git diff --check
```

Expected: no output.

- [ ] **Step 3: Commit the implementation**

```bash
git add inst/builder/ui/enhance_stage.R inst/builder/www/builder.css inst/builder/www/builder.js tests/testthat/test-builder-stages.R tests/testthat/test-builder-ui-contract.R
git commit -m "feat(builder): make enhance analyses selectable cards"
```

- [ ] **Step 4: Re-run focused regression after formatting hooks**

Run Steps 1 and 2 again. Expected: all tests pass and only the preserved untracked design draft remains.
