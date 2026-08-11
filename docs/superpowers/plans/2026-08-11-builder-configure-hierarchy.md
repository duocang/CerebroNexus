# Builder Configure hierarchy 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让 Builder Configure 具有清晰的信息层级、可靠的表格命名流、简洁的 Viewer App 文案和始终可见但不遮挡内容的四步导航。

**架构：** 表格名称校验集中为纯函数，并由上传服务器、Configure readiness 与 frozen Review 共用；Viewer 从真实可用类别推导单类别/多类别选择器。Configure 继续由 `render_configure_workbench()` 组装，但每个职责区域使用语义 section。进度条保留单一 Shiny output，移进右侧 workspace，以 CSS 作为 sticky 底栏。

**技术栈：** R/Shiny、htmltools、Builder workflow reducer、CSS/JS、testthat、shinytest2/AppDriver。

---

## 文件结构

- `inst/builder/server/enhancements.R`：表名校验与上传/改名/删除状态。
- `inst/builder/server/review.R`：Configure readiness、table row UI、语义 section 组装。
- `inst/builder/ui/{enhance_stage.R,inspect_stage.R,core_stage/stage.R}`：Configure section markup。
- `inst/builder/{prerequisite.R,ui/workflow.R,app.R}`：App copy、readiness copy、workspace progress owner。
- `inst/builder/www/{builder.layout.css,builder.components.css,builder.features.css}`：控件、层级和 sticky 底栏。
- `inst/viewer/extra_material/select_content.R`：Tables-only selector。
- `tests/testthat/test-builder-{stage-enhance,stage-server,ui-contract,staged-workflow-browser}.R`：Builder regression。
- `tests/testthat/test-viewer-content-contract.R`：Viewer category compatibility。

### 任务 1：补充表格名称校验与可见状态

**文件：**
- 修改：`inst/builder/server/enhancements.R:258-337`
- 修改：`inst/builder/server/review.R:320-369`
- 测试：`tests/testthat/test-builder-stage-server.R`
- 测试：`tests/testthat/test-builder-stage-enhance.R`

- [ ] **步骤 1：写失败测试**

```r
test_that("supplementary table names are required and unique", {
  expect_identical(builder_table_display_name_error("", "Counts"), "Enter a table name.")
  expect_identical(
    builder_table_display_name_error("Counts", c("Counts", "Other"), current = "Other"),
    "Table names must be unique."
  )
  expect_null(builder_table_display_name_error("Counts", "Counts", current = "Counts"))
})

test_that("invalid table rename stays visible and blocks review", {
  # testServer fixture contains Counts and Other, then sends an empty rename.
  session$setInputs(`enhance-table_action` = list(action = "rename", key = "Counts", name = ""))
  expect_true("Counts" %in% names(entry_of(current())$settings$tables))
  expect_false(configure_readiness()$can_continue)
})
```

- [ ] **步骤 2：确认测试红灯**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-stage-server.R", reporter="summary")'`

预期：FAIL，缺少 `builder_table_display_name_error()`，或空/重复名称仍能通过。

- [ ] **步骤 3：写最小实现**

```r
builder_table_display_name_error <- function(name, existing, current = NULL) {
  name <- trimws(as.character(name %||% ""))
  if (!nzchar(name)) return("Enter a table name.")
  others <- setdiff(as.character(existing %||% character()), current %||% "")
  if (name %in% others) return("Table names must be unique.")
  NULL
}
```

Use this helper in rename handling and readiness. Table rows render filename, type/size, status, named input, inline error, and Remove. A parse failure stays as a row until removed; a rename changes only the display name.

- [ ] **步骤 4：确认 focused tests 绿灯**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-stage-server.R", reporter="summary"); testthat::test_file("tests/testthat/test-builder-stage-enhance.R", reporter="summary")'`

预期：0 failures、0 warnings。

- [ ] **步骤 5：提交**

```bash
git add inst/builder/server/enhancements.R inst/builder/server/review.R tests/testthat/test-builder-stage-server.R tests/testthat/test-builder-stage-enhance.R
git commit -m "feat(builder): validate supplementary table names"
```

### 任务 2：重组 Configure 层级并简化 App 文案

**文件：**
- 修改：`inst/builder/server/review.R:442-570`
- 修改：`inst/builder/ui/inspect_stage.R:112-170`
- 修改：`inst/builder/ui/core_stage/stage.R:1-90`
- 修改：`inst/builder/ui/enhance_stage.R:500-550`
- 修改：`inst/builder/prerequisite.R:96-130`
- 修改：`inst/builder/www/builder.components.css`
- 修改：`inst/builder/www/builder.features.css`
- 测试：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：写失败结构和 copy tests**

```r
test_that("Configure has one page heading and four scoped sections", {
  html <- builder_stage_html(render_configure_workbench())
  expect_length(regmatches(html, gregexpr("<h2[^>]*>Configure</h2>", html))[[1L]], 1L)
  expect_match(html, 'builder-configure-inspect', fixed = TRUE)
  expect_match(html, '<h3>Core settings</h3>', fixed = TRUE)
  expect_match(html, '<h3>Optional enhancements</h3>', fixed = TRUE)
  expect_match(html, '<h3>Viewer app</h3>', fixed = TRUE)
})

test_that("app publication uses product language", {
  html <- builder_stage_html(builder_app_control(list(available = FALSE, version = 0L)))
  expect_match(html, "Create a Viewer app", fixed = TRUE)
  expect_match(html, "You can still build CRB files.", fixed = TRUE)
  expect_false(grepl("privacy contract", html, fixed = TRUE))
})
```

- [ ] **步骤 2：确认测试红灯**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R", reporter="summary")'`

预期：FAIL，当前 Configure 堆叠竞争性 headings，App control 暴露内部 contract wording。

- [ ] **步骤 3：写最小 markup/CSS 实现**

```r
tags$section(
  class = "builder-configure-section builder-configure-core",
  h3("Core settings"),
  builder_core_stage_ui("core", core_model)
)
```

Render only one Configure H2. Convert child stage headings to their scoped H3/H4 role. Give text inputs and Selectize controls the same height, padding, label spacing, and focus treatment. Group the app toggle, capability copy, and auth UI beneath one `builder-configure-app` section. Use `Create a Viewer app` and the approved plain-language fallback copy.

- [ ] **步骤 4：确认 tests 绿灯**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R", reporter="summary"); testthat::test_file("tests/testthat/test-builder-stage-server.R", reporter="summary")'`

预期：0 failures、0 warnings；one H2, four H3 sections, no user-facing internal contract wording, and equal Dataset name/Organism geometry.

- [ ] **步骤 5：提交**

```bash
git add inst/builder/server/review.R inst/builder/ui/inspect_stage.R inst/builder/ui/core_stage/stage.R inst/builder/ui/enhance_stage.R inst/builder/prerequisite.R inst/builder/www/builder.components.css inst/builder/www/builder.features.css tests/testthat/test-builder-ui-contract.R
git commit -m "feat(builder): clarify configure hierarchy"
```

### 任务 3：按实际类别简化 Viewer Extra material 选择器

**文件：**
- 修改：`inst/viewer/extra_material/select_content.R:1-112`
- 测试：`tests/testthat/test-viewer-content-contract.R`

- [ ] **步骤 1：写失败 tests**

```r
test_that("Extra material skips category for tables-only data", {
  html <- render_extra_material_selector(categories = "tables", tables = "Counts")
  expect_false(grepl("Choose a category", html, fixed = TRUE))
  expect_match(html, "Choose a table", fixed = TRUE)
})

test_that("Extra material keeps category for tables and plots", {
  html <- render_extra_material_selector(categories = c("tables", "plots"))
  expect_match(html, "Choose a category", fixed = TRUE)
})
```

- [ ] **步骤 2：确认红灯**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-viewer-content-contract.R", reporter="summary")'`

预期：FAIL，因为 category selector currently renders unconditionally.

- [ ] **步骤 3：实现 category predicate**

```r
extra_material_category_needed <- function(categories) {
  length(unique(categories %||% character())) > 1L
}
```

Use `getExtraMaterialCategories()` as the source of truth. Directly render the only content selector for Tables-only data; retain current category selection for legacy Tables+Plots data.

- [ ] **步骤 4：确认绿灯**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-viewer-content-contract.R", reporter="summary")'`

预期：0 failures、0 warnings。

- [ ] **步骤 5：提交**

```bash
git add inst/viewer/extra_material/select_content.R tests/testthat/test-viewer-content-contract.R
git commit -m "feat(viewer): streamline table selection"
```

### 任务 4：把四步进度放入 workspace sticky 底栏

**文件：**
- 修改：`inst/builder/app.R:375-392`
- 修改：`inst/builder/ui/workflow.R:1-40`
- 修改：`inst/builder/www/builder.layout.css:35-90`
- 修改：`inst/builder/www/builder.components.css:901-938`
- 测试：`tests/testthat/test-builder-ui-contract.R`
- 测试：`tests/testthat/test-builder-staged-workflow-browser.R`

- [ ] **步骤 1：写失败 workspace/sticky tests**

```r
test_that("workflow progress belongs to the workspace and sticks at its bottom", {
  app <- builder_asset_text("app.R")
  layout <- builder_asset_text("www", "builder.layout.css")
  expect_match(app, 'id = "builder-workspace"', fixed = TRUE)
  expect_match(layout, "#builder-workspace .builder-workflow-progress", fixed = TRUE)
  expect_match(layout, "position: sticky", fixed = TRUE)
  expect_match(layout, "bottom:", fixed = TRUE)
})
```

Browser assertion:

```r
expect_true(app$eval_js("(() => { const p = document.querySelector('.builder-workflow-progress'); const rail = document.querySelector('.rail'); return p && p.getBoundingClientRect().bottom <= innerHeight && p.getBoundingClientRect().left >= rail.getBoundingClientRect().right; })()"))
```

- [ ] **步骤 2：确认红灯**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R", reporter="summary")'`

预期：FAIL，因为 workflow progress 是 shell 的 sibling，不在 workspace 内。

- [ ] **步骤 3：移动 output 并写 sticky CSS**

```r
div(
  id = "builder-workspace",
  class = "builder-content",
  div(id = "workbench", class = "shiny-html-output", tabindex = "-1"),
  uiOutput("workflow_progress")
)
```

```css
#builder-workspace { min-width: 0; padding-bottom: 5rem; }
#builder-workspace .builder-workflow-progress {
  position: sticky;
  bottom: max(.75rem, env(safe-area-inset-bottom));
  z-index: 10;
}
```

Keep mobile current-step labeling, a surface/shadow, 0s reduced motion, and focused-control clearance.

- [ ] **步骤 4：确认 desktop/mobile browser green**

运行：`CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-staged-workflow-browser.R", reporter="summary")'`

预期：0 failures、0 warnings；desktop and 390px widths keep progress visible inside workspace without covering rail or final focused control.

- [ ] **步骤 5：提交**

```bash
git add inst/builder/app.R inst/builder/ui/workflow.R inst/builder/www/builder.layout.css inst/builder/www/builder.components.css tests/testthat/test-builder-ui-contract.R tests/testthat/test-builder-staged-workflow-browser.R
git commit -m "feat(builder): pin workflow progress in workspace"
```

### 任务 5：整合验证与最终提交

- [ ] **步骤 1：格式和静态检查**

运行：`air format inst/builder/server/enhancements.R inst/builder/server/review.R inst/builder/ui/enhance_stage.R inst/builder/ui/inspect_stage.R inst/builder/ui/core_stage/stage.R inst/builder/prerequisite.R inst/builder/ui/workflow.R inst/builder/app.R inst/viewer/extra_material/select_content.R tests/testthat/test-builder-stage-enhance.R tests/testthat/test-builder-stage-server.R tests/testthat/test-builder-ui-contract.R tests/testthat/test-viewer-content-contract.R tests/testthat/test-builder-staged-workflow-browser.R && git diff --check`

预期：format succeeds; diff check has no output.

- [ ] **步骤 2：focused non-browser regressions**

运行：`Rscript -e 'testthat::test_dir("tests/testthat", filter="builder-(stage-enhance|stage-server|ui-contract)$", reporter="summary", stop_on_failure=TRUE); testthat::test_file("tests/testthat/test-viewer-content-contract.R", reporter="summary")'`

预期：0 failures、0 warnings。

- [ ] **步骤 3：focused browser regression**

运行：`CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-staged-workflow-browser.R", reporter="summary")'`

预期：0 failures、0 warnings，且 console 没有新增 error/warning/assert/throw。

- [ ] **步骤 4：最终 commit**

```bash
git diff --check
git add <only files changed by this plan>
git commit -m "feat(builder): refine configure workflow"
```

预期：commit 后 worktree clean。完整仓库 gate 若仍被已知 adapter/export 或 pkgdown/docs 阻塞，只报告原因，不修改它们。

## 自检

- 规格覆盖：任务 1 覆盖表格行、校验、错误和 readiness；任务 2 覆盖 hierarchy、字段密度、文案与分组；任务 3 覆盖 Viewer category compatibility；任务 4 覆盖 desktop/mobile sticky navigation；任务 5 覆盖格式和回归。
- 占位符：无 TODO、TBD 或未定义的后续步骤。
- 一致性：`builder_table_display_name_error()` 是唯一名称校验接口；`builder-workspace` 是唯一 sticky 边界；Viewer 一律从 `getExtraMaterialCategories()` 推导类别。
