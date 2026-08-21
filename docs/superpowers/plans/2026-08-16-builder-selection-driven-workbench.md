# Builder selection-driven workbench 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 多文件导入期间，workbench 严格显示用户当前选择的 Ready 数据内容或 importing 数据加载页。

**架构：** 复用现有互斥选择状态：`current()` 表示 Ready dataset，`active_import_id` 表示 importing row。workbench 和 workflow progress 只读取 `active_import_id`，不再用全局 pending queue 推导导航。

**技术栈：** R、Shiny、testthat、shinytest2。

---

### 任务 1：让 workbench 遵循 rail selection

**文件：**
- 修改：`inst/builder/server/workflow.R`
- 修改：`tests/testthat/test-builder-loading-ui.R`
- 修改：`tests/testthat/test-builder-loading-browser.R`

- [x] **步骤 1：编写失败的契约测试**

在 `test-builder-loading-ui.R` 断言 `workflow.R` 的 loading workbench 与 progress 使用 `active_import_id()`，且不再读取 `import_focus_id()`：

```r
expect_match(workflow, "loading_id <- active_import_id()", fixed = TRUE)
expect_false(grepl("loading_id <- import_focus_id()", workflow, fixed = TRUE))
```

- [x] **步骤 2：运行测试确认失败**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-ui.R")'
```

预期：FAIL，现有源码仍包含 `loading_id <- import_focus_id()`。

- [x] **步骤 3：实施最小状态源修复**

在 `workflow.R` 中将 progress visibility 和 workbench loading selection 都改为 `active_import_id()`；当底层 stage 仍为 `upload`、已有 Ready dataset 且没有选中 import 时，派生显示阶段 `configure`。不修改 import queue、`current()` 或 `pick_import` observer。

- [x] **步骤 4：增加浏览器切换回归**

在现有多文件 loading browser 场景中，等待第一条 Ready 与第二条 importing 同时存在；点击 Ready 后断言 loading stage 消失且 configure 内容存在，再点击 importing row 后断言对应 loading stage 恢复。

- [x] **步骤 5：运行聚焦验证**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-ui.R"); testthat::test_file("tests/testthat/test-builder-loading-state.R")'
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-browser.R")'
```

预期：全部通过，FIFO 测试保持通过。

- [x] **步骤 6：提交**

```bash
git add inst/builder/server/workflow.R tests/testthat/test-builder-loading-ui.R tests/testthat/test-builder-loading-browser.R
git commit -m "fix(builder): follow dataset rail selection"
```
