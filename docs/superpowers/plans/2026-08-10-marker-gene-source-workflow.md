# Marker gene source workflow 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 恢复原始的 Marker genes 全 Groups 计算路径，并用独立的 Shiny action card 重做“计算或上传预计算结果”的完整工作流。

**架构：** Marker genes 卡片只发送 action intent，选中状态完全由服务端 `settings` 渲染，不再共享或拦截 checkbox 状态。上传文件先转换为可编辑的临时 source records，逐项完成 cluster 映射后才保存为 `settings$marker_imports`；冻结计划只携带验证后的安全表格，worker 明确加载合并模块后写入 `object@misc$marker_genes`。

**技术栈：** R、Shiny、testthat、shinytest2、JavaScript/CSS、readxl、writexl、callr background worker

---

## 文件结构

- 删除旧功能文件：`inst/builder/marker_import.R`、`inst/builder/ui/marker_import.R` 以及旧功能专用测试和文档；先回到 `ddcaed3e` 的已知可用实现。
- 创建 `inst/builder/marker_import.R`：纯函数文件，负责文件 inventory、cluster 推断与映射、验证、冻结和对象合并；不得包含 Shiny reactive 状态。
- 创建 `inst/builder/ui/marker_import.R`：choice modal 与 import workbench 的无状态 UI 构造器。
- 修改 `inst/builder/ui/enhance_stage.R`：Marker genes 专用 action card；其余分析仍用 checkbox。
- 修改 `inst/builder/server/enhancements.R`：source choice、上传草稿、逐项映射、保存与移除事件。
- 修改 `inst/builder/server/review.R`：把持久状态传给 action card，并渲染导入摘要。
- 修改 `inst/builder/server/foundation.R`：为每个 dataset 初始化临时导入草稿。
- 修改 `inst/builder/plan/freeze.R`：仅冻结 ready imports。
- 修改 `inst/builder/build.R`：把导入方法合并进构建对象。
- 修改 `inst/builder/app.R`、`inst/builder/worker.R`：主进程和真实 worker 都显式 source 导入模块。
- 修改 `inst/builder/www/builder.components.css`：使用现有主题 token 的 action card、modal、状态、响应式与 reduced-motion 样式。
- 修改 `DESCRIPTION`：`readxl` 放入 Imports，`writexl` 放入 Suggests。
- 创建/修改 `tests/testthat/test-builder-marker-import.R`、`test-builder-stage-enhance.R`、`test-builder-plan-content.R`、`test-builder-build.R`、`test-builder-worker.R`、`test-builder-ui-contract.R`：纯函数、冻结、构建、worker 和 UI contract 覆盖。
- 创建 `tests/testthat/test-builder-marker-choice-browser.R`：点击可见卡片的真实浏览器回归，不直接操作隐藏 input。

### 任务 1：完整撤回旧 Marker import 实现并验证原始路径

**文件：**
- 恢复：`DESCRIPTION`
- 恢复：`inst/builder/app.R`
- 恢复：`inst/builder/build.R`
- 删除：`inst/builder/marker_import.R`
- 恢复：`inst/builder/plan/freeze.R`
- 恢复：`inst/builder/server/datasets.R`
- 删除：`inst/builder/server/enhancements.R`
- 恢复：`inst/builder/server/foundation.R`
- 恢复：`inst/builder/server/review.R`
- 恢复：`inst/builder/ui/enhance_stage.R`
- 删除：`inst/builder/ui/marker_import.R`
- 恢复：`inst/builder/worker.R`
- 恢复：`inst/builder/www/builder.components.css`
- 恢复：`inst/builder/www/builder.js`
- 删除：`tests/testthat/test-builder-marker-choice-browser.R`
- 删除：`tests/testthat/test-builder-marker-import.R`
- 恢复：其他由 `ddcaed3e..44c4b371` 修改的 feature-only 测试和旧文档

- [ ] **步骤 1：确认回滚区间只包含本功能**

运行：

```bash
git log --format='%h %s' ddcaed3e..44c4b371
git diff --stat ddcaed3e..44c4b371
```

预期：仅出现 imported Marker genes 的设计、依赖、解析、UI、worker 和修复提交；没有别的产品功能。

- [ ] **步骤 2：反向应用完整功能提交区间**

运行：

```bash
git revert --no-commit ddcaed3e..44c4b371
git diff --check
```

预期：revert 无冲突；新规格和本计划保留；所有旧 import 代码消失。

- [ ] **步骤 3：验证原始 Marker genes UI 与 Basic PBMC Build**

运行：

```bash
Rscript -e 'devtools::test(filter = "builder-stage-enhance|builder-build|builder-worker")'
```

预期：PASS；`builder_enhance_modules_ui()` 再次输出原始 `enhance-analysis_marker_genes` checkbox，未引用 `builder_attach_marker_imports`。

- [ ] **步骤 4：提交干净基线**

```bash
git add -A
git commit -m "revert(builder): remove marker gene import prototype"
```

### 任务 2：用独立 action card 建立可靠的 source choice

**文件：**
- 修改：`inst/builder/ui/enhance_stage.R`
- 创建：`inst/builder/ui/marker_import.R`
- 创建：`inst/builder/server/enhancements.R`
- 修改：`inst/builder/server/datasets.R`
- 修改：`inst/builder/server/review.R`
- 修改：`inst/builder/app.R`
- 修改：`inst/builder/www/builder.components.css`
- 测试：`tests/testthat/test-builder-stage-enhance.R`
- 创建：`tests/testthat/test-builder-marker-choice-browser.R`
- 修改：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：编写 action card 失败测试**

在 `test-builder-stage-enhance.R` 断言 Marker genes 使用按钮而非 checkbox：

```r
html <- as.character(builder_enhance_modules_ui("enhance", modules))
expect_match(html, 'id="enhance-analysis_marker_genes_action"', fixed = TRUE)
expect_match(html, 'aria-pressed="false"', fixed = TRUE)
expect_false(grepl('id="enhance-analysis_marker_genes" type="checkbox"', html, fixed = TRUE))
expect_match(html, 'id="enhance-analysis_percent_mt_ribo"', fixed = TRUE)
```

运行：`Rscript -e 'devtools::test(filter = "builder-stage-enhance")'`

预期：FAIL，找不到 `enhance-analysis_marker_genes_action`。

- [ ] **步骤 2：最小实现 action card 和 source modal**

在 `builder_enhance_modules_ui()` 中对 Marker genes 分支输出原生按钮：

```r
tags$button(
  id = ns("analysis_marker_genes_action"),
  type = "button",
  class = paste("enhance-module-select marker-genes-action", if (module$selected) "is-selected"),
  `aria-pressed` = if (module$selected) "true" else "false",
  disabled = if (module$blocked) "disabled",
  tags$span(class = "enhance-module-title", module$label),
  p(class = "consequence", module$consequence %||% "")
)
```

`builder_marker_source_choice_ui("enhance")` 输出 `enhance-marker_genes_calculate` 与 `enhance-marker_genes_upload` 两个 `actionButton()`；服务端监听 `enhance-analysis_marker_genes_action`：未选中时 `showModal()`，已选中时删除 `marker_genes` 和 `marker_imports`。

- [ ] **步骤 3：编写并运行真实浏览器红绿测试**

测试必须点击可见标题，不允许 `.click()` 隐藏 checkbox：

```r
app$click(selector = ".marker-genes-action .enhance-module-title")
app$wait_for_js("document.querySelector('.modal.show') !== null")
app$click("enhance-marker_genes_calculate")
app$wait_for_js("document.querySelector('.marker-genes-action').getAttribute('aria-pressed') === 'true'")
app$click(selector = ".marker-genes-action .enhance-module-title")
app$wait_for_js("document.querySelector('.marker-genes-action').getAttribute('aria-pressed') === 'false'")
```

运行：`Rscript -e 'devtools::test(filter = "builder-marker-choice-browser|builder-stage-enhance|builder-ui-contract")'`

预期：PASS；Cancel 不改变状态，Calculate 选中，再点击可取消。

- [ ] **步骤 4：提交 source choice**

```bash
git add inst/builder tests/testthat
git commit -m "feat(builder): choose marker gene source"
```

### 任务 3：解析文件并逐项确认 cluster 映射

**文件：**
- 创建：`inst/builder/marker_import.R`
- 修改：`DESCRIPTION`
- 修改：`inst/builder/app.R`
- 修改：`inst/builder/ui/marker_import.R`
- 修改：`inst/builder/server/enhancements.R`
- 修改：`inst/builder/server/foundation.R`
- 创建：`tests/testthat/test-builder-marker-import.R`

- [ ] **步骤 1：编写 inventory 与映射失败测试**

覆盖 CSV、TSV、多 sheet XLSX、推断后未确认、单 cluster 修改、多 cluster 列选择、未知 level、重复单 cluster 和 partial coverage：

```r
sources <- builder_marker_import_inventory(paths, names)
expect_identical(vapply(sources, `[[`, "source_name", FUN.VALUE = ""), c("T.csv", "B", "NK"))
draft <- builder_marker_import_map_single(sources[[1]], "cell_type", "T", c("T", "B"), confirmed = FALSE)
expect_false(builder_marker_import_source_ready(draft))
confirmed <- builder_marker_import_map_single(sources[[1]], "cell_type", "B", c("T", "B"), confirmed = TRUE)
expect_identical(names(confirmed$table)[[1]], "cell_type")
```

运行：`Rscript -e 'devtools::test(filter = "builder-marker-import")'`

预期：FAIL，导入函数不存在。

- [ ] **步骤 2：实现有界解析和纯映射函数**

实现固定返回结构：

```r
list(
  id = source_id,
  file_name = basename(filename),
  sheet = sheet,
  source_name = source_name,
  rows = as.integer(nrow(table)),
  columns = names(table),
  raw_table = table,
  table = NULL,
  mapping = NULL,
  cluster_column = NULL,
  cluster = NULL,
  confirmed = FALSE,
  error = NULL
)
```

CSV/TSV 使用 `utils::read.delim()`；XLSX 使用 `readxl::excel_sheets()` 与 `readxl::read_excel()`。拒绝超过 Builder 现有上传大小或行数上限的输入。`builder_marker_import_validate()` 返回 `ready`、`errors`、`coverage` 和 `warnings`，而不是让 server 重复业务规则。

- [ ] **步骤 3：实现可编辑 workbench**

上传后写入 dataset-local reactive draft，而非 `settings$marker_imports`。每行渲染：文件/sheet、维度、single 或 multi 模式、cluster select 或 column select、Confirm 按钮、文本状态；任一 unresolved 时 Save 为 disabled，missing known levels 只显示 warning。

服务端事件使用稳定 source id：

```r
observeEvent(input[[paste0("enhance-marker_source_confirm_", source$id)]], {
  draft <- marker_import_draft_of(current())
  draft$sources[[source$id]] <- builder_marker_import_confirm_source(
    draft$sources[[source$id]], input, draft$group, draft$known_levels
  )
  replace_marker_import_draft(current(), draft)
})
```

- [ ] **步骤 4：运行纯函数与 UI contract 测试**

运行：

```bash
Rscript -e 'devtools::test(filter = "builder-marker-import|builder-stage-enhance|builder-ui-contract")'
```

预期：PASS；错误有文本，Save 只在所有 source ready 时启用；所有颜色来自已有 CSS token。

- [ ] **步骤 5：提交 import workbench**

```bash
git add DESCRIPTION inst/builder tests/testthat
git commit -m "feat(builder): map imported marker tables"
```

### 任务 4：冻结安全记录并在 Build/worker 中合并

**文件：**
- 修改：`inst/builder/marker_import.R`
- 修改：`inst/builder/plan/freeze.R`
- 修改：`inst/builder/build.R`
- 修改：`inst/builder/worker.R`
- 修改：`inst/builder/server/enhancements.R`
- 测试：`tests/testthat/test-builder-marker-import.R`
- 修改：`tests/testthat/test-builder-plan-content.R`
- 修改：`tests/testthat/test-builder-build.R`
- 修改：`tests/testthat/test-builder-worker.R`

- [ ] **步骤 1：编写冻结与合并失败测试**

```r
frozen <- builder_freeze_marker_imports(settings$marker_imports)
expect_null(frozen[[1]]$sources[[1]]$raw_table)
expect_null(frozen[[1]]$sources[[1]]$datapath)
expect_true(is.data.frame(frozen[[1]]$sources[[1]]$table))

object@misc$marker_genes <- list(cerebro_seurat = list())
got <- builder_attach_marker_imports(object, frozen)
expect_named(got@misc$marker_genes, c("cerebro_seurat", "Scanpy Wilcoxon"))
expect_error(builder_attach_marker_imports(got, frozen), "already exists")
```

真实 worker contract 还需在独立进程断言：

```r
callr::r(function(worker) {
  source(worker, local = globalenv())
  stopifnot(exists("builder_attach_marker_imports", mode = "function"))
}, list(worker = worker_path))
```

运行：`Rscript -e 'devtools::test(filter = "builder-marker-import|builder-plan-content|builder-build|builder-worker")'`

预期：FAIL，freeze/attach 函数和 worker source contract 尚未完成。

- [ ] **步骤 2：保存 ready method 并冻结白名单字段**

Save 时先调用统一 validator，失败则保持 dialog 与原 settings；成功才写入：

```r
entry$settings$marker_imports[[draft$id]] <- list(
  id = draft$id,
  method = trimws(draft$method),
  group = draft$group,
  sources = lapply(draft$sources, builder_marker_import_safe_source),
  coverage = validation$coverage,
  warnings = validation$warnings,
  ready = TRUE
)
entry$settings$analyses <- setdiff(entry$settings$analyses, "marker_genes")
```

`builder_marker_import_safe_source()` 仅返回 `source_name`、`file_name`、`sheet`、`rows`、`columns`、`mapping`、`cluster_column`、`cluster`、`levels` 和 normalized `table`。

- [ ] **步骤 3：合并对象并显式加载 worker 模块**

`builder_attach_marker_imports()` 对每个方法拒绝重名，再按 group 合并所有 source table：

```r
tables <- lapply(record$sources, `[[`, "table")
merged <- do.call(rbind, tables)
rownames(merged) <- NULL
object@misc$marker_genes[[record$method]][[record$group]] <- merged
```

`worker.R` 在 source `build.R` 前 source `marker_import.R`；worker startup contract 在排队前检查 `exists("builder_attach_marker_imports", mode = "function")`。

- [ ] **步骤 4：运行 Build/worker 红绿测试**

运行：

```bash
Rscript -e 'devtools::test(filter = "builder-marker-import|builder-plan-content|builder-build|builder-worker")'
```

预期：PASS；无 import 的 Basic PBMC 不调用 attach，有 import 的对象正确合并，重复 method 明确失败，真实 worker 能找到函数。

- [ ] **步骤 5：提交 build integration**

```bash
git add inst/builder tests/testthat
git commit -m "feat(builder): build imported marker methods"
```

### 任务 5：端到端视觉、安装与最终回归

**文件：**
- 修改：`inst/builder/www/builder.components.css`
- 修改：`tests/testthat/test-builder-marker-choice-browser.R`
- 修改：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：补齐上传流程浏览器测试**

用 fixture 上传一份 filename 可推断的 CSV 与一份 multi-sheet XLSX，逐项修改/确认 cluster，断言 unresolved 禁止 Save、partial coverage 显示 warning、保存后 card `aria-pressed=true`，Review 显示 method/group/source 数。

运行：`Rscript -e 'devtools::test(filter = "builder-marker-choice-browser")'`

预期：PASS，且测试只点击可见元素。

- [ ] **步骤 2：重启 Builder 并手动验证可见路径**

停止旧 3838 进程，使用当前 worktree 启动：

```bash
Rscript inst/builder/run.R --host 127.0.0.1 --port 3838
```

在浏览器验证：第一次点击弹 choice；Cancel 不选中；Calculate 选中且 Basic PBMC Build 完成；取消后 Upload 打开 workbench；卡片不闪烁；窄屏布局一列；键盘 focus 可见。

- [ ] **步骤 3：运行一次完整项目验证**

运行：

```bash
Rscript -e 'devtools::test()'
R CMD INSTALL .
git diff --check
```

另运行项目现有的 `builder-coordinator`、`builder-auth-browser`、`builder-ui-contract` 和 pkgdown/vignette 检查命令。

预期：全部 exit 0；没有 `builder_attach_marker_imports` 缺失、浏览器 console error 或闪烁回归。

- [ ] **步骤 4：规格与质量复核**

逐条对照 `docs/superpowers/specs/2026-08-10-marker-gene-source-design.md`：source choice、互斥状态、逐项确认、partial coverage、safe freeze、method collision、worker startup、主题 token、accessibility 均有对应实现和测试。运行：

```bash
rg -n "TODO|TBD|placeholder|marker-genes-choice-checkbox|enhance-marker_genes_mode_request" inst/builder tests/testthat
```

预期：无占位符、无旧 checkbox interception 标识。

- [ ] **步骤 5：提交最终回归修整**

```bash
git add inst/builder tests/testthat DESCRIPTION
git commit -m "test(builder): verify marker gene source workflow"
```
