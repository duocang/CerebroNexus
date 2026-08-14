# Spatial live-preview review fixes implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 修复 Spatial 实时预览拆分后的结构性渲染竞态、历史 scale 不一致、dataset 切换丢 draft，以及 apply-to-all coverage/原子性问题，并保留拖动期间零服务端 Plotly 重绘。

**架构：** 浏览器用服务端 render token 与 gated `plotly_afterplot` 完成结构性恢复；Builder state 与 preflight 统一输出 `scale = 1`；dataset rail 在提交选择前调用 Spatial gate；apply-to-all 先在内存完成所有编码，再通过 worker 批量计算全量坐标 coverage，最后单次提交。

**技术栈：** R、Shiny、Plotly、JavaScript、callr worker、testthat、shinytest2/Chrome。

---

## 文件职责

- `inst/builder/www/builder.js`：服务端 Plotly render-token 恢复握手。
- `inst/builder/spatial_alignment_server.R`：render token、dataset 切换 gate、apply-to-all 事务状态机。
- `inst/builder/state/core.R`：Builder state 的 rotation-only 坐标归一。
- `inst/builder/plan/preflight.R`：冻结前的 rotation-only 防御性归一。
- `inst/builder/ui/dataset_rail.R`：选择提交前的可注入 gate。
- `inst/builder/session.R`：worker 端全量坐标 coverage 调用。
- `inst/builder/server/foundation.R`：coverage response reactive。
- `inst/builder/server/imports.R`：coverage 请求分发和响应路由。
- `inst/builder/server/enhancements.R`：把 coverage reactive 传入 Spatial server。
- `inst/builder/server/build.R`：仅在现有用户改动不冲突时，把 rail selection 接到 Spatial gate；必须逐块保存用户现有内容。
- `inst/builder/extras.R`：matching-label 中间 bounds 使用 oriented bounds。
- `tests/testthat/test-builder-spatial.R`：R server、worker、事务和 bounds 回归。
- `tests/testthat/test-builder-dataset-rail.R`：dataset selection gate 回归。
- `tests/testthat/test-builder-spatial-live-preview-browser.R`：真实浏览器 render race 回归。
- `tests/testthat/test-builder-state.R` 或现有等价 clean 测试文件：legacy scale state/preflight 回归。

### 任务 1：用 render token 代替 80 ms 恢复

**文件：**
- 修改：`tests/testthat/test-builder-spatial-live-preview-browser.R`
- 修改：`inst/builder/spatial_alignment_server.R`
- 修改：`inst/builder/www/builder.js`

- [ ] **步骤 1：编写失败的浏览器/源码契约测试**

先加入源码契约断言，要求不存在 `setTimeout(..., 80)`，并要求 Plotly meta
包含 `builder_alignment_render_token`。在真实浏览器用 JavaScript 延迟结构性
render 完成，保存 45 度后再拖至 50 度，按 pivot 计算期望坐标：

```r
expect_false(grepl("}, 80);", builder_js, fixed = TRUE))
expect_match(server, "builder_alignment_render_token", fixed = TRUE)
expect_equal(after$x, expected_50_x, tolerance = 1e-8)
expect_equal(after$y, expected_50_y, tolerance = 1e-8)
```

- [ ] **步骤 2：运行测试验证正确失败**

运行：

```bash
Rscript -e 'devtools::test(filter = "builder-spatial-live-preview-browser", stop_on_failure = TRUE)'
```

预期：FAIL，旧实现仍依赖 80 ms，或延迟场景得到叠加旋转。

- [ ] **步骤 3：写最少 render-token 实现**

R 的 `renderPlotly` 每次执行递增非 reactive counter，并把 token 写入 layout
meta：

```r
alignment_plot_render_token <- 0L
output[["enhance-alignment_spatial_plot"]] <- plotly::renderPlotly({
  alignment_plot_render_token <<- alignment_plot_render_token + 1L
  plot <- builder_alignment_plot(...)
  plot$x$layout$meta$builder_alignment_render_token <-
    alignment_plot_render_token
  plot
})
```

JS 用 pending 记录旧 token，`shiny:value` 只 invalidate；共享函数在
`plotly_afterplot` 或新节点首次 enhance 时确认 token 已改变后才重新读取 source：

```js
var spatialDraftRestorePending = null;
function beginSpatialAlignmentServerRestore() {
  var plot = spatialAlignmentPlot();
  if (!spatialDraftRestorePending) {
    spatialDraftRestorePending = {
      plot: plot,
      token: spatialDraftRenderToken(plot)
    };
  }
  resetSpatialAlignmentDraft();
}
function completeSpatialAlignmentServerRestore(plot) {
  if (!spatialDraftRestorePending) return;
  if (spatialDraftRenderToken(plot) === spatialDraftRestorePending.token) return;
  spatialDraftRestorePending = null;
  delete plot.__builderSpatialDraftSource;
  scheduleSpatialAlignmentDraft();
}
```

- [ ] **步骤 4：运行测试验证通过**

运行同一步骤 2，并确认 drag 阶段 `__builderSpatialPlotValues === 0`。

- [ ] **步骤 5：Commit**

```bash
git add inst/builder/www/builder.js inst/builder/spatial_alignment_server.R tests/testthat/test-builder-spatial-live-preview-browser.R
git commit -m "fix(builder): synchronize spatial draft restoration"
```

### 任务 2：Builder rotation-only scale 归一

**文件：**
- 修改：`inst/builder/state/core.R`
- 修改：`inst/builder/plan/preflight.R`
- 测试：选择一个未被用户修改的 Builder state/plan 测试文件。

- [ ] **步骤 1：编写失败的 legacy scale 测试**

```r
legacy <- list(FOV_A = list(rotation_degrees = 37, scale = 1.7))
state_value <- .builder_state_spatial_coordinate_transforms(legacy)
expect_identical(state_value$FOV_A$scale, 1)
plan_value <- .builder_plan_coordinate_transform_specs(legacy, "FOV_A")
expect_identical(plan_value$FOV_A$scale, 1)
```

同时保留 generic contract 测试：`.spx_apply_coordinate_transform()` 对
`scale = 1.7` 仍按 1.7 工作。

- [ ] **步骤 2：运行测试验证正确失败**

运行选定测试文件，预期两个 Builder 断言得到 1.7 而失败。

- [ ] **步骤 3：实现 Builder-only canonicalizer**

在两个 Builder normalizer 中只在通用 normalizer 成功后改写：

```r
spec <- .spx_coordinate_transform_spec_normalize(...)
spec$scale <- 1
spec
```

不修改 `R/exportFromSeurat.R` 或 generic spatial coordinate contract。

- [ ] **步骤 4：运行 state/plan 与 generic coordinate tests**

预期 Builder 两条为 1，generic 非 1 scale 测试继续通过。

- [ ] **步骤 5：Commit**

```bash
git add inst/builder/state/core.R inst/builder/plan/preflight.R <clean-test-file>
git commit -m "fix(builder): canonicalize spatial coordinate scale"
```

### 任务 3：dataset rail 在选择提交前保护 draft

**文件：**
- 修改：`inst/builder/ui/dataset_rail.R`
- 修改：`inst/builder/spatial_alignment_server.R`
- 修改：`inst/builder/server/build.R`（只 patch rail 调用块）
- 测试：`tests/testthat/test-builder-dataset-rail.R`
- 测试：`tests/testthat/test-builder-spatial.R`

- [ ] **步骤 1：编写失败的 rail gate 与 server 测试**

rail 测试传入不执行 commit 的 gate，触发 `pick` 后断言 store 未改变；执行捕获
的 closure 后断言选择变化。Spatial server 测试覆盖无 draft 立即提交，以及
dirty draft 下 Save、Discard、Cancel。

```r
select_gate <- function(id, commit) pending_commit <<- commit
session$setInputs(pick = "dataset-b")
expect_identical(store()$current_dataset, "dataset-a")
pending_commit()
expect_identical(store()$current_dataset, "dataset-b")
```

- [ ] **步骤 2：运行测试验证正确失败**

预期 `builder_dataset_rail_server()` 不接受 gate 参数，或先切换 store。

- [ ] **步骤 3：实现 one-shot pre-commit gate**

rail 增加默认参数：

```r
select_dataset = function(id, commit) commit()
```

`input$pick` 构造 re-read/validate/reduce/on_select closure 后交给 gate。Spatial
server 增加 `pending_dataset` 与 `request_dataset_switch()`，并扩展现有
Save/Discard/Cancel observer；返回列表暴露 request 方法。

`server/build.R` 的 rail 调用只增加：

```r
select_dataset = alignment_server$request_dataset_switch,
```

修改前后分别记录该文件现有 diff，确保用户修改字节不被覆盖。

- [ ] **步骤 4：运行两组测试验证通过**

预期 Cancel 从未改变 `current_dataset`；Save/Discard 只提交一次目标选择。

- [ ] **步骤 5：Commit（仅 stage 本任务 hunks）**

```bash
git add inst/builder/ui/dataset_rail.R inst/builder/spatial_alignment_server.R tests/testthat/test-builder-dataset-rail.R tests/testthat/test-builder-spatial.R
git add -p inst/builder/server/build.R
git commit -m "fix(builder): guard spatial dataset switches"
```

### 任务 4：原子 apply-to-all 与全量 coverage

**文件：**
- 修改：`inst/builder/session.R`
- 修改：`inst/builder/server/foundation.R`
- 修改：`inst/builder/server/imports.R`
- 修改：`inst/builder/server/enhancements.R`
- 修改：`inst/builder/spatial_alignment_server.R`
- 修改：`inst/builder/extras.R`
- 修改：`tests/testthat/test-builder-spatial.R`

- [ ] **步骤 1：编写失败的 worker 和事务测试**

先测 worker-facing pure coverage helper/response，使用两个不同 section 和不同
最终 bounds，断言每个 section 独立 `outside/total`。再用可注入 coverage
reactive 测 apply-to-all：任一编码失败、coverage 缺失、`outside > 0`、请求期间
draft 改变均为零 commit；全部有效时恰好一次 commit。

```r
expect_identical(commit_count, 0L)
coverage_response(list(token = token, sections = valid_cover))
expect_identical(commit_count, 1L)
```

并把 matching-label bounds 期望改为：

```r
builder_alignment_transform_bounds(
  builder_alignment_oriented_bounds(target$base_bounds, target),
  target
)
```

- [ ] **步骤 2：运行测试验证正确失败**

预期当前 target 继承旧 coverage、source 提前 commit，且 helper 忽略 oriented
bounds。

- [ ] **步骤 3：实现 coverage worker 通道**

增加 `builder_session_spatial_coverage()`：worker 读取每个 section 的完整坐标，
应用 canonical coordinate transform，然后返回 final bounds 的
`builder_bounds_cover()`。协议新增 `spatial_coverage` kind，foundation reactive
接收 response，enhancements 把它传进 alignment server。

- [ ] **步骤 4：实现 apply-to-all pending transaction**

替换 source-first `save_current()`：source/targets 全部 finalize 到局部 images，
任何图像错误立即返回；然后 enqueue coverage。响应 observer 严格匹配 dataset、
snapshot、revision、section、label、draft parameters 和 request token。验证全部
coverage 后设置 `saved = TRUE` 并执行一次 `commit_images()`。

- [ ] **步骤 5：修正 oriented intermediate bounds**

在 `builder_alignment_apply_transform_to_matching_label()` 中与 sibling helper
使用同样的 oriented -> transform 顺序。

- [ ] **步骤 6：运行 focused spatial/worker tests**

```bash
Rscript -e 'devtools::test(filter = "builder-spatial$|multisection-spatial|builder-worker", stop_on_failure = TRUE)'
```

预期全部 PASS，无 partial commit。

- [ ] **步骤 7：Commit**

```bash
git add inst/builder/session.R inst/builder/server/foundation.R inst/builder/server/imports.R inst/builder/server/enhancements.R inst/builder/spatial_alignment_server.R inst/builder/extras.R tests/testthat/test-builder-spatial.R
git commit -m "fix(builder): finalize spatial batches atomically"
```

### 任务 5：最终回归和质量审查

**文件：**
- 仅修复由本计划引入的测试/格式问题。

- [ ] **步骤 1：运行 focused suite**

```bash
Rscript -e 'devtools::test(filter = "builder-spatial$|builder-ui-contract|builder-dataset-rail|builder-spatial-live-preview-browser|multisection-spatial|spatial-coordinate-contract|spatial-image-manifest|spatial-image-payload", stop_on_failure = TRUE)'
```

- [ ] **步骤 2：隔离重跑真实 Chrome Spatial 测试**

```bash
Rscript -e 'devtools::test(filter = "builder-spatial-live-preview-browser", stop_on_failure = TRUE)'
```

预期所有实时预览、Save 后恢复、dataset gating 和 live/saved geometry 断言通过。

- [ ] **步骤 3：运行格式、语法与 diff 检查**

```bash
node --check inst/builder/www/builder.js
Rscript -e 'air::format(file = c("inst/builder/spatial_alignment_server.R", "inst/builder/session.R", "inst/builder/server/imports.R", "inst/builder/ui/dataset_rail.R", "inst/builder/state/core.R", "inst/builder/plan/preflight.R"), check = TRUE)'
git diff --check
```

- [ ] **步骤 4：运行一次项目 final check**

```bash
scripts/precheck.sh
```

只把与本计划相关的失败视为必须修复；对已有 dirty launcher 或资源竞争失败记录
复现证据，不覆盖用户工作。

- [ ] **步骤 5：最终 specification review 和 quality review**

逐条对照设计的四个根因、失败路径和测试，确认没有 timeout 恢复、generic
export scale 退化、先切 dataset 再确认、或 source-first apply-to-all。

- [ ] **步骤 6：提交仅由最终验证产生的必要修正**

```bash
git add <only-files-changed-for-this-plan>
git commit -m "test(builder): cover spatial review regressions"
```
