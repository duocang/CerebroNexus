# Builder 数据集切换反馈实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 数据集点击立即更新左栏并显示右侧轻量加载层，同时缓存契约未变化的 Spatial 预览，消除无反馈等待和重复 Worker 计算。

**架构：** 浏览器持有短生命周期的乐观切换状态，Shiny 的 rail patch 继续作为最终真相；Spatial 模块通过带数据集、section 和设置契约的 session cache 复用预览。服务器只发送 `spatial`、`ready`、`error` 三类权威里程碑，客户端用目标 dataset id 和本地 generation 拒绝过时完成信号。

**技术栈：** R/Shiny、原生 JavaScript、CSS、testthat、shiny::testServer、shinytest2 浏览器契约。

---

## 文件结构

- 修改 `inst/builder/app.R`：提供稳定的 Spatial cache key，复用既有 preview cache record 结构。
- 修改 `inst/builder/server/foundation.R`：创建 session-local `spatial_previews` reactive cache。
- 修改 `inst/builder/server/enhancements.R`：把 cache 注入 Spatial alignment 模块。
- 修改 `inst/builder/spatial_alignment_server.R`：执行 cache hit/miss、发送 Spatial 阶段和 ready 里程碑。
- 修改 `inst/builder/server/imports.R`：Worker 返回时先缓存，再只向匹配的当前数据集应用；失败时释放加载状态。
- 修改 `inst/builder/www/builder.js`：实现乐观 rail 选中、workbench veil、代际竞态保护和服务器 reconciliation。
- 修改 `inst/builder/www/builder.components.css`：实现轻量遮罩、spinner、淡出和 reduced-motion。
- 修改 `tests/testthat/test-builder-spatial.R`：验证首次请求、缓存复用、契约失效和 stale result 保护。
- 修改 `tests/testthat/test-builder-ui-contract.R`：验证客户端状态、ARIA 和 CSS 契约。
- 修改 `tests/testthat/test-builder-loading-browser.R`：验证点击后无需等待服务器即可得到视觉反馈，并在 ready 后清除。

### 任务 1：建立 Spatial preview cache 契约

**文件：**
- 修改：`inst/builder/app.R:141-166`
- 修改：`inst/builder/server/foundation.R:349-352`
- 修改：`inst/builder/server/enhancements.R:333-352`
- 测试：`tests/testthat/test-builder-spatial.R`

- [ ] **步骤 1：编写失败的 cache key 与注入测试**

在 `tests/testthat/test-builder-spatial.R` 增加：

```r
test_that("Spatial preview cache keys separate datasets and sections", {
  expect_identical(
    builder_spatial_preview_cache_key("dataset-a", "section-1"),
    "dataset-a::section-1"
  )
  expect_false(identical(
    builder_spatial_preview_cache_key("dataset-a", "section-1"),
    builder_spatial_preview_cache_key("dataset-a", "section-2")
  ))
})

test_that("Builder owns one session-local Spatial preview cache", {
  foundation <- readLines(
    builder_profile_inst_path("builder", "server", "foundation.R"),
    warn = FALSE
  )
  enhancements <- readLines(
    builder_profile_inst_path("builder", "server", "enhancements.R"),
    warn = FALSE
  )
  expect_match(paste(foundation, collapse = "\n"),
    "spatial_previews <- reactiveVal(list())", fixed = TRUE)
  expect_match(paste(enhancements, collapse = "\n"),
    "spatial_previews = spatial_previews", fixed = TRUE)
})
```

- [ ] **步骤 2：运行测试并确认因缺少 key/cache 而失败**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-spatial.R")'
```

预期：FAIL，提示 `builder_spatial_preview_cache_key` 不存在，且源码中没有 `spatial_previews`。

- [ ] **步骤 3：实现最小 cache key 和依赖注入**

在 `inst/builder/app.R` 的 preview cache helpers 旁增加：

```r
builder_spatial_preview_cache_key <- function(id, section) {
  stopifnot(
    is.character(id), length(id) == 1L, !is.na(id), nzchar(id),
    is.character(section), length(section) == 1L,
    !is.na(section), nzchar(section)
  )
  paste(id, section, sep = "::")
}
```

在 `inst/builder/server/foundation.R` 增加：

```r
spatial_previews <- reactiveVal(list())
```

在 `builder_spatial_alignment_server()` 参数列表和 `server/enhancements.R` 调用点增加：

```r
spatial_previews
```

并在模块入口的 `stopifnot()` 中确认它是函数。

- [ ] **步骤 4：运行 focused 测试确认通过**

运行同一步骤 2。预期：新增测试 PASS。

- [ ] **步骤 5：提交 cache 边界**

```bash
git add inst/builder/app.R inst/builder/server/foundation.R inst/builder/server/enhancements.R inst/builder/spatial_alignment_server.R tests/testthat/test-builder-spatial.R
git commit -m "feat(builder): add spatial preview cache boundary"
```

### 任务 2：缓存命中、写回和失效

**文件：**
- 修改：`inst/builder/spatial_alignment_server.R:190-225, 481-575`
- 修改：`inst/builder/server/imports.R:1340-1380`
- 测试：`tests/testthat/test-builder-spatial.R`

- [ ] **步骤 1：编写首次请求、复用和失效测试**

在 `tests/testthat/test-builder-spatial.R` 用 `shiny::testServer()` 创建两个 entry、一个 `reactiveVal(list())` cache 和记录请求的 `enqueue`，验证：

```r
expect_length(requests, 1L)
expect_identical(requests[[1L]]$kind, "spatial_preview")

key <- builder_spatial_preview_cache_key("dataset-a", "section-a")
cache <- builder_preview_cache_store(
  builder_preview_cache_begin(spatial_previews(), key, contract),
  key,
  preview
)
spatial_previews(cache)
current("dataset-b")
session$flushReact()
current("dataset-a")
session$flushReact()

expect_length(requests, 1L)
expect_identical(alignment_preview(), preview)

changed_entry <- current_entry()
changed_entry$settings$default_group <- "other_group"
current_entry(changed_entry)
session$flushReact()
expect_length(requests, 2L)
```

- [ ] **步骤 2：运行测试确认缓存尚未被消费**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-spatial.R")'
```

预期：FAIL；回访仍 enqueue 第二次，或 `alignment_preview()` 未从 cache 恢复。

- [ ] **步骤 3：实现 request_preview 的 pending/ready 分支**

在 `request_preview()` 中按以下顺序处理：

```r
contract <- preview_contract_for(entry, section)
key <- builder_spatial_preview_cache_key(entry$id, section)
cache <- shiny::isolate(spatial_previews())
record <- cache[[key]] %||% NULL

if (builder_preview_cache_hit(cache, key, contract)) {
  preview_contract(contract)
  if (identical(record$status, "ready")) {
    value <- builder_preview_cache_frames(cache, key)
    alignment_preview(value)
    if (isTRUE(value$available)) {
      spatial_coords(list(
        x = value$spatial$x, y = value$spatial$y,
        sx = value$spatial$x, sy = value$spatial$y
      ))
    }
  }
  return(invisible(TRUE))
}

queued <- enqueue(list(
  kind = "spatial_preview",
  id = entry$id,
  section = section,
  preview_cache_key = key,
  preview_contract = contract,
  default_projection = entry$settings$default_projection %||% NULL,
  group = entry$settings$default_group %||% NULL,
  assay = entry$settings$assay %||% NULL,
  layer = entry$settings$layer %||% "data",
  replaces = "spatial_alignment",
  note = paste0("Loading paired views for ", section, "…")
))
if (isTRUE(queued)) {
  spatial_previews(builder_preview_cache_begin(cache, key, contract))
  preview_contract(contract)
}
```

- [ ] **步骤 4：Worker 结果先写 cache，再应用当前视图**

在 `inst/builder/server/imports.R` 的 `spatial_preview` 分支开头增加：

```r
cache_key <- p$preview_cache_key %||%
  builder_spatial_preview_cache_key(p$id, p$section)
cache <- isolate(spatial_previews())
if (!builder_preview_cache_hit(cache, cache_key, p$preview_contract)) {
  cache <- builder_preview_cache_begin(cache, cache_key, p$preview_contract)
}
spatial_previews(builder_preview_cache_store(cache, cache_key, value))
```

保留现有的 `current()` 与 `active_slice()` 双重守卫；只有当前视图匹配时才设置 `alignment_preview()` 和 `spatial_coords()`。

- [ ] **步骤 5：运行测试确认通过并提交**

运行步骤 2；预期新增用例 PASS。

```bash
git add inst/builder/spatial_alignment_server.R inst/builder/server/imports.R tests/testthat/test-builder-spatial.R
git commit -m "perf(builder): reuse spatial previews across dataset switches"
```

### 任务 3：增加权威的切换阶段信号

**文件：**
- 修改：`inst/builder/spatial_alignment_server.R:481-575, 1110-1160`
- 修改：`inst/builder/server/imports.R:985-1020`
- 测试：`tests/testthat/test-builder-spatial.R`

- [ ] **步骤 1：编写 milestone 消息测试**

在 Spatial module 的 `shiny::testServer()` 用 `session$sendCustomMessage` spy 验证：

```r
expect_true(any(vapply(messages, function(item) {
  identical(item$type, "builder_dataset_switch_state") &&
    identical(item$message$dataset, "dataset-a") &&
    identical(item$message$state, "spatial")
}, logical(1))))

alignment_preview(preview)
session$flushReact()
expect_true(any(vapply(messages, function(item) {
  identical(item$message$state, "ready")
}, logical(1))))
```

另加非 Spatial entry 测试，`session$flushReact()` 后收到 `ready`，而不是永久停留在 `switching`。

- [ ] **步骤 2：运行测试确认当前没有 milestone**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-spatial.R")'
```

预期：FAIL；没有 `builder_dataset_switch_state` 消息。

- [ ] **步骤 3：实现统一消息函数和 ready 顺序**

在模块内增加：

```r
send_switch_state <- function(dataset, state, section = NULL) {
  session$sendCustomMessage(
    "builder_dataset_switch_state",
    list(dataset = dataset, state = state, section = section)
  )
}
```

在 preview miss/pending 时发送 `spatial`。在 `builder_spatial_canvas_scene` 消息之后发送 matching `ready`，确保浏览器先收到 canvas scene：

```r
session$sendCustomMessage("builder_spatial_canvas_scene", scene)
send_switch_state(entry$id, "ready", section)
```

没有 Spatial section 时，用 `session$onFlushed(..., once = TRUE)` 发送 matching `ready`，回调中再次检查 `identical(current(), dataset)`。

- [ ] **步骤 4：错误分支释放 matching loader**

在 `inst/builder/server/imports.R` 的 Worker error 分支，在重启协议前增加：

```r
if (identical(p$kind, "spatial_preview")) {
  session$sendCustomMessage(
    "builder_dataset_switch_state",
    list(dataset = p$id, state = "error", section = p$section)
  )
}
```

- [ ] **步骤 5：运行测试确认通过并提交**

运行步骤 2；预期新增用例 PASS。

```bash
git add inst/builder/spatial_alignment_server.R inst/builder/server/imports.R tests/testthat/test-builder-spatial.R
git commit -m "feat(builder): report dataset switch readiness"
```

### 任务 4：浏览器即时选中与轻量 workbench veil

**文件：**
- 修改：`inst/builder/www/builder.js:80-105, 1500-1695, 3270-3280, 3500-3560`
- 修改：`inst/builder/www/builder.components.css:1020-1040`
- 测试：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：编写失败的客户端和无障碍契约测试**

在 `tests/testthat/test-builder-ui-contract.R` 增加：

```r
test_that("dataset switching gives immediate honest feedback", {
  js <- builder_asset_text("www", "builder.js")
  css <- builder_stylesheet_text()

  expect_match(js, "function beginDatasetSwitch", fixed = TRUE)
  expect_match(js, "function settleDatasetSwitch", fixed = TRUE)
  expect_match(js, 'workbench.setAttribute("aria-busy", "true")', fixed = TRUE)
  expect_match(js, 'text.textContent = "Switching dataset…"', fixed = TRUE)
  expect_match(js, '"This is taking longer than expected…"', fixed = TRUE)
  expect_match(js, '"builder_dataset_switch_state"', fixed = TRUE)
  expect_match(js, "datasetSwitchState.generation", fixed = TRUE)
  expect_match(css, ".builder-dataset-switch-veil", fixed = TRUE)
  expect_match(css, "@media (prefers-reduced-motion: reduce)", fixed = TRUE)
})
```

- [ ] **步骤 2：运行测试确认缺少客户端状态机**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R")'
```

预期：FAIL；JS 函数和 CSS class 不存在。

- [ ] **步骤 3：实现 generation-aware 客户端状态**

在 `builder.js` 顶层状态旁增加：

```js
var datasetSwitchState = {
  target: null,
  generation: 0,
  phase: "idle",
  timeout: null,
};
```

实现 `beginDatasetSwitch(target)`：若 target 已是 authoritative current 则返回 `false`；否则增加 generation、乐观更新所有 `.builder-pick` 的 `aria-current` 和 `.ds.is-active`、给 `#workbench` 设置 `aria-busy="true"`，插入单例 `.builder-dataset-switch-veil`，并显示 `Switching dataset…`。捕获本次 generation，并在 8 秒后仅当 generation 和 target 仍匹配时把文案改成 `This is taking longer than expected…`；timeout 不清除遮罩、不伪装成功，也不取消服务器请求。

实现 `updateDatasetSwitchPhase(message)`：仅当 `message.dataset === datasetSwitchState.target` 时将文案改为 `Preparing Spatial preview…`，matching `ready` 调用 `settleDatasetSwitch(true)`，matching `error` 调用 `settleDatasetSwitch(false)`。

实现 `settleDatasetSwitch()`：清 timeout、移除 `aria-busy`、添加短暂退出 class 后删除 veil；不移动焦点。

- [ ] **步骤 4：接入点击和 authoritative rail reconciliation**

把 pick handler 改成：

```js
var pick = target.closest(".builder-pick");
if (pick) {
  if (beginDatasetSwitch(pick.dataset.ds)) {
    send("pick", pick.dataset.ds);
  }
  if (narrowManager.matches) closeDatasetManager();
  return;
}
```

在 `reconcileDatasetRail()` 完成 DOM patch 后，读取 authoritative `.builder-pick[aria-current=true]`。若其 id 与 pending target 不同且 rail patch 已明确选择其他数据集，调用 `settleDatasetSwitch(false)`；相同则保留 veil等待 workbench/Spatial ready。

注册：

```js
window.Shiny.addCustomMessageHandler(
  "builder_dataset_switch_state",
  updateDatasetSwitchPhase
);
```

- [ ] **步骤 5：实现 option A CSS**

在 `builder.components.css` 增加相对定位和遮罩：

```css
#workbench { position: relative; }
.builder-dataset-switch-veil {
  position: absolute;
  inset: 0;
  z-index: 20;
  display: grid;
  min-height: 18rem;
  place-items: center;
  border-radius: var(--r-lg);
  background: rgb(255 255 255 / .82);
  backdrop-filter: blur(1px);
  opacity: 1;
  transition: opacity var(--duration-fast) var(--ease);
}
.builder-dataset-switch-status {
  display: grid;
  justify-items: center;
  gap: var(--space-2);
  color: var(--c-text-muted);
  font-size: .9rem;
}
.builder-dataset-switch-veil.is-leaving { opacity: 0; }
@media (prefers-reduced-motion: reduce) {
  .builder-dataset-switch-veil { transition: none; }
  .builder-dataset-switch-status .spinner { animation: none; }
}
```

- [ ] **步骤 6：运行契约测试确认通过并提交**

运行步骤 2；预期新增用例 PASS。

```bash
git add inst/builder/www/builder.js inst/builder/www/builder.components.css tests/testthat/test-builder-ui-contract.R
git commit -m "feat(builder): show dataset switch progress immediately"
```

### 任务 5：浏览器竞态与最终回归

**文件：**
- 修改：`tests/testthat/test-builder-loading-browser.R`

- [ ] **步骤 1：添加真实浏览器回归用例**

在已加载两个数据集的 browser fixture 中，在第二行 click 的同一 JS task 内读取状态：

```r
immediate <- app$get_js(paste0(
  "(() => { const rows = document.querySelectorAll('.builder-pick'); ",
  "rows[1].click(); return { ",
  "selected: rows[1].getAttribute('aria-current'), ",
  "busy: document.getElementById('workbench').getAttribute('aria-busy'), ",
  "veil: Boolean(document.querySelector('.builder-dataset-switch-veil')) ",
  "}; })()"
))
expect_identical(immediate$selected, "true")
expect_identical(immediate$busy, "true")
expect_true(immediate$veil)
```

等待 matching ready 后验证：

```r
app$wait_for_js(
  "document.querySelector('.builder-dataset-switch-veil') === null && document.getElementById('workbench').getAttribute('aria-busy') !== 'true'",
  timeout = 30000
)
```

增加 A → B → A 快速点击，确认最终 `aria-current` 是 A 且 B 的 stale ready 不会清除 A 的 pending veil。

- [ ] **步骤 2：运行 browser test 观察红灯**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-browser.R")'
```

预期：修复前 immediate feedback 断言失败；实现后 PASS。若本机 browser harness 不可用，保留用例交 CI，并运行任务 1–4 的非 browser focused tests。

- [ ] **步骤 3：运行 focused 非浏览器回归**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-spatial.R"); testthat::test_file("tests/testthat/test-builder-ui-contract.R")'
```

预期：0 failed、0 errors。

- [ ] **步骤 4：检查范围和未提交文件**

```bash
git diff --check
git status --short
git diff --stat HEAD~4..HEAD
```

预期：无 whitespace error；只包含本计划文件和原先已存在的用户改动。不得把任务开始前已有的 `build.R`、`project.R`、`build_status.R` 及其测试改动误纳入提交。

- [ ] **步骤 5：提交浏览器回归**

```bash
git add tests/testthat/test-builder-loading-browser.R
git commit -m "test(builder): cover dataset switch feedback races"
```

- [ ] **步骤 6：推送并运行远程 CI**

```bash
git push duocang feat/builder-project-workspace
gh workflow run R-tests.yaml --ref feat/builder-project-workspace
gh workflow run R-cmd-check.yaml --ref feat/builder-project-workspace
```

预期：两个 workflow 都针对最终实现 SHA 排队；在 CI 完成前只报告“已推送并已触发”，不报告“全部通过”。
