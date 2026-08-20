# 本机 Linked View 快照库实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 在浏览器本机保存、管理和恢复当前数据集的 named linked-view snapshots。

**架构：** 服务器继续唯一负责将当前状态规范化为 canonical JSON，并新增一个同样严格的 `apply` 请求路径。浏览器用一个版本化、限额的 localStorage library 保存 canonical JSON 与本机名称；按配置内 fingerprint 过滤，并通过现有 apply 事务恢复。

**技术栈：** R/Shiny、vanilla JavaScript、localStorage、testthat/shinytest2。

---

### 任务 1：扩展服务器 canonicalize/apply transport

**文件：**
- 修改：`inst/viewer/coordinated_views/server.R:174-249`
- 测试：`tests/testthat/test-coordinated-views-config.R`

- [ ] **步骤 1：编写失败的契约测试**

```r
expect_match(server, 'c("copy", "download", "save", "apply")', fixed = TRUE)
expect_match(server, 'cv_config_decode(request$config_json, cells = bundle$cells)', fixed = TRUE)
```

- [ ] **步骤 2：运行测试验证失败**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R", stop_on_failure=TRUE)'`

预期：FAIL，缺少 `save` / browser-local `apply` transport。

- [ ] **步骤 3：最小实现**

```r
# `save` returns validated canonical JSON; `apply` decodes canonical JSON and
# sends normalized config through the existing `coordviews_config_result` path.
```

- [ ] **步骤 4：运行契约测试验证通过**

- [ ] **步骤 5：提交**

### 任务 2：实现受限的浏览器 snapshot library

**文件：**
- 修改：`inst/viewer/www/coordviews-config.js:1-275`
- 测试：`tests/testthat/test-coordinated-views-config-browser.R`

- [ ] **步骤 1：编写失败的浏览器断言**

```r
expect_true(app$get_js("document.querySelectorAll('.cv-snapshot-row').length === 1"))
expect_true(app$get_js("window.localStorage.getItem('cerebro.linked-views.snapshots.v1') !== null"))
```

- [ ] **步骤 2：运行浏览器测试确认失败**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config-browser.R", stop_on_failure=TRUE)'`

预期：FAIL，快照控制与 localStorage library 尚不存在。

- [ ] **步骤 3：最小实现**

```js
// Persist `{ id, name, savedAt, json }` records under one versioned key.
// Parse each JSON only to filter by its dataset fingerprint; reject malformed
// records, cap count and bytes, and retain the newest valid records.
```

- [ ] **步骤 4：运行最小浏览器/JS 检查**

- [ ] **步骤 5：提交**

### 任务 3：加入快照管理 UI 与恢复路径

**文件：**
- 修改：`inst/viewer/coordinated_views/UI.R:810-890`
- 修改：`inst/viewer/www/coordviews.css`
- 修改：`inst/viewer/www/coordviews-config.js`
- 测试：`tests/testthat/test-coordinated-views-config-browser.R`

- [ ] **步骤 1：编写失败的 UI/恢复断言**

```r
expect_true(app$get_js("document.getElementById('cv-snapshot-save') !== null"))
expect_true(app$get_js("window.cerebroLinkedViewsState.summary().selectedCells > 0"))
```

- [ ] **步骤 2：运行断言确认失败**

- [ ] **步骤 3：最小实现**

```js
// Save prompts for a non-empty name, list current-dataset records, and route
// restore through server action `apply`; rename/delete/download are local
// record operations and do not alter the active view until restore succeeds.
```

- [ ] **步骤 4：运行配置契约测试与 JS syntax 检查**

- [ ] **步骤 5：提交**
