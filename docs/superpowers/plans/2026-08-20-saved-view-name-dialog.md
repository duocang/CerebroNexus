# Saved-view name dialog 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 用一致的应用内命名 dialog 替换保存和重命名时的浏览器原生 prompt。

**架构：** `UI.R` 增加一个可复用 dialog。`coordviews-config.js` 以一个短生命周期的命名请求对象连接 Save/Rename 和该 dialog；原有 `snapshotName`、保存和重命名数据逻辑保持不变。CSS 用现有 workspace dialog 的 token 表达层级。

**技术栈：** Shiny tags、原生 DOM、CSS、testthat。

---

### 任务 1：先锁定 custom dialog 契约

**文件：**
- 修改：`tests/testthat/test-coordinated-views-config.R`

- [ ] **步骤 1：编写失败的测试**

加入：

```r
expect_match(ui, 'id = "cv-snapshot-name-dialog"', fixed = TRUE)
expect_match(ui, 'id = "cv-snapshot-name-input"', fixed = TRUE)
expect_match(controller, "openSnapshotNameDialog", fixed = TRUE)
expect_no_match(controller, "window.prompt", fixed = TRUE)
```

- [ ] **步骤 2：验证测试失败**

运行 `Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`。

预期：新 custom-dialog 断言失败。

### 任务 2：实现 dialog 和命名流程

**文件：**
- 修改：`inst/viewer/coordinated_views/UI.R`
- 修改：`inst/viewer/www/coordviews-config.js`

- [ ] **步骤 1：添加 reusable dialog markup**

在 `cv-config-dialog` 后增加 `cv-snapshot-name-dialog`，包含 eyebrow、
`cv-snapshot-name-title`、`cv-snapshot-name-help`、input、Cancel 和 Confirm。

- [ ] **步骤 2：替换 prompt 调用**

实现 `openSnapshotNameDialog(mode, record)`，保存 trigger 元素；Confirm
normalises input then calls `request('save')` for save, or the existing
`writeSnapshots(readSnapshots().map(...))` flow for rename. Cancel/close restores
focus and clears temporary state.

- [ ] **步骤 3：运行契约测试**

运行 `Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`。

预期：全部通过。

### 任务 3：实现视觉一致性并交付

**文件：**
- 修改：`inst/viewer/www/coordviews.css`

- [ ] **步骤 1：添加 name dialog styles**

Use workspace tokens for surface, border, 10px corners, amber focus ring and a
shallow amber confirm button; set z-index above `cv-config-dialog`.

- [ ] **步骤 2：检查与提交**

运行 `node --check inst/viewer/www/coordviews-config.js`、`git diff --check`，
then commit UI, JS, CSS and test changes with
`style(viewer): unify saved-view naming dialog`.
