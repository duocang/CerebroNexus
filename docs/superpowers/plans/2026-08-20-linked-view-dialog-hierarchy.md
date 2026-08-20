# Linked-view dialog hierarchy 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 将 Linked views 的 JSON 弹窗分为文件移动、本机保存、已保存视图三个清晰功能区，不改变配置行为。

**架构：** `UI.R` 重新组织语义化 section，但保留全部现有 id。`coordviews-config.js` 保留快照存储与回调，只增强记录行结构；`coordviews.css` 表达三区层级和响应式布局。

**技术栈：** Shiny HTML tags、原生 DOM、CSS Grid/Flex、testthat。

---

## 文件结构

- 修改：`inst/viewer/coordinated_views/UI.R` — 三个语义 section，保留交互 id。
- 修改：`inst/viewer/www/coordviews-config.js` — 快照 bookmark、Open 主操作分组。
- 修改：`inst/viewer/www/coordviews.css` — region、保存条、列表行与窄屏样式。
- 修改：`tests/testthat/test-coordinated-views-config.R` — 新 markup/static 契约。

### 任务 1：锁定弹窗语义结构

**文件：**
- 修改：`tests/testthat/test-coordinated-views-config.R:493-541`

- [ ] **步骤 1：编写失败的测试**

在 `Save and share markup is accessible and bundled in every Viewer` 中加入：

```r
expect_match(ui, 'class = "cv-config-region cv-config-transfer"', fixed = TRUE)
expect_match(ui, '"Move a view"', fixed = TRUE)
expect_match(ui, 'class = "cv-config-region cv-config-save-local"', fixed = TRUE)
expect_match(ui, '"Save on this device"', fixed = TRUE)
expect_match(ui, 'class = "cv-config-region cv-snapshots"', fixed = TRUE)
expect_match(controller, 'cv-snapshot-mark', fixed = TRUE)
expect_match(controller, 'cv-snapshot-primary', fixed = TRUE)
```

- [ ] **步骤 2：运行测试验证失败**

运行 `Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`。

预期：新增 section 和 snapshot row 的断言失败。

### 任务 2：重组可访问 markup 和快照行

**文件：**
- 修改：`inst/viewer/coordinated_views/UI.R:837-888`
- 修改：`inst/viewer/www/coordviews-config.js:126-184`

- [ ] **步骤 1：实现三个 section，保留所有行为 id**

将当前动作区和 snapshots 区替换为下列结构；download/copy/upload/save/list 的既有 id 完全不变：

```r
tags$section(class = "cv-config-region cv-config-transfer",
  `aria-labelledby` = "cv-config-transfer-title",
  tags$div(class = "cv-config-region-head",
    tags$h5(id = "cv-config-transfer-title", "Move a view"),
    tags$p("Download a JSON file, copy it, or open one from disk.")
  ),
  div(class = "cv-config-actions", ...)
)
tags$section(class = "cv-config-region cv-config-save-local",
  `aria-labelledby` = "cv-config-save-local-title", ...)
tags$section(class = "cv-config-region cv-snapshots",
  `aria-labelledby` = "cv-snapshots-title", ...)
```

- [ ] **步骤 2：实现结构化 snapshot 行**

在 `renderSnapshots()` 中为每一行添加：

```javascript
var mark = document.createElement('span');
mark.className = 'cv-snapshot-mark';
mark.setAttribute('aria-hidden', 'true');
mark.textContent = '⌑';
row.appendChild(mark);
var primary = document.createElement('div');
primary.className = 'cv-snapshot-primary';
primary.appendChild(snapshotButton('Open', restoreSnapshot, record));
```

Download、Rename、Delete 留在 `.cv-snapshot-actions` 中并保留当前回调和顺序。

- [ ] **步骤 3：运行结构契约测试验证通过**

运行 `Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`。

预期：配置契约测试全绿。

### 任务 3：实现视觉层级与窄屏规则

**文件：**
- 修改：`inst/viewer/www/coordviews.css:1401-1541`

- [ ] **步骤 1：实现三区表面与标题说明**

```css
.cv-config-region { margin-top:16px; padding:16px; border:1px solid var(--cv-border,#e5e7eb); border-radius:10px; background:var(--cv-surface,#fff); }
.cv-config-region-head { margin-bottom:12px; }
.cv-config-region-head h5 { margin:0 0 3px; font-size:13px; }
.cv-config-region-head p { margin:0; color:var(--cv-text3,#868990); font-size:11.5px; line-height:1.45; }
.cv-config-save-local { background:var(--cv-amber50,#fff4ec); border-color:var(--cv-amber200,#fed7aa); }
```

Keep `.cv-config-actions` as an equal-width three-column grid only inside the transfer region.

- [ ] **步骤 2：实现保存条与列表行层级**

```css
.cv-config-save-local .cv-snapshot-save { width:100%; min-height:40px; display:inline-flex; align-items:center; justify-content:center; gap:7px; border-color:var(--cv-amber200,#fed7aa); background:#fff; color:var(--cv-amber700,#c85a0e); }
.cv-snapshot-row { display:grid; grid-template-columns:auto minmax(0,1fr) auto auto; gap:10px; align-items:center; padding:10px 0; }
.cv-snapshot-mark { display:grid; place-items:center; width:28px; height:28px; border-radius:7px; background:var(--cv-amber50,#fff4ec); color:var(--cv-amber700,#c85a0e); }
.cv-snapshot-primary .cv-snapshot-action { border-color:var(--cv-amber200,#fed7aa); color:var(--cv-amber700,#c85a0e); }
```

At 620px stack transfer actions and snapshot row actions; retain name, timestamp and every action.

- [ ] **步骤 3：运行静态检查**

运行 `node --check inst/viewer/www/coordviews-config.js` 与 `git diff --check`。

预期：无输出，退出码为 0。

### 任务 4：浏览器审阅和提交

**文件：**
- 验证：`inst/viewer/coordinated_views/UI.R`
- 验证：`inst/viewer/www/coordviews-config.js`
- 验证：`inst/viewer/www/coordviews.css`

- [ ] **步骤 1：在本机 Viewer 检查视觉层级**

打开 `http://127.0.0.1:57369/`，进入 Linked views、选中细胞、打开 `Import / export view…`。确认三区标题/说明可见、文件按钮等宽、保存条独立，且保存项有 bookmark、时间、Open 主操作与全部次级操作。

- [ ] **步骤 2：提交实现**

运行 `git add inst/viewer/coordinated_views/UI.R inst/viewer/www/coordviews-config.js inst/viewer/www/coordviews.css tests/testthat/test-coordinated-views-config.R && git commit -m "style(viewer): clarify linked-view dialog hierarchy"`。

预期：提交只包含本弹窗的 markup、渲染、样式和测试变更。
