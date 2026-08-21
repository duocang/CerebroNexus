# Builder 数据栏状态视觉实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 将 Builder 左侧数据栏改为 Logo 橙选中、蓝色处理中、绿色完成、红色失败、灰色排队的分层状态系统。

**架构：** 数据生命周期通过稳定的 `data-load-state`、`is-ready` 和状态点表达；hover、selected、focus 作为独立交互层叠加。CSS 只消费语义状态，不修改导入队列或服务器状态机。

**技术栈：** R/Shiny UI、原生 JavaScript DOM、CSS custom properties、testthat 静态 UI contract。

---

### 任务 1：补齐数据栏语义标记

**文件：**
- 修改：`inst/builder/ui/dataset_rail.R`
- 修改：`inst/builder/www/builder.js`
- 测试：`tests/testthat/test-builder-rail.R`
- 测试：`tests/testthat/test-builder-loading-ui.R`

- [ ] **步骤 1：编写失败的语义标记测试**

在 rail 测试中断言 ready 行具有 `is-ready`、`data-load-state="ready"` 和 `ds-ready-dot`；在 loading UI contract 中断言客户端排队行创建 `ds-state-dot`。

- [ ] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-rail.R", reporter="summary"); testthat::test_file("tests/testthat/test-builder-loading-ui.R", reporter="summary")'
```

预期：新增加的语义标记断言失败。

- [ ] **步骤 3：添加最小语义标记**

Ready 行容器增加：

```r
class = paste(c("ds", "is-ready", if (active) "is-active"), collapse = " ")
`data-load-state` = "ready"
```

Ready 行按钮末尾增加：

```r
shiny::span(class = "ds-state-dot ds-ready-dot", `aria-hidden` = "true")
```

客户端队列行在 body 后增加：

```javascript
var dot = document.createElement("span");
dot.className = "ds-state-dot";
dot.setAttribute("aria-hidden", "true");
row.appendChild(dot);
```

- [ ] **步骤 4：运行聚焦测试验证通过**

运行任务 1 步骤 2 的命令，预期全部通过。

- [ ] **步骤 5：提交**

```bash
git add inst/builder/ui/dataset_rail.R inst/builder/www/builder.js tests/testthat/test-builder-rail.R tests/testthat/test-builder-loading-ui.R
git commit -m "refactor(builder): expose dataset rail visual states"
```

### 任务 2：实现分层状态配色

**文件：**
- 修改：`inst/builder/www/builder.tokens.css`
- 修改：`inst/builder/www/builder.layout.css`
- 修改：`inst/builder/www/builder.components.css`
- 测试：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：编写失败的状态 token 和 selector 测试**

断言存在以下语义 token：

```text
--builder-rail-selected-bg
--builder-rail-selected-marker
--builder-rail-hover-bg
--builder-rail-progress-bg
--builder-rail-progress-fg
--builder-rail-ready-fg
--builder-rail-error-bg
--builder-rail-error-fg
```

并断言选中行不再使用 `background: var(--builder-selection-bg)`，状态选择器覆盖 queued、uploading/reading/processing、ready、error/rejected、paused/cancelled 和 unknown。

- [ ] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R", reporter="summary")'
```

预期：语义 token 和新 selector 断言失败。

- [ ] **步骤 3：添加语义 token**

在 `builder.tokens.css` 中定义：

```css
--builder-rail-selected-bg: var(--c-amber-50);
--builder-rail-selected-marker: var(--c-amber);
--builder-rail-hover-bg: #fffaf6;
--builder-rail-progress-bg: var(--c-blue-50);
--builder-rail-progress-fg: var(--c-blue);
--builder-rail-ready-fg: var(--c-success);
--builder-rail-error-bg: var(--c-error-50);
--builder-rail-error-fg: var(--c-error);
```

- [ ] **步骤 4：实现交互层与生命周期层**

将 `.ds.is-active` 改为浅橙背景、深色文字和 Logo 橙左标；hover 使用暖白背景和浅橙 inset border；focus 使用蓝色 ring。使用 `data-load-state` 设置状态背景和状态点，selected selector 只覆盖背景与左标，不覆盖状态点颜色。

- [ ] **步骤 5：实现 reduced-motion 与窄屏一致性**

暂停、断线和 reduced-motion 下关闭状态点动画；窄屏继续显示状态点、文字和选中左标。

- [ ] **步骤 6：运行聚焦测试验证通过**

运行任务 2 步骤 2 的命令，预期全部通过。

- [ ] **步骤 7：提交**

```bash
git add inst/builder/www/builder.tokens.css inst/builder/www/builder.layout.css inst/builder/www/builder.components.css tests/testthat/test-builder-ui-contract.R
git commit -m "style(builder): distinguish dataset rail states"
```

### 任务 3：浏览器状态组合检查

**文件：**
- 修改：`tests/testthat/test-builder-loading-browser.R`

- [ ] **步骤 1：增加状态组合浏览器断言**

通过示例导入检查 importing 使用蓝色语义状态，ready 使用绿色状态点；点击 ready 行后断言选中背景为浅 Logo 橙且绿色状态点保持不变；模拟 hover 后断言未选中行与选中行背景不同。

- [ ] **步骤 2：运行聚焦浏览器测试**

运行：

```bash
CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-browser.R", reporter="summary")'
```

预期：新增组合状态断言通过。

- [ ] **步骤 3：提交**

```bash
git add tests/testthat/test-builder-loading-browser.R
git commit -m "test(builder): cover dataset rail visual states"
```
