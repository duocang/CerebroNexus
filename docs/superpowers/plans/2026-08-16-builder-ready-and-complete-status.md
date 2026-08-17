# Builder Ready and Complete Status 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 用双线绿色 `READY` 验收章、明确的行悬停反馈和绿色 Build 完成标记替换含混的状态圆点，并将 `Open App` 改名为 `Launch App`。

**架构：** 保留现有 dataset lifecycle 属性、选择状态、Build pipeline 状态机以及 `open_app` 事件 ID。只调整服务端生成的状态标记、客户端状态类映射和现有 CSS 组件；所有行为边界由当前 R/浏览器合同测试覆盖。

**技术栈：** R Shiny、htmltools、原生 JavaScript、CSS、testthat、Node 浏览器合同测试。

---

## 文件结构

- 修改 `inst/builder/ui/dataset_rail.R`：ready 行输出文字印章，元数据不重复 Ready。
- 修改 `inst/builder/www/builder.layout.css`：非选中行 hover/focus 的表面、描边、位移与阴影。
- 修改 `inst/builder/www/builder.components.css`：READY 印章及 reduced-motion 样式，移除 ready 圆点依赖。
- 修改 `inst/builder/www/builder.js`：成功时把 Complete 视为完成阶段，而非橙色当前阶段。
- 修改 `inst/builder/www/builder.features.css`：Complete 白勾绿色完成标记。
- 修改 `inst/builder/ui/build_status.R`：显示文案改成 `Launch App`，保留 `open_app` ID 和处理器。
- 修改 `tests/testthat/test-builder-rail.R`、`test-builder-ui-contract.R`、`test-builder-loading-browser.R`：数据栏标记与交互合同。
- 修改 `tests/testthat/test-builder-stage-review.R`、`test-builder-worker-app.R`、`test-builder-end-to-end.R`：Build 完成态和按钮文案合同。
- 修改现有 Builder 用户文档中准确出现的 `Open App` 可见文案为 `Launch App`。

### 任务 1：数据栏 READY 验收章与 hover

- [ ] **步骤 1：编写失败的合同测试**

在 `tests/testthat/test-builder-rail.R` 断言 ready 行包含：

```r
expect_match(html, 'class="ds-ready-stamp"', fixed = TRUE)
expect_match(html, ">READY<", fixed = TRUE)
expect_false(grepl("ds-ready-dot", html, fixed = TRUE))
expect_false(grepl("qs2 Ready", html, fixed = TRUE))
```

在 `tests/testthat/test-builder-ui-contract.R` 断言存在 `.ds-ready-stamp`、`:focus-visible`、`prefers-reduced-motion`，且 hover 规则不打开 `.ds::before` 左标记。

- [ ] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'devtools::test(filter = "builder-(rail|ui-contract)", reporter = "summary", stop_on_failure = FALSE)'
```

预期：FAIL，缺少 `ds-ready-stamp` 且仍存在 `ds-ready-dot`。

- [ ] **步骤 3：实现最小标记与样式**

在 `inst/builder/ui/dataset_rail.R` 将 ready dot 替换为：

```r
shiny::span(class = "ds-ready-stamp", role = "status", "READY")
```

并只在 Needs attention 时保留 metadata readiness 文案。CSS 使用绿色双线边框、约 `rotate(-2deg)`、淡绿色底色；hover 使用浅 Logo 橙表面、暖色边框、`translateY(-1px)` 和轻阴影。`:focus-visible` 提供等价可见反馈，reduced motion 禁用位移和 transition。

- [ ] **步骤 4：运行聚焦测试验证通过**

运行同上，预期相关测试 PASS。

- [ ] **步骤 5：Commit**

```bash
git add inst/builder/ui/dataset_rail.R inst/builder/www/builder.layout.css inst/builder/www/builder.components.css tests/testthat/test-builder-rail.R tests/testthat/test-builder-ui-contract.R tests/testthat/test-builder-loading-browser.R
git commit -m "style(builder): replace ready dot with verification stamp"
```

### 任务 2：Build 完成标记与 Launch App 文案

- [ ] **步骤 1：编写失败的合同测试**

在 Build 状态测试中断言：

```r
expect_match(verified_html, "Launch App", fixed = TRUE)
expect_false(grepl(">Open App<", verified_html, fixed = TRUE))
expect_match(verified_html, 'id="open_app"', fixed = TRUE)
```

在 UI 合同中断言成功 pipeline 的 Complete 阶段使用 `is-complete is-terminal`，CSS 为 terminal light 添加白色 `✓`，且成功态不保留橙色 current ring。

- [ ] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'devtools::test(filter = "builder-(stage-review|worker-app|ui-contract)", reporter = "summary", stop_on_failure = FALSE)'
```

预期：FAIL，页面仍显示 `Open App`，Complete 仍使用 current 样式。

- [ ] **步骤 3：实现完成态和文案**

在 `updatePipelines()` 中，成功 `complete` 状态把 queued、building、complete 全部标为 `is-complete`，并给 complete 添加 `is-terminal`；只有 queued/building 运行中状态使用 `is-current`。CSS 将 terminal light 放大、填充 success green，并通过伪元素显示白色 `✓`。把 actionButton 的可见文字改成 `Launch App`，不改变 ID、事件处理器或 `builder_open_final_app()`。

- [ ] **步骤 4：同步可见用户文档**

将 Builder 文档中描述该按钮的 `Open App` 更新为 `Launch App`；内部函数名和测试描述可继续使用 Open App 作为历史行为名称，但 UI 断言只接受 `Launch App`。

- [ ] **步骤 5：运行聚焦测试验证通过**

运行同上，预期相关测试 PASS。

- [ ] **步骤 6：Commit**

```bash
git add inst/builder/www/builder.js inst/builder/www/builder.features.css inst/builder/ui/build_status.R tests/testthat
git add vignettes/build_a_data_set_by_pointing.Rmd
git commit -m "style(builder): clarify successful build completion"
```

### 任务 3：浏览器回归与最终验证

- [ ] **步骤 1：运行数据栏浏览器测试**

```bash
Rscript -e 'devtools::test(filter = "builder-loading-browser", reporter = "summary", stop_on_failure = FALSE)'
```

预期：PASS；串行 import DOM 状态仍按原顺序转换，ready 行使用 stamp。

- [ ] **步骤 2：运行相关 R 测试集合**

```bash
Rscript -e 'devtools::test(filter = "builder-(rail|ui-contract|stage-review|worker-app|end-to-end)", reporter = "summary", stop_on_failure = FALSE)'
```

预期：PASS。

- [ ] **步骤 3：静态质量检查**

```bash
git diff --check
git status --short
```

预期：无 whitespace 错误，只包含计划内文件。

- [ ] **步骤 4：重启 Builder 并目视检查**

在 `http://127.0.0.1:3838/` 检查非选中 hover、选中 ready 行、Build complete 卡片和 `Launch App`。确认点击 `Launch App` 前不会启动生成 App。

- [ ] **步骤 5：最终 Commit 和 push**

```bash
git add inst/builder/ui/dataset_rail.R inst/builder/ui/build_status.R inst/builder/www/builder.layout.css inst/builder/www/builder.components.css inst/builder/www/builder.features.css inst/builder/www/builder.js tests/testthat/test-builder-rail.R tests/testthat/test-builder-ui-contract.R tests/testthat/test-builder-loading-browser.R tests/testthat/test-builder-stage-review.R tests/testthat/test-builder-worker-app.R tests/testthat/test-builder-end-to-end.R vignettes/build_a_data_set_by_pointing.Rmd
git commit -m "test(builder): cover ready and complete visual states"
git push origin integration/colleague-spatial-builder_codex
```
