# Extra-material workbook-row polish 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 使 Extra material 工作簿行使用标准 disclosure 箭头、右对齐操作区和静态上传确认样式。

**架构：** `review.R` 继续输出原生 `<details>`，以额外的 SVG chevron 标记替代文本 glyph；CSS 用 `[open]` 控制图标状态并负责布局与新增态。上传消息和保存行为保持不变。一个 UI contract 检查标记和样式约束，已有上传集成测试继续覆盖消息路径。

**技术栈：** R Shiny、htmltools、原生 CSS、testthat。

---

### 任务 1：定义外观回归约束

**文件：**
- 修改：`tests/testthat/test-builder-ui-contract.R:800-850`
- 测试：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：编写失败的 UI contract**

在现有 extra-material contract 中加入：

```r
expect_match(review, 'class = "enhance-workbook-chevron",', fixed = TRUE)
expect_match(review, 'viewBox = "0 0 24 24"', fixed = TRUE)
expect_match(components, ".enhance-workbook-item\\[open\\] .enhance-workbook-chevron", fixed = TRUE)
expect_match(components, "margin-left: auto", fixed = TRUE)
expect_false(grepl("@keyframes enhance-workbook-added", components, fixed = TRUE))
```

- [ ] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'devtools::test(filter = "builder-ui-contract")'
```

预期：失败，缺少 workbook chevron、native `open` state 或旧 keyframe 仍存在。

- [ ] **步骤 3：实现最少 UI 标记与样式**

在 `inst/builder/server/review.R` 将 workbook summary 的文本 chevron 替换为：

```r
tags$span(
  class = "enhance-workbook-chevron",
  `aria-hidden` = "true",
  tags$svg(
    viewBox = "0 0 24 24",
    fill = "none",
    tags$path(d = "m9 18 6-6-6-6")
  )
)
```

在 `inst/builder/www/builder.components.css`：

```css
.enhance-workbook-chevron { width: 1.75rem; height: 1.75rem; }
.enhance-workbook-chevron svg { transition: transform .16s ease; }
.enhance-workbook-item[open] .enhance-workbook-chevron svg { transform: rotate(90deg); }
.enhance-workbook-summary > .enhance-attachment-actions { margin-left: auto; }
.enhance-workbook-item--new { border-color: var(--c-success); background: var(--c-success-50); }
```

删除 `enhance-workbook-added` 的 keyframes 和 animation。

- [ ] **步骤 4：运行测试验证通过**

运行：

```bash
Rscript -e 'devtools::test(filter = "builder-ui-contract")'
```

预期：新 contract 通过；若该文件仍有已知基线失败，报告其数量与位置且新增断言不得失败。

- [ ] **步骤 5：Commit**

```bash
git add inst/builder/server/review.R inst/builder/www/builder.components.css tests/testthat/test-builder-ui-contract.R
git commit -m "feat(builder): polish workbook row disclosure"
```

### 任务 2：验证上传与源文件完整性

**文件：**
- 修改：无
- 测试：`tests/testthat/test-builder-stage-server.R`

- [ ] **步骤 1：运行上传集成测试**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-stage-server.R", desc = "dynamic Core and Enhance contracts update only their owned controls")'
```

预期：77 个断言通过，首次上传发出 `enhance_tables_added` 消息。

- [ ] **步骤 2：运行语法与差异检查**

运行：

```bash
Rscript -e 'invisible(parse(file = "inst/builder/server/review.R"))'
node --check inst/builder/www/builder.js
git diff --check
```

预期：三条命令均无错误。

- [ ] **步骤 3：Commit 验证覆盖补充**

若任务 1 所有变更已在同一提交中包含，不额外创建空提交；否则仅提交任务 1 涉及的测试文件。
