# Builder Cancel 按钮样式统一实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让服务端当前导入与客户端排队导入的 Cancel 按钮使用完全一致的浅红危险样式和尺寸。

**架构：** 保留两套既有取消事件，只给服务端运行中按钮增加语义 class。CSS 继续用 `btn-remove-soft` 负责颜色，并用共享 Cancel 选择器固定尺寸；非 Cancel 的数据集 Remove 按钮继续使用白底样式。

**技术栈：** R/Shiny、HTML/CSS、testthat、shinytest2。

---

## 文件结构

- 修改 `inst/builder/ui/dataset_rail.R`：为运行中导入的 Cancel 添加 `btn` 与 `builder-cancel-import` class。
- 修改 `inst/builder/www/builder.components.css`：排除 Cancel 的白底覆盖，并统一两种 Cancel 的盒模型。
- 修改 `tests/testthat/test-builder-loading-ui.R`：锁定服务端 Cancel 的语义 class 和 CSS 选择器。
- 修改 `tests/testthat/test-builder-upload-row-browser.R`：比较两种 Cancel 的计算样式。

### 任务 1：统一当前导入与排队导入的 Cancel

**文件：**
- 修改：`inst/builder/ui/dataset_rail.R:711-740`
- 修改：`inst/builder/www/builder.components.css:1372-1377,1550-1571`
- 测试：`tests/testthat/test-builder-loading-ui.R:323-345`
- 测试：`tests/testthat/test-builder-upload-row-browser.R:17-49`

- [ ] **步骤 1：编写失败的语义与浏览器样式测试**

在 active import HTML 契约中要求运行中按钮包含：

```r
expect_match(
  rail_html,
  "ds-del btn btn-remove-soft builder-cancel-import builder-remove-import",
  fixed = TRUE
)
```

在浏览器 fixture 中增加同 class 的服务端 Cancel，并比较两者的 `backgroundColor`、`color`、`borderRadius`、`minHeight`、`padding` 与 `fontSize`。

- [ ] **步骤 2：运行测试确认缺少服务端 Cancel class**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-ui.R")'
```

预期：新增断言 FAIL，因为当前按钮没有 `btn` 和 `builder-cancel-import`。

- [ ] **步骤 3：实现最小 class 与 CSS 收敛**

运行中按钮 class 使用：

```r
class = paste(
  "ds-del btn btn-remove-soft builder-cancel-import builder-remove-import"
)
```

失败和排队移除按钮保持原 class。白底覆盖改为只匹配非 Cancel：

```css
.ds-del.btn-remove-soft:not(.builder-cancel-import) { /* 既有白底规则 */ }
```

两种 Cancel 使用共享尺寸规则：

```css
.builder-cancel-import,
.builder-cancel-client-import {
  display: inline-flex;
  min-height: 2.3rem;
  align-items: center;
  justify-content: center;
  padding: .45rem .95rem;
  border-radius: var(--r-sm);
  font-size: .85rem;
  font-weight: 600;
  line-height: normal;
}
```

- [ ] **步骤 4：运行 focused 测试和语法检查**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-ui.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-builder-upload-row-browser.R")'
git diff --check
```

预期：非浏览器契约 0 failed；未启用浏览器环境时 browser 用例被明确 skip。

- [ ] **步骤 5：只提交本功能 hunk**

```bash
git add inst/builder/ui/dataset_rail.R tests/testthat/test-builder-loading-ui.R tests/testthat/test-builder-upload-row-browser.R
git add -p inst/builder/www/builder.components.css
git commit -m "style(builder): unify import cancel buttons"
```

`builder.components.css` 已有其他工作区改动，必须交互式只暂存本功能 hunk。
