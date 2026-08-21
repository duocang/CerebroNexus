# Review Output and Build Status 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 隐藏 Review 的内部临时目录，并确保 Build status 只描述当前用户启动的构建。

**架构：** Review 继续使用临时目录冻结内部计划，但 UI 模型不再把该路径呈现给用户。Build 状态继续由同一个 typed projection 驱动；新一轮确认进入 Build 时清除旧结果，ready 状态不产生状态面板正文。

**技术栈：** R、Shiny、htmltools、testthat

---

## 文件结构

- `inst/builder/ui/review_stage.R`：呈现 Review 的用户可见输出摘要。
- `inst/builder/ui/build_status.R`：定义 Build status 在不同状态下的可见正文。
- `inst/builder/server/review.R`：在确认新一轮 Review 时重置旧构建结果。
- `tests/testthat/test-builder-stage-review.R`：锁定 Review 和 typed status UI 契约。
- `tests/testthat/test-builder-stage-server.R`：锁定进入新 Build 周期时清理结果的服务端契约。

### 任务 1：隐藏 Review 临时目录

**文件：**
- 修改：`tests/testthat/test-builder-stage-review.R`
- 修改：`inst/builder/ui/review_stage.R`

- [ ] **步骤 1：编写失败测试**

在 Review UI 测试中断言不存在 `Folder`，存在下载说明和三项输出摘要：

```r
expect_false(grepl(">Folder<", html, fixed = TRUE))
expect_match(
  html,
  "CRB files will be available to download after the build completes.",
  fixed = TRUE
)
expect_match(html, "Creates", fixed = TRUE)
expect_match(html, "Estimated size", fixed = TRUE)
expect_match(html, "Estimated build time", fixed = TRUE)
```

- [ ] **步骤 2：验证测试因旧 Folder 字段而失败**

运行：

```bash
Rscript -e 'devtools::test(filter="builder-stage-review$", stop_on_failure=TRUE)'
```

预期：新断言失败，指出仍有 Folder 或缺少下载说明。

- [ ] **步骤 3：实现最小 UI 改动**

在 Output section 中删除：

```r
field("Folder", model$output$directory, "is-path")
```

并在字段列表前加入：

```r
p(
  class = "review-output-download-note",
  "CRB files will be available to download after the build completes."
)
```

- [ ] **步骤 4：验证 Review 测试通过**

运行同一步骤 2，预期 `FAIL 0`。

### 任务 2：隔离每轮 Build status

**文件：**
- 修改：`tests/testthat/test-builder-stage-review.R`
- 修改：`tests/testthat/test-builder-stage-server.R`
- 修改：`inst/builder/ui/build_status.R`
- 修改：`inst/builder/server/review.R`

- [ ] **步骤 1：编写失败测试**

增加两个契约：ready 模型不产生 status body；确认 Review 时清除旧 result。

```r
expect_null(builder_build_stage_status_body_ui(idle))
```

服务端确认用例先设置旧结果，再触发 `confirm_review`，断言：

```r
result(builder_result_success(published = TRUE, built = "/old/a.crb"))
session$setInputs(confirm_review = 1)
session$flushReact()
expect_null(result())
```

- [ ] **步骤 2：验证测试正确失败**

运行：

```bash
Rscript -e 'devtools::test(filter="builder-stage-(review|server)$", stop_on_failure=TRUE)'
```

预期：ready body 仍包含 readiness 文本，或确认后旧 result 仍存在。

- [ ] **步骤 3：实现状态生命周期修复**

将 ready body 改为不可见：

```r
ready = NULL
```

在成功确认 Review、切换到 Build 状态之前执行：

```r
result(NULL)
```

Destination 卡片继续承担“未选择目录”的说明，footer 继续根据 `can_build` 控制 Build 按钮。

- [ ] **步骤 4：验证 focused 测试通过**

运行同一步骤 2，预期 `FAIL 0`。

- [ ] **步骤 5：最终检查与提交**

```bash
git diff --check
git status --short
git add inst/builder/ui/review_stage.R inst/builder/ui/build_status.R \
  inst/builder/server/review.R tests/testthat/test-builder-stage-review.R \
  tests/testthat/test-builder-stage-server.R
git commit -m "fix(builder): clarify review output and reset build status"
```
