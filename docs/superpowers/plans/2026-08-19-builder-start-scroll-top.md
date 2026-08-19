# Builder Start Scroll-to-Top 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让已被服务端接受的 Build 请求把整个页面平滑滚到绝对顶部。

**架构：** `start_confirmed_build()` 在进入 `preparing` 后发送专用 Shiny custom message；`builder.js` 注册单一 handler 调用 `window.scrollTo({top: 0})`，并复用现有 reduced-motion 状态。

**技术栈：** R/Shiny、原生 JavaScript、testthat 静态与 server 契约测试。

---

### 任务 1：锁定消息契约

**文件：**
- 修改：`tests/testthat/test-builder-stage-server.R`
- 修改：`tests/testthat/test-builder-ui-contract.R`

- [ ] 添加失败断言：Build 进入 `preparing` 后发送 `builder_scroll_page_top`。
- [ ] 添加失败断言：前端注册同名 handler，调用 `window.scrollTo`、`top: 0` 并读取 `reducedMotion.matches`。
- [ ] 运行 `Rscript -e 'devtools::test(filter="builder-(stage-server|ui-contract)$", stop_on_failure=TRUE)'`，确认因功能缺失而失败。

### 任务 2：实现 accepted-Build 滚顶

**文件：**
- 修改：`inst/builder/server/build.R`
- 修改：`inst/builder/www/builder.js`

- [ ] 在 `build_flow(list(stage = "preparing", plan = NULL))` 后发送 `builder_scroll_page_top`。
- [ ] 增加 handler，以 smooth/auto 模式调用 `window.scrollTo({top: 0})`。
- [ ] 重跑聚焦测试，预期 0 failed。
- [ ] 提交 `fix(builder): scroll to top when build starts`。

### 任务 3：交付

- [ ] 推送 `feat/builder-project-workspace`。
- [ ] 手动触发 R tests、R-CMD-check、pkgdown，并记录 run URL。
