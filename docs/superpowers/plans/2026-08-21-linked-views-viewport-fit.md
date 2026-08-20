# Linked views viewport fit implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让任意数量的 Linked views 面板完整、等比例并尽可能大地占用当前首屏。

**架构：** 在客户端布局器中枚举列数并同时受可用宽高约束，保留现有 ResizeObserver 作为普通进入与分享恢复的统一重排入口。首页 UI 移除面板下方的衍生分析区。

**技术栈：** R Shiny UI、原生 JavaScript、CSS Grid、testthat、shinytest2。

---

### 任务 1：锁定布局契约

**文件：**
- 修改：`tests/testthat/test-coordinated-views.R`

- [ ] 添加源码回归，要求存在二维网格选择器、宽高边长计算及安全留白常量。
- [ ] 添加 UI 回归，要求首页不再包含 Composition 和 selected-cell 输出容器。
- [ ] 运行 `Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views.R")'`，确认新断言因功能缺失而失败。

### 任务 2：实现二维自适应网格

**文件：**
- 修改：`inst/viewer/www/coordviews.js`
- 修改：`inst/viewer/www/coordviews.css`

- [ ] 新增 `bestOverviewGrid(panelCount, availW, availH, chromeX, chromeY, gap)`，枚举所有列数并返回最大完整正方形。
- [ ] 从网格顶部、滚动容器底部、卡片 chrome、间距及 18px gutter 推导可用矩形。
- [ ] overview 使用二维结果；低于 300px 时保留可读性下限并允许滚动；focus 行为保持不变。
- [ ] 给网格加入 18px 内边距和 border-box 计算，确保视觉留白稳定。

### 任务 3：精简首页并验证

**文件：**
- 修改：`inst/viewer/coordinated_views/UI.R`
- 测试：`tests/testthat/test-coordinated-views-browser.R`

- [ ] 移除 Composition/Clonotypes quick readout 和 selected-cell server UI。
- [ ] 运行 `node --check inst/viewer/www/coordviews.js` 和 `git diff --check`。
- [ ] 运行 Linked views 单元及浏览器回归，确认普通与恢复布局都从最终容器重新计算。
