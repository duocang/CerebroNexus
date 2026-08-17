# Builder keyed dataset rail implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让 Builder ready dataset rail 按 dataset ID 局部协调 DOM，避免单个数据变化造成整栏重建。

**架构：** R 端继续唯一负责行模型和 HTML；服务器在 `store()` 或 `current()` 改变后发送完整的目标快照；浏览器比较 fingerprint，并只增删、替换或移动必要节点。静态空 rail 是首次渲染与异常回退，上传队列完全独立。

**技术栈：** R/Shiny、htmltools、原生 JavaScript、testthat。

---

## 文件职责

- 修改 `inst/builder/ui/dataset_rail.R`：提取行模型、fingerprint、单行 renderer 和完整 patch payload。
- 修改 `inst/builder/server/datasets.R`：移除 monolithic `renderUI`，按完整可见模型门控并发送快照。
- 修改 `inst/builder/www/builder.js`：校验并执行 keyed reconciliation，保留焦点并刷新 rail summary。
- 修改 `tests/testthat/test-builder-rail.R`：覆盖模型字段、fingerprint 和局部更新协议。
- 修改 `tests/testthat/test-builder-loading-browser.R`：覆盖节点身份保持与 selection/reorder 的局部变化。

### 任务 1：建立服务端行模型

- [x] 在 `tests/testthat/test-builder-rail.R` 添加失败测试：两个相同 entry 产生相同 fingerprint；selected、index、readiness、confirm、can_up、can_down 任一变化都会改变 fingerprint；row HTML 包含 `data-rail-fingerprint`。
- [x] 运行 `Rscript -e 'testthat::test_file("tests/testthat/test-builder-rail.R", filter="fingerprint")'`，确认 helper 尚不存在而失败。
- [x] 在 `inst/builder/ui/dataset_rail.R` 实现 `builder_dataset_rail_row_model()`、`builder_dataset_rail_row_fingerprint()`、`builder_dataset_rail_row_ui()`，并让 `builder_dataset_rail_ui()` 委托这些函数。fingerprint 使用模型可见字段的稳定 UTF-8 编码，不包含对象地址或运行时随机值。
- [x] 再运行上述 rail 测试，预期通过。
- [x] 提交 `feat(builder): model keyed dataset rail rows`。

### 任务 2：建立完整目标快照与响应门控

- [x] 在 `tests/testthat/test-builder-rail.R` 添加失败测试：`builder_dataset_rail_patch()` 返回有序的 `id/fingerprint/html` 行，空状态返回 `empty_html`，相同可见模型 identical；修改非可见 entry 字段不会改变 payload。
- [x] 运行 rail 定向测试，确认 patch helper 尚不存在而失败。
- [x] 在 `inst/builder/ui/dataset_rail.R` 实现 `builder_dataset_rail_patch(state, current)`，使用 `htmltools::renderTags()` 得到安全行 HTML。
- [x] 在 `inst/builder/server/datasets.R` 删除 `output$ds_ready_list <- renderUI(...)`。新增 observer 同时读取 `store()` 与 `current()`，用 `reactiveVal` 保存上一 payload，只在 `!identical(previous, next)` 时调用 `session$sendCustomMessage("builder_dataset_rail_patch", next)`；初始空 rail 由 `app.R` 的静态 markup 提供。
- [x] 运行 rail 定向测试和 `Rscript -e 'parse(file="inst/builder/server/datasets.R")'`，预期通过。
- [x] 提交 `feat(builder): publish dataset rail snapshots`。

### 任务 3：实现浏览器 keyed reconciliation

- [x] 在 `tests/testthat/test-builder-rail.R` 添加静态协议测试，要求存在 `builder_dataset_rail_patch` handler、重复 ID/错误 HTML 校验、fingerprint 比较、`replaceWith`、`appendChild`、缺失 ID 删除、焦点恢复和 `updateRailSummary()`。
- [x] 运行静态协议测试，确认 handler 尚不存在而失败。
- [x] 在 `inst/builder/www/builder.js` 实现纯函数式校验和同步 reconcile：先完整解析/验证 payload，再执行 DOM mutation；相同 fingerprint 复用节点；不同 fingerprint 替换；按快照顺序 append/move；删除剩余节点；空集合使用 `empty_html`。
- [x] 焦点记录为 `{datasetId, action, direction}`；替换后查询等价 `.builder-pick/.builder-reorder/.builder-drop`。找不到时调用既有 rail focus fallback。最后调用 `updateRailSummary()`。
- [x] 在 Shiny 初始化区注册 `window.Shiny.addCustomMessageHandler("builder_dataset_rail_patch", reconcileDatasetRail)`。
- [x] 运行 rail 定向测试和 `node --check inst/builder/www/builder.js`，预期通过。
- [x] 提交 `feat(builder): reconcile dataset rail by key`。

### 任务 4：浏览器行为回归

- [x] 在 `tests/testthat/test-builder-loading-browser.R` 保存既有 ready row DOM 引用，在添加另一个示例后断言旧 row 仍为同一节点。
- [x] 添加 selection 断言：切换当前 dataset 后，仅旧/新 selected 行允许被替换，第三个未变化节点身份保持。
- [x] 添加 reorder 断言：未变 fingerprint 的中间节点被移动而非重建；首尾按钮状态正确。
- [x] 添加 Spatial Save 断言：当 rail-visible readiness 不变时，所有 ready row 节点身份保持且 rail 不获得 `.recalculating`。
- [x] 仅运行 `CEREBRO_RUN_BROWSER_TESTS=true Rscript -e 'testthat::test_file("tests/testthat/test-builder-loading-browser.R")'`，预期新增场景通过。
- [x] 提交 `test(builder): cover keyed rail reconciliation`。

### 任务 5：最终聚焦核对

- [x] 运行 `Rscript -e 'testthat::test_file("tests/testthat/test-builder-rail.R")'`。
- [x] 运行 `Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R")'`。
- [x] 运行 `node --check inst/builder/www/builder.js`、相关 R 文件 parse 和 `git diff --check`。
- [x] 对照规格逐项检查：无 `ds_ready_list` monolithic `renderUI`；消息是完整目标快照；串行上传路径无 diff；Reset 仍为描边按钮；CSS fallback 仍严格限定 ready rail。
- [x] 将计划复选框更新为完成并提交 `docs(builder): complete keyed rail plan`。
