# Extra material calm ledger implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 将 Builder Extra material 的工作簿列表改为自然增高的 Calm ledger，并明确反馈新上传的已折叠工作簿。

**架构：** Server 继续按 `file_name` 产生 workbook cards，但不再为任何 card 写入 `open`。上传完成后 Server 在输出刷新后发送一个小的 workbook/count 通知；浏览器只负责显示该瞬态通知、给相应 card 加一次 highlight，并在必要时将其滚入页面视口。CSS 将 Extra material 的列表从共享的受限文件列表样式中分离，使它只有页面滚动而没有内部滚动。

**技术栈：** Shiny server/UI、原生浏览器 JavaScript、CSS、testthat。

---

## 文件与职责

- `inst/builder/extras.R`：为每个 table 保留独立的可见 `display_name`，并将它写入 CRB table index。
- `inst/builder/server/enhancements.R`：收集一次上传中每个 workbook 新增的 table 数，在 render flush 后通知浏览器；编辑 table 时只更新 `display_name`。
- `inst/builder/server/review.R`：保持 workbook cards 默认折叠，公开稳定的 workbook key，并以 `display_name` 显示 sheet title。
- `inst/builder/www/builder.js`：消费上传完成消息，显示一次提示、为新增 cards 添加一次 highlight，并只在需要时滚动页面。
- `inst/builder/www/builder.components.css`：移除 Extra material 的列表高度/内部滚动限制，应用 Calm ledger 的卡片、sheet-row、反馈与 reduced-motion 样式。
- `tests/testthat/test-builder-stage-server.R`：验证上传后列表默认折叠、输出含有消息承载点和 workbook key，以及 Server 发送 workbook/count 负载。
- `tests/testthat/test-builder-ui-contract.R`：验证 Extra material selector 不含 `max-height` 或 `overflow-y`，且客户端包含一次性上传反馈处理器。

### 任务 1：写出失败的列表与上传反馈契约

**文件：**
- 修改：`tests/testthat/test-builder-stage-server.R:2850-2910`
- 修改：`tests/testthat/test-builder-ui-contract.R:777-850`

- [ ] **步骤 1：增加默认折叠与反馈 HTML 的 server 断言**

  在现有 Extra material 上传 session 测试中，捕获 `session$sendCustomMessage()` 的 `enhance_tables_added` 调用；上传一个 CSV 后断言：

  ```r
  expect_false(grepl('<details[^>]*\\bopen=', table_list_html, perl = TRUE))
  expect_match(table_list_html, 'data-workbook-key="clinical-results.csv"', fixed = TRUE)
  expect_match(table_list_html, 'id="enhance-table-upload-notice"', fixed = TRUE)
  expect_identical(
    added_messages[[1L]],
    list(workbooks = list(list(key = "clinical-results.csv", count = 1L)))
  )
  ```

- [ ] **步骤 2：增加失败的客户端/CSS 契约断言**

  在 supplementary tables UI contract 测试中取 `builder.components.css` 与 `builder.js`，断言 Extra material 规则没有受限高度或内部纵向滚动，并且新消息 handler 和一次性 highlight class 存在：

  ```r
  expect_false(grepl(
    "\\.enhance-table-list\\s*\\{[^}]*max-height",
    components,
    perl = TRUE
  ))
  expect_false(grepl(
    "\\.enhance-table-list\\s*\\{[^}]*overflow-y",
    components,
    perl = TRUE
  ))
  expect_match(js, 'enhance_tables_added', fixed = TRUE)
  expect_match(js, 'enhance-workbook-item--new', fixed = TRUE)
  ```

- [ ] **步骤 3：运行测试确认失败**

  运行：

  ```bash
  R --vanilla -q -e 'devtools::test(filter = "builder-stage-server|builder-ui-contract")'
  ```

  预期：新断言失败，因为 cards 仍写 `open`、没有上传完成消息，且 CSS 仍将 `.enhance-table-list` 限制为 `22rem`。

- [ ] **步骤 4：提交红色测试**

  ```bash
  git add tests/testthat/test-builder-stage-server.R tests/testthat/test-builder-ui-contract.R
  git commit -m "test(builder): cover calm extra-material ledger"
  ```

### 任务 2：实现 server 到浏览器的一次性上传反馈

**文件：**
- 修改：`inst/builder/extras.R:44-218`
- 修改：`inst/builder/server/enhancements.R:270-302`
- 修改：`inst/builder/server/review.R:363-535`
- 修改：`inst/builder/www/builder.js:3510-4010`

- [ ] **步骤 1：在上传 handler 收集稳定 workbook key 和新增数**

  初始化一个命名计数列表，在每个成功 `got` 写入 settings 后增加该文件名的计数；调用 `replace_entry(entry)` 后，通过 `session$onFlushed(..., once = TRUE)` 发送：

  ```r
  session$sendCustomMessage(
    "enhance_tables_added",
    list(workbooks = unname(lapply(names(added), function(key) {
      list(key = key, count = unname(added[[key]]))
    })))
  )
  ```

  仅当至少成功加入一个 table 时发送。保留现有逐 worksheet 错误通知。

- [ ] **步骤 1a：将可见 sheet label 从内部 table key 分离**

  `builder_read_table()` 和每个 Excel worksheet record 都初始化
  `display_name` 为原始 sheet/table 名。`builder_attach_tables()` 将
  `display_name` 写入 `table_index`。`rename` action 只设置
  `tables[[key]]$display_name`，要求非空但不重命名 list key；不同
  workbooks 可以使用相同可见 sheet label。

- [ ] **步骤 2：让 cards 默认折叠并提供浏览器锚点**

  在 `renderUI()` 中删除 `open = identical(index, 1L)`；在每个 `tags$details()` 上增加 `` `data-workbook-key` = filename ``，并以 `table$display_name %||% sheet_name` 作为 sheet 标题和编辑输入值。在列表计数标题之后增加：

  ```r
  div(
    id = "enhance-table-upload-notice",
    class = "enhance-table-upload-notice",
    role = "status",
    `aria-live` = "polite",
    hidden = NA
  )
  ```

  不把上传反馈写入 entry settings，避免它在后续重渲染中重复显示。

- [ ] **步骤 3：添加小型 client-only 消息 handler**

  在现有 `Shiny.addCustomMessageHandler` 区域实现 `enhance_tables_added`：根据 `data-workbook-key` 找到 card，确保 `details.open = false`，增加 `enhance-workbook-item--new` class，并在一次 CSS animation 完成后移除 class。将每个 workbook 的 count 汇总为自然语言提示，填入 `#enhance-table-upload-notice`，显示约 5 秒；只在目标不在可视区域时调用 `scrollIntoView({ block: "nearest", behavior: "smooth" })`。不要调用 `send()`，也不要改动 Edit/Save 的本地提交逻辑。

- [ ] **步骤 4：运行新增的 server/UI 测试确认通过**

  运行：

  ```bash
  R --vanilla -q -e 'devtools::test(filter = "builder-stage-server|builder-ui-contract")'
  node --check inst/builder/www/builder.js
  ```

  预期：新 Extra material 断言通过；若 UI contract 保留与此功能无关的既有 metadata-control 失败，记录其确切数量与行号，而不修改它们。

- [ ] **步骤 5：提交 server/client 行为**

  ```bash
  git add inst/builder/server/enhancements.R inst/builder/server/review.R inst/builder/www/builder.js tests/testthat/test-builder-stage-server.R tests/testthat/test-builder-ui-contract.R
  git commit -m "feat(builder): announce newly added extra tables"
  ```

### 任务 3：实现无内部滚动的 Calm ledger 视觉层级

**文件：**
- 修改：`inst/builder/www/builder.components.css:635-760`
- 测试：`tests/testthat/test-builder-ui-contract.R:777-850`

- [ ] **步骤 1：从共享文件列表约束中分离 Extra material**

  保留 `.builder-file-list` 的现有受限高度行为给其他文件列表；将 `.enhance-table-list` 改成独立的 grid 规则，明确使用 `max-height: none`、`overflow: visible` 和 `padding-right: 0`。页面自身是唯一滚动容器。

- [ ] **步骤 2：应用 Calm ledger CSS**

  使用白底、细中性色边框和克制圆角定义 `.enhance-workbook-item`；让 workbook header 视觉强于 sheet rows，并用细分隔线和左 inset 组织 sheet。实现：

  ```css
  .enhance-workbook-item--new {
    animation: enhance-workbook-added 1.4s var(--ease-standard) both;
  }
  @keyframes enhance-workbook-added {
    0% { border-color: var(--builder-action); box-shadow: 0 0 0 0 color-mix(in srgb, var(--builder-action) 24%, transparent); }
    100% { border-color: var(--c-border); box-shadow: var(--shadow-1); }
  }
  @media (prefers-reduced-motion: reduce) {
    .enhance-workbook-item--new { animation: none; border-color: var(--builder-action); }
  }
  ```

  样式 `.enhance-table-upload-notice` 为紧凑绿色状态提示；保持 amber 只用于新 card 的一次性 cue，red 只用于删除操作。窄屏下 actions 换行且 sheet inset 收回。

- [ ] **步骤 3：运行聚焦视觉与数据测试**

  运行：

  ```bash
  R --vanilla -q -e 'devtools::test(filter = "builder-stage-enhance")'
  node --check inst/builder/www/builder.js
  git diff --check
  ```

  预期：`builder-stage-enhance` 为 0 failures，JavaScript 无语法错误，whitespace 检查无输出。

- [ ] **步骤 4：在隔离 Builder 中做浏览器核验**

  启动当前 worktree 的 Builder，上传多-sheet workbook 和第二个 workbook，核对：

  ```text
  - 页面高度增加；附件区没有滚轮条。
  - 两个 cards 都是折叠状态。
  - 最新 card 有一次 highlight 和“tables added”可见提示。
  - 展开 card 后 sheet 全部可见；Edit 输入不触发刷新。
  ```

- [ ] **步骤 5：提交视觉完成项**

  ```bash
  git add inst/builder/www/builder.components.css tests/testthat/test-builder-ui-contract.R
  git commit -m "style(builder): polish extra-material workbook ledger"
  ```

## 计划自检

- 规格的默认折叠、无内部滚动、页面自然增高、一次性新增反馈、Calm ledger hierarchy、local-only editing 和可访问性均有明确任务覆盖。
- Server 的 `workbooks` payload、review 的 `data-workbook-key`、browser handler 和 CSS class 使用同一命名。
- 计划没有新增 Viewer selector、无限状态持久化或额外依赖；上传反馈是一次性的 client state。
