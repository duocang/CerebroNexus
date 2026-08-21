# Extra-material workbook groups 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 在 Builder 中以可折叠的 workbook 卡片管理导入的表，允许独立提交 workbook／sheet 显示名，并将 workbook→sheet 索引写入 CRB。

**架构：** `settings$tables` 仍以唯一 Viewer table key 保存表数据。每条记录新增 immutable source metadata（`file_name`、`sheet_name`）与 editable `workbook_name`；`builder_attach_tables()` 同时写平坦的 `tables` 和兼容 Viewer 消费的 `table_index`。Builder 以 `file_name` 分组，客户端只在 Save／Enter 时发送 action。

**技术栈：** R/Shiny、htmltools、base R list metadata、浏览器原生 `click` / `keydown`。

---

## 文件结构

- 修改：`inst/builder/extras.R` — table-record defaults、workbook grouping、CRB `table_index`。
- 修改：`inst/builder/server/enhancements.R` — upload metadata 和 rename/remove workbook actions。
- 修改：`inst/builder/server/review.R` — workbook cards 与紧凑 sheet rows 的动态 UI。
- 修改：`inst/builder/www/builder.js` — 显式 edit/save/cancel action；删除每次输入的提交。
- 修改：`inst/builder/www/builder.components.css` — workbook card、sheet row、inline editor 样式。
- 修改：`tests/testthat/test-builder-stage-enhance.R` — record/index 单测。
- 修改：`tests/testthat/test-builder-stage-server.R` — server action 与渲染契约。
- 修改：`tests/testthat/test-builder-ui-contract.R` — DOM/JS 事件契约。

### 任务 1：定义 record 与 CRB index 契约

**文件：**
- 修改：`tests/testthat/test-builder-stage-enhance.R:500-580`
- 修改：`inst/builder/extras.R:16-200`

- [ ] **步骤 1：编写失败的测试**

```r
test_that("table records retain workbook and source-sheet metadata", {
  got <- builder_read_tables(path, filename = "supplement.xlsx")
  expect_identical(got$Clinical$workbook_name, "supplement")
  expect_identical(got$Clinical$sheet_name, "Clinical")
})

test_that("attached tables retain a Viewer workbook index", {
  object <- builder_attach_tables(object, list(
    list(name = "30mins", workbook_name = "All samples",
      file_name = "all.xlsx", sheet_name = "30mins", table = data.frame(x = 1))
  ))
  expect_identical(object@misc$extra_material$table_index$`30mins`$workbook_name, "All samples")
})
```

- [ ] **步骤 2：运行测试验证失败**

运行：`R --vanilla -q -e 'devtools::test(filter = "builder-stage-enhance")'`

预期：FAIL，`workbook_name` / `table_index` 不存在。

- [ ] **步骤 3：编写最少实现代码**

```r
## builder_read_tables() workbook records
list(
  name = paste(workbook, sheet, sep = " · "),
  workbook_name = workbook,
  sheet_name = sheet,
  sheet = sheet,
  table = table
)

## builder_attach_tables()
index <- object@misc$extra_material$table_index %||% list()
for (t in tables) {
  existing[[t$name]] <- t$table
  index[[t$name]] <- list(
    workbook_name = t$workbook_name %||% builder_table_default_name(t$file_name),
    file_name = t$file_name %||% "",
    sheet_name = t$sheet_name %||% t$sheet %||% t$name
  )
}
object@misc$extra_material$table_index <- index
```

- [ ] **步骤 4：运行测试验证通过**

运行：`R --vanilla -q -e 'devtools::test(filter = "builder-stage-enhance")'`

预期：PASS，新增 metadata/index 断言通过。

- [ ] **步骤 5：Commit**

```bash
git add inst/builder/extras.R tests/testthat/test-builder-stage-enhance.R
git commit -m "feat(builder): retain extra-table workbook metadata"
```

### 任务 2：实现 workbook 级 Builder 操作与 card UI

**文件：**
- 修改：`tests/testthat/test-builder-stage-server.R:2840-2900`
- 修改：`inst/builder/server/enhancements.R:258-340`
- 修改：`inst/builder/server/review.R:357-405`
- 修改：`inst/builder/www/builder.components.css:680-725`

- [ ] **步骤 1：编写失败的测试**

```r
session$setInputs(`enhance-table_action` = list(
  action = "rename_workbook", key = "all.xlsx", name = "All samples", nonce = 3
))
session$flushReact()
expect_true(all(vapply(sets()[[1L]]$settings$tables, function(x) {
  identical(x$workbook_name, "All samples")
}, logical(1))))

expect_match(table_list_html, "enhance-workbook-item", fixed = TRUE)
expect_match(table_list_html, "Remove workbook", fixed = TRUE)
```

- [ ] **步骤 2：运行测试验证失败**

运行：`R --vanilla -q -e 'devtools::test(filter = "builder-stage-server")'`

预期：FAIL，`rename_workbook` 无 action，UI 没有 workbook card。

- [ ] **步骤 3：编写最少实现代码**

```r
if (identical(action$action, "rename_workbook")) {
  new_name <- trimws(as.character(action$name %||% ""))
  rows <- vapply(tables, function(table) identical(table$file_name, action$key), logical(1))
  if (!nzchar(new_name) || !any(rows)) return()
  tables[rows] <- lapply(tables[rows], function(table) {
    table$workbook_name <- new_name
    table
  })
}
```

In `review.R`, group `tables` by `file_name`; render one `details` workbook
card with the metadata once, then a `sheet` row per table. Use `workbook_name`
for the card title and `sheet_name` beneath the editable table name.

- [ ] **步骤 4：运行测试验证通过**

运行：`R --vanilla -q -e 'devtools::test(filter = "builder-stage-server")'`

预期：PASS，server action 与 rendered card assertions 通过。

- [ ] **步骤 5：Commit**

```bash
git add inst/builder/server/enhancements.R inst/builder/server/review.R inst/builder/www/builder.components.css tests/testthat/test-builder-stage-server.R
git commit -m "feat(builder): group extra tables by workbook"
```

### 任务 3：使改名只在显式提交时发生

**文件：**
- 修改：`tests/testthat/test-builder-ui-contract.R:790-820`
- 修改：`inst/builder/www/builder.js:3618-3632,3819-3914`
- 修改：`inst/builder/server/review.R:357-405`

- [ ] **步骤 1：编写失败的测试**

```r
expect_no_match(js, 'event.target.matches(".enhance-table-display-name")')
expect_match(js, 'action: "rename"', fixed = TRUE)
expect_match(js, 'event.key === "Enter"', fixed = TRUE)
expect_match(html, "enhance-table-save", fixed = TRUE)
expect_match(html, "enhance-table-cancel", fixed = TRUE)
```

- [ ] **步骤 2：运行测试验证失败**

运行：`R --vanilla -q -e 'devtools::test(filter = "builder-ui-contract")'`

预期：FAIL，因为 document `input` listener 仍提交 rename。

- [ ] **步骤 3：编写最少实现代码**

```js
function commitTableRename(row) {
  var input = row.querySelector(".enhance-table-display-name");
  send("enhance-table_action", { action: "rename", key: input.dataset.tableKey,
    name: input.value, nonce: Date.now() });
}

document.addEventListener("click", function (event) {
  var save = event.target.closest(".enhance-table-save");
  if (save) commitTableRename(save.closest(".enhance-sheet-item"));
});
document.addEventListener("keydown", function (event) {
  if (event.key === "Enter" && event.target.matches(".enhance-table-display-name")) {
    event.preventDefault();
    commitTableRename(event.target.closest(".enhance-sheet-item"));
  }
});
```

Delete the `.enhance-table-display-name` branch from the document `input`
listener. `Edit` only toggles a local class; `Cancel` restores the captured
value without a Shiny message. Apply the same helper and classes to the
workbook name editor.

- [ ] **步骤 4：运行测试验证通过**

运行：`R --vanilla -q -e 'devtools::test(filter = "builder-ui-contract")'`

预期：PASS，HTML/JS contract proves explicit-only commits.

- [ ] **步骤 5：Commit**

```bash
git add inst/builder/server/review.R inst/builder/www/builder.js tests/testthat/test-builder-ui-contract.R
git commit -m "fix(builder): commit extra-table names explicitly"
```

### 任务 4：验证真实交互和兼容输出

**文件：**
- 测试：`tests/testthat/test-builder-stage-enhance.R`
- 测试：`tests/testthat/test-builder-stage-server.R`
- 测试：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：运行 focused suite**

运行：`R --vanilla -q -e 'devtools::test(filter = "^(builder-stage-enhance|builder-stage-server|builder-ui-contract)$")'`

预期：`FAIL 0`。

- [ ] **步骤 2：运行 browser smoke test**

运行：`NOT_CRAN=true R --vanilla -q -e 'devtools::test(filter = "builder.*browser")'`

预期：相关 Builder browser tests 通过或显示已知、与本改动无关的 skip。

- [ ] **步骤 3：检查 package diff**

运行：`git diff --check HEAD~3..HEAD && git status --short`

预期：无 whitespace error，只有本功能文件的已提交改动。

- [ ] **步骤 4：Commit verification note if needed**

不新增代码；仅在本次实际产生未提交的测试/格式更正时，按所属任务的 conventional commit 提交。
