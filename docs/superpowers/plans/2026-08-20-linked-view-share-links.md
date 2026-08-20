# Linked-view share links 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 创建不可猜、可撤销、90 天后自动失效的 linked-view 配置分享链接。

**架构：** `share_store.R` 以 SQLite 保存服务器已规范化的 JSON 和 token/撤销凭证哈希；`server.R` 将 create/fetch/revoke 接入现有消息协议。浏览器维护本机撤销凭证和分享链接清单，并在 URL 含 `linked_view` token 时请求恢复。

**技术栈：** Shiny、jsonlite、DBI/RSQLite、浏览器 localStorage。

---

## 文件职责

- 创建：`inst/viewer/coordinated_views/share_store.R` — SQLite 初始化、token、创建、读取、撤销、过期清理。
- 修改：`inst/viewer/coordinated_views/server.R` — 扩展分享协议 action。
- 修改：`inst/viewer/coordinated_views/UI.R` — 分享区域和链接列表容器。
- 修改：`inst/viewer/www/coordviews-config.js` — 创建、复制、撤销和 URL 恢复。
- 修改：`inst/viewer/www/coordviews.css` — 分享区域样式。
- 修改：`DESCRIPTION`、`R/createShinyApp.R` — SQLite 依赖与 `linked_view_share_db` 配置。
- 创建：`tests/testthat/test-coordinated-views-share-store.R` — 纯存储单元测试。
- 修改：`tests/testthat/test-coordinated-views-config.R` — 协议与 UI 静态约束。

### 任务 1：存储层

**文件：** `tests/testthat/test-coordinated-views-share-store.R`、`inst/viewer/coordinated_views/share_store.R`、`DESCRIPTION`

- [ ] **步骤 1：编写失败的测试**

```r
test_that("a share record is opaque, revocable and expires", {
  store <- cv_share_store_open(tempfile(fileext = ".sqlite"))
  created <- cv_share_store_create(store, '{"schema":"test"}', "fp-1")
  expect_match(created$token, "^[A-Za-z0-9_-]{43}$")
  expect_equal(cv_share_store_fetch(store, created$token, "fp-1")$json, '{"schema":"test"}')
  expect_error(cv_share_store_revoke(store, created$token, "wrong"), class = "cv_share_error")
  cv_share_store_revoke(store, created$token, created$receipt)
  expect_error(cv_share_store_fetch(store, created$token, "fp-1"), class = "cv_share_error")
})
```

- [ ] **步骤 2：运行红灯测试**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-share-store.R")'`

预期：FAIL，`cv_share_store_open` 不存在。

- [ ] **步骤 3：编写最小实现**

```r
cv_share_store_open <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS linked_view_shares (...)")
  structure(list(con = con), class = "cv_share_store")
}
```

Use 32 random bytes for both URL-safe token and receipt, persist only the
receipt hash, set `expires_at = created_at + 90 * 24 * 60 * 60`, and reject
unknown, revoked, expired, or fingerprint-mismatched records with
`cv_share_error`.

- [ ] **步骤 4：运行绿灯测试**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-share-store.R")'`

预期：PASS，覆盖创建、读取、错误撤销、正确撤销、过期和数据集不匹配。

- [ ] **步骤 5：Commit**

```bash
git add DESCRIPTION inst/viewer/coordinated_views/share_store.R tests/testthat/test-coordinated-views-share-store.R
git commit -m "feat(viewer): add expiring linked-view share store"
```

### 任务 2：服务端协议与应用配置

**文件：** `inst/viewer/coordinated_views/server.R`、`R/createShinyApp.R`、`tests/testthat/test-coordinated-views-config.R`

- [ ] **步骤 1：编写失败的协议断言**

Assert the action allowlist includes `share_create`, `share_open`, and
`share_revoke`; assert creation calls `cv_config_prepare()` and opening calls
`cv_config_decode()` before applying state.

- [ ] **步骤 2：运行红灯测试**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`

预期：FAIL，新的 action 和存储调用不存在。

- [ ] **步骤 3：编写最小实现**

```r
if (identical(action, "share_create")) {
  prepared <- cv_config_prepare(request$config, cells = bundle$cells)
  created <- cv_share_store_create(store, prepared$json, bundle$dataset_fingerprint)
  cv_config_send_result(nonce, action, TRUE, token = created$token,
    receipt = created$receipt, expires_at = created$expires_at)
}
```

`share_open` fetches the JSON then runs `cv_config_decode()` and
`cv_config_validate_genes()`. `share_revoke` requires token plus receipt.
`createShinyApp()` gains `linked_view_share_db = NULL`; an absent configured
path returns `share_unavailable` and does not affect local save/import/export.

- [ ] **步骤 4：运行绿灯测试并提交**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`

预期：PASS。

```bash
git add R/createShinyApp.R inst/viewer/coordinated_views/server.R tests/testthat/test-coordinated-views-config.R
git commit -m "feat(viewer): serve linked-view share links"
```

### 任务 3：浏览器交互

**文件：** `inst/viewer/coordinated_views/UI.R`、`inst/viewer/www/coordviews-config.js`、`inst/viewer/www/coordviews.css`、`tests/testthat/test-coordinated-views-config.R`

- [ ] **步骤 1：编写失败的 UI/客户端断言**

Assert `cv-config-share`, `cv-share-create`, `cv-share-list`, `linked_view`,
`share_create`, `share_open`, `share_revoke`, and the receipt key
`cerebro.linked-views.share-receipts.v1` occur in their intended files.

- [ ] **步骤 2：运行红灯测试**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`

预期：FAIL，分享区域和浏览器 action 不存在。

- [ ] **步骤 3：编写最小实现**

```js
function createShareLink() { request('share_create'); }
function openShareFromUrl() {
  var token = new URLSearchParams(window.location.search).get('linked_view');
  if (token) requestShareOpen(token);
}
```

Show a separate `Share with a link` region, copy/revoke actions, 90-day
explanation, and a local list of receipt-bearing links. Store only token,
receipt, and expiry locally; never put the receipt in the URL. Remove a
successfully restored token with `history.replaceState()`.

- [ ] **步骤 4：运行绿灯测试并提交**

运行：`node --check inst/viewer/www/coordviews-config.js && Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`

预期：PASS。

```bash
git add inst/viewer/coordinated_views/UI.R inst/viewer/www/coordviews-config.js inst/viewer/www/coordviews.css tests/testthat/test-coordinated-views-config.R
git commit -m "feat(viewer): add share-link controls"
```

### 任务 4：验收

**文件：** `tests/testthat/test-coordinated-views-share-store.R`、`tests/testthat/test-coordinated-views-config.R`

- [ ] **步骤 1：执行重点验证**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-share-store.R"); testthat::test_file("tests/testthat/test-coordinated-views-config.R")' && node --check inst/viewer/www/coordviews-config.js && git diff --check`

预期：所有断言通过、JavaScript 可解析、没有 whitespace 错误。

- [ ] **步骤 2：人工验收**

用设置了 `linked_view_share_db` 的 Viewer 创建链接，在新浏览器 session 打开并恢复，随后撤销，确认旧 URL 提示失效且不改变当前 selection。
