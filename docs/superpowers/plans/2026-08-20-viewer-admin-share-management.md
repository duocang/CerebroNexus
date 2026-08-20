# Viewer Admin 分享链接管理实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让现有认证数据库中的 Administrator 在 Viewer 内创建并统一管理多个只读分享链接，同时从普通 Viewer 隐藏并阻断所有管理能力。

**架构：** `viewer_auth_apply()` 把服务器认证得到的用户和 `admin` 角色写入 session 私有上下文；Viewer 根据该上下文动态呈现 Admin 导航并在每个管理 handler 再次鉴权。分享 SQLite 增加可查询的审计元数据；Admin 页管理全部链接，Linked views 弹窗只负责为当前快照创建新链接。`/admin` 由 Shiny HTTP handler 重写到应用根页面，再由认证后的服务器导航到 Admin tab。

**技术栈：** R/Shiny、shinydashboard、shinymanager、DBI/RSQLite、原生 JavaScript、testthat。

---

## 文件结构

- 修改 `inst/viewer/auth.R`：从认证结果提取服务器可信的用户与 Admin 角色，并写入 session 私有上下文。
- 修改 `inst/viewer/_bundle_app.R`：为 `/admin` 增加安全的应用入口重写。
- 创建 `inst/viewer/admin/UI.R`：Admin tab 的结构和空容器。
- 创建 `inst/viewer/admin/server.R`：动态导航、Admin bootstrap、链接列表和撤销 handler。
- 创建 `inst/viewer/www/admin.css`：沿用 Viewer 的卡片、琥珀色和表格层级。
- 创建 `inst/viewer/www/admin.js`：列表渲染、复制反馈、撤销和 `/admin` 导航反馈。
- 修改 `inst/viewer/shiny_UI.R`、`inst/viewer/shiny_server.R`：接入 Admin tab、资源和服务器逻辑。
- 修改 `inst/viewer/coordinated_views/share_store.R`：迁移审计字段，增加 Admin 列表与按 token 撤销接口。
- 修改 `inst/viewer/coordinated_views/server.R`：只允许 Admin 创建链接并写入创建者/数据集名称。
- 修改 `inst/viewer/coordinated_views/UI.R`、`inst/viewer/www/coordviews-config.js`、`inst/viewer/www/coordviews.css`：隐藏普通用户分享区，移除浏览器 revocation receipt 多链接库。
- 修改 `tests/testthat/test-viewer-auth-runtime.R`：认证角色和 session capability 回归测试。
- 修改 `tests/testthat/test-coordinated-views-share-store.R`：迁移、列表、多链接、Admin 撤销测试。
- 修改 `tests/testthat/test-coordinated-views-config.R`：普通用户伪造请求、Admin 创建和客户端状态测试。
- 创建 `tests/testthat/test-viewer-admin-ui.R`：Admin 导航/UI/HTTP route 合约测试。

### 任务 1：服务器可信的认证角色

**文件：**
- 修改：`inst/viewer/auth.R`
- 测试：`tests/testthat/test-viewer-auth-runtime.R`

- [ ] **步骤 1：编写失败的角色传播测试**

```r
test_that("Viewer server receives server-authoritative Administrator context", {
  auth_state <- shiny::reactiveValues(user = NULL, admin = NULL)
  observed <- NULL
  viewer_server <- function(input, output, session) {
    observed <<- session$userData$viewer_auth
  }
  # authenticate alice with auth_state$admin <- "TRUE"
  expect_identical(observed$user, "alice")
  expect_true(observed$is_admin)
})
```

- [ ] **步骤 2：运行测试并确认角色上下文缺失导致失败**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-viewer-auth-runtime.R")'`
预期：新断言失败，因为 `session$userData$viewer_auth` 尚未建立。

- [ ] **步骤 3：实现严格角色解析和 session 上下文**

```r
.viewer_auth_is_admin <- function(value) {
  isTRUE(value) || identical(value, "TRUE")
}

session$userData$viewer_auth <- list(
  authenticated = TRUE,
  user = user,
  is_admin = .viewer_auth_is_admin(auth$admin)
)
server(input, output, session)
```

公共模式由 Viewer 服务器的辅助函数回退为
`list(authenticated = FALSE, user = NULL, is_admin = FALSE)`，不信任任何
Shiny input 或浏览器存储。

- [ ] **步骤 4：运行认证测试并确认通过**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-viewer-auth-runtime.R")'`
预期：全部通过，Admin 与普通 Viewer 的角色均由 mock 认证结果决定。

- [ ] **步骤 5：提交认证上下文**

```bash
git add inst/viewer/auth.R tests/testthat/test-viewer-auth-runtime.R
git commit -m "feat(viewer): expose authenticated admin capability"
```

### 任务 2：可审计的多链接存储接口

**文件：**
- 修改：`inst/viewer/coordinated_views/share_store.R`
- 测试：`tests/testthat/test-coordinated-views-share-store.R`

- [ ] **步骤 1：编写失败的迁移、多链接列表和 Admin 撤销测试**

```r
first <- cv_share_store_create(store, json, "fp", creator = "alice", dataset = "PBMC")
second <- cv_share_store_create(store, json, "fp", creator = "alice", dataset = "PBMC")
rows <- cv_share_store_list(store, now)
expect_setequal(rows$token, c(first$token, second$token))
expect_true(all(rows$creator == "alice"))
cv_share_store_revoke_admin(store, first$token, now)
expect_error(cv_share_store_fetch(store, first$token, "fp", now), class = "cv_share_error")
expect_equal(cv_share_store_fetch(store, second$token, "fp", now)$json, json)
```

- [ ] **步骤 2：运行存储测试并确认新接口不存在**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-share-store.R")'`
预期：FAIL，缺少 `creator`/`dataset` 参数及 list/Admin revoke 函数。

- [ ] **步骤 3：实现向后兼容迁移和查询接口**

`cv_share_store_open()` 使用 `PRAGMA table_info` 幂等添加 `creator` 与
`dataset_label`；`cv_share_store_create()` 写入两列；
`cv_share_store_list()` 只返回 token、fingerprint、dataset_label、creator、
created_at、expires_at、revoked_at，不返回 JSON 或 receipt hash；
`cv_share_store_revoke_admin()` 只按严格 token 标记撤销。

- [ ] **步骤 4：运行存储测试并确认通过**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-share-store.R")'`
预期：全部通过，包括同一 fingerprint 同时存在多个有效链接。

- [ ] **步骤 5：提交存储接口**

```bash
git add inst/viewer/coordinated_views/share_store.R tests/testthat/test-coordinated-views-share-store.R
git commit -m "feat(viewer): add admin share link inventory"
```

### 任务 3：Admin 页面、导航与 `/admin` 入口

**文件：**
- 创建：`inst/viewer/admin/UI.R`
- 创建：`inst/viewer/admin/server.R`
- 创建：`inst/viewer/www/admin.css`
- 创建：`inst/viewer/www/admin.js`
- 修改：`inst/viewer/shiny_UI.R`
- 修改：`inst/viewer/shiny_server.R`
- 修改：`inst/viewer/_bundle_app.R`
- 创建：`tests/testthat/test-viewer-admin-ui.R`

- [ ] **步骤 1：编写失败的 Admin UI 和 route 合约测试**

```r
expect_match(rendered_ui, 'tabName="admin"', fixed = TRUE)
expect_match(rendered_ui, 'admin-sidebar-item', fixed = TRUE)
expect_true(viewer_admin_route("/admin"))
expect_false(viewer_admin_route("/administrator"))
```

测试还断言普通 session 的 `output$admin_sidebar_item` 为空，Admin session
返回位于 About 后的菜单项，非 Admin 的管理请求返回 `forbidden`。

- [ ] **步骤 2：运行新测试并确认文件/函数缺失**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-viewer-admin-ui.R")'`
预期：FAIL，Admin UI 和 route helper 尚不存在。

- [ ] **步骤 3：实现 Admin tab 和动态菜单**

`admin/UI.R` 定义 `tab_admin <- tabItem(tabName = "admin", ...)`；静态
sidebar 在 About 后放置 `uiOutput("admin_sidebar_item")`。服务器仅在
`session$userData$viewer_auth$is_admin` 为真时渲染菜单、读取列表并处理撤销。

- [ ] **步骤 4：实现 `/admin` HTTP 重写与登录后导航**

```r
viewer_admin_route <- function(path) identical(path, "/admin")
original_handler <- app$httpHandler
app$httpHandler <- function(req) {
  if (viewer_admin_route(req$PATH_INFO)) req$PATH_INFO <- "/"
  original_handler(req)
}
```

`admin.js` 从 `window.location.pathname` 报告深链意图；服务器只为 Admin
调用 `updateTabItems(..., selected = "admin")`，普通 Viewer 得到明确拒绝消息。

- [ ] **步骤 5：实现管理列表交互和视觉样式**

Admin bootstrap 只发送安全元数据。JS 生成语义化表格，Copy 立即变为
`Copied ✓`，Revoke 先显示进行状态，成功后移除对应行。CSS 复用现有
琥珀色、白色卡片和克制边框，不引入第二套视觉系统。

- [ ] **步骤 6：运行 Admin UI 测试并确认通过**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-viewer-admin-ui.R")'`
预期：全部通过。

- [ ] **步骤 7：提交 Admin 页面**

```bash
git add inst/viewer/admin inst/viewer/www/admin.css inst/viewer/www/admin.js inst/viewer/shiny_UI.R inst/viewer/shiny_server.R inst/viewer/_bundle_app.R tests/testthat/test-viewer-admin-ui.R
git commit -m "feat(viewer): add administrator share management page"
```

### 任务 4：Admin-only 创建与简化的 Linked views 分享区

**文件：**
- 修改：`inst/viewer/coordinated_views/server.R`
- 修改：`inst/viewer/coordinated_views/UI.R`
- 修改：`inst/viewer/www/coordviews-config.js`
- 修改：`inst/viewer/www/coordviews.css`
- 修改：`tests/testthat/test-coordinated-views-config.R`

- [ ] **步骤 1：编写失败的权限和客户端状态测试**

```r
expect_share_error(non_admin_create, code = "forbidden")
expect_true(admin_create$ok)
expect_identical(stored$creator, "alice")
```

浏览器合约断言分享区域默认 hidden、普通 Viewer capability 不显示、Admin
capability 显示，且脚本不再读写 `cerebro.linked-views.share-receipts.v1`。

- [ ] **步骤 2：运行相关测试并确认权限断言失败**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`
预期：FAIL，当前任何客户端都能发送 create，且仍使用 localStorage receipt。

- [ ] **步骤 3：实现服务端 Admin gate 和审计字段**

`share_create` 在解析/规范化配置前检查服务器 session capability；通过后把
`session$userData$viewer_auth$user` 和 `cv_selected_dataset_name()` 写入存储。
`share_open` 保持只读且不需要 Admin。旧的 receipt revoke handler 从
Linked views 通道移除，撤销只存在于 Admin server。

- [ ] **步骤 4：简化浏览器分享状态**

移除 `SHARE_KEY`、`readShares()`、`writeShares()` 和多行 `renderShares()`。
服务器发送 `coordviews_share_capability` 后才显示分享区；创建成功只在当前
弹窗显示新 URL 的 Copy 控件。关闭弹窗可以丢弃这个临时展示，因为 Admin
页面持久列出所有链接。

- [ ] **步骤 5：运行相关测试并确认通过**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`
预期：全部通过，普通 Viewer 伪造 create 被服务端拒绝，open 仍可用。

- [ ] **步骤 6：提交权限与弹窗简化**

```bash
git add inst/viewer/coordinated_views inst/viewer/www/coordviews-config.js inst/viewer/www/coordviews.css tests/testthat/test-coordinated-views-config.R
git commit -m "feat(viewer): restrict share creation to administrators"
```

### 任务 5：集成验证与文档收尾

**文件：**
- 修改：`docs/superpowers/specs/2026-08-20-admin-managed-linked-view-shares-design.md`
- 修改：`.loci/memory.md`
- 创建：`.loci/decisions/2026-08-20-admin-managed-linked-view-sharing.md`

- [ ] **步骤 1：运行聚焦测试**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-viewer-auth-runtime.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-share-store.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-viewer-admin-ui.R")'
```

预期：四组测试全部通过。

- [ ] **步骤 2：运行语法、格式与既有分享回归检查**

运行：

```bash
node --check inst/viewer/www/admin.js
node --check inst/viewer/www/coordviews-config.js
Rscript -e 'parse(file="inst/viewer/auth.R"); parse(file="inst/viewer/admin/server.R"); parse(file="inst/viewer/coordinated_views/server.R")'
git diff --check
```

预期：所有命令退出码为 0。

- [ ] **步骤 3：用本地浏览器验证真实行为**

使用带一个 `admin = TRUE` 与一个 `admin = FALSE` 账户的认证 fixture：

1. Admin 登录后看见 About 下方 Admin；创建同一数据的两个链接，二者均列出。
2. Copy link 显示 `Copied ✓`；撤销其中一个不影响另一个。
3. 普通 Viewer 登录后无 Admin 导航、无分享创建区；直接 `/admin` 显示拒绝。
4. 普通 Viewer 打开仍有效的分享链接，可以恢复但无法管理。

- [ ] **步骤 4：记录 Viewer-only 范围和 Builder 延后决策**

规格和项目记忆明确：本轮不修改 Builder；Builder 角色编辑与默认 Admin
账户是单独的后续功能。

- [ ] **步骤 5：提交收尾**

```bash
git add docs/superpowers/specs/2026-08-20-admin-managed-linked-view-shares-design.md .loci/memory.md .loci/decisions/2026-08-20-admin-managed-linked-view-sharing.md
git commit -m "docs(viewer): record admin share ownership model"
```
