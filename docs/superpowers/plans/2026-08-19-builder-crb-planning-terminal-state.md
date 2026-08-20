# Builder CRB Planning Terminal-State 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 确保 Prepare checked CRBs 从 Step 1 开始总能收到确认或失败终态，不再让页面永久 inert。

**架构：** 浏览器为 CRB 请求分配 id 并启动一次性确认计时器；服务端在任何耗时工作前回送 `planning`，之后所有 guard、保存失败和异常均通过统一 helper 回送 `failed`。进度消息携带请求 id，浏览器只接受当前请求；断连或确认超时在本地显示可关闭的失败终态。

**技术栈：** R/Shiny reactive server、Shiny custom messages、原生 JavaScript、testthat 源码合同测试。

---

## 文件结构

- 修改 `tests/testthat/test-builder-project.R`：锁定服务端 Planning 确认、统一失败和 save-false 闭环。
- 修改 `tests/testthat/test-builder-ui-contract.R`：锁定请求关联、确认超时和断连复位。
- 修改 `inst/builder/server/project.R`：实现 CRB 请求 id、progress/failure helper 和完整早退闭环。
- 修改 `inst/builder/www/builder.js`：实现请求确认计时器、进度关联和本地失败终态。

### 任务 1：先写失败的回归合同

**文件：**
- 测试：`tests/testthat/test-builder-project.R`
- 测试：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：加入服务端合同**

断言 `prepare_builder_project_crbs` 在构建 plan 前发送 `planning`；断言 capability、空项目、预算、路径、目录、save-false 和异常路径都调用统一 failure helper；断言 CRB progress 带当前 request id。

- [ ] **步骤 2：加入浏览器合同**

断言浏览器保存当前 request id、只维护一个 acknowledgement timer、任意匹配 progress 都清 timer、不同 id 的消息被忽略，关闭对话框和 `shiny:disconnected` 都结束 Planning 等待。

- [ ] **步骤 3：记录 red 命令**

```bash
Rscript -e 'testthat::test_local(filter = "builder-(project$|ui-contract)", load_package = "source")'
```

预期：生产代码尚无 request id、planning acknowledgement 和 timer，因此新增断言 FAIL。本次按用户要求不执行该命令。

### 任务 2：实现服务端终态闭环

**文件：**
- 修改：`inst/builder/server/project.R:3-34,1720-1832,1834-2071`

- [ ] **步骤 1：增加请求状态和发送 helper**

增加当前 CRB request id reactive，并用单一 helper 组装 `status`、`completed`、`total`、可选 `error` 和 `request_id` 后调用 `session$sendCustomMessage()`。

- [ ] **步骤 2：增加统一 failure helper**

failure helper 必须先发送 `status = "failed"`，再按需显示 notification，最后返回 `invisible(FALSE)`。

- [ ] **步骤 3：闭合 Planning 前所有路径**

observer 收到输入后先保存 request id 并发送 `planning`。所有 expected guard 改走 failure helper；dirty save 的 `after(FALSE)` 和 already-saving 返回均发送失败；`prepare_builder_project_crbs()` 用一个边界 `tryCatch` 将 plan-construction exception 转为失败并安全清理已创建 checkpoint。

- [ ] **步骤 4：关联后续进度**

将 `building`、`registering`、`ready` 和所有已有 `failed` 消息统一改走 progress helper，使整个异步生命周期携带同一 request id。

### 任务 3：实现浏览器确认与复位

**文件：**
- 修改：`inst/builder/www/builder.js:111-132,295-507,4122-4147`

- [ ] **步骤 1：增加请求状态**

保存当前 request id 和 acknowledgement timer；提供 clear/start helper，保证任一时刻只有一个 timer。

- [ ] **步骤 2：发送并等待确认**

Prepare action 先验证连接/capability，再设置 CRB ownership、生成 id、启动 timer 并发送。无法发送或 timer 到期时，通过现有 terminal rendering 路径显示失败和 `Done`。

- [ ] **步骤 3：关联进度并处理断连**

progress handler 忽略不匹配 id，匹配消息先清 timer；`planning` 保持 Step 1 文案；disconnect 在活动请求仍等待时显示连接失败终态。关闭对话框清 timer、request id 和 ownership。

### 任务 4：静态复审并提交

**文件：**
- 检查上述四个修改文件和两份设计/计划文档。

- [ ] **步骤 1：记录 green 命令**

```bash
Rscript -e 'testthat::test_local(filter = "builder-(project$|ui-contract)", load_package = "source")'
```

预期：focused tests PASS。本次按用户要求不执行该命令，也不运行 App、lint、formatter、benchmark 或语法验证。

- [ ] **步骤 2：人工静态复审**

逐个核对 Step 1 之前的 return/exception，确认都有 progress 终态；核对 timer 在 progress、close、disconnect 三处清理，且没有修改 Spatial wheel handler。

- [ ] **步骤 3：提交**

```bash
git add docs/superpowers/specs/2026-08-19-builder-crb-planning-terminal-state-design.md \
  docs/superpowers/plans/2026-08-19-builder-crb-planning-terminal-state.md \
  tests/testthat/test-builder-project.R \
  tests/testthat/test-builder-ui-contract.R \
  inst/builder/server/project.R \
  inst/builder/www/builder.js
git commit -m "fix(builder): close CRB planning failure paths"
```

