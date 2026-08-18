# Builder 冷启动优化实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让 Builder 首屏不再等待 Worker 完整初始化，并通过最小 bootstrap 与按需能力加载减少真实冷启动时间和内存。

**架构：** `builder_worker_start()` 只创建 `callr::r_session`、异步提交 bootstrap 并返回 `starting` 句柄；`builder_worker_poll_startup()` 驱动 `starting -> ready/failed`。子进程由独立 bootstrap 加载基础运行时，并用幂等 capability registry 在首次相关请求前加载 Spatial、Immune、Analysis 或 Build 文件。

**技术栈：** R、Shiny、callr `r_session`、later、testthat。

---

## 文件结构

- 创建 `inst/builder/worker_bootstrap.R`：源码运行时加载、基础 Worker 初始化和 capability registry。
- 修改 `inst/builder/worker.R`：非阻塞启动句柄、启动轮询、停止/重启对 starting 状态的支持。
- 修改 `inst/builder/session.R`：对外暴露会话启动轮询，并在请求派发前声明能力。
- 修改 `inst/builder/server/foundation.R`：轮询启动状态，ready 后安装协议并释放队列。
- 修改 `inst/builder/app.R`：主进程 source 新 bootstrap 辅助模块。
- 修改 `tests/testthat/test-builder-worker.R`：非阻塞启动和 capability 幂等测试。
- 修改 `tests/testthat/test-builder-async-import.R`：启动期间排队、ready 后派发测试。
- 修改 `tests/testthat/test-builder-loading-ui.R`：真实首次 flush 生命周期测试，替代纯字符串保证。

### 任务 1：建立非阻塞启动 contract

**文件：** 修改 `tests/testthat/test-builder-worker.R`、`inst/builder/worker.R`、`inst/builder/session.R`。

- [ ] **步骤 1：编写失败的 Worker 启动测试**

增加可控 `.bootstrap` 替身：在子进程等待 gate 文件。断言 `builder_worker_start()` 在 gate 打开前返回、`worker$state == "starting"`，并且启动轮询在 gate 打开后返回 `ready`。

```r
gate <- tempfile("builder-start-gate-")
elapsed <- system.time({
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    .bootstrap = function(...) {
      while (!file.exists(gate)) Sys.sleep(0.01)
      character()
    }
  )
})[["elapsed"]]
expect_lt(elapsed, 0.5)
expect_identical(worker$state, "starting")
file.create(gate)
ready <- .builder_wait_for_startup(worker)
expect_identical(ready$state, "ready")
```

- [ ] **步骤 2：运行测试确认失败**

```bash
Rscript -e 'devtools::test(filter="builder-worker", stop_on_failure=TRUE)'
```

预期：旧 `process$run()` 等待 gate，测试 FAIL。

- [ ] **步骤 3：实现异步 bootstrap 提交**

将同步 `process$run()` 改为 `process$call()`。句柄记录 `state = "starting"`、`ready = FALSE`、启动时间和超时。实现 `builder_worker_poll_startup(worker)`：无结果保持 starting；成功设置 ready 和 restored；错误或超时设置 failed 并清理进程。

- [ ] **步骤 4：增加 session 启动轮询接口**

```r
builder_session_poll_startup <- function(worker) {
  builder_worker_poll_startup(worker)
}
```

- [ ] **步骤 5：运行聚焦测试并提交**

预期 Worker 测试 PASS。

```bash
git add inst/builder/worker.R inst/builder/session.R tests/testthat/test-builder-worker.R
git commit -m "perf(builder): make worker bootstrap non-blocking"
```

### 任务 2：让 Shiny 生命周期消费 starting Worker

**文件：** 修改 `tests/testthat/test-builder-loading-ui.R`、`tests/testthat/test-builder-async-import.R`、`inst/builder/server/foundation.R`。

- [ ] **步骤 1：编写失败的 testServer 生命周期测试**

假的 `builder_session_start()` 返回 starting Worker，假的启动轮询先返回 starting、再返回 ready。断言首次 flush 已结束且 `worker_available()` 为假，ready 后才创建 request protocol。

- [ ] **步骤 2：编写启动期间请求排队测试**

Worker starting 时提交 load command；断言 queue 保留命令，ready 后第一次 dispatch 才调用 `builder_session_load()`，请求序列号不变。

验收还必须覆盖：protocol 在 starting 阶段已经拥有稳定 epoch；多个请求按 seq 排序释放；启动失败时 pending/queue 转为带明确原因的终态或进入现有可重试恢复路径，不能清空后静默丢弃。

- [ ] **步骤 3：运行测试确认失败**

```bash
Rscript -e 'devtools::test(filter="builder-(loading-ui|async-import)", stop_on_failure=TRUE)'
```

预期：当前服务端立即把 starting Worker 标为可用，测试 FAIL。

- [ ] **步骤 4：实现服务端启动轮询**

`start_builder_worker()` 保存 starting 句柄但不设置 available。`poll_builder_worker_startup()` 用短 `later::later()` 周期轮询；只有 ready 才安装 protocol 并发送 ready 消息，failed 时发送错误并停止轮询。

- [ ] **步骤 5：支持 session 提前结束并验证**

cleanup 对 starting 句柄同样停止子进程；later 回调先检查 session 是否关闭。运行步骤 3 命令，预期 PASS。

测试必须持有一个被 gate 阻塞的真实 callr starting 进程，在 session cleanup 后断言父进程及已发现的子孙进程均已退出，避免断连泄漏半启动 Worker。

- [ ] **步骤 6：提交**

```bash
git add inst/builder/server/foundation.R tests/testthat/test-builder-loading-ui.R tests/testthat/test-builder-async-import.R
git commit -m "perf(builder): keep startup off the Shiny event loop"
```

### 任务 3：建立最小 bootstrap 并移除 pkgload

**文件：** 创建 `inst/builder/worker_bootstrap.R`；修改 `inst/builder/app.R`、`inst/builder/worker.R`、`tests/testthat/test-builder-worker.R`。

- [ ] **步骤 1：编写失败的 bootstrap contract 测试**

断言源码模式 bootstrap 不调用 `pkgload::load_all()`；基础初始化后存在对象、快照和 capability registry，但 Spatial、Immune、Analysis、Build 均未加载。

- [ ] **步骤 2：运行 Worker 测试确认失败**

预期旧 bootstrap 仍调用 pkgload 并一次 source 全部模块。

- [ ] **步骤 3：实现源码运行时加载器**

`builder_worker_source_package_runtime(package_source, envir)` 按 `DESCRIPTION` 的 `Collate` 顺序加载 `R/` 文件；无 Collate 时按文件名稳定排序。不得调用 pkgload 或执行开发钩子。

- [ ] **步骤 4：实现基础 bootstrap**

`builder_worker_bootstrap(dir, root, registry, package_source)` 只加载包运行时和 core 文件，初始化对象、快照、根目录及 capability registry，返回恢复的数据集名称。

bootstrap 返回 ready 前执行冒烟自检：逐项断言请求协议、对象/快照注册表、core loader 和错误包装函数存在且类型正确。任何缺失均作为 bootstrap failed 返回，禁止生成不完整的 ready Worker。

- [ ] **步骤 5：替换 worker.R 中内联 source 长清单**

```r
source(file.path(dir, "worker_bootstrap.R"), local = globalenv())
builder_worker_bootstrap(dir, root, registry, package_source)
```

- [ ] **步骤 6：运行 Worker 测试并提交**

```bash
Rscript -e 'devtools::test(filter="builder-worker", stop_on_failure=TRUE)'
git add inst/builder/worker_bootstrap.R inst/builder/app.R inst/builder/worker.R tests/testthat/test-builder-worker.R
git commit -m "perf(builder): bootstrap workers without pkgload"
```

### 任务 4：按请求加载 Worker 能力

**文件：** 修改 `inst/builder/worker_bootstrap.R`、`inst/builder/session.R`、`tests/testthat/test-builder-worker.R`。

- [ ] **步骤 1：编写失败的 capability 测试**

测试基础导入不加载 spatial、immune、analysis、build；两个 Spatial 请求只执行一次 loader；失败 loader 不写 ready 标记。

- [ ] **步骤 2：实现幂等 capability registry**

`.builder_worker_capabilities` 记录 unloaded/loading/ready/failed。`builder_worker_ensure_capability(name)` 先加载依赖，再按固定清单 source；全部成功后才写 ready，失败保留阶段错误。

同一能力处于 loading 时不得再次执行 loader；后续请求复用同一初始化结果。failed 状态默认稳定报出首次失败原因，只有显式 reset/restart 才允许重试，避免自动重试死循环或假 ready。

- [ ] **步骤 3：为 session 请求声明能力**

load/example/drop 使用 core；coords/spatial preview/section bounds 使用 spatial；projection/trajectory 使用 analysis；build 使用 build。Immune 文件仅在数据 profile 或 build 确认需要时加载。

- [ ] **步骤 4：运行 Worker 与 Spatial 聚焦测试**

```bash
Rscript -e 'devtools::test(filter="builder-(worker|spatial-live-preview|spatial)$", stop_on_failure=TRUE)'
```

预期：PASS。

- [ ] **步骤 5：提交**

```bash
git add inst/builder/worker_bootstrap.R inst/builder/session.R tests/testthat/test-builder-worker.R
git commit -m "perf(builder): lazy-load worker capabilities"
```

### 任务 5：兼容性与一次性验证

**文件：** 仅修改本计划引入回退所对应的文件。

- [ ] **步骤 1：运行启动与导入测试集合**

```bash
Rscript -e 'devtools::test(filter="builder-(worker|async-import|loading-ui|import-queue)$", stop_on_failure=TRUE)'
```

- [ ] **步骤 2：运行 Spatial 与 Open Project 测试**

```bash
Rscript -e 'devtools::test(filter="builder-(project|spatial-live-preview|spatial)$", stop_on_failure=TRUE)'
```

- [ ] **步骤 3：执行一次冷启动测量**

记录启动 API 返回耗时、ready 耗时和空白 Worker 已加载能力。断言 API 返回早于 ready，且空白 Worker 只有 core ready；不把机器相关绝对秒数作为测试阈值。

- [ ] **步骤 4：检查差异并提交最终修正**

```bash
git diff --check
git status --short
```

只提交本计划涉及的文件，不暂存用户原有改动。
