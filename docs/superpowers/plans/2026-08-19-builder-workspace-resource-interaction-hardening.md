# Builder workspace resource and interaction hardening implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 修复已确认的 Builder 交互状态竞态、主线程阻塞、内存常驻和重复 I/O，同时保留既定的 Spatial sidebar 滚轮设计。

**架构：** 交互状态由显式 generation 和权威状态驱动；项目恢复在单次 transaction 中复用轻量 status；大型数据与图像只在 worker 或有界表示中流动；文件生命周期以 manifest commit 为边界进行 rebase、promotion 和保守清理。

**技术栈：** R/Shiny、JavaScript、callr worker、testthat、现有 Builder browser contract helpers。

---

### 任务 1：交互状态与焦点

**文件：**
- 修改：`inst/builder/www/builder.js`
- 修改：`inst/builder/ui/build_status.R`
- 修改：`inst/builder/server/build.R`
- 修改：`inst/builder/app.R`
- 测试：`tests/testthat/test-builder-ui-contract.R`
- 测试：`tests/testthat/test-builder-stage-review.R`

- [ ] 先添加模态快捷键隔离、dataset error/timeout 回滚、Retry active-flow 优先级、Build overlay 焦点恢复、Spatial section generation 的回归测试。
- [ ] 不执行测试；本轮明确禁止本地验证。
- [ ] 实现最小状态变更，并保持 Spatial sidebar wheel handler 不变。
- [ ] 静态审查事件生命周期、旧 token 拒绝和焦点 fallback。
- [ ] 独立提交交互修复。

### 任务 2：浏览器后台开销

**文件：**
- 修改：`inst/builder/www/builder.js`
- 修改：`inst/builder/www/builder-spatial-canvas.js`
- 修改：`inst/builder/www/stats.js`
- 测试：`tests/testthat/test-builder-ui-contract.R`

- [ ] 先添加计时器按需启停、observer pruning、Shiny value 合并和 canvas hover 单帧执行的合同测试。
- [ ] 不执行测试；本轮明确禁止本地验证。
- [ ] 让 load timer 仅在存在运行项时存活并避免相同文本写入。
- [ ] 清理断开 DOM 的 stage observer targets；将 stats 更新合并为单个 animation frame。
- [ ] 将 Spatial hover 限制到 canvas、按帧合并并把旋转三角函数移出点循环。
- [ ] 独立提交浏览器性能修复。

### 任务 3：项目 restore、source 与 artifact I/O

**文件：**
- 修改：`inst/builder/project.R`
- 修改：`inst/builder/server/project.R`
- 修改：`inst/builder/server/foundation.R`
- 测试：`tests/testthat/test-builder-project.R`

- [ ] 先添加单次 restore status、无 image hydration、source live-path rebase、session-source cleanup、artifact fingerprint reuse、checkpoint promotion 和 conservative GC 测试。
- [ ] 不执行测试；本轮明确禁止本地验证。
- [ ] 在 restore transaction 中创建并传递每 dataset 的轻量 status snapshot。
- [ ] source commit 后更新 live entry，并只删除验证为 owned 的 session source。
- [ ] artifact registration 复用 staged fingerprint；可行时原子 promote，commit 后清 checkpoint。
- [ ] manifest commit 后 mark-and-sweep，仅保留 current、`.bak` 与 active-operation 引用。
- [ ] 独立提交项目 I/O 修复。

### 任务 4：Spatial 内存与 preview cache

**文件：**
- 修改：`inst/builder/preview.R`
- 修改：`inst/builder/spatial_alignment_server.R`
- 修改：`inst/builder/extras.R`
- 修改：`inst/builder/server/imports.R`
- 修改：`inst/builder/server/build.R`
- 测试：`tests/testthat/test-builder-stage-server.R`
- 测试：`tests/testthat/test-builder-project.R`

- [ ] 先添加 split-layer cell-membership、bounded coverage、image pixel budget、bounded retained raster 和 deleted-dataset cache cleanup 测试。
- [ ] 不执行测试；本轮明确禁止本地验证。
- [ ] 用 layer membership API 取 cell names，不获取或 join expression values。
- [ ] 只让抽样点、bounds 与 aggregate coverage 跨 worker 边界。
- [ ] 解码前后执行 image pixel budget，并只保留 bounded editable image。
- [ ] dataset 最终删除和 project replacement 时清理 projection、trajectory、spatial preview caches。
- [ ] 独立提交 Spatial 资源修复。

### 任务 5：最终静态审查

**文件：** 所有上述修改文件。

- [ ] 对照设计逐项检查实现与测试合同，确认没有修改 Spatial wheel 行为。
- [ ] 检查不同任务是否修改同一状态优先级或资源所有权假设并消解冲突。
- [ ] 不运行 App、测试、lint、formatter 或 benchmark。
- [ ] 提交最终整合修正，并在交付中明确列出未执行的验证。
