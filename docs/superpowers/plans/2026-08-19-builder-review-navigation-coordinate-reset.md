# Builder review navigation and coordinate reset implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 自动进入下一个待检查数据集时回到新工作区顶部，并使 Coordinate settings Reset 完整恢复当前 section 的旋转与点样式。

**架构：** Review 服务端在成功切换数据集后发送一个窄范围客户端消息；客户端等待新 workbench 标题存在后执行滚动与焦点。现有 coordinate-reset observer 继续作为唯一重置入口，使用 `builder_alignment_defaults()` 同步 rotation、point opacity、point size、draft 和 canvas token。

**技术栈：** R/Shiny、原生 JavaScript、testthat、shinytest2。

---

## 文件结构

- 修改 `inst/builder/server/review.R`：只在自动切换成功后请求顶部导航。
- 修改 `inst/builder/www/builder.js`：处理顶部滚动和新 workbench 标题焦点。
- 修改 `inst/builder/spatial_alignment_server.R`：扩展现有 Coordinate Reset 权威。
- 修改 `tests/testthat/test-builder-stage-server.R`：覆盖自动前进消息的服务端契约。
- 修改 `tests/testthat/test-builder-loading-ui.R`：覆盖客户端消息处理契约。
- 修改 `tests/testthat/test-builder-spatial.R`：覆盖完整坐标设置重置。

### 任务 1：自动前进后的顶部导航

**文件：**
- 修改：`tests/testthat/test-builder-stage-server.R`
- 修改：`tests/testthat/test-builder-loading-ui.R`
- 修改：`inst/builder/server/review.R`
- 修改：`inst/builder/www/builder.js`

- [ ] **步骤 1：编写失败测试**

在服务端源码契约中要求成功的 `request_dataset_switch()` 回调发送
`builder_focus_dataset_start`，载荷包含目标 dataset ID；在客户端源码契约中要求注册同名消息，并验证处理器调用顶部滚动和标题聚焦。

- [ ] **步骤 2：确认测试失败**

运行：

```bash
Rscript -e 'devtools::test(filter="builder-(stage-server|loading-ui)$", stop_on_failure=TRUE)'
```

预期：FAIL，缺少 `builder_focus_dataset_start`。

- [ ] **步骤 3：实现最小行为**

在自动切换回调中加入：

```r
session$sendCustomMessage(
  "builder_focus_dataset_start",
  list(dataset = target)
)
```

客户端处理器确认当前 ready row 与消息 dataset 一致，等待动态内容增强帧后执行：

```js
window.scrollTo({ top: 0, behavior: reducedMotion.matches ? "auto" : "smooth" });
heading.setAttribute("tabindex", "-1");
heading.focus({ preventScroll: true });
```

- [ ] **步骤 4：确认测试通过**

重新运行任务 1 命令，预期 0 FAIL。

### 任务 2：完整重置 Coordinate settings

**文件：**
- 修改：`tests/testthat/test-builder-spatial.R`
- 修改：`inst/builder/spatial_alignment_server.R`

- [ ] **步骤 1：编写失败测试**

构造当前 section 已设置非默认 rotation、point opacity、point size 的
`testServer()` 场景，点击 `enhance-reset_coordinate_transform` 后断言：

```r
expect_identical(input$`enhance-coordinate_rotation`, 0)
expect_identical(input$`enhance-point_opacity`, defaults$point_opacity * 100)
expect_identical(input$`enhance-point_size`, defaults$point_size)
```

并断言保存的当前-section point appearance、coordinate draft 和 canvas reset token 使用默认值，而图片 alignment 的 dx/dy/scale/rotation/flip 保持不变。

- [ ] **步骤 2：确认测试失败**

运行：

```bash
Rscript -e 'devtools::test(filter="builder-spatial$", stop_on_failure=TRUE)'
```

预期：FAIL，opacity/size 仍为修改值或持久状态未更新。

- [ ] **步骤 3：实现最小行为**

读取一次 `defaults <- builder_alignment_defaults()`，强制保存默认 coordinate draft，冻结并更新三个输入；同步无图片 draft 时的 `spatial_point_appearance`，有图片 draft 时让现有 alignment control observer 持久化点样式；最后只递增现有 canvas reset token，不调用 image alignment reset。

- [ ] **步骤 4：确认测试通过**

重新运行任务 2 命令，预期 0 FAIL。

### 任务 3：综合验证与交付

**文件：**
- 验证本计划涉及的全部文件。

- [ ] **步骤 1：格式与静态检查**

```bash
air format inst/builder/server/review.R inst/builder/spatial_alignment_server.R tests/testthat/test-builder-stage-server.R tests/testthat/test-builder-loading-ui.R tests/testthat/test-builder-spatial.R
git diff --check
```

- [ ] **步骤 2：运行相关回归**

```bash
Rscript -e 'devtools::test(filter="builder-(stage-server|loading-ui|spatial|worker-app)$", stop_on_failure=TRUE)'
```

预期：0 FAIL、0 WARN。

- [ ] **步骤 3：提交并推送**

```bash
git add inst/builder/server/review.R inst/builder/www/builder.js \
  inst/builder/spatial_alignment_server.R \
  tests/testthat/test-builder-stage-server.R \
  tests/testthat/test-builder-loading-ui.R tests/testthat/test-builder-spatial.R
git commit -m "fix(builder): reset review navigation and coordinates"
git push origin feat/builder-project-workspace
```

- [ ] **步骤 4：启动远程 CI**

对 `feat/builder-project-workspace` 手动触发 R tests、R-CMD-check、pkgdown，并记录运行链接。
