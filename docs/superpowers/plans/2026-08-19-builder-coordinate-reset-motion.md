# Builder Coordinate Reset Motion 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让 Coordinate settings 的三个滑块在 Reset 时用 320 ms 缓动滑回默认值，同时保持服务端状态立即复位。

**架构：** Reset observer 在调用 `updateSliderInput()` 前发送一次浏览器消息。前端消息处理器给指定 slider 容器加短期 CSS class；CSS 只过渡 ionRangeSlider 的 handle、bar 和 value label，业务数据流不参与插值。

**技术栈：** R/Shiny custom messages、原生 JavaScript、ionRangeSlider DOM、CSS transitions、testthat 静态契约测试。

---

## 文件职责

- `inst/builder/spatial_alignment_server.R`：在有效 Reset 动作中发出一次动画触发消息。
- `inst/builder/www/builder.js`：处理消息、限定三个 slider、重启 class 生命周期。
- `inst/builder/www/builder.components.css`：定义 320 ms 缓动和 reduced-motion 覆盖。
- `tests/testthat/test-builder-loading-ui.R`：验证浏览器消息、作用域和 CSS 契约。
- `tests/testthat/test-builder-spatial.R`：验证 Reset observer 发消息且原有复位语义不变。

### 任务 1：锁定 Reset 动效契约

**文件：**
- 修改：`tests/testthat/test-builder-loading-ui.R`
- 修改：`tests/testthat/test-builder-spatial.R`

- [ ] **步骤 1：编写失败的浏览器契约测试**

读取 `builder.js` 和 `builder.components.css`，断言存在 `builder_coordinate_reset_motion` handler、只允许 `enhance-coordinate_rotation`、`enhance-point_opacity`、`enhance-point_size`，并断言 `.builder-slider-reset-motion` 使用 `320ms cubic-bezier(.22, 1, .36, 1)` 且 reduced-motion 禁用 transition。

- [ ] **步骤 2：编写失败的服务端契约测试**

在现有 points-only Reset `testServer` 场景中检查 `session$lastCustomMessage`，断言消息类型为 `builder_coordinate_reset_motion`，ids 与三个 Coordinate slider 完全一致。

- [ ] **步骤 3：运行测试并确认正确失败**

运行：

```bash
Rscript -e 'devtools::test(filter="builder-(loading-ui|spatial)$", stop_on_failure=TRUE)'
```

预期：FAIL；失败原因是消息处理器、CSS class 和服务端消息尚不存在。

### 任务 2：实现仅限 Reset 的视觉动画

**文件：**
- 修改：`inst/builder/spatial_alignment_server.R`
- 修改：`inst/builder/www/builder.js`
- 修改：`inst/builder/www/builder.components.css`

- [ ] **步骤 1：服务端在 slider 更新前发送消息**

在确认 entry 和 section 有效后调用：

```r
session$sendCustomMessage(
  "builder_coordinate_reset_motion",
  list(ids = c(
    "enhance-coordinate_rotation",
    "enhance-point_opacity",
    "enhance-point_size"
  ))
)
```

- [ ] **步骤 2：实现浏览器 class 生命周期**

增加允许列表和 timer map。handler 对合法 id 的 `.shiny-input-container` 移除 class、强制一次 style flush、重新添加 class，并在约 380 ms 后移除；重复点击先清除旧 timer。

- [ ] **步骤 3：定义 CSS transition**

只在 `.builder-slider-reset-motion` 下为 `.irs-handle` 的 `left`、`.irs-bar` 的 `width/left`、`.irs-single` 的 `left` 添加 `320ms cubic-bezier(.22, 1, .36, 1)`；在 `@media (prefers-reduced-motion: reduce)` 下设置 `transition: none !important`。

- [ ] **步骤 4：运行聚焦测试确认通过**

运行：

```bash
Rscript -e 'devtools::test(filter="builder-(loading-ui|spatial)$", stop_on_failure=TRUE)'
```

预期：0 failed，原有 Reset 数值和持久化断言继续通过。

- [ ] **步骤 5：提交实现**

```bash
git add inst/builder/spatial_alignment_server.R inst/builder/www/builder.js \
  inst/builder/www/builder.components.css tests/testthat/test-builder-loading-ui.R \
  tests/testthat/test-builder-spatial.R
git commit -m "feat(builder): animate coordinate reset sliders"
```

### 任务 3：最终检查与远程验证

**文件：**
- 修改：`docs/superpowers/plans/2026-08-19-builder-coordinate-reset-motion.md`

- [ ] **步骤 1：规格与质量检查**

确认普通 slider 更新没有动画入口，动画不发送中间 Shiny input，非法 ids 被忽略，timer 不泄漏，reduced-motion 生效。

- [ ] **步骤 2：推送分支并触发工作流**

```bash
git push origin feat/builder-project-workspace
gh workflow run test.yml --ref feat/builder-project-workspace
gh workflow run R-CMD-check.yaml --ref feat/builder-project-workspace
gh workflow run pkgdown.yaml --ref feat/builder-project-workspace
```

记录精确 run URL；只有查询到成功状态后才声明远程 CI 通过。
