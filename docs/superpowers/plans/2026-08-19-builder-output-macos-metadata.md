# Builder output macOS metadata implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让 `.DS_Store` 不再阻断 Builder 输出目录选择和发布，同时保持其他外来文件 fail-closed。

**架构：** 在 `publish.R` 定义按 basename 判断 `.DS_Store` 的统一 predicate，并在 release identity 枚举阶段排除它。轻量目录选择预检复用该 predicate；协调器通过过滤后的 identity 自动获得相同语义。

**技术栈：** R、Shiny、testthat

---

### 任务 1：统一忽略 Finder 元数据

**文件：**
- 修改：`inst/builder/publish.R`
- 修改：`inst/builder/server/build.R`
- 测试：`tests/testthat/test-builder-publish.R`
- 测试：`tests/testthat/test-builder-coordinator.R`
- 测试：`tests/testthat/test-builder-worker-app.R`

- [x] **步骤 1：编写失败的测试**

新增测试，要求 release identity 和 coordinator preflight 忽略根目录及嵌套 `.DS_Store`，但仍报告 `.unknown`；静态合同要求目录选择调用统一 predicate。

- [x] **步骤 2：运行测试验证失败**

运行：

```sh
Rscript -e 'testthat::test_local(filter = "builder-publish|builder-coordinator|builder-worker-app", load_package = "source")'
```

预期：新增 `.DS_Store` 合同失败。

- [x] **步骤 3：编写最少实现代码**

在 `publish.R` 增加 `.builder_release_ignorable_metadata()`，release identity 在链接检查和哈希前过滤匹配项；`server/build.R` 的 top-level scan 复用该函数。

- [x] **步骤 4：运行测试确认通过**

运行发布、协调器和目录选择相关测试，并用实际 `reuse` Project 验证 ready CRB → Review → Build → 选择带 `.DS_Store` 的目录。

- [x] **步骤 5：Commit**

```sh
git add inst/builder/publish.R inst/builder/server/build.R \
  tests/testthat/test-builder-publish.R \
  tests/testthat/test-builder-coordinator.R \
  tests/testthat/test-builder-worker-app.R
git commit -m "fix(builder): ignore Finder metadata in outputs"
```
