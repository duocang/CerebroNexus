# Builder Fixture Generation Boundary 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 将所有合成数据制造逻辑移出安装后的 Builder 运行时，同时保持 All content Gallery 和现有能力测试行为不变。

**架构：** All content 构造与写盘由 `data-raw/build_builder_fixtures.R` 独占；immune/spatial 测试工厂进入 test helper；`inst/builder/io.R` 只负责定位、读取和声明已经序列化的资源。

**技术栈：** R、Seurat/SeuratObject、Matrix、png、testthat。

---

## 文件职责

- 修改 `inst/builder/io.R`：删除全部 fixture 构造和写盘函数。
- 修改 `data-raw/build_builder_fixtures.R`：内联 All content、FOV、Trekker、PNG 与序列化实现。
- 创建 `tests/testthat/helper-builder-synthetic-fixtures.R`：承接仅供能力测试使用的 immune/spatial 工厂。
- 修改 `tests/testthat/test-builder-seurat-omnibus.R` 与 `test-builder-end-to-end.R`：通过 catalog 读取 committed RDS。
- 修改 `tests/testthat/test-builder-fixture-script.R`：锁定安装时代码不含生成器。

### 任务 1：锁定运行时边界

- [ ] 在 `test-builder-fixture-script.R` 增加失败测试，断言 `io.R` 不定义 `.builder_fixture_*`、`builder_make_permanent_fixture` 或 `builder_write_permanent_fixtures`。
- [ ] 运行该测试，确认因当前生成器仍在 `io.R` 中而失败。

### 任务 2：迁移制造代码

- [ ] 将 All content、FOV、Trekker、seed 隔离、PNG 和 writer 实现移入 `data-raw/build_builder_fixtures.R`。
- [ ] 将 immune/spatial 测试工厂移入 `helper-builder-synthetic-fixtures.R`。
- [ ] 从 `inst/builder/io.R` 删除整个 fixture 生成区块。
- [ ] 将 omnibus 和 end-to-end 测试改为读取 catalog 的序列化对象。

### 任务 3：重建并验证

- [ ] 运行 `Rscript data-raw/build_builder_fixtures.R`，确认只生成一个 RDS 和五张 PNG。
- [ ] 运行 `Rscript -e 'devtools::test(filter = "builder-(fixture-script|seurat-omnibus|example-registry|end-to-end)$", reporter = "summary")'`。
- [ ] 运行 generated-app fixture 重点测试，确认 test-only factory 可用。
- [ ] 运行 `git diff --check`，检查工作树内容并重启 Builder App。

### 任务 4：精简生成脚本与测试归属

- [ ] 先运行现有 deterministic fixture 测试，保存绿色基线和 committed
  fixture 哈希。
- [ ] 在 `data-raw/build_builder_fixtures.R` 删除 caller RNG 保存/恢复、无效的
  image seed 和 one-use writer，改为单一对象 seed 的直接执行流。
- [ ] 将确定性生成测试从 `test-builder-end-to-end.R` 移到
  `test-builder-fixture-script.R`，比较两次生成结果、六文件清单和 committed
  fixture 的逐字节内容。
- [ ] 将 runtime 边界测试改为隔离 source `io.R` 后检查实际符号，不再用源码
  正则表达式推断。
- [ ] 运行 fixture-script、omnibus、end-to-end 和 generated-app fixture 测试，
  确认行为与生成字节不变后提交。
