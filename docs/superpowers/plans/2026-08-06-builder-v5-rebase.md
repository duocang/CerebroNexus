# Builder 5.0 upstream rebase 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 `superpowers:executing-plans` 逐任务实现此计划，并在提交前执行 `superpowers:verification-before-completion`。

**目标：** 将 `feat/cerebro-builder` 从旧的 `114c45a` 基线安全重放到 `upstream/master` 的 CerebroNexus 5.0 基线，完成 versionless Viewer 目录/launcher 迁移，并保留 Builder 的可复现示例矩阵与运行时契约。

**架构：** 先在隔离分支上重放现有 Builder 提交，再用独立适配提交统一 5.0 的 Viewer、示例资源和生成 App 路径。`Cerebro_v1.3` 数据类与 `cerebro_version` 运行时字段继续保留；仅删除当前入口中的版本化 launcher 和内部 Viewer/fixture 路径。

**技术栈：** Git rebase/worktree、R package、testthat/shinytest2、`R CMD check`、pkgdown、Shiny HTTP smoke。

---

### 任务 1：准备保护点与隔离 rebase

**文件：** Git refs；不修改用户未跟踪草稿。

- [x] **步骤 1：创建备份分支**

运行：

```bash
git branch backup/cerebro-builder-pre-v5-rebase-20260806 feat/cerebro-builder
```

预期：备份分支指向 `915f0e8c`。

- [x] **步骤 2：在隔离分支启用可复用冲突记录并执行 rebase**

运行：

```bash
git config rerere.enabled true
git rebase --rebase-merges --onto upstream/master 114c45a integration/builder-v5-rebase
```

冲突原则：接受 upstream 的 5.0 目录和 launcher 结构；Builder 新增行为保留并迁移到新路径。每次停止时记录冲突文件、解决、`git add`、`git rebase --continue`。

- [x] **步骤 3：检查重放完整性**

运行：

```bash
git range-diff 114c45a..backup/cerebro-builder-pre-v5-rebase-20260806 114c45a..integration/builder-v5-rebase
git status --short
```

预期：32 个 Builder 主题提交均有对应重放，没有意外删除 Builder 功能。

- [x] **步骤 4：提交 rebase 结果**

```bash
git commit --allow-empty -m "chore: rebase builder onto CerebroNexus 5.0"
```

只有在 rebase 清洁完成时执行；若 rebase 自身已生成完整提交，则保留一个空提交用于审计标记。

### 任务 2：先写 5.0 版本与路径契约测试

**文件：**
- 修改：`tests/testthat/test-versionless-layout-contract.R`、`tests/testthat/test-builder-app-bundle.R`、`tests/testthat/test-builder-end-to-end.R`
- 修改：`tests/testthat/test-app-version-contract.R`

- [x] **步骤 1：添加失败契约**

新增断言：

```r
expect_equal(as.character(utils::packageVersion("CerebroNexus")), "5.0")
expect_false(any(grepl("shiny/v1\\.4|extdata/v1\\.4|launchCerebroV1\\.", current_sources)))
expect_true(file.exists(system.file("viewer/shiny_UI.R", package = "CerebroNexus")))
expect_true(file.exists(system.file("extdata/examples/pbmc_seurat.rds", package = "CerebroNexus")))
```

运行对应 testthat 文件，预期因当前路径或版本仍未迁移而失败。

- [x] **步骤 2：确认失败原因**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-versionless-layout-contract.R")'
```

预期：失败只来自 5.0 版本/路径契约，不接受加载错误或测试拼写错误。

### 任务 3：完成 5.0 运行时和 Builder 资源迁移

**文件：**
- 修改：`DESCRIPTION`、`NEWS.md`、`R/launchCerebro.R`、`R/createShinyApp.R`、`R/exportFromSeurat.R`
- 修改：`inst/builder/app.R`、`inst/builder/app_bundle.R`、`inst/builder/io.R`
- 修改：Builder 相关测试、vignettes、`_pkgdown.yml`
- 生成：`NAMESPACE`、相关 `man/*.Rd`

- [x] **步骤 1：统一 5.0 版本元数据**

将 `DESCRIPTION` 的 `Version` 设为 `5.0`；在 NEWS 顶部增加未发布的 5.0 开发区，Builder 变更放入该区，不改写历史 4.0/3.x 记录。

- [x] **步骤 2：迁移当前资源路径**

将当前 Builder 的资源引用统一为：

```text
inst/viewer/
inst/extdata/examples/
launchCerebro()
```

保留 `Cerebro_v1.3` 和 `cerebro_version`，不做全局版本号删除。

- [x] **步骤 3：更新 Builder manifest 与生成 App**

同步 app bundle 的 allowlist、checksum、Viewer 根目录、logo 路径和 fixture lookup；确保生成 App 仍能从共享导入管线进入 Core/Enhance/Review。

- [x] **步骤 4：更新 roxygen 与生成文档**

```bash
Rscript -e 'devtools::document()'
```

检查 `NAMESPACE` 和 `man/` diff，只保留由源码注释产生的变更。

- [x] **步骤 5：运行适配测试**

```bash
Rscript -e 'testthat::test_local(stop_on_failure = TRUE)'
```

预期：新增 5.0 契约和 Builder 路径测试通过；若失败，先修复适配代码，不放宽断言。

### 任务 4：Builder 运行时回归与文档审计

**文件：** `tests/testthat/test-builder-end-to-end.R`、`tests/testthat/test-builder-app-bundle.R`、相关 vignettes/README/NEWS。

- [x] **步骤 1：验证完整 Builder 矩阵**

运行现有 9 个隔离生成 App 案例，覆盖 backend × content × output，确认 PBMC、spatial、trekker、immune/HLA、legacy 和 all-content fixture 均能加载。

- [x] **步骤 2：执行版本化 stale-reference 搜索**

```bash
rg -n --hidden --glob '!work/**' --glob '!*.rds' --glob '!*.png' \
  'shiny/v1\.4|extdata/v1\.4|launchCerebroV1\.[0-9]' \
  R inst tests vignettes README.md _pkgdown.yml data-raw
```

预期：只剩明确标注的历史记录或迁移说明；当前运行代码、Builder manifest、测试和文档不得残留旧路径。

- [x] **步骤 3：构建 pkgdown 并检查链接/文章**

```bash
Rscript -e 'pkgdown::build_site()'
```

预期：5.0 launcher、Builder 文档和图片可访问；缺图、旧 API 链接和旧路径提示均需处理或明确记录为既有问题。

### 任务 5：最终质量门禁与集成

**文件：** 无额外目标文件；只在验证通过后更新分支 refs。

- [x] **步骤 1：执行完整项目门禁**

```bash
scripts/precheck.sh
git diff --check upstream/master...integration/builder-v5-rebase
```

同时完成 fresh source build、fresh `R CMD check`、clean temporary library install、`launchCerebro()` HTTP smoke 和 Builder HTTP smoke。

- [x] **步骤 2：核对版本和分支差异**

```bash
Rscript -e 'cat(as.character(utils::packageVersion("CerebroNexus")), "\\n")'
git status --short
git log --oneline --decorate -8
```

预期：版本为 `5.0`；工作区只保留用户已有草稿或明确生成物；无 push。

- [x] **步骤 3：将集成结果移交到 Builder 分支**

验证全部通过后，才把 `feat/cerebro-builder` 快进/重置到集成 tip；保留 `backup/cerebro-builder-pre-v5-rebase-20260806` 作为恢复点，不创建 PR、不推送。
