# Builder 非空项目目录确认实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 在首次创建 Builder project 时识别非空目录，并在任何写入发生前要求用户明确确认。

**架构：** 核心层提供无副作用的目录分类函数；UI 层提供专用确认弹窗；Server 层保存一次性的待确认路径，并让首次选择、换目录、确认创建复用同一条创建路径。确认时重新分类目录，防止选择后出现 Builder manifest 的竞态。

**技术栈：** R、Shiny、htmltools、testthat

---

## 文件结构

- 修改 `inst/builder/project.R`：新增纯函数 `builder_project_folder_state()`，统一分类空目录、已有 Builder project、普通非空目录。
- 修改 `inst/builder/ui/project.R`：新增非空目录确认弹窗。
- 修改 `inst/builder/server/project.R`：管理待确认路径、选择与确认流程，以及确认前二次检查。
- 修改 `tests/testthat/test-builder-project.R`：验证目录分类、UI 和 Server 契约；避免碰触工作区中已有的 UI-contract 测试改动。

### 任务 1：目录分类契约

**文件：**
- 修改：`tests/testthat/test-builder-project.R`
- 修改：`inst/builder/project.R`

- [ ] **步骤 1：编写失败的目录分类测试**

```r
test_that("project folders distinguish empty, existing, and unrelated content", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()

  expect_identical(runtime$builder_project_folder_state(root)$kind, "empty")
  writeLines("keep", file.path(root, ".keep"))
  expect_identical(runtime$builder_project_folder_state(root)$kind, "nonempty")
  writeLines("{}", runtime$builder_project_manifest_path(root))
  expect_identical(runtime$builder_project_folder_state(root)$kind, "project")
})
```

- [ ] **步骤 2：运行测试并确认因函数不存在而失败**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-project.R")'`

预期：新增测试 FAIL，提示找不到 `builder_project_folder_state`。

- [ ] **步骤 3：实现最小目录分类函数**

```r
builder_project_folder_state <- function(root) {
  root <- builder_project_normalize_root(root)
  if (file.exists(builder_project_manifest_path(root))) {
    return(list(kind = "project", root = root))
  }
  entries <- list.files(root, all.files = TRUE, no.. = TRUE)
  list(kind = if (length(entries)) "nonempty" else "empty", root = root)
}
```

- [ ] **步骤 4：运行测试确认分类通过**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-project.R")'`

预期：新增目录分类断言通过。

### 任务 2：确认弹窗与 Server 流程

**文件：**
- 修改：`tests/testthat/test-builder-project.R`
- 修改：`inst/builder/ui/project.R`
- 修改：`inst/builder/server/project.R`

- [ ] **步骤 1：编写失败的 UI 与 Server 契约测试**

```r
test_that("non-empty project folders require explicit confirmation", {
  runtime <- new.env(parent = globalenv())
  sys.source(testthat::test_path("..", "..", "inst", "builder", "ui", "project.R"), envir = runtime)
  html <- htmltools::renderTags(
    runtime$builder_project_nonempty_folder_dialog("/tmp/existing-files")
  )$html
  expect_match(html, "Folder already contains files", fixed = TRUE)
  expect_match(html, 'id="choose_another_builder_project_folder"', fixed = TRUE)
  expect_match(html, 'id="confirm_builder_project_folder"', fixed = TRUE)
})
```

在 `test-builder-project.R` 中读取 `server/project.R` 源码并截取 `choose_builder_project_folder` observer，断言普通非空目录只设置 `builder_project_pending_folder()` 并显示确认弹窗；截取确认 observer，断言调用创建帮助函数前再次执行 `builder_project_folder_state(path)`。

- [ ] **步骤 2：运行测试并确认缺少弹窗和确认 observer**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-project.R")'`

预期：新增契约因 UI 函数、pending state 和确认 handler 不存在而失败。

- [ ] **步骤 3：实现确认弹窗**

```r
builder_project_nonempty_folder_dialog <- function(path) {
  modalDialog(
    title = "Folder already contains files",
    builder_project_dialog_content(
      "Create the Builder project in this folder?",
      paste0("Existing files in ", path, " will be kept."),
      "triangle-exclamation"
    ),
    footer = tagList(
      actionButton("cancel_builder_project_folder", "Cancel", class = "btn btn-outline-secondary"),
      actionButton("choose_another_builder_project_folder", "Choose another folder", class = "btn btn-outline-secondary"),
      actionButton("confirm_builder_project_folder", "Create project here", class = "btn btn-primary")
    ),
    easyClose = FALSE,
    size = "m"
  )
}
```

- [ ] **步骤 4：实现一次性待确认状态和复用创建路径**

在 `server/project.R` 增加：

```r
builder_project_pending_folder <- reactiveVal(NULL)
```

抽取 `create_builder_project_in_folder(path)`，只在目录分类不是 `project` 时创建 manifest 和调用现有保存流程。抽取 `request_builder_project_folder()` 运行 picker 并分类：`empty` 直接创建，`project` 显示 Open project 警告，`nonempty` 仅保存 pending path 并显示确认弹窗。

确认 observer 必须重新调用 `builder_project_folder_state(path)`；若此时变成 `project` 或目录不可用，则清除 pending、停止创建并显示错误。Cancel、Choose another、成功创建和 `session$onSessionEnded()` 都清除 pending。

- [ ] **步骤 5：运行聚焦测试**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-project.R")'`

预期：新增目录分类、弹窗和 Server 契约通过；记录任何与本功能无关的既有失败。

### 任务 3：质量检查与提交

**文件：**
- 检查上述所有修改文件。

- [ ] **步骤 1：运行格式与差异检查**

运行：`git diff --check`，并人工确认没有修改当前工作区中 `inst/builder/server/build.R`、既有 `builder.js` 工作和其既有测试差异。

- [ ] **步骤 2：运行项目聚焦测试**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-builder-project.R"); testthat::test_file("tests/testthat/test-builder-stage-server.R")'`

预期：本次相关断言全部通过。

- [ ] **步骤 3：只提交本功能文件**

```bash
git add inst/builder/project.R inst/builder/ui/project.R inst/builder/server/project.R tests/testthat/test-builder-project.R
git commit -m "feat(builder): warn before creating in non-empty folder"
```

- [ ] **步骤 4：推送并触发远程 CI**

运行：`git push origin feat/builder-project-workspace`，随后对 `duocang/CerebroNexus` 的 R-CMD-check、R tests、pkgdown 执行 workflow dispatch。
