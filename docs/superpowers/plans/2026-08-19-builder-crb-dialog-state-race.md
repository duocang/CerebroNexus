# Builder CRB Dialog State Race 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 防止 Prepare checked CRBs 期间弹窗缩窄或被迟到的源文件保存消息覆盖。

**架构：** 在现有 `builder.js` 对话框控制器中增加一个局部 CRB 弹窗所有权标记。它从 Prepare 点击持续到用户关闭终态弹窗；期间保留 `is-result` 的稳定几何，并拒绝迟到的 save-result 和 source-progress 改写。

**技术栈：** Shiny 自定义消息、原生 JavaScript、testthat 静态 UI 契约。

---

## 文件结构

- 修改 `inst/builder/www/builder.js`：管理 CRB 弹窗活动状态和消息所有权。
- 修改 `tests/testthat/test-builder-ui-contract.R`：锁定稳定布局、消息隔离和状态清理契约。

### 任务 1：用契约测试复现状态竞争

**文件：**
- 测试：`tests/testthat/test-builder-ui-contract.R`

- [ ] **步骤 1：编写失败测试**

在 `project CRB preparation stays in one progress dialog` 测试中加入：

```r
expect_match(js, "var builderProjectCrbDialogActive = false;", fixed = TRUE)
expect_match(js, "if (builderProjectCrbDialogActive) return;", fixed = TRUE)
expect_match(js, "builderProjectCrbDialogActive = true;", fixed = TRUE)
expect_match(js, "builderProjectCrbDialogActive = false;", fixed = TRUE)

prepare_action <- substr(
  js,
  regexpr('label: "Prepare checked CRBs"', js, fixed = TRUE)[[1L]],
  regexpr('send("prepare_builder_project_crbs"', js, fixed = TRUE)[[1L]]
)
expect_false(grepl('"is-result"', prepare_action, fixed = TRUE))
```

- [ ] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R", reporter = "summary")'
```

预期：FAIL，因为 CRB 活动标记和 source-progress 守卫尚不存在，且 prepare action 仍删除 `is-result`。

### 任务 2：实现最小状态所有权修复

**文件：**
- 修改：`inst/builder/www/builder.js:106-480`

- [ ] **步骤 1：增加 CRB 活动状态**

在项目保存弹窗状态旁增加：

```js
var builderProjectCrbPreparing = false;
```

- [ ] **步骤 2：稳定 Prepare 转换**

点击 Prepare 时先设置活动状态，并只移除终态颜色与 actions class，保留 `is-result`：

```js
builderProjectCrbDialogActive = true;
elements.card.classList.remove("is-success", "is-error", "has-actions");
```

- [ ] **步骤 3：隔离迟到的保存消息**

在 `updateBuilderProjectSourceProgress()` 的入口加入：

```js
if (
  !builderProjectSaveResultOpen ||
  !builderProjectSaveResult ||
  builderProjectCrbDialogActive
) return;
```

在 `showBuilderProjectSaveResult()` 的入口也加入同一所有权守卫：

```js
if (builderProjectCrbDialogActive) return;
```

- [ ] **步骤 4：清理活动状态**

只在 `closeBuilderProjectSaveResult()` 中设置：

```js
builderProjectCrbDialogActive = false;
```

- [ ] **步骤 5：运行 focused 测试验证通过**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-builder-ui-contract.R", reporter = "summary")'
```

预期：PASS。

- [ ] **步骤 6：检查并提交**

运行：

```bash
git diff --check
git add -p inst/builder/www/builder.js tests/testthat/test-builder-ui-contract.R
git commit -m "fix(builder): keep CRB dialog progress authoritative"
```

暂存时只选择本计划的 hunk，不包含两个文件中用户原有的其它未提交修改。
