# Builder Storage and Metadata Parity 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让 Builder 明确控制表达矩阵与空间图片的独立存储方式，完整保留受支持的普通 metadata，并让一个空间 section 管理多张命名图片，使 Builder 能复现 `anna_lena` 命令行导出的核心内容。

**架构：** 保留现有 `exportFromSeurat()` 的 Embedded/HDF5/BPCells 表达后端；为 Builder frozen plan 增加独立的 `spatial_image_storage`。Metadata policy 拆成 retention 与 Group 两层；空间图片状态改为 `section -> label -> alignment record`，App 模式下外置图片通过现有 `createShinyApp(spatial_images=, spatial_image_settings=)` 合约进入 `spatial-assets/`。所有新决策必须在纯 state、frozen plan、App request、Review 和 build report 中一致。

**技术栈：** R、Shiny、testthat、JavaScript、Cerebro R6/CRB、HDF5Array、BPCells、现有 Builder worker/staging/App bundle 合约。

---

## 执行前边界

当前 `/Users/nuioi/projects/shiny/_wt_colleague_spatial_builder` 有大量用户未提交修改，且与本计划会修改的文件重叠。执行者不得把这些修改混入本功能提交，也不得 reset、checkout 或清理它们。

在新会话中先执行：

```bash
cd /Users/nuioi/projects/shiny/_wt_colleague_spatial_builder
git status --short --branch
git log -3 --oneline
```

预期：能看到规格 commit `9bfbb20a` 和计划 commit `6a7a8c6e`，同时看到现有 dirty files。

然后从已提交的规格 commit 建立隔离 worktree：

```bash
git worktree add \
  /Users/nuioi/projects/shiny/_wt_builder_storage_metadata \
  -b feat/builder-storage-metadata-parity \
  integration/colleague-spatial-builder
cd /Users/nuioi/projects/shiny/_wt_builder_storage_metadata
git status --short --branch
```

预期：新 worktree 为 clean；原 worktree 的未提交修改保持原样。最终不要直接强行合并，先把本分支 rebase/merge 到包含原 dirty 工作成果的正式 commit，再解决重叠。

## 文件结构

### Metadata retention

- 修改：`inst/builder/recommend.R` — 分别计算 metadata retention 与 Group recommendation。
- 修改：`inst/builder/state/metadata.R` — 验证 retention、Group、forced、sensitive 不变量和旧 policy 升级。
- 修改：`inst/builder/state/core.R` — 校验设置里的 retained metadata 与 included Groups。
- 修改：`inst/builder/plan/defaults.R` — 新导入使用 retain-all-supported 默认值。
- 修改：`inst/builder/plan/assets.R` — 冻结 retained metadata 和依赖字段。
- 修改：`inst/builder/plan/freeze.R` — 把最终 retention policy 写入 BuildPlan。
- 修改：`inst/builder/server/datasets.R` — 处理 Keep metadata 与 Group 两套 UI action。
- 修改：`inst/builder/ui/core_stage/groups.R` — Metadata/Groups 双控制 UI。
- 修改：`inst/builder/www/builder.js` — 汇总 metadata retention 和 Group 事件。
- 修改：`inst/builder/www/builder.features.css` — Metadata retention 控件布局与状态。
- 修改：`inst/builder/build.R` — 严格按 frozen retained columns 裁剪源对象。
- 修改：`inst/builder/ui/review_stage.R` — 正确显示 retained/excluded 与原因。
- 修改：`inst/builder/report.R` — 输出 typed metadata report。

### Spatial image storage and multi-image state

- 修改：`inst/builder/extras.R` — image collection、data-URI materialization、embedded/external payload helpers。
- 修改：`inst/builder/plan/defaults.R` — 新项目 external、旧项目 embedded 的升级默认值。
- 修改：`inst/builder/plan/preflight.R` — 校验 storage mode、集合内 label、saved alignment 和 CRB-only 限制。
- 修改：`inst/builder/plan/freeze.R` — 冻结 `section -> label -> record` 和 storage mode。
- 修改：`inst/builder/spatial_alignment_server.R` — active section + active image，新增/改名/删除/切换多图。
- 修改：`inst/builder/ui/enhance_stage.R` — Image storage selector 与 image stack editor。
- 修改：`inst/builder/server/imports.R` — 保存 nested image collection。
- 修改：`inst/builder/server/enhancements.R` — 连接新 spatial server 参数与 storage setting。
- 修改：`inst/builder/build.R` — embedded CRB attachment、external manifest staging 和验证。
- 修改：`inst/builder/app_bundle/request.R` — 把 verified image manifest/settings 送入 App request。
- 修改：`inst/builder/app_bundle/contract.R` — 验证 inert manifest、source identity 和字段集合。
- 修改：`inst/builder/app_bundle/build.R` — 调用 `createShinyApp()` 时传递空间图片参数。
- 修改：`inst/builder/app_bundle/topology.R` — 报告并验证 `spatial-assets/` 成员。
- 修改：`R/spatial_image_manifest.R` — 允许 per-image `image_opacity`，保持严格白名单。
- 修改：`R/createShinyApp.R` — 文档化扩展后的 setting。
- 修改：`inst/viewer/coordinated_views/bundle.R` — external image 读取 `image_opacity`。
- 修改：`inst/builder/report.R`、`inst/builder/ui/review_stage.R` — section/image 数量和 storage mode。

### Tests

- 修改：`tests/testthat/test-builder-recommend.R`
- 修改：`tests/testthat/test-builder-plan-content.R`
- 修改：`tests/testthat/test-builder-build.R`
- 修改：`tests/testthat/test-builder-stage-inspect-core.R`
- 修改：`tests/testthat/test-builder-browser.R`
- 修改：`tests/testthat/test-builder-spatial.R`
- 修改：`tests/testthat/test-builder-plan-readiness.R`
- 修改：`tests/testthat/test-builder-app-bundle.R`
- 修改：`tests/testthat/test-builder-end-to-end.R`
- 修改：`tests/testthat/test-builder-viewer-review.R`
- 修改：`tests/testthat/test-builder-report.R`
- 修改：`tests/testthat/test-createShinyApp-sibling.R`
- 修改：`tests/testthat/test-multisection-spatial.R`
- 修改：`tests/testthat/test-generated-app-pages-spatial.R`
- 创建：`tests/testthat/test-builder-storage-metadata-parity.R` — 跨层 synthetic acceptance contract。

---

### 任务 1：锁定当前行为并建立新 metadata contract

**文件：**
- 修改：`tests/testthat/test-builder-recommend.R`
- 修改：`inst/builder/recommend.R`

- [ ] **步骤 1：为 constant、continuous、high-cardinality、unsupported、sensitive metadata 写失败测试**

在现有 `metadata recommendations use one deterministic value contract` 附近增加断言：

```r
expect_true(first$columns$constant$retain_in_crb)
expect_false(first$columns$constant$group_eligible)
expect_true(first$columns$continuous$retain_in_crb)
expect_false(first$columns$continuous$group_eligible)
expect_true(first$columns$unique_cell$retain_in_crb)
expect_false(first$columns$unique_cell$group_eligible)
expect_false(first$columns$unsupported$retain_in_crb)
expect_false(first$columns$patientName$retain_in_crb)
expect_true(first$columns$patientName$requires_confirmation)
expect_contains(first$retained, c("constant", "continuous", "unique_cell"))
expect_false("unsupported" %in% first$retained)
```

另加一个明确的 `orig.ident` 测试：

```r
test_that("constant orig.ident is retained but not recommended as a Group", {
  profile <- recommendation_profile(
    n_cells = 100L,
    columns = list(
      orig.ident = metadata_fact(
        "orig.ident",
        class = "factor",
        non_missing = 100L,
        unique_non_missing = 1L
      )
    )
  )
  policy <- builder_recommend_metadata(profile)

  expect_true(policy$columns$orig.ident$retain_in_crb)
  expect_false(policy$columns$orig.ident$group_eligible)
  expect_contains(policy$retained, "orig.ident")
  expect_false("orig.ident" %in% policy$group_candidates)
})
```

- [ ] **步骤 2：运行测试并确认失败原因是新字段尚不存在**

```bash
Rscript -e 'devtools::test(filter = "builder-recommend", stop_on_failure = TRUE)'
```

预期：FAIL，缺少 `retain_in_crb`、`retained` 或 `group_candidates`；不得出现 parse error。

- [ ] **步骤 3：最小修改 recommendation record**

在 `.builder_recommend_metadata_fact()` 中保留现有 `disposition` 作为兼容证据，新增明确字段：

```r
retain_in_crb <- isTRUE(fact$supported) && !sensitive
group_eligible <- FALSE
group_recommended <- FALSE
```

规则按以下顺序实现：

```r
if (!isTRUE(fact$supported)) {
  retain_in_crb <- FALSE
} else if (sensitive) {
  retain_in_crb <- is_required
} else {
  retain_in_crb <- TRUE
}

group_eligible <- isTRUE(fact$supported) &&
  !sensitive && safe_type && low_cardinality
group_recommended <- group_eligible
```

数字型字段继续只有显式 category name 才可 Group。将以下字段写进每列 record：

```r
retain_in_crb = retain_in_crb,
group_eligible = group_eligible,
group_recommended = group_recommended,
forced = is_required
```

顶层 policy 新增：

```r
retained = names(records)[vapply(records, `[[`, FALSE, "retain_in_crb")],
group_candidates = names(records)[vapply(records, `[[`, FALSE, "group_eligible")],
forced = names(records)[vapply(records, `[[`, FALSE, "forced")]
```

现有 `included` 暂时继续等于 retained，供旧调用者过渡；`excluded` 必须改为所有 `retain_in_crb = FALSE` 的列，不能继续表达“不适合当 Group”。

- [ ] **步骤 4：运行 recommendation tests**

```bash
Rscript -e 'devtools::test(filter = "builder-recommend", stop_on_failure = TRUE)'
```

预期：PASS。

- [ ] **步骤 5：提交**

```bash
git add inst/builder/recommend.R tests/testthat/test-builder-recommend.R
git commit -m "feat(builder): separate metadata retention recommendations"
```

---

### 任务 2：升级并验证 final metadata policy

**文件：**
- 修改：`inst/builder/state/metadata.R`
- 修改：`inst/builder/state/core.R`
- 修改：`tests/testthat/test-builder-plan-content.R`

- [ ] **步骤 1：写 policy invariant 失败测试**

增加测试覆盖：

```r
metadata_retention_entry <- function() {
  entry <- builder_task6_entry()
  entry$dataset_profile$metadata$columns$orig.ident <- list(
    name = "orig.ident",
    class = "factor",
    supported = TRUE,
    non_missing = 100L,
    unique_non_missing = 1L
  )
  recommendation <- builder_recommend_metadata(
    entry$dataset_profile,
    required = c("nCount_RNA", "nFeature_RNA")
  )
  entry$settings$recommendations$metadata <- recommendation
  entry$settings$metadata_policy <- builder_task6_final_metadata_policy(
    recommendation,
    list(nCount_RNA = "included", nFeature_RNA = "included")
  )
  entry
}

test_that("metadata retention is independent from Group selection", {
  entry <- metadata_retention_entry()
  final <- .builder_state_effective_metadata_policy(
    entry,
    entry$dataset_profile
  )

  expect_contains(final$retained, "orig.ident")
  expect_false("orig.ident" %in% final$groups)
})

test_that("a Group cannot be excluded from retained metadata", {
  entry <- metadata_retention_entry()
  entry$settings$groups <- "cluster"
  entry$settings$included_groups <- "cluster"
  policy <- entry$settings$metadata_policy
  policy$retained <- setdiff(policy$retained, "cluster")
  policy$columns$cluster$retain_in_crb <- FALSE

  expect_builder_state_error(
    .builder_state_validate_metadata_policy(
      policy, entry$dataset_profile, entry
    ),
    "metadata_dependency_conflict"
  )
})
```

再覆盖 forced、unsupported、sensitive-without-acknowledgement、旧 policy shape。

- [ ] **步骤 2：运行并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-plan-content", stop_on_failure = TRUE)'
```

预期：FAIL，validator 尚未理解 `retained`/`groups`。

- [ ] **步骤 3：添加统一升级函数**

在 `inst/builder/state/metadata.R` 中增加：

```r
.builder_state_upgrade_metadata_policy <- function(policy) {
  if (is.null(policy)) return(NULL)
  columns <- policy$columns %||% list()
  for (id in names(columns)) {
    record <- columns[[id]]
    if (is.null(record$retain_in_crb)) {
      record$retain_in_crb <- isTRUE(record$effective_included)
    }
    if (is.null(record$group_enabled)) {
      record$group_enabled <- FALSE
    }
    if (is.null(record$forced)) {
      record$forced <- isTRUE(record$required)
    }
    columns[[id]] <- record
  }
  policy$columns <- columns
  policy$retained <- names(columns)[vapply(
    columns, function(x) isTRUE(x$retain_in_crb), logical(1)
  )]
  policy$groups <- names(columns)[vapply(
    columns, function(x) isTRUE(x$group_enabled), logical(1)
  )]
  policy
}
```

旧 session 的 excluded 列不能因 upgrade 自动变成 retained；新 retain-all 行为只来自新的 recommendation。

- [ ] **步骤 4：把 validator 改为明确不变量**

`.builder_state_validate_metadata_policy()` 必须验证：

```r
setequal(policy$retained, derived_retained)
setequal(policy$groups, derived_groups)
all(policy$groups %in% policy$retained)
all(forced_ids %in% policy$retained)
!any(unsupported_ids %in% policy$retained)
```

`.builder_state_validate_metadata_dependencies()` 改读 `retain_in_crb`，不再把 Group eligibility 当 retention。

同时增加两个纯 helper，供 state 与 server 共用：

```r
builder_metadata_policy_set_retained <- function(policy, retained) {
  retained <- unique(as.character(retained))
  for (id in names(policy$columns)) {
    policy$columns[[id]]$retain_in_crb <- id %in% retained ||
      isTRUE(policy$columns[[id]]$forced)
  }
  .builder_state_upgrade_metadata_policy(policy)
}

builder_metadata_policy_set_groups <- function(policy, groups) {
  groups <- unique(as.character(groups))
  for (id in names(policy$columns)) {
    policy$columns[[id]]$group_enabled <- id %in% groups
    if (id %in% groups) policy$columns[[id]]$retain_in_crb <- TRUE
  }
  .builder_state_upgrade_metadata_policy(policy)
}
```

- [ ] **步骤 5：让 entry 的 included Groups 同步到 policy**

在 state 汇编时，将 `.builder_state_included_groups(entry)` 写入各列 `group_enabled`，然后重新派生 `policy$groups`。任何 Group 不在 `retained` 时返回 `metadata_dependency_conflict`。

- [ ] **步骤 6：运行 state tests**

```bash
Rscript -e 'devtools::test(filter = "builder-plan-content|builder-plan-readiness", stop_on_failure = TRUE)'
```

预期：PASS。

- [ ] **步骤 7：提交**

```bash
git add inst/builder/state/metadata.R inst/builder/state/core.R \
  tests/testthat/test-builder-plan-content.R \
  tests/testthat/test-builder-plan-readiness.R
git commit -m "refactor(builder): validate metadata retention separately"
```

---

### 任务 3：在 Builder UI 中分开 Keep metadata 与 Group

**文件：**
- 修改：`inst/builder/ui/core_stage/groups.R`
- 修改：`inst/builder/server/datasets.R`
- 修改：`inst/builder/server/review.R`
- 修改：`inst/builder/www/builder.js`
- 修改：`inst/builder/www/builder.features.css`
- 修改：`tests/testthat/test-builder-stage-inspect-core.R`
- 修改：`tests/testthat/test-builder-browser.R`

- [ ] **步骤 1：写静态 UI 失败测试**

断言 HTML 同时包含：

```r
expect_match(html, "Keep in CRB", fixed = TRUE)
expect_match(html, "Keep all supported metadata", fixed = TRUE)
expect_match(html, "Restore recommended retention", fixed = TRUE)
expect_match(html, 'class="viewer-metadata-retain"', fixed = TRUE)
expect_match(detail, "Kept as ordinary metadata", fixed = TRUE)
expect_match(detail, "Not eligible as a Group", fixed = TRUE)
```

并断言 constant `orig.ident` 的 Keep checkbox 已选中、Group checkbox 不出现。

- [ ] **步骤 2：写浏览器 contract 失败测试**

模拟点击 `Keep all supported metadata`，期望向 Shiny 发送：

```js
{
  action: "set-retention",
  retained: ["orig.ident", "cell_type", "score"]
}
```

Group action 继续发送：

```js
{
  action: "set-groups",
  included: ["cell_type"],
  default: "cell_type"
}
```

- [ ] **步骤 3：运行测试并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-stage-inspect-core|builder-browser", stop_on_failure = TRUE)'
```

- [ ] **步骤 4：扩展 catalog model**

`builder_group_catalog_model()` 的每项新增：

```r
retained = isTRUE(record$retain_in_crb),
retention_locked = isTRUE(record$forced) || !isTRUE(column$supported),
retention_sensitive = isTRUE(record$sensitive),
group_enabled = id %in% included_groups
```

模型顶层新增 `retained`, `supported`, `recommended_retained`。

- [ ] **步骤 5：实现两套控件和事件**

行布局必须包含独立的 Keep checkbox 和 Group checkbox。JS 分别监听 `.viewer-metadata-retain` 与 `.viewer-group-include`，分别写入 `core-metadata_action` 和 `core-group_action`。

敏感列从未确认状态切换为 Keep 时，server 不直接提交，先走现有 acknowledgement token 机制。

- [ ] **步骤 6：server 更新 policy 而不改变 Group**

新增 `observeEvent(input[["core-metadata_action"]], ...)`：

```r
retained <- intersect(action$retained, supported_ids)
retained <- union(retained, forced_ids)
entry$settings$metadata_policy <- builder_metadata_policy_set_retained(
  entry$settings$metadata_policy,
  retained
)
replace_entry(entry)
```

Group observer只修改 `group_enabled`，并保证 included Group 自动 retained。

- [ ] **步骤 7：运行 UI tests**

```bash
Rscript -e 'devtools::test(filter = "builder-stage-inspect-core|builder-browser", stop_on_failure = TRUE)'
```

预期：PASS。

- [ ] **步骤 8：提交**

```bash
git add inst/builder/ui/core_stage/groups.R inst/builder/server/datasets.R \
  inst/builder/server/review.R inst/builder/www/builder.js \
  inst/builder/www/builder.features.css \
  tests/testthat/test-builder-stage-inspect-core.R \
  tests/testthat/test-builder-browser.R
git commit -m "feat(builder): add explicit metadata retention controls"
```

---

### 任务 4：让 frozen plan、CRB 和 Review 使用同一 metadata policy

**文件：**
- 修改：`inst/builder/plan/assets.R`
- 修改：`inst/builder/plan/freeze.R`
- 修改：`inst/builder/build.R`
- 修改：`inst/builder/ui/review_stage.R`
- 修改：`inst/builder/report.R`
- 修改：`tests/testthat/test-builder-build.R`
- 修改：`tests/testthat/test-builder-viewer-review.R`
- 修改：`tests/testthat/test-builder-report.R`

- [ ] **步骤 1：写 `orig.ident` end-to-end 失败测试**

使用 `SeuratObject::pbmc_small`，添加 constant 列：

```r
object$orig.ident <- factor(rep("sample_a", ncol(object)))
```

冻结 policy 后断言：

```r
expect_contains(plan$items[[1L]]$metadata_policy$retained, "orig.ident")
prepared <- .builder_build_apply_metadata_policy(object, plan$items[[1L]])
expect_contains(colnames(prepared@meta.data), "orig.ident")
```

保存并读回 CRB 后：

```r
expect_contains(colnames(readRDS(crb)$meta_data), "orig.ident")
```

- [ ] **步骤 2：写 Review/report 计数失败测试**

构造 12 retained、1 excluded policy，断言：

```r
expect_identical(model$viewer_content$metadata$kept_count, 12L)
expect_identical(model$viewer_content$metadata$excluded_count, 1L)
expect_identical(report$datasets[[1L]]$metadata$retained, retained)
expect_identical(report$datasets[[1L]]$metadata$excluded, "secret_note")
```

- [ ] **步骤 3：运行并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-build|builder-viewer-review|builder-report", stop_on_failure = TRUE)'
```

- [ ] **步骤 4：冻结 exact retained columns**

`.builder_plan_metadata_identity()` 与 freeze item 只使用：

```r
item$metadata_policy$retained
```

并验证 Groups、cell cycle、nUMI、nGene 全部包含在 retained 中。

- [ ] **步骤 5：构建前仅裁剪 frozen retained columns**

`.builder_build_apply_metadata_policy()` 使用：

```r
metadata <- setdiff(item$metadata_policy$retained, "cell_barcode")
object@meta.data <- object@meta.data[, metadata, drop = FALSE]
```

禁止从 `included_groups` 重新推导 metadata 集合。

- [ ] **步骤 6：修复 Review 和 report**

`builder_review_metadata_model()` 直接以 `retain_in_crb` 派生计数；report 新增：

```r
metadata = list(
  retained = unname(item$metadata_policy$retained),
  excluded = unname(item$metadata_policy$excluded),
  forced = unname(item$metadata_policy$forced)
)
```

保留 `metadata_columns` 作为兼容字段，值必须等于验证后 CRB 列名。

- [ ] **步骤 7：运行目标 tests**

```bash
Rscript -e 'devtools::test(filter = "builder-build|builder-viewer-review|builder-report", stop_on_failure = TRUE)'
```

预期：PASS。

- [ ] **步骤 8：提交**

```bash
git add inst/builder/plan/assets.R inst/builder/plan/freeze.R \
  inst/builder/build.R inst/builder/ui/review_stage.R inst/builder/report.R \
  tests/testthat/test-builder-build.R \
  tests/testthat/test-builder-viewer-review.R \
  tests/testthat/test-builder-report.R
git commit -m "fix(builder): preserve frozen metadata through export"
```

---

### 任务 5：定义空间图片 storage 和 nested collection 合约

**文件：**
- 修改：`inst/builder/extras.R`
- 修改：`inst/builder/plan/defaults.R`
- 修改：`inst/builder/state/core.R`
- 修改：`tests/testthat/test-builder-spatial.R`
- 修改：`tests/testthat/test-builder-end-to-end.R`

- [ ] **步骤 1：写 nested collection 失败测试**

目标形状：

```r
test_alignment_record <- function(section, filename) {
  builder_alignment_record(
    source = list(name = filename, type = "image/png", size = 4),
    source_uri = "data:image/png;base64,AAAA",
    uri = "data:image/png;base64,AAAA",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    saved = TRUE,
    section = list(id = section, kind = "spatial")
  )
}

images <- list(
  section_a = list(
    `H&E` = test_alignment_record("section_a", "H&E.png"),
    DAPI = test_alignment_record("section_a", "DAPI.png")
  )
)
```

测试以下 helpers：

```r
expect_named(builder_image_collection_normalize(images), "section_a")
expect_named(
  builder_image_collection_normalize(images)$section_a,
  c("H&E", "DAPI")
)
expect_error(
  builder_image_collection_normalize(list(section_a = list("" = record))),
  "non-empty"
)
expect_error(
  builder_image_collection_normalize(list(section_a = list(A = record, A = record))),
  "unique"
)
```

增加旧 shape 升级测试：

```r
legacy <- list(section_a = test_alignment_record("section_a", "H&E.png"))
upgraded <- builder_image_collection_normalize(legacy)
expect_named(upgraded$section_a, "H&E.png")
```

- [ ] **步骤 2：写 storage defaults 失败测试**

```r
expect_identical(builder_default_settings(profile)$spatial_image_storage, "external")
expect_identical(
  builder_upgrade_viewer_content_entry(legacy_entry)$settings$spatial_image_storage,
  "embedded"
)
```

- [ ] **步骤 3：运行并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-spatial|builder-end-to-end", stop_on_failure = TRUE)'
```

- [ ] **步骤 4：实现 collection helper**

在 `inst/builder/extras.R` 增加：

```r
builder_image_collection_normalize <- function(images) {
  # returns section -> label -> normalized alignment record
}

builder_image_collection_flatten <- function(images) {
  # returns rows/records carrying section_id + image_label
}

builder_image_collection_count <- function(images) {
  sum(lengths(images %||% list()))
}
```

Label 默认来自安全化 basename；同一 section 内冲突使用 `make.unique()`，不得跨 section 强制唯一。

`trekker` 是保留 identity：继续使用单个 `trekker_alignment` record，不进入 `section -> label` collection，也不计入 Spatial section/image 数量。`builder_partition_alignments()` 必须显式分离 Trekker 后再规范化 spatial collection。

- [ ] **步骤 5：实现新/旧默认值**

新 import 的 settings 明确包含：

```r
spatial_image_storage = "external"
```

`builder_upgrade_viewer_content_entry()` 只有在读取旧 entry 且字段缺失时补 `embedded`。不要让新 defaults 经过 upgrade 后被覆盖回 embedded；用 entry schema/version 或 `settings` 是否已包含该字段区分。

- [ ] **步骤 6：运行 tests**

```bash
Rscript -e 'devtools::test(filter = "builder-spatial|builder-end-to-end", stop_on_failure = TRUE)'
```

- [ ] **步骤 7：提交**

```bash
git add inst/builder/extras.R inst/builder/plan/defaults.R \
  inst/builder/state/core.R tests/testthat/test-builder-spatial.R \
  tests/testthat/test-builder-end-to-end.R
git commit -m "refactor(builder): model named spatial image collections"
```

---

### 任务 6：冻结并 preflight 图片 storage

**文件：**
- 修改：`inst/builder/plan/preflight.R`
- 修改：`inst/builder/plan/freeze.R`
- 修改：`tests/testthat/test-builder-plan-readiness.R`
- 修改：`tests/testthat/test-builder-build.R`

- [ ] **步骤 1：写失败测试**

覆盖：

```r
entry <- builder_task6_entry()
record <- list(
  source = list(name = "H&E.png", type = "image/png", size = 4),
  source_uri = "data:image/png;base64,AAAA",
  uri = "data:image/png;base64,AAAA",
  base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
  bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
  dx = 0, dy = 0, scale = 1, rotation = 0,
  flip_x = FALSE, flip_y = FALSE,
  image_opacity = 0.8, point_opacity = 0.85, point_size = 5,
  saved = TRUE, outside = 0L,
  section_id = "fov", section_kind = "spatial"
)
entry$settings$images <- list(fov = list(`H&E` = record))
entry$settings$spatial_image_storage <- "external"

blocked <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)
expect_identical(blocked$error_code, "external_images_require_app")

entry$settings$spatial_image_storage <- "invalid"
blocked <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
expect_identical(blocked$error_code, "invalid_spatial_image_storage")

entry$settings$spatial_image_storage <- "external"
ready <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
expect_null(ready$error)
expect_identical(ready$items[[1L]]$spatial_image_storage, "external")
expect_identical(ready$items[[1L]]$spatial_alignment$image_count, 1L)
```

重复 label 不能用普通 R `list()` 表达后再依赖 names，因为 R 允许重复名称；测试直接构造 `structure(list(record, record), names = c("H&E", "H&E"))`，预期 `duplicate_spatial_image_label`。

- [ ] **步骤 2：运行并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-plan-readiness|builder-build", stop_on_failure = TRUE)'
```

- [ ] **步骤 3：更新 preflight**

逐 section、逐 label 检查 `saved`, bounds, outside count。错误消息同时包含 dataset、section、label。

当 `make_app = FALSE && storage == "external" && image_count > 0` 时返回：

```r
builder_plan_error(
  "External spatial images require CRB files + Viewer App output.",
  "external_images_require_app"
)
```

- [ ] **步骤 4：冻结 storage 与准确计数**

Build item 写入：

```r
spatial_image_storage = settings$spatial_image_storage,
images = builder_image_collection_normalize(alignments$spatial),
spatial_alignment = list(
  section_count = length(spatial_sections),
  image_count = builder_image_collection_count(alignments$spatial),
  saved_count = sum(saved_flags),
  points_only = setdiff(spatial_sections, names(alignments$spatial))
)
```

这里 `section_count` 必须来自实际 spatial entries，不能来自 fixture image catalog。

- [ ] **步骤 5：运行 tests**

```bash
Rscript -e 'devtools::test(filter = "builder-plan-readiness|builder-build", stop_on_failure = TRUE)'
```

- [ ] **步骤 6：提交**

```bash
git add inst/builder/plan/preflight.R inst/builder/plan/freeze.R \
  tests/testthat/test-builder-plan-readiness.R \
  tests/testthat/test-builder-build.R
git commit -m "feat(builder): freeze spatial image storage policy"
```

---

### 任务 7：构建 embedded 或 external 图片输出

**文件：**
- 修改：`inst/builder/extras.R`
- 修改：`inst/builder/build.R`
- 修改：`R/spatial_image_manifest.R`
- 修改：`R/createShinyApp.R`
- 修改：`inst/viewer/coordinated_views/bundle.R`
- 修改：`tests/testthat/test-builder-build.R`
- 修改：`tests/testthat/test-createShinyApp-sibling.R`
- 修改：`tests/testthat/test-generated-app-pages-spatial.R`

- [ ] **步骤 1：写 embedded/external 等价失败测试**

Embedded：

```r
expect_named(spatial$histology_images, c("H&E", "DAPI"))
expect_match(spatial$histology_images[["H&E"]]$histology_image, "^data:image/")
expect_null(result$external_images)
```

External：

```r
expect_length(spatial$histology_images, 0L)
expect_named(result$external_images$Dataset$section_a, c("H&E", "DAPI"))
expect_true(file.exists(result$external_images$Dataset$section_a[["H&E"]]$path))
expect_identical(
  result$external_settings$Dataset$section_a[["H&E"]]$image_opacity,
  0.8
)
```

读 App config 后，断言 relative `spatial-assets/` path，且 CRB 无 Builder 新 Base64 payload。

- [ ] **步骤 2：运行并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-build|createShinyApp-sibling|generated-app-pages-spatial", stop_on_failure = TRUE)'
```

- [ ] **步骤 3：实现安全 data URI materialization**

在 `inst/builder/extras.R` 增加只接受 image data URI 的 helper：

```r
builder_materialize_image_uri <- function(uri, path) {
  parsed <- builder_parse_image_uri(uri)
  if (!parsed$mime %in% c("image/png", "image/jpeg", "image/svg+xml")) {
    stop("Builder image URI has an unsupported MIME type.", call. = FALSE)
  }
  writeBin(parsed$bytes, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
```

实际写文件通过现有 build/stage IO seam；测试不得向 repo 写临时文件。

- [ ] **步骤 4：embedded mode 遍历所有 named records**

`builder_attach_spatial_images()` 对一个 section 的每个 label 调用 canonical payload helper；保留非 Builder embedded images，label 冲突 fail closed，不再 `make.unique()` 静默改名。

- [ ] **步骤 5：external mode 生成 manifest**

使用 `record$source_uri` 物化原始、已限制尺寸的 PNG，descriptor 使用 `record$base_bounds`：

```r
spatial_images[[dataset]][[section]][[label]] <- list(
  path = materialized_path,
  bounds = unlist(record$base_bounds[c("xmin", "xmax", "ymin", "ymax")])
)
spatial_image_settings[[dataset]][[section]][[label]] <- list(
  flip_x = record$flip_x,
  flip_y = record$flip_y,
  scale_x = record$scale,
  scale_y = record$scale,
  offset_x = record$dx,
  offset_y = record$dy,
  rotation = record$rotation,
  image_opacity = record$image_opacity
)
```

CRB 只保留小型 `histology_alignment` appearance record，使 point opacity/size 仍可用；不得写 Builder image bytes。

- [ ] **步骤 6：扩展 public image settings 白名单**

在 `R/spatial_image_manifest.R` 将 `image_opacity` 加入 allowed numeric fields，范围严格为 `[0, 1]`。更新 `createShinyApp()` roxygen。Viewer `cv_image_preset()` 使用：

```r
opacity = number("image_opacity", defaults$opacity)
```

- [ ] **步骤 7：运行目标 tests**

```bash
Rscript -e 'devtools::test(filter = "builder-build|createShinyApp-sibling|generated-app-pages-spatial", stop_on_failure = TRUE)'
```

预期：PASS；external App config 只含 relative paths。

- [ ] **步骤 8：提交**

```bash
git add inst/builder/extras.R inst/builder/build.R \
  R/spatial_image_manifest.R R/createShinyApp.R \
  inst/viewer/coordinated_views/bundle.R \
  tests/testthat/test-builder-build.R \
  tests/testthat/test-createShinyApp-sibling.R \
  tests/testthat/test-generated-app-pages-spatial.R
git commit -m "feat(builder): support external spatial image assets"
```

---

### 任务 8：把 external manifest 纳入 App bundle 安全合约

**文件：**
- 修改：`inst/builder/app_bundle/contract.R`
- 修改：`inst/builder/app_bundle/request.R`
- 修改：`inst/builder/app_bundle/build.R`
- 修改：`inst/builder/app_bundle/topology.R`
- 修改：`tests/testthat/test-builder-app-bundle.R`

- [ ] **步骤 1：写 request/build 失败测试**

断言 request 新字段：

```r
expect_named(request, c(
  .builder_app_request_fields,
  "spatial_images",
  "spatial_image_settings",
  "spatial_image_identities"
))
```

覆盖 source 文件 build 前被修改、symlink、重复 source/target、绝对路径泄露和未声明 `spatial-assets` 文件。

- [ ] **步骤 2：运行并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-app-bundle", stop_on_failure = TRUE)'
```

- [ ] **步骤 3：扩展 app plan projection**

`.builder_app_plan_contract()` 的 item projection 加入：

```r
spatial_image_storage
external_images
external_image_settings
```

保持 inert/reference-free/deep-copy 限制。

- [ ] **步骤 4：capture external image identities**

沿用 `.builder_app_capture_file_identity()`，identity key 使用 dataset/section/label。App build 前再次验证 size、mtime、ctime、inode、md5。

- [ ] **步骤 5：调用 `createShinyApp()`**

`builder_build_app()` 的 `create_arguments` 新增：

```r
spatial_images = request$spatial_images,
spatial_image_settings = request$spatial_image_settings
```

不要塞进 `cerebro_options` 绕过 formal validation。

- [ ] **步骤 6：拓扑验证**

允许且只允许 config manifest 声明的：

```text
cerebro_app/spatial-assets/<dataset>/<section>/<filename>
```

CRB、H5、BPCells 继续只允许在 `private-data/`。未配置的文件、symlink、hardlink 异常继续 fail closed。

- [ ] **步骤 7：运行 tests**

```bash
Rscript -e 'devtools::test(filter = "builder-app-bundle", stop_on_failure = TRUE)'
```

- [ ] **步骤 8：提交**

```bash
git add inst/builder/app_bundle/contract.R \
  inst/builder/app_bundle/request.R inst/builder/app_bundle/build.R \
  inst/builder/app_bundle/topology.R \
  tests/testthat/test-builder-app-bundle.R
git commit -m "feat(builder): secure external image app assembly"
```

---

### 任务 9：实现多图 Spatial editor

**文件：**
- 修改：`inst/builder/spatial_alignment_server.R`
- 修改：`inst/builder/ui/enhance_stage.R`
- 修改：`inst/builder/server/imports.R`
- 修改：`inst/builder/server/enhancements.R`
- 修改：`inst/builder/www/builder.js`
- 修改：`inst/builder/www/builder.features.css`
- 修改：`tests/testthat/test-builder-spatial.R`
- 修改：`tests/testthat/test-builder-browser.R`

- [ ] **步骤 1：写 server 状态转换失败测试**

覆盖：

```r
add_image(section = "section_a", label = "H&E")
add_image(section = "section_a", label = "DAPI")
rename_image(section = "section_a", from = "DAPI", to = "IF")
remove_image(section = "section_a", label = "H&E")
```

每次断言 active image、saved baseline、其他 image record 不变。删除是可恢复 session state mutation，不删除源文件。

- [ ] **步骤 2：写 UI contract 失败测试**

HTML/Browser 断言包含：

```text
Image storage
External files in App
Embedded in CRB
+ Add image
Rename image
Remove image
```

并验证 section_a 下的 H&E 与 DAPI 均可切换。

- [ ] **步骤 3：运行并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-spatial|builder-browser", stop_on_failure = TRUE)'
```

- [ ] **步骤 4：增加 active image identity**

Server 增加：

```r
active_image <- shiny::reactiveVal(NULL)
pending_image <- shiny::reactiveVal(NULL)
```

所有 `restore()`, `commit_section()`, `switch_to()` 改为 `(section, label)`；切换 section 或 image 前都复用现有 unsaved-changes modal。

- [ ] **步骤 5：实现 image stack actions**

- Add：上传后先以安全 basename 为 label；冲突时要求用户输入新 label，不静默覆盖。
- Rename：验证非空、同 section 唯一，保持 record 和 alignment。
- Remove：如果是当前 unsaved image，先确认；删除后选相邻 image 或空状态。
- Apply transform to all sections：第一版改名为 **Apply transform to matching image label**，只影响其他 section 的同 label；不得把 H&E transform 应用到 DAPI。

- [ ] **步骤 6：实现 storage selector**

Dataset scope select input：

```r
selectInput(
  ns("spatial_image_storage"),
  "Image storage",
  choices = c(
    "External files in App (spatial-assets/)" = "external",
    "Embedded in CRB" = "embedded"
  ),
  selected = model$spatial_image_storage
)
```

切换只修改 setting，不改变图片记录。

- [ ] **步骤 7：运行 tests**

```bash
Rscript -e 'devtools::test(filter = "builder-spatial|builder-browser", stop_on_failure = TRUE)'
```

- [ ] **步骤 8：提交**

```bash
git add inst/builder/spatial_alignment_server.R \
  inst/builder/ui/enhance_stage.R inst/builder/server/imports.R \
  inst/builder/server/enhancements.R inst/builder/www/builder.js \
  inst/builder/www/builder.features.css \
  tests/testthat/test-builder-spatial.R tests/testthat/test-builder-browser.R
git commit -m "feat(builder): edit multiple named images per section"
```

---

### 任务 10：完成 Review 与 build-report 的 storage 说明

**文件：**
- 修改：`inst/builder/ui/review_stage.R`
- 修改：`inst/builder/report.R`
- 修改：`tests/testthat/test-builder-viewer-review.R`
- 修改：`tests/testthat/test-builder-report.R`

- [ ] **步骤 1：写失败测试**

Review：

```r
expect_identical(item$spatial_alignment$section_count, 6L)
expect_identical(item$spatial_alignment$image_count, 7L)
expect_identical(item$spatial_alignment$storage, "External spatial-assets")
```

Report：

```r
expect_identical(dataset$expression_storage$mode, "embedded")
expect_identical(dataset$spatial_image_storage, list(
  mode = "external",
  image_count = 7L,
  section_count = 6L
))
```

- [ ] **步骤 2：运行并确认失败**

```bash
Rscript -e 'devtools::test(filter = "builder-viewer-review|builder-report", stop_on_failure = TRUE)'
```

- [ ] **步骤 3：实现 typed summaries**

Review 显示：

```text
Metadata: 12 retained · 0 excluded
Groups: 8 included · Default: Cell type
Expression storage: Embedded
Spatial: 6 sections · 7 images · External spatial-assets
```

Report 保留旧 `expression_backend` 与 `metadata_columns`，新增 typed records；validator 要求新旧字段一致。

- [ ] **步骤 4：运行 tests**

```bash
Rscript -e 'devtools::test(filter = "builder-viewer-review|builder-report", stop_on_failure = TRUE)'
```

- [ ] **步骤 5：提交**

```bash
git add inst/builder/ui/review_stage.R inst/builder/report.R \
  tests/testthat/test-builder-viewer-review.R \
  tests/testthat/test-builder-report.R
git commit -m "fix(builder): report exact metadata and image topology"
```

---

### 任务 11：回归 Embedded/HDF5/BPCells 表达后端

**文件：**
- 修改：`tests/testthat/test-builder-storage-metadata-parity.R`
- 修改：`tests/testthat/test-builder-app-bundle.R`
- 修改：`tests/testthat/test-multisection-spatial.R`

- [ ] **步骤 1：写三后端参数化 acceptance test**

```r
for (backend in c("embedded", "h5", "bpcells")) {
  test_that(paste("image storage is independent of", backend), {
    skip_backend_if_unavailable(backend)
    result <- build_storage_parity_fixture(
      expression_backend = backend,
      spatial_image_storage = "external"
    )
    expect_identical(result$crb$getExpressionBackend()$type, backend)
    expect_true(all(result$image_paths_exist))
    expect_false(result$builder_images_embedded)
  })
}
```

再对 embedded image storage 做一轮小型 matrix 测试。

- [ ] **步骤 2：运行并确认所需 fixture helper 缺失**

```bash
Rscript -e 'devtools::test(filter = "builder-storage-metadata-parity", stop_on_failure = TRUE)'
```

- [ ] **步骤 3：实现 synthetic fixture helper**

Helper 必须创建：2 datasets、constant `orig.ident`、8 categorical Groups、6 spatial sections、每数据集 7 images、Trekker。图片使用小型临时 PNG，不依赖 Downloads。

测试文件内实现以下明确入口：

```r
skip_backend_if_unavailable <- function(backend) {
  if (identical(backend, "h5")) testthat::skip_if_not_installed("HDF5Array")
  if (identical(backend, "bpcells")) testthat::skip_if_not_installed("BPCells")
  invisible(backend)
}

build_storage_parity_fixture <- function(
  expression_backend,
  spatial_image_storage
) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  fixture <- builder_storage_parity_entry(
    root,
    expression_backend = expression_backend,
    spatial_image_storage = spatial_image_storage
  )
  plan <- builder_freeze_plan(
    fixture$entries,
    file.path(root, "release"),
    make_app = TRUE
  )
  stage <- file.path(root, "stage")
  dir.create(stage, mode = "0700")
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = fixture$snapshots
  )
  config <- readRDS(file.path(result$app_dir, "cerebro_config.rds"))
  crb <- readRDS(file.path(result$app_dir, "private-data", plan$items[[1L]]$filename))
  image_paths <- unlist(lapply(
    config$spatial_images,
    function(dataset) unlist(lapply(
      dataset,
      function(section) vapply(
        section,
        function(descriptor) {
          if (is.list(descriptor)) descriptor$path else descriptor
        },
        character(1)
      )
    ), use.names = FALSE)
  ), use.names = FALSE)
  list(
    crb = crb,
    config = config,
    image_paths_exist = file.exists(file.path(result$app_dir, image_paths)),
    builder_images_embedded = any(vapply(
      crb$spatial,
      function(x) length(x$histology_images %||% list()) > 0L,
      logical(1)
    ))
  )
}
```

`builder_storage_parity_entry()` 使用现有 `builder_task6_entry()`、synthetic Seurat fixture 和 `builder_alignment_record()` 组装并返回 `list(entries = list(entry), snapshots = named_snapshot_registry)`；不得绕过正常 `builder_freeze_plan()`/`builder_execute_plan()`。

- [ ] **步骤 4：运行 acceptance 与既有 backend tests**

```bash
Rscript -e 'devtools::test(filter = "builder-storage-metadata-parity|builder-app-bundle|multisection-spatial", stop_on_failure = TRUE)'
```

预期：可用依赖对应的 cases PASS；缺失的 optional dependency 只能显式 SKIP。

- [ ] **步骤 5：提交**

```bash
git add tests/testthat/test-builder-storage-metadata-parity.R \
  tests/testthat/test-builder-app-bundle.R \
  tests/testthat/test-multisection-spatial.R
git commit -m "test(builder): cover storage and metadata parity"
```

---

### 任务 12：用真实 `anna_lena` 数据做 UI 与命令行验收

**文件：**
- 不提交真实数据或导出物。
- 使用：`/Users/nuioi/Downloads/anna_lena/export_cli_app.R`
- 使用：`/Users/nuioi/Downloads/anna_lena/data/data1/all_content.rds`
- 使用：`/Users/nuioi/Downloads/anna_lena/data/data2/all_content_2.rds`

- [ ] **步骤 1：在隔离输出目录重新运行命令行基准**

复制现有小白脚本到验收文件：

```text
/Users/nuioi/Downloads/anna_lena/export_cli_app_acceptance.R
```

```bash
cp /Users/nuioi/Downloads/anna_lena/export_cli_app.R \
  /Users/nuioi/Downloads/anna_lena/export_cli_app_acceptance.R
```

然后使用 `apply_patch`，只在副本中把 `/cli_export` 替换为 `/acceptance_cli`，并把 `load_all()` worktree 改成 `/Users/nuioi/projects/shiny/_wt_builder_storage_metadata`。不得修改原脚本。

运行：

```bash
Rscript /Users/nuioi/Downloads/anna_lena/export_cli_app_acceptance.R
```

预期：exit 0；两个 CRB、完整 App、14 个 external spatial assets。

- [ ] **步骤 2：启动新 Builder 并通过 UI 导出**

```bash
Rscript -e 'pkgload::load_all("/Users/nuioi/projects/shiny/_wt_builder_storage_metadata"); launchCerebroBuilder(host="127.0.0.1", port=7799, launch_browser=TRUE)'
```

UI 选择：

- 两个数据集；
- 12 metadata 全部 retained，包括 `orig.ident`；
- 8 Groups，默认 `cell_type`；
- UMAP、t-SNE；
- Spatial + Trekker；
- Image storage = External；
- 每数据集 7 张图片，labels 为 DAPI、H&E、IF、PAS；
- CRB files + Viewer App；
- 输出 `/Users/nuioi/Downloads/anna_lena/acceptance_builder`。

- [ ] **步骤 3：用 R 比较 CRB 核心内容**

```bash
Rscript - <<'RS'
pairs <- list(
  c(
    "/Users/nuioi/Downloads/anna_lena/acceptance_builder/cerebro_app/private-data/01-all-content-ds1.crb",
    "/Users/nuioi/Downloads/anna_lena/acceptance_cli/app/private-data/all_content.crb"
  ),
  c(
    "/Users/nuioi/Downloads/anna_lena/acceptance_builder/cerebro_app/private-data/02-all-content-2-ds2.crb",
    "/Users/nuioi/Downloads/anna_lena/acceptance_cli/app/private-data/all_content_2.crb"
  )
)
for (pair in pairs) {
  builder <- readRDS(pair[[1L]])
  cli <- readRDS(pair[[2L]])
  stopifnot(
    identical(builder$expression, cli$expression),
    identical(builder$projections, cli$projections),
    setequal(names(builder$groups), names(cli$groups)),
    "orig.ident" %in% colnames(builder$meta_data),
    ncol(builder$meta_data) == 12L,
    length(builder$spatial) == 6L,
    sum(vapply(
      builder$spatial,
      function(x) length(x$histology_images %||% list()),
      integer(1)
    )) == 0L
  )
}
cat("CRB parity OK\n")
RS
```

若 `%||%` 不在全局，脚本开头定义：

```r
`%||%` <- function(x, y) if (is.null(x)) y else x
```

- [ ] **步骤 4：检查 external assets 和 report**

```bash
find /Users/nuioi/Downloads/anna_lena/acceptance_builder/cerebro_app/spatial-assets \
  -type f | sort
jq '.datasets[] | {
  name,
  metadata,
  expression_storage,
  spatial_image_storage
}' /Users/nuioi/Downloads/anna_lena/acceptance_builder/build-report.json
```

预期：14 files；每 dataset 12 retained、8 groups、6 sections、7 images、external mode。

- [ ] **步骤 5：分别启动两个 App**

用临时副本把端口改为 18081、18082，然后：

```bash
curl --fail --silent http://127.0.0.1:18081/ | rg '<title>CerebroNexus'
curl --fail --silent http://127.0.0.1:18082/ | rg '<title>CerebroNexus'
```

预期：两条命令 exit 0。浏览器检查两个 dataset 的 Spatial image picker 和 Trekker 页面。

- [ ] **步骤 6：记录验收结论，不提交真实数据**

在最终回复中列出：metadata columns、Groups、sections、images、backend、HTTP 状态，以及任何仍存在的非能力性差异。

---

### 任务 13：最终质量与规格审查

**文件：**
- 可能修改：本计划涉及的生产与测试文件，仅修复审查发现的问题。

- [ ] **步骤 1：运行聚焦 suite**

```bash
Rscript -e 'devtools::test(filter = "builder-(recommend|plan-content|plan-readiness|build|spatial|app-bundle|viewer-review|report|storage-metadata-parity)|createShinyApp-sibling|multisection-spatial|generated-app-pages-spatial", stop_on_failure = TRUE)'
```

预期：0 failures；optional dependency 只能产生说明明确的 skips。

- [ ] **步骤 2：运行全量 package tests**

```bash
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
```

预期：0 failures。

- [ ] **步骤 3：运行 package check**

```bash
R CMD build .
R CMD check --no-manual --as-cran CerebroNexus_*.tar.gz
```

预期：0 ERROR、0 WARNING；NOTE 必须逐条判断是否为已知环境问题。

- [ ] **步骤 4：检查差异与提交边界**

```bash
git diff --check
git status --short --branch
git log --oneline 9bfbb20a..HEAD
```

预期：无未提交实现修改；commit 只包含本计划范围。

- [ ] **步骤 5：对照规格逐项审查**

逐项确认：

- expression/image 两个 storage 轴独立；
- 新项目 external、旧项目 embedded；
- CRB-only + external 被阻止；
- `orig.ident` retained 但不自动 Group；
- unsupported/sensitive/forced 规则成立；
- Review/report/CRB 数量一致；
- multi-image identity 为 dataset/section/label；
- external assets 不暴露 private data；
- H5/BPCells 未回归；
- `anna_lena` 达到 12 metadata、8 Groups、6 sections、7 images/dataset。

- [ ] **步骤 6：只在审查产生修复时提交最终修复**

```bash
git add -u
git commit -m "fix(builder): address storage parity review"
```

如果无修复，不创建空 commit。

---

## 新会话启动提示词

将下面内容原样复制到新的 Codex 会话：

```text
请执行以下实现计划：
/Users/nuioi/projects/shiny/_wt_colleague_spatial_builder/docs/superpowers/plans/2026-08-12-builder-storage-metadata-parity.md

使用 superpowers:executing-plans，严格按任务顺序执行并在每个 commit 后检查。

重要边界：
1. 原 worktree /Users/nuioi/projects/shiny/_wt_colleague_spatial_builder 当前有用户未提交修改，不得 reset、checkout、清理或混入提交。
2. 按计划从 integration/colleague-spatial-builder 当前已提交 tip 创建隔离 worktree：
   /Users/nuioi/projects/shiny/_wt_builder_storage_metadata
3. 使用 feat/builder-storage-metadata-parity 分支。
4. 先完成 metadata retention，再完成 spatial external storage，再完成多图 UI。
5. 所有修复先写失败测试；不能跳过 Builder App privacy/topology tests。
6. 最后必须用 /Users/nuioi/Downloads/anna_lena 的真实数据完成 Builder UI 与命令行等价验收。
7. 不修改或删除原有 anna_lena 导出；验收使用 acceptance_cli 和 acceptance_builder 新目录。
```
