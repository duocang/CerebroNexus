# Builder Task 14 发布验收实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 用确定性 Builder 输入示例、精确 18 组合制品矩阵、代表性真实浏览器流程和发布生命周期回归完成 Builder 的发布验收，并同步最终英文文档、截图和 3.2.0 未发布版本三件套。

**架构：** 一个 catalog 同时驱动交互 ExampleAdapter 和序列化 SeuratFileAdapter；语义示例、制品矩阵、浏览器行为和发布生命周期分四层验证。只有新 RED 测试证明的 page-gate、默认 group/projection 或结果动作缺口允许进入运行时修改，发布仍完全由 coordinator/worker 既有边界负责。

**技术栈：** R、testthat、SeuratObject/Seurat、Shiny、shinytest2/chromote、HDF5Array、BPCells、pkgdown、air、Git。

---

## 文件结构

### 代码与 fixtures

- 修改 `inst/builder/io.R`：定义 catalog、fixture 路径和 gallery 投影。
- 修改 `inst/builder/build.R`：仅修复 RED 证明的 read-back page gate 和默认 group/projection 落地。
- 修改 `inst/builder/www/builder.js` 或 `inst/builder/app.R`：仅当结果动作浏览器 RED 证明现有 handler 不工作时修改。
- 修改 `data-raw/build_builder_fixtures.R`：确定性生成永久 Builder 输入。
- 修改 `data-raw/builder_fixtures.md`：记录已安装永久 fixture 与本地压力 fixture 的边界。
- 创建 `inst/builder/fixtures/spatial_multi_section.rds`。
- 创建 `inst/builder/fixtures/spatial_section_a.png`。
- 创建 `inst/builder/fixtures/spatial_section_b.png`。
- 创建 `inst/builder/fixtures/immune_tcr_hla.rds`。
- 创建 `inst/builder/fixtures/immune_tcr_only.rds`。
- 创建 `inst/builder/fixtures/immune_hla_only.rds`。
- 创建 `inst/builder/fixtures/immune_bcr_only.rds`。
- 创建 `inst/builder/fixtures/immune_metadata_tcr.rds`。
- 创建 `inst/builder/fixtures/immune_legacy_tcr.rds`。
- 创建 `inst/builder/fixtures/all_content.rds`。

`Basic PBMC` 继续使用 `inst/extdata/v1.4/pbmc_seurat.rds`，不复制。
invalid-content 只由测试 helper 构造，不进入用户 gallery。

### 测试

- 创建 `tests/testthat/helper-builder-end-to-end.R`：加载运行时、缓存不可变输入、冻结计划、执行/发布和启动代表性 App。
- 创建 `tests/testthat/test-builder-end-to-end.R`：catalog、adapter 等价、语义场景、18 组合、浏览器和生命周期契约。
- 修改 `tests/testthat/test-builder-build.R`：page gate 与默认值的最小纯回归。
- 修改 `tests/testthat/test-builder-browser.R`：只保留/扩展属于 Builder UI 结果动作的断言。
- 修改 `tests/testthat/test-smoke-production.R`：只扩展属于真实生成 App/HTTP 隐私的已有共享 fixture。
- 修改 `tests/testthat/test-builder-coordinator.R`：只扩展属于 ownership/release shrink 的既有断言。

### 用户文档与版本

- 修改 `README.md`。
- 修改 `vignettes/build_a_data_set_by_pointing.Rmd`。
- 替换 `vignettes/img/builder_start.png`。
- 替换 `vignettes/img/builder_loaded.png`。
- 替换 `vignettes/img/builder_alignment.png`。
- 替换 `vignettes/img/builder_overlay.png`。
- 视最终信息量删除或替换 `vignettes/img/builder_file_browser.png` 与 `vignettes/img/builder_colours.png`，并同步 vignette `resource_files`。
- 修改 `NEWS.md`、`DESCRIPTION`、`inst/app.R`。
- `_pkgdown.yml` 仅在文章路径/标题变化时修改；默认不改。

---

### 任务 0：保存当前分支并同步最新 upstream

**文件：** 无内容修改；只改变本地 refs 与提交基线。

- [ ] **步骤 1：确认起点干净并记录设计/计划提交**

运行：

```bash
git status --short
git log -3 --oneline
```

预期：工作树干净；最新提交包含 Task 14 规格和本计划，Task 13 提交
`e7484279` 仍在历史中。

- [ ] **步骤 2：创建精确备份引用**

运行：

```bash
git branch backup/cerebro-builder-pre-task14-20260806 HEAD
git rev-parse backup/cerebro-builder-pre-task14-20260806
```

预期：输出当前 pre-rebase HEAD。若引用已存在，先确认它恰好指向当前
HEAD；不删除或覆盖未知引用。

- [ ] **步骤 3：获取并检查 upstream**

运行：

```bash
git fetch upstream
git log --oneline HEAD..upstream/master
git show upstream/master:DESCRIPTION | sed -n '1,8p'
git show upstream/master:NEWS.md | sed -n '1,35p'
```

预期：确认 upstream 当前未发布版本；计划基线预计为 `3.2.0`。

- [ ] **步骤 4：rebase 并解决冲突**

运行：

```bash
git rebase upstream/master
```

冲突处理规则：

- 保留 upstream 的 benchmark/README/3.2.0 内容；
- 把 Builder NEWS 放进当前未发布 3.2.0，不覆盖历史 3.1.0；
- 保留 Task 0–13 的行为和 Task 14 规格/计划；
- 不在冲突中引入 Task 14 实现。

- [ ] **步骤 5：审计重放结果**

运行：

```bash
git rev-list --left-right --count HEAD...upstream/master
git range-diff \
  $(git merge-base backup/cerebro-builder-pre-task14-20260806 upstream/master)..backup/cerebro-builder-pre-task14-20260806 \
  upstream/master..HEAD
git diff --check upstream/master...HEAD
```

预期：behind 为 `0`；range-diff 只有 conflict integration 所需变化。

- [ ] **步骤 6：重跑 Task 13 回归作为新基线**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-(browser|ui-contract|rail|stages)", reporter = "summary")'
```

预期：0 failures。

---

### 任务 1：先建立失败的 catalog 与 fixture 契约

**文件：**

- 创建 `tests/testthat/helper-builder-end-to-end.R`
- 创建 `tests/testthat/test-builder-end-to-end.R`
- 修改 `inst/builder/io.R`
- 修改 `data-raw/build_builder_fixtures.R`
- 修改 `data-raw/builder_fixtures.md`
- 创建上方列出的 `inst/builder/fixtures/*`

- [ ] **步骤 1：创建运行时 helper**

在 `tests/testthat/helper-builder-end-to-end.R` 中定义：

```r
builder_e2e_source_runtime <- function(local = parent.frame()) {
  files <- c(
    "io.R", "spatial.R", "manifest.R", "content_tables.R",
    "content_immune.R", "content_spatial.R", "content.R", "profile.R",
    "inspect.R", "adapters.R", "recommend.R", "analysis.R", "build.R",
    "state.R", "plan.R", "app_bundle.R", "report.R", "publish.R",
    "coordinator.R"
  )
  for (file in files) {
    sys.source(builder_profile_inst_path("builder", file), envir = local)
  }
  invisible(local)
}

builder_e2e_catalog_ids <- c(
  "basic_pbmc", "spatial_multi_section", "immune_tcr_hla",
  "immune_tcr_only", "immune_hla_only", "immune_bcr_only",
  "immune_metadata_tcr", "immune_legacy_tcr", "all_content"
)
```

如果 source 顺序需要共享 Viewer/HLA contract，使用现有
`builder_profile_source_runtime()`，不要复制 contract 实现。

- [ ] **步骤 2：编写 catalog RED 测试**

在 `tests/testthat/test-builder-end-to-end.R` 中先加入：

```r
builder_e2e_source_runtime()

test_that("the permanent example catalog is explicit and offline", {
  catalog <- builder_example_catalog()
  ids <- vapply(catalog, `[[`, character(1), "id")
  expect_identical(ids, builder_e2e_catalog_ids)
  expect_length(unique(vapply(catalog, `[[`, character(1), "label")), 9L)
  expect_setequal(
    unique(vapply(catalog, `[[`, character(1), "provenance")),
    c("real", "synthetic")
  )
  expect_true(all(vapply(catalog, function(entry) {
    is.function(entry$make) &&
      is.character(entry$serialized_path) &&
      length(entry$serialized_path) == 1L &&
      file.exists(entry$serialized_path) &&
      !grepl("https?://", entry$serialized_path)
  }, logical(1))))
})
```

- [ ] **步骤 3：运行并确认 RED**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-end-to-end", reporter = "summary")'
```

预期：FAIL，明确报告 `builder_example_catalog()` 不存在。

- [ ] **步骤 4：实现 catalog 最小结构**

在 `inst/builder/io.R` 增加固定字段构造器：

```r
builder_example_record <- function(
  id,
  label,
  detail,
  provenance,
  make,
  serialized_path,
  expected_manifest,
  expected_dispositions,
  expected_pages,
  expected_supporting_content = character(),
  gallery_visible = TRUE
) {
  stopifnot(
    length(id) == 1L,
    provenance %in% c("real", "synthetic"),
    is.function(make),
    is.logical(gallery_visible),
    length(gallery_visible) == 1L
  )
  list(
    id = id,
    label = label,
    detail = detail,
    provenance = provenance,
    make = make,
    serialized_path = serialized_path,
    expected_manifest = expected_manifest,
    expected_dispositions = expected_dispositions,
    expected_pages = expected_pages,
    expected_supporting_content = expected_supporting_content,
    gallery_visible = gallery_visible
  )
}
```

定义 `builder_example_catalog()` 返回稳定顺序的九条记录；把
`builder_examples()` 改为只投影 `gallery_visible` 条目的
`id/label/detail/make`，保持现有 UI 接口不变。

- [ ] **步骤 5：改造 generator 并生成永久 fixtures**

`data-raw/build_builder_fixtures.R` 必须：

- 把永久小 fixture 写到 `inst/builder/fixtures/`；
- 把 15,000-cell 压力 fixture 和可选 `.qs/.qs2` 仍写到 gitignored 的
  `data-raw/builder_fixtures/`；
- 用一个保存/恢复 `.Random.seed` 的 wrapper 包住所有随机生成；
- 固定 cell/feature/barcode/section 顺序；
- 生成两张小 PNG；
- six immune/HLA fixtures 使用同一基础对象但独立保存；
- `all_content.rds` 使用有效 zero-based integer Trekker cluster contract；
- 不访问网络。

运行：

```bash
Rscript data-raw/build_builder_fixtures.R
find inst/builder/fixtures -maxdepth 1 -type f -print | sort
du -h inst/builder/fixtures/* | sort -h
```

预期：九个新 `.rds`/图片文件存在；总大小保持适合随包安装，不生成
未计划的大型文件。

- [ ] **步骤 6：补 adapter 等价与 RNG RED/GREEN 测试**

加入：

```r
test_that("example and serialized adapters converge after inspect", {
  for (entry in builder_example_catalog()) {
    made <- entry$make()
    expect_null(made$error, info = entry$id)
    from_example <- builder_adapter_inspect(
      builder_example_adapter(entry$id, made$object)
    )
    from_file <- builder_adapter_inspect(
      builder_seurat_file_adapter(entry$serialized_path)
    )
    expect_identical(from_example$legacy_profile, from_file$legacy_profile,
      info = entry$id
    )
    expect_identical(from_example$levels, from_file$levels, info = entry$id)
    expect_identical(
      .builder_normalize_profile_sources(from_example$profile),
      .builder_normalize_profile_sources(from_file$profile),
      info = entry$id
    )
  }
})

test_that("example constructors are deterministic without leaking RNG", {
  set.seed(90210)
  before <- .Random.seed
  first <- lapply(builder_example_catalog(), function(entry) serialize(entry$make()$object, NULL))
  expect_identical(.Random.seed, before)
  second <- lapply(builder_example_catalog(), function(entry) serialize(entry$make()$object, NULL))
  expect_identical(first, second)
  expect_identical(.Random.seed, before)
})
```

- [ ] **步骤 7：验证 GREEN**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-(end-to-end|adapters|profile|content)", reporter = "summary")'
```

预期：0 failures；无网络访问。

---

### 任务 2：用真实语义示例暴露并修复 page gate 与默认值

**文件：**

- 修改 `tests/testthat/helper-builder-end-to-end.R`
- 修改 `tests/testthat/test-builder-end-to-end.R`
- 修改 `tests/testthat/test-builder-build.R`
- 修改 `inst/builder/build.R`

- [ ] **步骤 1：实现测试侧完整流程 helper**

在 helper 中定义以下稳定接口，内部复用现有 fixture/state/plan 工厂，
不复制生产规则：

```r
builder_e2e_inspect <- function(entry, source = c("example", "file"))
builder_e2e_entry <- function(entry, source = c("example", "file"), overrides = list())
builder_e2e_plan <- function(entries, out_dir, make_app, overwrite = FALSE, app_options = list())
builder_e2e_execute <- function(plan, snapshots, publish = TRUE)
builder_e2e_case_label <- function(fixture, backend, content, output)
```

`builder_e2e_execute()` 必须执行 coordinator prepare → worker-equivalent
`builder_execute_plan()` → coordinator publish，并返回 `plan/handle/result/final`；
不得绕过 verify/read-back/publication。

- [ ] **步骤 2：编写 immune/HLA page matrix RED**

加入：

```r
immune_pages <- list(
  immune_tcr_hla = c("immune_repertoire", "hla_tcr_motifs"),
  immune_tcr_only = c("immune_repertoire", "hla_tcr_motifs"),
  immune_hla_only = character(),
  immune_bcr_only = "immune_repertoire",
  immune_metadata_tcr = c("immune_repertoire", "hla_tcr_motifs"),
  immune_legacy_tcr = c("immune_repertoire", "hla_tcr_motifs")
)

test_that("real immune fixtures produce the exact Viewer pages", {
  catalog <- builder_example_catalog()
  for (id in names(immune_pages)) {
    entry <- catalog[[match(id, vapply(catalog, `[[`, character(1), "id"))]]
    built <- builder_e2e_execute(
      builder_e2e_plan(
        list(builder_e2e_entry(entry)),
        withr::local_tempdir(),
        make_app = FALSE
      ),
      snapshots = builder_e2e_snapshots(entry)
    )
    expect_setequal(
      built$result$verifications[[id]]$visible_pages,
      immune_pages[[id]],
      info = id
    )
  }
})
```

- [ ] **步骤 3：确认 RED 是现有错误 gate**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-end-to-end", reporter = "summary")'
```

预期：至少 `immune_tcr_only` 和 `immune_bcr_only` 在 Motif page 判定上失败；
失败必须来自 read-back gate，不是 malformed fixture。

- [ ] **步骤 4：最小修复 read-back page gate**

在 `inst/builder/build.R` 中让 `.builder_crb_visible_pages()` 使用
`content_immune.R` 已有 `.builder_immune_detect_chains()` 解析实际
`CTgene` 内容：

```r
immune <- .builder_build_field(object, "immune_repertoire")
chains <- unique(unlist(lapply(immune %||% list(), function(table) {
  genes <- if (is.data.frame(table)) table$CTgene else NULL
  if (is.null(genes)) character() else .builder_immune_detect_chains(genes)
})))
has_tcr <- length(intersect(chains, c("TRA", "TRB"))) > 0L
```

Motif page 只由 `has_tcr` 决定；Immune page 仍由有效 immune repertoire
决定。不要把 HLA 当作 Motif page 的必要条件。

- [ ] **步骤 5：增加默认 group/projection RED**

用一个含至少两个 group、两个 projection 且默认值不是第一项的 fixture：

```r
test_that("confirmed defaults control the built Viewer order", {
  entry <- builder_e2e_entry(
    builder_example_catalog()[[1L]],
    overrides = list(
      included_groups = c("sample", "cell_type"),
      default_group = "cell_type",
      included_projections = c("tsne", "umap"),
      default_projection = "umap"
    )
  )
  built <- builder_e2e_execute(
    builder_e2e_plan(list(entry), withr::local_tempdir(), make_app = TRUE),
    builder_e2e_snapshots(entry)
  )
  verification <- built$result$verifications[[entry$id]]
  expect_identical(verification$default_group, "cell_type")
  expect_identical(verification$default_projection, "umap")
  expect_identical(built$result$app_verification$defaults[[entry$name]],
    list(group = "cell_type", projection = "umap")
  )
})
```

- [ ] **步骤 6：运行并确认默认值 RED**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-(end-to-end|build|app-bundle)", reporter = "summary")'
```

预期：default projection 或 app startup contract 不存在/错误；不要先改测试预期。

- [ ] **步骤 7：实现最小默认值落地**

优先使用当前 Viewer 已消费的有序 group/projection contract：在
`.builder_build_export()` 前以一个纯 helper 把确认的默认值移到 included
集合首位，同时保持集合成员不变：

```r
.builder_default_first <- function(values, selected, label) {
  values <- as.character(values)
  if (length(selected) != 1L || !selected %in% values) {
    stop("The frozen default ", label, " is not included.", call. = FALSE)
  }
  c(selected, values[values != selected])
}
```

groups 与 projections 都使用该 helper；CRB read-back 增加默认值字段并验证。
若 rebase 后 Viewer 已提供显式 per-dataset defaults contract，则使用该
contract，不再依赖排序；测试预期保持不变。

- [ ] **步骤 8：验证语义层 GREEN**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-(end-to-end|build|app-bundle|content-immune|plan)", reporter = "summary")'
```

预期：0 failures；六种 page matrix 和非首项默认值全部通过。

---

### 任务 3：实现精确 18 组合制品矩阵

**文件：**

- 修改 `tests/testthat/helper-builder-end-to-end.R`
- 修改 `tests/testthat/test-builder-end-to-end.R`
- 必要时修改 `inst/builder/build.R` 或 `inst/builder/app_bundle.R`，但只处理矩阵 RED 证明的直接缺口

- [ ] **步骤 1：定义唯一矩阵**

在 helper 中加入：

```r
builder_e2e_artifact_matrix <- function() {
  expand.grid(
    backend = c("embedded", "h5", "bpcells"),
    content = c("plain", "histology", "trekker"),
    output = c("crb_only", "generated_app"),
    stringsAsFactors = FALSE
  )
}
```

断言 `nrow(...) == 18L` 且无重复 coordinate。

- [ ] **步骤 2：先写 18 组合 RED 测试**

```r
test_that("all eighteen artifact combinations build and reopen", {
  matrix <- builder_e2e_artifact_matrix()
  expect_equal(nrow(matrix), 18L)
  for (index in seq_len(nrow(matrix))) {
    case <- matrix[index, ]
    label <- builder_e2e_case_label(
      "artifact", case$backend, case$content, case$output
    )
    built <- builder_e2e_artifact_case(case)
    expect_identical(built$result$state, "success", info = label)
    expect_true(built$result$published, info = label)
    expect_identical(
      isTRUE(built$result$app_verified),
      identical(case$output, "generated_app"),
      info = label
    )
    expect_identical(
      built$verification$expression_backend,
      case$backend,
      info = label
    )
  }
})
```

`builder_e2e_artifact_case()` 每次使用独立 stage/release；只缓存不可变
fixture bytes。H5/BPCells 依赖缺失要 FAIL 并标明 coordinate，不得 skip。

- [ ] **步骤 3：运行并确认 RED**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-end-to-end", reporter = "summary")'
```

预期：失败 coordinate 明确为 `fixture/backend/content/output`；先区分测试
helper 缺失和生产缺口。

- [ ] **步骤 4：补齐最小实现**

只修复矩阵暴露的直接问题：backend descriptor、sidecar 相对路径、lazy
reopen、histology/Trekker 附加、stage→release remap 或 generated App
verification。不要复制已有 app-bundle/coordinator 实现。

- [ ] **步骤 5：验证矩阵 GREEN 与既有安全契约**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-(end-to-end|app-bundle|build|coordinator|spatial)", reporter = "summary")'
```

预期：18/18 通过；0 failures。

---

### 任务 4：补真实浏览器与发布生命周期验收

**文件：**

- 修改 `tests/testthat/test-builder-end-to-end.R`
- 修改 `tests/testthat/test-builder-browser.R`
- 修改 `tests/testthat/test-smoke-production.R`
- 修改 `tests/testthat/test-builder-coordinator.R`
- 仅在 RED 证明时修改 `inst/builder/app.R`、`inst/builder/www/builder.js` 或直接相关运行时文件

- [ ] **步骤 1：建立共享代表性 App fixture**

复用 `tests/testthat/helper-app-privacy.R` 的真实 private App builder。生成：

- 一个三数据集 App：Basic、TCR-only、All-content；
- 一个 Spatial App：Builder-normalized embedded histology；
- 自动 initial 与显式 initial 两种 plan。

每个昂贵 fixture 在文件内只构建一次，消费者只读。

- [ ] **步骤 2：写 Viewer browser RED**

用 `AppDriver` 验证：

```r
test_that("generated Viewer follows pages defaults and privacy", {
  app <- builder_e2e_viewer_driver("three_dataset_explicit")
  on.exit(app$stop(), add = TRUE)
  expect_identical(app$get_value(input = "data_set"), "TCR only")
  expect_identical(app$get_value(input = "groups_selected_group"), "cell_type")
  expect_identical(app$get_value(input = "overview_projection_to_display"), "umap")
  builder_e2e_expect_page(app, "hla_tcr_motifs", visible = TRUE)
  builder_e2e_select_dataset(app, "Basic PBMC")
  builder_e2e_expect_page(app, "hla_tcr_motifs", visible = FALSE)
  builder_e2e_select_dataset(app, "All content")
  builder_e2e_expect_page(app, "hla_tcr_motifs", visible = TRUE)
})
```

实际 input ID 以 rebase 后 Viewer DOM 为准；不要通过配置文本替代浏览器
读值。

- [ ] **步骤 3：写 HTTP/Spatial RED**

复用现有 HTTP helper，断言 CRB/H5/BPCells、`private-data`、
`spatial-assets` 直接 URL 返回 404；Builder-normalized histology 是嵌入
data URI、保留 bounds/transforms 并在浏览器渲染。保留 direct
`createShinyApp()` 外部图片字节一致 + allowlist render + URL 404 既有测试。

- [ ] **步骤 4：写 Builder 结果动作 RED**

在 Builder browser 中等待真实 success card，断言四个原生按钮可 Tab
到达。Copy Path/Copy Report 点击后拦截 `builder_copy_text` custom message 并
核对 final release/report 路径；Open App/Reveal Folder 的平台副作用继续由
`test-builder-stages.R` 注入边界验证，浏览器只验证事件可达和按钮状态，
不启动 Finder/外部浏览器。

- [ ] **步骤 5：写发布生命周期 RED**

在同一 release 中执行：

```text
two datasets + generated App
  -> one dataset + CRB-only
```

断言旧 owned CRB/App 被删除、新 CRB 与 ownership 精确匹配。分别建立无
record 旧 release、malformed record、顶层 foreign、嵌套 foreign；每个都
必须阻断替换并保持原 bytes/identity。

- [ ] **步骤 6：运行 RED 并最小修复**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-(end-to-end|browser|coordinator|stages)|smoke-production", reporter = "summary")'
```

只修复真实产品 RED；测试驱动/等待条件错误必须修测试，不得改变产品语义
迎合 flaky 断言。

- [ ] **步骤 7：验证浏览器/生命周期 GREEN**

运行相同命令，预期 0 failures。随后运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder", reporter = "summary")'
```

预期所有 Builder 分组通过。

- [ ] **步骤 8：提交实现前完整门禁**

运行：

```bash
node --check inst/builder/www/builder.js
git diff --check
scripts/precheck.sh
```

另从新建 source tarball 安装到 clean temporary library，加载
`CerebroNexus`，mock `shiny::runApp` 后调用 `launchCerebroBuilder()`，确认
launcher 解析的是该临时库中的 `builder/app.R`。

预期：完整测试 0 failures；fresh-package R CMD check 0 errors；只有经证实
的既有 warning/NOTEs；pkgdown 通过；installed launcher smoke 通过。

- [ ] **步骤 9：提交实现与测试**

运行：

```bash
git add inst/builder data-raw tests/testthat
git commit -m "feat(builder): complete example matrix"
```

检查提交中不含 README/vignette/version，也不含 `.superpowers/` 原型。

---

### 任务 5：重写最终英文文档、截图和未发布版本三件套

**文件：**

- 修改 `README.md`
- 修改 `vignettes/build_a_data_set_by_pointing.Rmd`
- 修改/删除上方列出的 `vignettes/img/builder_*.png`
- 修改 `NEWS.md`
- 修改 `DESCRIPTION`
- 修改 `inst/app.R`

- [ ] **步骤 1：先写文档契约 RED**

在 `tests/testthat/test-builder-end-to-end.R` 增加静态契约：

- README 包含四阶段名称和 vignette 链接；
- vignette 明确 CRB-only 与 generated App；
- vignette 同时出现 `private-data`、`viewer_bundle_assets`、HTTP exposure；
- vignette说明 snapshot disk cost、Needs decision、rollback/recovery、ownership migration；
- vignette不再声称 analysis failure 被 silently skipped；
- 版本三件套一致且 NEWS 顶部是当前未发布版本；
- 所有 `resource_files` 图片存在。

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-end-to-end", reporter = "summary")'
```

预期：旧 README/vignette 文案和版本布局触发明确失败。

- [ ] **步骤 2：精简 README**

保留：Builder 定位、启动命令、Import and Inspect → Core setup → Enhance
content → Review and Build、主要本地/安全边界、vignette 链接。删除过时的
card-grid、旧三示例和旧 fixture generator 描述。

- [ ] **步骤 3：按最终四阶段重写 vignette**

必须覆盖规格的输出、隐私、snapshot、恢复、ownership 与 phase-one
限制；把 analysis failure 明确写为 `Needs decision`；结果章节说明 Open
App、Reveal Folder、Copy Path、Copy Report。

- [ ] **步骤 4：从最终安装包捕获并检查截图**

先 `R CMD INSTALL .` 到临时库，再用该安装包启动 Builder。捕获：

- source/gallery；
- Core/preview；
- Enhance spatial/content；
- exact Review；
- success/actions；
- 390×844 Dataset Manager（仅在它补充桌面截图信息时保留）。

使用现有文件名时确保 vignette 语义对应；不再需要的图片从
`resource_files` 和仓库同时移除。截图不得包含用户名、绝对路径或临时
token。用 `view_image` 逐张检查文字裁切、焦点、隐私和布局。

- [ ] **步骤 5：同步 3.2.0 未发布版本**

以 rebase 后 upstream 为准。若顶部仍为未发布 `3.2.0`：

- `DESCRIPTION` 保持/设为 `Version: 3.2.0`；
- `inst/app.R` 的 `cerebro_version` 设为 `3.2.0`；
- Builder Task 13/14 条目放在顶部 `# CerebroNexus 3.2.0`；
- 不改写 `# CerebroNexus 3.1.0` 历史内容。

若 upstream 已发布 3.2.0，则三处统一为下一个未发布版本并记录依据。

- [ ] **步骤 6：验证文档 GREEN 和 stale references**

运行：

```bash
Rscript -e 'devtools::test(".", filter = "builder-end-to-end", reporter = "summary")'
rg -n "responsive card grid|logged and skipped|No cancel button|three examples|3\.1\.0" README.md vignettes/build_a_data_set_by_pointing.Rmd NEWS.md DESCRIPTION inst/app.R
git diff --check
```

预期：文档契约通过；搜索结果只剩有意历史记录，当前文档无旧 Builder
描述。

- [ ] **步骤 7：提交文档前完整门禁**

运行：

```bash
scripts/precheck.sh
```

再次从 fresh source tarball 安装到 clean temporary library，执行 package
load + launcher smoke。预期与任务 4 的门禁标准相同。

- [ ] **步骤 8：提交文档与版本**

运行：

```bash
git add DESCRIPTION NEWS.md README.md inst/app.R \
  vignettes/build_a_data_set_by_pointing.Rmd vignettes/img
git commit -m "docs(builder): document guided workflow"
```

---

### 任务 6：最终规格审查、质量审查和分支收尾

**文件：** 默认不修改；发现问题时回到所属任务做 RED/GREEN 修复。

- [ ] **步骤 1：逐项规格覆盖审查**

对照
`docs/superpowers/specs/2026-08-06-builder-task14-release-acceptance-design.md`
检查：catalog、9 个有效示例、invalid、六种 immune/HLA、18 组合、默认
dataset/group/projection、page has→no→has、palettes/upload/results、HTTP
privacy、embedded histology、release shrink/foreign/ownership、docs/version。

- [ ] **步骤 2：质量与对抗审查**

重点检查：manifest truthfulness、file-backed source independence、
single-flight/retry、publication crash recovery、generated-app privacy、真实
Viewer page gate、responsive reachability、keyboard/focus、fixture RNG、测试
缓存是否跨 case 泄漏可变状态。

Critical 或 Important finding 必须先写/补失败测试、最小修复、跑 focused
GREEN，再重新执行最终门禁。

- [ ] **步骤 3：最终证据**

运行：

```bash
air format --check .
node --check inst/builder/www/builder.js
git diff --check upstream/master...HEAD
git status --short
git log --oneline upstream/master..HEAD
git diff --stat upstream/master...HEAD
git branch -r --contains HEAD
```

预期：air-clean、JS 语法通过、diff check 通过、工作树干净、Task 14 两个
实现/文档提交存在、没有远端包含最终 HEAD。

- [ ] **步骤 4：记录本地项目里程碑**

更新项目 `.loci/memory.md`、brain project index 与 activity ledger：Task 14
完成、最终提交、精确测试/check/pkgdown/smoke 结果、分支 local/unpushed。
这些本地记忆文件不得进入产品提交。

---

## 执行方式

按用户决定，使用 `gpt-5.6-sol`、`medium` 通过
`superpowers:subagent-driven-development` 执行。主代理负责在每个逻辑
提交前检查范围与证据，并独立完成最终规格/质量审查；任何远端 push 仍需
新的明确授权。
