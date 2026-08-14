# 当前 integration 导出等价性实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 用当前 integration 源码重新生成 Builder/Script 交付物，并以受 Git 追踪的合成回归测试证明两条导出链的语义、安全和可搬移性。

**架构：** 在现有 Builder E2E fixture 上增加一个小型、确定性的导出等价 fixture；共享审查 helper 负责按语义键比较 CRB、图片、坐标和嵌套环境。产品测试只写临时目录，Downloads 交付目录由独立再生脚本消费当前源码快照并输出审查报告。

**技术栈：** R、testthat、Cerebro R6、SeuratObject、Shiny、Builder plan/bundle pipeline、Chromote、Git archive。

---

## 文件职责

- 创建 `tests/testthat/helper-export-equivalence.R`：构造确定性 CRB、图片和预期值，并提供语义比较及深度路径扫描函数。
- 创建 `tests/testthat/test-export-equivalence-current.R`：覆盖 Builder/Script 双路径、legacy hardening、非空 extra material、搬移启动和浏览器映射。
- 修改 `tests/testthat/helper-generated-app-e2e.R`：仅补充复用真实 Builder plan/bundle 的窄接口，不复制 Builder 实现。
- 创建 `scripts/audit-export-equivalence.R`：对两个已生成交付目录输出机器可读和 Markdown 审查结果，失败时非零退出。
- 修改 `/Users/nuioi/Downloads/anna_lena/export_shiny_app.R`：在交付 fixture 中加入确定性非空 extra table，并记录当前源码 SHA。
- 修改 `/Users/nuioi/Downloads/anna_lena` 下的 Builder 再生输入：通过当前真实 Builder pipeline 生成对应 App，不后处理结果。

### 任务 1：锁定当前导出基线和失败断言

**文件：**
- 创建：`tests/testthat/helper-export-equivalence.R`
- 创建：`tests/testthat/test-export-equivalence-current.R`

- [ ] **步骤 1：编写失败测试，要求两个生成 App 记录当前源码 revision**

```r
test_that("generated apps identify the current source revision", {
  apps <- export_equivalence_fixture_apps()
  expected <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  expect_identical(app_source_revision(apps$builder), expected)
  expect_identical(app_source_revision(apps$script), expected)
})
```

- [ ] **步骤 2：运行并确认红灯**

运行：`Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-export-equivalence-current.R")'`

预期：FAIL，原因是 revision manifest 尚不存在。

- [ ] **步骤 3：实现最小 fixture 和 revision manifest 写入**

```r
write_export_revision <- function(app_dir, revision) {
  saveRDS(list(schema_version = 1L, source_revision = revision),
          file.path(app_dir, "export-provenance.rds"))
}
```

- [ ] **步骤 4：运行测试确认通过**

运行同上，预期该测试 PASS。

- [ ] **步骤 5：提交**

```bash
git add tests/testthat/helper-export-equivalence.R tests/testthat/test-export-equivalence-current.R
git commit -m "test(export): establish current revision fixture"
```

### 任务 2：证明非空数据语义等价

**文件：**
- 修改：`tests/testthat/helper-export-equivalence.R`
- 修改：`tests/testthat/test-export-equivalence-current.R`

- [ ] **步骤 1：加入明确 expected values 的非空 extra table fixture**

```r
expected_extra <- data.frame(
  cell_barcode = c("cell_a", "cell_b"),
  score = c(1.25, 9.5),
  note = c("left", "right"),
  stringsAsFactors = FALSE
)
crb$addExtraTable("audit_expected", expected_extra)
```

- [ ] **步骤 2：先断言现有双路径比较失败**

```r
expect_export_semantics_equal(builder_crb, script_crb,
                              expected_extra = expected_extra)
```

运行聚焦测试；预期 FAIL，指出 Builder 或 Script 尚未携带 `audit_expected`。

- [ ] **步骤 3：让两个 fixture 导出路径都消费同一语义输入**

实现时按 cell barcode、feature name、metadata column、projection rowname 和 extra table key 对齐；不按文件名或列表顺序比较。

- [ ] **步骤 4：验证 cells/features/metadata/groups/projections/expression/extra table 全部 PASS**

运行：`Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-export-equivalence-current.R", filter="non-empty")'`

- [ ] **步骤 5：提交**

```bash
git add tests/testthat/helper-export-equivalence.R tests/testthat/test-export-equivalence-current.R
git commit -m "test(export): compare non-empty semantic content"
```

### 任务 3：证明多图片 target 和 legacy hardening

**文件：**
- 修改：`tests/testthat/helper-export-equivalence.R`
- 修改：`tests/testthat/test-export-equivalence-current.R`
- 复用：`tests/testthat/helper-spatial-helpers.R`

- [ ] **步骤 1：构造同 basename、不同 dataset/FOV/label 的彩色 PNG**

```r
targets <- data.frame(
  dataset = c("ds_a", "ds_a", "ds_b"),
  fov = c("fov_1", "fov_1", "fov_2"),
  label = c("DAPI", "H&E", "DAPI"),
  basename = "image.png",
  rgb = c("ff0000", "00ff00", "0000ff")
)
```

- [ ] **步骤 2：写失败断言，要求完整四维 target 唯一且 decoded bytes 对应**

```r
expect_equal(nrow(unique(manifest[c("dataset", "fov", "label", "path")])), 3L)
expect_identical(external_png_md5(builder, targets), embedded_png_md5(script, targets))
```

- [ ] **步骤 3：同时加入旧 CRB 和 legacy multi-path 声明测试**

断言 `.isPreSpatialCerebroV1_3()`、逐 CRB preflight、`length(declaration) >= 1L` 与四维 target 均在真实导出调用中被执行；测试必须在临时目录使用至少两个 legacy path。

- [ ] **步骤 4：运行聚焦测试并修复发现的产品代码问题**

运行：`Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-export-equivalence-current.R", filter="image|legacy")'`

预期：3 个 semantic targets 均唯一、图片字节对应、legacy 输入安全迁移。

- [ ] **步骤 5：提交**

```bash
git add tests/testthat/helper-export-equivalence.R tests/testthat/test-export-equivalence-current.R R/createShinyApp.R R/spatial_image_manifest.R inst/builder/app_bundle
git commit -m "test(export): cover hardened spatial targets"
```

仅在测试揭示真实缺陷时加入产品文件；否则提交中不得包含无关产品改动。

### 任务 4：证明坐标约定和 Viewer 渲染选择

**文件：**
- 修改：`tests/testthat/helper-export-equivalence.R`
- 修改：`tests/testthat/test-export-equivalence-current.R`
- 复用：`tests/testthat/test-coordinated-views-browser.R`

- [ ] **步骤 1：定义非对称坐标、bounds-center pivot 和精确 expected values**

```r
source <- data.frame(x = c(0, 2, 7), y = c(1, 5, 9), row.names = c("a", "b", "c"))
expected <- expected_bounds_center_transform(source, rotation_degrees = 90,
                                             scale = 2,
                                             y_axis = "source-native")
```

- [ ] **步骤 2：先断言 source 顺序打乱仍按 cell ID 得到同一结果**

比较 tolerance 固定为 `1e-10`，并显式断言 pivot、轴约定、source/final fingerprint。

- [ ] **步骤 3：浏览器遍历全部 dataset/FOV/label**

对每个选择执行：选 dataset、选 FOV、选 label，读取 active semantic target、canvas image fingerprint 和实际 plotted coordinate extent；不得仅检查 HTTP 200。

- [ ] **步骤 4：运行浏览器聚焦测试**

运行：`Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-export-equivalence-current.R", filter="coordinate|viewer")'`

预期：所有 target 的 active key、图片 fingerprint、坐标 extent 与 expected fixture 一致。

- [ ] **步骤 5：提交**

```bash
git add tests/testthat/helper-export-equivalence.R tests/testthat/test-export-equivalence-current.R
git commit -m "test(viewer): verify spatial export alignment"
```

### 任务 5：证明序列化安全和可搬移启动

**文件：**
- 修改：`tests/testthat/helper-export-equivalence.R`
- 修改：`tests/testthat/test-export-equivalence-current.R`

- [ ] **步骤 1：实现带 cycle guard 的深度字符扫描器**

扫描普通字段、attributes、S4 slots、R6 environment、函数 environment 及 parent chain；禁止模式包括 `/Users/`、`/tmp/`、worktree 名和 builder library 临时名。

- [ ] **步骤 2：用注入绝对路径的 closure fixture 验证测试先失败**

```r
bad <- local({ leaked_path <- "/Users/example/private"; function() leaked_path })
expect_snapshot(export_path_leaks(bad), cran = TRUE)
```

- [ ] **步骤 3：验证真实 config/CRB/App 扫描结果为空**

扫描不因 namespace 中普通包安装路径误报；只报告会被序列化或运行时读取的绑定。

- [ ] **步骤 4：复制两个 App 到新建临时父目录并串行启动**

使用 `callr::r_bg()`，工作目录设为移动后的 App 根；等待 `Listening on`，HTTP GET 数据页和 Linked views，然后停止进程。另加一个从错误 CWD 启动的契约测试，明确其失败属于调用方式限制。

- [ ] **步骤 5：提交**

```bash
git add tests/testthat/helper-export-equivalence.R tests/testthat/test-export-equivalence-current.R
git commit -m "test(export): verify serialized portability"
```

### 任务 6：刷新当前 integration 交付物并独立审查

**文件：**
- 创建：`scripts/audit-export-equivalence.R`
- 修改：`/Users/nuioi/Downloads/anna_lena/export_shiny_app.R`
- 重新生成：`/Users/nuioi/Downloads/anna_lena/packages/CerebroNexus`
- 重新生成：`/Users/nuioi/Downloads/anna_lena/export/builder/cerebro_app`
- 重新生成：`/Users/nuioi/Downloads/anna_lena/export/script/cerebro_app`

- [ ] **步骤 1：在 `/tmp` 保存旧交付清单和 SHA-256**

清单覆盖 package snapshot、两个 App、CRB、config 和 spatial assets。

- [ ] **步骤 2：从当前 HEAD 创建纯净 snapshot**

使用 `git archive HEAD`，确保没有 `.git`、`.superpowers`、`Rplots.pdf` 或 `.new.png`。

- [ ] **步骤 3：运行 Script 和真实 Builder 导出**

两条路径必须独立消费输入，不得复制对方 CRB、config 或 App 文件。

- [ ] **步骤 4：运行机器审查脚本**

```bash
Rscript scripts/audit-export-equivalence.R \
  --builder /Users/nuioi/Downloads/anna_lena/export/builder/cerebro_app \
  --script /Users/nuioi/Downloads/anna_lena/export/script/cerebro_app \
  --source-revision "$(git rev-parse HEAD)"
```

预期：退出 0，并输出全部语义断言、路径扫描和搬移启动结果。

- [ ] **步骤 5：提交受追踪的审查脚本**

```bash
git add scripts/audit-export-equivalence.R
git commit -m "test(export): add semantic delivery auditor"
```

### 任务 7：最终验证和交接

**文件：**
- 检查全部已改文件

- [ ] **步骤 1：运行聚焦测试**

```bash
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-export-equivalence-current.R", reporter="summary")'
```

- [ ] **步骤 2：运行 JavaScript 语法检查**

```bash
node --check inst/viewer/www/coordviews.js
node --check inst/builder/www/builder.js
```

- [ ] **步骤 3：运行一次完整测试套件**

```bash
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary")'
```

预期：0 failures、0 errors；记录 warnings/skips 的准确数量和原因。

- [ ] **步骤 4：检查历史和工作树**

```bash
git diff --check HEAD~6..HEAD
git status --short
git log --oneline --decorate -10
```

预期：diff check 无输出、工作树 clean、没有 merge commit 或无关文件。

- [ ] **步骤 5：形成最终审查结论**

分别陈述已证明、高可信推断、未验证项；报告生成物 exact SHA 和当前 integration HEAD，不推送。
