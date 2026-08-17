# Seurat Omnibus Builder Fixture 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 删除 Builder 现有示例矩阵，改为一个可序列化、可重新读取并走真实 Seurat adapter 的 Xenium-style All content fixture。

**架构：** 将 fixture 生成职责保留在 `inst/builder/io.R` 的纯构造函数中，由 `data-raw/build_builder_fixtures.R` 生成一个 RDS 和五张 PNG。Gallery/catalog 只投影该记录；测试从产品行为出发验证 Seurat、FOV、sidecar、Trekker 和 adapter/build 合约。

**技术栈：** R、Seurat/SeuratObject、Matrix、png、testthat、Builder adapter/build runtime。

---

## 文件职责

- 修改 `inst/builder/io.R`：构造单一 Seurat Omnibus fixture、组织图、catalog 和 gallery。
- 修改 `data-raw/build_builder_fixtures.R`：调用新的单 fixture writer。
- 更新 `inst/builder/fixtures/`：只保留 `all_content.rds` 与五张 section PNG。
- 修改 `tests/testthat/test-builder-end-to-end.R`：替换九示例 catalog/fixture 合约。
- 修改 `tests/testthat/test-builder-example-registry.R`：锁定单一 gallery 记录。
- 修改与旧 fixture id 直接耦合的 Builder tests/helpers：改用 `all_content` 或局部自建对象，不删除生产能力测试。

### 任务 1：用失败测试锁定单一 Seurat fixture

- [ ] 修改 `tests/testthat/test-builder-end-to-end.R`，断言 catalog id 仅为 `all_content`，对象为 Seurat，病人 section 数为 2/3/1，reductions 为 `pca/umap/tsne`，六个 FOV 均有 centroids，禁止的分析 `@misc` 字段不存在，Trekker barcodes 属于对象。
- [ ] 修改 `tests/testthat/test-builder-example-registry.R`，断言静态目录仅包含 `all_content`。
- [ ] 运行 `Rscript -e 'devtools::test(filter = "builder-(end-to-end|example-registry)$")'`，确认测试因旧 catalog 和旧 fixture 结构失败。
- [ ] 提交测试：`test(builder): require one Seurat omnibus fixture`。

### 任务 2：实现紧凑的 Omnibus Seurat 构造器

- [ ] 在 `inst/builder/io.R` 中以固定 seed 构造稀疏 marker-driven counts、标准 metadata 和归一化 data。
- [ ] 用 `CreateDimReducObject()` 添加对齐的 PCA、UMAP、t-SNE。
- [ ] 用 `CreateFOV()`/`CreateCentroids()` 添加 A1/A2、B1/B2/B3、C1 六个原生 FOV；每个 cell 只属于一个 section。
- [ ] 从现有 Trekker fixture 读取结构模板，裁剪并重写 barcode/坐标/字段，使其严格对齐当前 Seurat cells；不写入 marker、most expressed、enrichment、trajectory 或 extra material。
- [ ] 运行任务 1 测试，确认对象合约通过。
- [ ] 提交：`feat(builder): create Seurat omnibus fixture`。

### 任务 3：生成五张 sidecar 并收缩 gallery/catalog

- [ ] 将 tissue PNG writer 扩展为五个不同尺寸/seed 的 deterministic sidecars：A1/A2/B1/B2/B3；不生成 C1 图片。
- [ ] 将 `builder_write_permanent_fixtures()` 收缩为一个 `all_content.rds` 和五张 PNG，并在写入前 round-trip 校验 RDS。
- [ ] 将 `builder_example_catalog()` 和 `builder_example_directory()` 收缩为单一 `all_content` 记录，expected reductions/spatial/Trekker 与新对象一致，precomputed analysis pages 不再声明。
- [ ] 更新 `data-raw/build_builder_fixtures.R` 并运行它刷新 `inst/builder/fixtures/`。
- [ ] 删除八个旧 fixture RDS 和旧两张 spatial PNG；确认 fixture 目录只有七个预期成员（RDS、五 PNG，以及目录本身不计）。
- [ ] 运行任务 1 测试并提交：`feat(builder): expose only Seurat omnibus example`。

### 任务 4：解耦旧示例测试并验证真实上传路径

- [ ] 搜索所有旧 example ids；产品能力 unit tests 保留局部 fixture，只有 gallery/catalog/e2e 测试改为 `all_content`。
- [ ] 更新 resource-root、determinism、adapter parity 和 fixture script assertions，全部以单一 RDS + 五 PNG 为准。
- [ ] 添加 `builder_seurat_file_adapter(record$serialized_path)` 与 gallery adapter profile 等价断言。
- [ ] 添加 freeze/build smoke test，验证新对象通过现有 Seurat snapshot、plan 和 build 路径。
- [ ] 运行 `Rscript -e 'devtools::test(filter = "builder-(example|end-to-end|adapters|content-spatial|rail|loading)")'`。
- [ ] 提交：`test(builder): align contracts with Seurat omnibus`。

### 任务 5：最终验证与运行中的 Builder

- [ ] 运行 `Rscript -e 'devtools::test(filter = "builder-(coordinator|build|worker|ui-contract|end-to-end|example-registry)$")'`。
- [ ] 运行 `R CMD INSTALL .` 和 `git diff --check`。
- [ ] 重启 `shiny::runApp("inst/builder", host="127.0.0.1", port=3838)`，确认 Gallery 只有 All content，选择后显示三个病人、六个空间 section 和 PCA/UMAP/t-SNE。
- [ ] 提交剩余必要修复：`fix(builder): finalize Seurat omnibus example`。
