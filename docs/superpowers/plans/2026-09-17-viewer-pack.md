# Viewer Pack implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 为大型 CRB 构建可校验、可删除、可安全回退的 Viewer Pack，并先加速 HLA/TCR 稳定前处理。

**架构：** `R/viewer_pack.R` 负责 dataset-level gating、canonical fingerprints、原子构建、manifest 与校验。`inst/viewer/core/viewer_pack.R` 是 bundle-safe runtime reader；Viewer 仅在完整校验通过时 lazy-load asset。CRB 及现有 runtime path 不变。

**技术栈：** R, qs2, jsonlite, testthat, Shiny

---

### 任务 1：Viewer Pack 核心契约

**文件：**
- 创建：`R/viewer_pack.R`
- 创建：`tests/testthat/test-viewer-pack.R`
- 修改：`NAMESPACE`

- [ ] 先写 threshold、manifest、checksum、fingerprint、atomic publish 与 fallback 的失败测试。
- [ ] 运行 `devtools::test(filter = "viewer-pack")`，确认因 API 缺失失败。
- [ ] 实现 `buildViewerPack()`、`validateViewerPack()` 与内部 reader，使测试通过。
- [ ] 运行同一 focused suite 并提交 `feat(builder): add large-dataset viewer pack`。

### 任务 2：共享 projection/metadata assets

**文件：**
- 修改：`R/viewer_pack.R`
- 修改：`tests/testthat/test-viewer-pack.R`

- [ ] 先写多 embedding Float32 tolerance、categorical dictionary dtype、numeric missingness与 canonical order parity 的失败测试。
- [ ] 实现每份 projection 只写一次及 metadata dictionary/numeric assets。
- [ ] 验证 focused suite 并提交 `feat(builder): encode shared viewer data`。

### 任务 3：HLA normalized fast path

**文件：**
- 创建：`inst/viewer/core/viewer_pack.R`
- 修改：`inst/viewer/shiny_server.R`
- 修改：`inst/viewer/hla_tcr_motifs/data.R`
- 修改：`tests/testthat/test-viewer-pack.R`
- 修改：`tests/testthat/test-hla-tcr-motifs.R`

- [ ] 先写 valid asset parity 与 missing/corrupt/version/fingerprint fallback 的失败测试。
- [ ] 在 pack 中生成全部可用 TCR chain normalized segments；Viewer lazy-load 并按 active cohort继续 runtime filter。
- [ ] 运行 Viewer Pack、HLA/TCR、IR definition sharing suites并提交 `perf(viewer): load precomputed HLA segments`。

### 任务 4：真实数据验证与后续 profile

**文件：**
- 修改：`tests/bench/benchmark_viewer_1m_pages.R`（仅当现有 harness 无法记录 pack 信息）

- [ ] 在当前 PR5 HEAD 对 Ren 生成 pack，记录总大小和模块大小。
- [ ] 复测 Projection、Linked tSNE、Linked Clonal expansion、Immune Clonal projection、Abundance、HLA & TCR Motifs 的 cold/return ready time与点数。
- [ ] 仅根据新 profile 决定是否增加 immune index/abundance aggregate；不为形式完整扩展模块。
- [ ] 运行指定回归、`git diff --check` 和适当 precheck，保持 PR5 工作树干净。
