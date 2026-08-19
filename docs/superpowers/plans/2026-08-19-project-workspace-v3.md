# Project Workspace v3 Implementation Plan

> **面向 AI 代理的工作者：** 在当前功能 worktree 内实现；用户明确要求本地不运行验证，测试仅编写并交给远程 CI。

**目标：** 将 Builder Project 从内联完整 entry 的 schema v2 改为小型 manifest、独立配置文件和可替换二进制 profile cache 的 schema v3。

**架构：** `project.R` 提供 sidecar 路径、原子写入、配置/cache 读写、legacy payload 兼容与瘦身 digest；Project server 在生成 manifest 时写 sidecars。Review server 和浏览器为 Done checking 提供立即反馈并在小配置摘要完成后切换。

**技术栈：** R、Shiny、jsonlite、RDS、JavaScript、testthat

---

### Task 1: Schema v3 storage primitives

**Files:** `inst/builder/project.R`

- Raise the current schema to 3 while accepting versions 1 and 2.
- Add safe managed paths for dataset config and profile cache.
- Atomically write minimal config JSON and source-derived profile RDS.
- Return path/fingerprint descriptors and reuse a valid existing cache.
- Read v3 config without eagerly reading profile cache.

### Task 2: Manifest writer and legacy reader

**Files:** `inst/builder/project.R`, `inst/builder/server/project.R`

- Pass project root and prior record into dataset-record construction.
- Store config/cache descriptors instead of inline payload.
- Preserve v1/v2 inline payload only as `legacy_payload` in memory.
- On the next save, emit v3 sidecars and remove all inline payloads.
- Make spatial-asset status read either v3 config or legacy payload.

### Task 3: Small configuration identities

**Files:** `inst/builder/project.R`

- Build the digest input directly from settings, acknowledgements, and spatial
  drafts.
- Normalize spatial image content to asset fingerprints without cloning the
  full runtime entry.
- Keep artifact reuse and Checked identity as separate comparisons.

### Task 4: Done-checking responsiveness

**Files:** `inst/builder/server/review.R`, `inst/builder/www/builder.js`

- Immediately disable and relabel the Done button.
- Apply drafts, compute the small digest, set the mark, and select the next
  dataset.
- Release the busy state after the next UI flush; preview remains asynchronous.

### Task 5: Contracts

**Files:** `tests/testthat/test-builder-project.R`, `tests/testthat/test-builder-project-hydration.R`, `tests/testthat/test-builder-stage-server.R`, `tests/testthat/test-builder-ui-contract.R`

- Assert schema v3 manifests contain descriptors and no inline payload.
- Assert config round trips without profile data.
- Assert legacy payload remains readable and migrates on save.
- Assert cache descriptors are optional and cache failures fall back safely.
- Assert Done-checking busy messages use the one-argument Shiny handler.

### Task 6: Delivery

- Do not execute local tests or validation commands.
- Commit implementation and test contracts.
- Push `feat/builder-project-workspace` and dispatch remote workflows.
