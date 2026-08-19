# Project Workspace v3 Implementation Plan

> **面向 AI 代理的工作者：** 在当前功能 worktree 内实现；用户明确要求本地不运行验证，测试仅编写并交给远程 CI。

**目标：** 将 Builder Project 从内联完整 entry 的 schema v2 改为小型 manifest 和独立配置文件的 schema v3，不持久化无法复用的派生 profile。

**架构：** `project.R` 提供 sidecar 路径、原子写入、配置读写、legacy payload 兼容与瘦身 digest；Project server 在生成 manifest 时只写配置 sidecars。Review server 和浏览器为 Done checking 提供立即反馈并在小配置摘要完成后切换。

**技术栈：** R、Shiny、jsonlite、JavaScript、testthat

---

### Task 1: Schema v3 storage primitives

**Files:** `inst/builder/project.R`

- Raise the current schema to 3 while accepting versions 1 and 2.
- Add a safe managed path for each dataset config.
- Atomically write minimal config JSON.
- Exclude source-derived profiles and obsolete cache descriptors.
- Read v3 config without loading any derived profile data.

### Task 2: Manifest writer and legacy reader

**Files:** `inst/builder/project.R`, `inst/builder/server/project.R`

- Pass project root into dataset-record construction.
- Store config descriptors instead of inline payload.
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
- Assert source-derived profiles and obsolete cache descriptors are excluded.
- Assert Done-checking busy messages use the one-argument Shiny handler.

### Task 6: Delivery

- Run only focused contracts for the changed storage and Done-checking paths.
- Commit implementation and test contracts.
- Push `feat/builder-project-workspace` and dispatch remote workflows.
