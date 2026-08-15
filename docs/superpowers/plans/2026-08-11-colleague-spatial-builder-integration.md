# Colleague Spatial Builder integration implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** Produce one local colleague-delivery branch containing the four-PR stack, coordinated views, and Builder.

**架构：** Use the multi-spatial-image stack tip as the first-parent base, merge coordinated views, then merge Builder. Resolve shared Viewer files according to the approved ownership rules without restoring removed legacy pages.

**技术栈：** Git, R, Shiny, JavaScript

---

### 任务 1：Record the integration boundary

**文件：**
- 创建：`docs/superpowers/specs/2026-08-11-colleague-spatial-builder-integration-design.md`
- 创建：`docs/superpowers/plans/2026-08-11-colleague-spatial-builder-integration.md`

- [ ] **步骤 1：Commit the approved design and plan**

```bash
git add docs/superpowers/specs/2026-08-11-colleague-spatial-builder-integration-design.md docs/superpowers/plans/2026-08-11-colleague-spatial-builder-integration.md
git commit -m "docs: plan colleague spatial Builder integration"
```

### 任务 2：Merge coordinated views

- [ ] **步骤 1：Merge `feat/coordinated-views-nexus` with coordinated views owning navigation and removed pages.**

```bash
git merge --no-ff feat/coordinated-views-nexus
```

### 任务 3：Merge Builder

- [ ] **步骤 1：Merge `feat/cerebro-builder`, retaining Builder workflow files and the multi-image data contract without restoring legacy Viewer pages.**

```bash
git merge --no-ff feat/cerebro-builder
```

### 任务 4：Perform lightweight verification

- [ ] **步骤 1：Check ancestry, whitespace, conflict markers, and worktree cleanliness.**

```bash
git merge-base --is-ancestor feat/multi-spatial-image-api HEAD
git merge-base --is-ancestor feat/coordinated-views-nexus HEAD
git merge-base --is-ancestor feat/cerebro-builder HEAD
git diff --check
git status --short --branch
```

Expected: all ancestry checks return zero, no unresolved conflict markers or whitespace errors, and a clean worktree.
