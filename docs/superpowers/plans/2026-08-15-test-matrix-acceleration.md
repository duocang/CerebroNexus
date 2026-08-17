# 测试矩阵加速实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 在不减少测试覆盖或改变隔离边界的前提下，通过确定性加权分片和本地受控并行，把远端 R tests 降至约 8–9 分钟、本地完整验证降至约 15–18 分钟。

**架构：** 在现有 `run-test-shard.R` 中保留 round-robin 并新增基于版本化 CSV 权重表的 LPT 分片；GitHub Actions 显式选择 weighted。本地新增一个使用 processx 调度现有 shard runner 的独立入口，logic 有界并行、process-sensitive 独占、browser 有界并行，所有失败延迟汇总。

**技术栈：** R 4.6、testthat、processx、GitHub Actions、CSV 调度数据、现有 Nix 测试环境。

---

## 文件结构

- 创建 `scripts/test-runtime-weights.csv`：保存当前测试文件的分组、秒数和 measured/estimated 来源。
- 修改 `scripts/run-test-shard.R`：读取并校验权重、实现 LPT、解析 `--strategy` 和输出计划。
- 修改 `tests/testthat/test-ci-test-plan.R`：覆盖权重校验、LPT 确定性、回滚策略和 workflow 静态契约。
- 修改 `.github/workflows/R-tests.yaml`：在 logic/browser 命令中显式传 `--strategy weighted`。
- 创建 `scripts/run-local-validation.R`：实现本地阶段调度、并发限制、独立日志、fail-late 汇总和可选 check/pkgdown。
- 创建 `tests/testthat/test-local-validation-runner.R`：用短生命周期真实子进程验证并发、隔离、失败汇总和清理。
- 修改 `.gitignore`：仅在运行器确实需要仓库内默认输出时增加规则；若实现按规格使用临时目录则不修改。

### 任务 1：锁定权重表契约

**文件：**
- 创建：`scripts/test-runtime-weights.csv`
- 修改：`tests/testthat/test-ci-test-plan.R`
- 修改：`scripts/run-test-shard.R`

- [ ] **步骤 1：先写权重读取与校验失败测试**

在 `test-ci-test-plan.R` 中用临时 CSV 覆盖：有效记录、重复文件、非正数、未知分组、过期文件和新文件默认权重。期望 API：

```r
weights <- test_plan_api$ci_test_runtime_weights(
  plan,
  path = weights_path
)
expect_named(weights, sort(plan$all))
expect_true(all(is.finite(weights) & weights > 0))
```

- [ ] **步骤 2：运行测试确认正确红灯**

运行：

```bash
Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-ci-test-plan.R", reporter="summary")'
```

预期：FAIL，原因是 `ci_test_runtime_weights()` 尚不存在。

- [ ] **步骤 3：实现最小权重读取与验证**

在 `run-test-shard.R` 中新增：

```r
ci_test_runtime_weights <- function(
  plan,
  path = file.path("scripts", "test-runtime-weights.csv")
) {
  # read.csv(check.names = FALSE, stringsAsFactors = FALSE)
  # validate columns group/file/seconds/basis
  # reject duplicate, stale, cross-group, invalid records
  # fill unregistered current files with the measured group median
  # return a named numeric vector in sorted plan$all order
}
```

- [ ] **步骤 4：生成初始完整权重表**

从 `/tmp/cerebro-verify-4c62b4ec/` 的 11 个 shard 日志恢复可用文件级耗时。无法可靠拆分的文件使用所属组的 measured 中位数并标记 `estimated`。确认 CSV 中每个当前文件恰好一行。

- [ ] **步骤 5：运行测试转绿并检查 CSV**

运行上述 focused test，并额外运行：

```bash
Rscript scripts/run-test-shard.R --validate
```

预期：PASS，且验证输出说明权重表完整有效。

- [ ] **步骤 6：提交独立权重契约**

```bash
git add scripts/test-runtime-weights.csv scripts/run-test-shard.R tests/testthat/test-ci-test-plan.R
git commit -m "test(ci): define runtime weight registry"
```

### 任务 2：实现确定性 LPT 分片

**文件：**
- 修改：`tests/testthat/test-ci-test-plan.R`
- 修改：`scripts/run-test-shard.R`

- [ ] **步骤 1：先写 LPT 行为测试**

测试固定文件与权重输入，覆盖：最长优先、最低负载、负载并列选低编号、文件名并列稳定、输入倒序结果不变、完整无重复，以及 round-robin 结果保持原样。期望 API：

```r
weighted <- test_plan_api$ci_test_shards(
  files,
  shards = 3L,
  strategy = "weighted",
  weights = weights
)
round_robin <- test_plan_api$ci_test_shards(
  files,
  shards = 3L,
  strategy = "round-robin"
)
```

- [ ] **步骤 2：运行测试确认策略参数红灯**

运行 focused CI plan test。预期：FAIL，原因是现有函数不接受 `strategy`/`weights`。

- [ ] **步骤 3：实现最小 LPT 算法**

扩展 `ci_test_shards()`：默认 `round-robin`；weighted 时校验命名权重，按 `-weight, filename` 排序，依次分配到当前总权重最低且编号最小的 shard，最终 shard 内排序。

- [ ] **步骤 4：增加预测负载输出**

让 `--list` 或新的 `--show-plan` 输出每个 shard 的文件和预测秒数，供本地与 CI 审核，但不影响实际 testthat filter。

- [ ] **步骤 5：运行 focused tests 转绿**

预期：所有旧 round-robin 测试和新 weighted 测试通过。

- [ ] **步骤 6：提交算法**

```bash
git add scripts/run-test-shard.R tests/testthat/test-ci-test-plan.R
git commit -m "ci: balance test shards by runtime"
```

### 任务 3：接入 CLI 和 GitHub Actions

**文件：**
- 修改：`tests/testthat/test-ci-test-plan.R`
- 修改：`scripts/run-test-shard.R`
- 修改：`.github/workflows/R-tests.yaml`

- [ ] **步骤 1：先写 CLI 与 workflow 静态失败测试**

断言 `--strategy` 只接受 `round-robin|weighted`；logic/browser workflow 命令均含精确 `--strategy weighted`；process-sensitive 仍独立且无需分片策略；所有既有 job、env、artifact、summary 契约不变。

- [ ] **步骤 2：运行 focused test 确认红灯**

预期：FAIL，缺少 CLI 参数和 workflow 参数。

- [ ] **步骤 3：实现 CLI 参数传递**

在 `ci_parse_args()` 默认 `strategy = "round-robin"`，解析 `--strategy`，并把 strategy/weights 传入 `ci_test_shard_files()`；仅 weighted 时读取 CSV。

- [ ] **步骤 4：更新 workflow**

只在 logic 与 browser 的 shard runner 命令加入 `--strategy weighted`，不改变 matrix、job 名、并发、env、artifact 或 summary needs。

- [ ] **步骤 5：验证 focused tests 与计划输出**

运行：

```bash
Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-ci-test-plan.R", reporter="summary")'
Rscript scripts/run-test-shard.R --group logic --shard 1 --shards 4 --strategy weighted --list
Rscript scripts/run-test-shard.R --group browser --shard 1 --shards 6 --strategy weighted --list
```

- [ ] **步骤 6：提交 CI 接入**

```bash
git add scripts/run-test-shard.R tests/testthat/test-ci-test-plan.R .github/workflows/R-tests.yaml
git commit -m "ci: use weighted test shards"
```

### 任务 4：定义本地调度器纯逻辑

**文件：**
- 创建：`tests/testthat/test-local-validation-runner.R`
- 创建：`scripts/run-local-validation.R`

- [ ] **步骤 1：先写调度状态机失败测试**

以 `sys.source()` 加载脚本到隔离环境，测试参数校验、阶段顺序、logic/browser 并发上限、process-sensitive 独占和 fail-late 汇总。期望暴露纯函数：

```r
schedule <- local_validation_schedule(
  logic_workers = 3L,
  browser_workers = 2L,
  mode = "tests"
)
expect_identical(unique(schedule$phase), c("logic", "process-sensitive", "browser"))
```

- [ ] **步骤 2：运行新测试确认红灯**

运行：

```bash
Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-local-validation-runner.R", reporter="summary")'
```

预期：FAIL，脚本或函数不存在。

- [ ] **步骤 3：实现最小参数与阶段模型**

脚本支持：

```text
--mode tests|full
--logic-workers 1..4
--browser-workers 1..3
--output-dir PATH
--dry-run
```

默认 `full`, logic=3, browser=2, output=tempfile。`--dry-run` 只打印命令、阶段、并发和预测负载。

- [ ] **步骤 4：运行纯逻辑测试转绿**

确认无需启动真实测试即可验证所有调度不变量。

- [ ] **步骤 5：提交调度模型**

```bash
git add scripts/run-local-validation.R tests/testthat/test-local-validation-runner.R
git commit -m "test(local): define validation scheduler"
```

### 任务 5：实现真实 processx 有界并行

**文件：**
- 修改：`tests/testthat/test-local-validation-runner.R`
- 修改：`scripts/run-local-validation.R`

- [ ] **步骤 1：先写真实短子进程失败测试**

使用脚本生成的临时 R 子命令验证最大同时运行数、一个失败后后续任务仍启动、每个任务独立日志、SIGINT/清理只影响 owned children，以及 aggregate exit code。

- [ ] **步骤 2：运行测试确认红灯**

预期：FAIL，真实 runner 尚未实现。

- [ ] **步骤 3：实现最小 processx 调度循环**

每个阶段维护 pending/running/completed；最多启动 cap 个 `processx::process$new()`；轮询存活状态；记录 started/ended/duration/status；阶段结束后进入下一阶段。process-sensitive cap 固定 1 且前后均等待其他阶段清空。

- [ ] **步骤 4：实现实际命令和环境**

logic/browser 调用现有 shard runner 并显式 weighted；browser 设置 `CEREBRO_RUN_BROWSER_TESTS=true`、独立 `CEREBRO_TEST_ARTIFACT_DIR` 和源码根；full mode 后续任务调用项目现有 check/pkgdown 命令。

- [ ] **步骤 5：实现预检和总结**

检查 Rscript/processx、仓库根和可识别 stray Shiny/Cerebro 进程；发现 stray 只报告退出。最终打印每项退出码、耗时、日志、总 wall time与最慢 shards，并以任一失败为非零退出码。

- [ ] **步骤 6：运行 runner tests 和 dry-run**

```bash
Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-local-validation-runner.R", reporter="summary")'
Rscript scripts/run-local-validation.R --mode tests --dry-run
```

预期：PASS；dry-run 显示 4 logic、1 process-sensitive、6 browser，无实际子进程测试执行。

- [ ] **步骤 7：提交本地运行器**

```bash
git add scripts/run-local-validation.R tests/testthat/test-local-validation-runner.R
git commit -m "feat(local): run test shards concurrently"
```

### 任务 6：完整验证、审查和远端计时

**文件：**
- 可能修改：仅修复验证中暴露的本轮相关文件
- 不修改：`scripts/precheck.sh`

- [ ] **步骤 1：运行静态与 focused 验证**

```bash
git diff --check 4c62b4ec..HEAD
Rscript scripts/run-test-shard.R --validate
Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-ci-test-plan.R", reporter="summary")'
Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-local-validation-runner.R", reporter="summary")'
```

- [ ] **步骤 2：连续运行两次本地 tests 矩阵**

```bash
Rscript scripts/run-local-validation.R --mode tests
Rscript scripts/run-local-validation.R --mode tests
```

记录两轮 11 shards 的全部结果、wall time、最慢 shard 和机器负载异常。两轮都必须全绿。

- [ ] **步骤 3：运行 full 后置验证一次**

```bash
Rscript scripts/run-local-validation.R --mode full
```

如果第三次完整测试成本不合理，可在已完成两轮 tests 后只运行同一 runner 的 check/pkgdown 后置任务；报告必须准确区分。

- [ ] **步骤 4：规格与质量审查**

逐项核对设计规格，确认覆盖、隔离、workflow 契约、失败处理和回滚；审查并发资源泄漏、shell quoting、跨平台路径和用户进程安全。

- [ ] **步骤 5：确认提交边界并推送**

确认 status 中 `scripts/precheck.sh` 和 `pkgdown/` 未被本轮提交。推送 `integration/colleague-spatial-builder` 到 `origin`。

- [ ] **步骤 6：运行并监控远端 CI**

手动 dispatch R tests、R-CMD-check、pkgdown；监控所有 job 到终态。任一失败则取日志、按 TDD 修复、重新推送和完整监控。

- [ ] **步骤 7：报告实际收益和回滚点**

报告 weighted 前后预测/实际分片、两轮本地 wall time、远端 workflow wall time、断言统计、失败/警告/跳过，以及本轮每个 commit。明确整体回滚范围为 `4c62b4ec..HEAD`，不会影响此前功能优化。
