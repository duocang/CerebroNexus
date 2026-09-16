# Instant dataset information implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让多数据集 Viewer 的 Data Info 从紧凑 catalog 即时渲染，同时保持 CRB、BPCells 和页面 payload 按需加载。

**架构：** 构建期或 `launchCerebro()` 启动期从已经读取的 CRB payload 提取三项标量并写入现有配置。Viewer 将信息卡 reactive 与完整 `data_set()` reactive 解耦，并在离开 Data Info 前关闭数据加载门。进程缓存先 clone 未附着 expression 的 prototype，再给 session clone 安装 lazy backend。

**技术栈：** R、Shiny、R6、BPCells、testthat、shinytest2/browser benchmark

---

## 文件结构

- 修改 `R/createShinyApp.R`：提取、验证并冻结 generated-app dataset catalog。
- 修改 `R/launchCerebro.R`：为直接启动的文件型 CRB 构建同形 catalog。
- 修改 `inst/viewer/utility_functions.R`：解析 catalog，并在 clone 后安装 lazy runtime backend。
- 修改 `inst/viewer/shiny_server.R`：提供当前 catalog entry 与数据加载门。
- 修改 `inst/viewer/load_data/sample_info.R`：让三个 value box 优先读取 catalog。
- 修改 `tests/testthat/test-createShinyApp-sibling.R`：覆盖 catalog preflight、配置和 lazy clone。
- 修改 `tests/testthat/test-r-functions.R`：覆盖 `launchCerebro()` 的 catalog。
- 修改 `tests/testthat/test-utility_functions.R`：覆盖 catalog 解析和 Data Info fallback。
- 修改 `tests/testthat/test-app-inst.R`：覆盖 Data Info 不加载 CRB、离开页面后加载的浏览器行为。

### 任务 1：恢复 session clone 的 lazy expression

- [ ] **步骤 1：编写失败测试**

在 `tests/testthat/test-createShinyApp-sibling.R` 添加 runtime 测试：构造 BPCells bundle CRB，调用 `get_or_load_crb()` 两次，断言对象不相同，并断言两次返回对象的 `expression` binding 都保持 lazy，直到显式读取 expression。

```r
first <- runtime$get_or_load_crb(path, plan, path)
second <- runtime$get_or_load_crb(path, plan, path)
expect_false(identical(first, second))
expect_true(rlang::env_binding_are_lazy(first, "expression"))
expect_true(rlang::env_binding_are_lazy(second, "expression"))
```

- [ ] **步骤 2：运行测试验证失败**

运行：`R --no-echo --no-restore -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-createShinyApp-sibling.R", reporter="summary")'`

预期：新断言失败，因为当前 `.cloneCachedCrb()` 会强制求值 expression binding。

- [ ] **步骤 3：最少实现**

调整 `get_or_load_crb()`，让 cache 保存 `read_cerebro_file()` 返回的未附着 prototype。cache miss 和 hit 都执行相同顺序：clone prototype、`.attachExternalExpression()`、`.attachImmuneRepertoireBackend()`、`.attachSpatialMoleculeBackend()`，最后返回 session clone。

```r
prototype <- if (is.null(cached)) read_cerebro_file(path) else cached$object
obj <- .cloneCachedCrb(prototype)
obj <- .attachExternalExpression(obj, path, effective_backend)
obj <- .attachImmuneRepertoireBackend(obj, path, effective_backend)
.attachSpatialMoleculeBackend(obj, path)
```

- [ ] **步骤 4：验证绿灯**

重新运行 `test-createShinyApp-sibling.R`，预期 0 failures。

- [ ] **步骤 5：提交**

```bash
git add inst/viewer/utility_functions.R tests/testthat/test-createShinyApp-sibling.R
git commit -m "fix(viewer): preserve lazy CRB clones"
```

### 任务 2：生成统一 dataset catalog

- [ ] **步骤 1：编写失败测试**

在 `tests/testthat/test-createShinyApp-sibling.R` 添加测试，构建两个 CRB 后断言 `cerebro_config.rds$.dataset_catalog` 以 runtime CRB path 为键，并准确保存 label、cells、organism、date。测试同时断言 `.preflightBundleData()` 不增加第二次 payload read。

在 `tests/testthat/test-r-functions.R` 添加 `launchCerebro()` 捕获测试，断言直接启动生成相同 entry，并跳过不存在或变量型条目。

- [ ] **步骤 2：运行测试验证失败**

运行：`R --no-echo --no-restore -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-createShinyApp-sibling.R", reporter="summary"); testthat::test_file("tests/testthat/test-r-functions.R", reporter="summary")'`

预期：`.dataset_catalog` 缺失导致新断言失败。

- [ ] **步骤 3：最少实现**

在 `R/createShinyApp.R` 新增单一 extractor，并让 `.preflightBundleData()` 从已经读取的 object 收集 entry。generated-app 将 key 从 source path 映射为 `private-data/...` target。`launchCerebro()` 对存在的配置文件调用同一 extractor，并把结果放进 `Cerebro.options$.dataset_catalog`。

```r
.datasetInfoFromObject <- function(object, label, path) {
  experiment <- object$getExperiment()
  list(label = label, path = path, cells = nrow(object$getMetaData()), organism = experiment$organism, date = as.character(experiment$date_of_export))
}
```

- [ ] **步骤 4：验证绿灯**

重新运行两个测试文件，预期 0 failures。

- [ ] **步骤 5：提交**

```bash
git add R/createShinyApp.R R/launchCerebro.R tests/testthat/test-createShinyApp-sibling.R tests/testthat/test-r-functions.R
git commit -m "perf(viewer): freeze dataset info catalog"
```

### 任务 3：Data Info 使用 catalog 并门控完整加载

- [ ] **步骤 1：编写失败测试**

在 `tests/testthat/test-utility_functions.R` 添加 catalog entry 校验测试。在 `tests/testthat/test-app-inst.R` 添加两数据集应用回归：停留 Data Info 时切换 selector，等待第二个 cell count 和 organism 出现，断言 CRB loader 计数未增加；点击 Projection 后断言 loader 才增加。

- [ ] **步骤 2：运行测试验证失败**

运行：`R --no-echo --no-restore -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-utility_functions.R", reporter="summary"); testthat::test_file("tests/testthat/test-app-inst.R", reporter="summary")'`

预期：Data Info 仍调用 `data_set()`，loader 计数在切换时增加。

- [ ] **步骤 3：最少实现**

在 `inst/viewer/shiny_server.R` 添加 `dataset_load_requested`、`current_dataset_info()` 和 sidebar/selector gate。`data_set()` 在读取 CRB 前执行 `req(dataset_load_requested())`。在 `inst/viewer/load_data/sample_info.R` 中，三个 value box 读取 `current_dataset_info()`；catalog 缺失时才回退到已加载对象。

```r
dataset_load_requested <- reactiveVal(FALSE)
observeEvent(input[["sidebar"]], {
  if (!identical(input[["sidebar"]], "loadData")) dataset_load_requested(TRUE)
})
data_set <- reactive({
  req(dataset_load_requested())
  # existing loader
})
```

- [ ] **步骤 4：验证绿灯**

重新运行两个测试文件，预期 0 failures。

- [ ] **步骤 5：提交**

```bash
git add inst/viewer/shiny_server.R inst/viewer/load_data/sample_info.R inst/viewer/utility_functions.R tests/testthat/test-utility_functions.R tests/testthat/test-app-inst.R
git commit -m "perf(viewer): render dataset info instantly"
```

### 任务 4：完整验证与百万细胞浏览器基准

- [ ] **步骤 1：运行相关完整检查**

运行：`R --no-echo --no-restore -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-createShinyApp-sibling.R", reporter="summary"); testthat::test_file("tests/testthat/test-r-functions.R", reporter="summary"); testthat::test_file("tests/testthat/test-utility_functions.R", reporter="summary"); testthat::test_file("tests/testthat/test-app-inst.R", reporter="summary"); testthat::test_file("tests/testthat/test-coordinated-views.R", reporter="summary")'`

预期：所有文件 0 failures。

- [ ] **步骤 2：运行真实浏览器基准**

启动 PR5 Viewer，配置 10x E18 1M 与 Ren 1.46M。停留 Data Info 切换到 Ren，记录选择到 `1,462,702 / Homo sapiens` 出现的时间；检查服务端日志在这一步没有 `Attaching bpcells backend`。随后进入 Projection，确认数据完成加载且图可见。

预期：Data Info 小于 500 ms；BPCells 只在进入数据页后附着；Projection 正确显示 tSNE。

- [ ] **步骤 3：规格复核**

逐项核对设计文档的 Goals、Compatibility 和 Verification，记录任何未满足项，不以单元测试代替浏览器结果。

