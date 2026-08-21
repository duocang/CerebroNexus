# Extra material XLSX 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框跟踪进度。

**目标：** 让 Extra material 表格上传支持 XLSX，并把每个非空 worksheet 作为独立表传入现有 CRB/Viewer 链路。

**架构：** 将现有单表读取函数扩展为统一的多记录读取接口；CSV/TSV/TXT 返回一个记录，XLSX 通过 `readxl` 返回每个 sheet 的记录。Shiny 上传 observer 逐记录做唯一命名并写入现有 `settings$tables`。

**技术栈：** R、Shiny、readxl、testthat

---

### 任务 1：多格式读取器

**文件：**
- 修改：`inst/builder/extras.R`
- 测试：`tests/testthat/test-builder-extras.R`

- [ ] 添加多-sheet XLSX 与 delimited 兼容测试。
- [ ] 运行 `test-builder-extras.R`，确认 XLSX 用例失败。
- [ ] 实现 `builder_read_tables()`：delimited 返回单记录，XLSX 枚举 sheet、跳过空表并保留逐-sheet 错误。
- [ ] 保留 `builder_read_table()` 作为单表 delimited 入口，避免无关调用变化。
- [ ] 运行针对性测试并提交。

### 任务 2：Builder 上传接入

**文件：**
- 修改：`inst/builder/server/enhancements.R`
- 修改：`inst/builder/ui/enhance_stage.R`
- 测试：`tests/testthat/test-builder-stage-server.R`
- 测试：`tests/testthat/test-builder-stage-enhance.R`

- [ ] 添加一个 workbook 产生多张 `settings$tables` 的 server 测试。
- [ ] 将上传 observer 改为遍历 `builder_read_tables()` 结果，并对每个 sheet 单独唯一命名、提示错误。
- [ ] 上传 input 接受 `.xlsx`，文案改为 CSV、TSV 或 XLSX。
- [ ] 运行相关测试并提交。

### 任务 3：回归验证

**文件：**
- 验证：`inst/builder/build.R`
- 验证：`R/exportFromSeurat.R`

- [ ] 运行 Extra material、Builder enhance 与 build 相关测试。
- [ ] 确认 `builder_attach_tables()` 仍把每个 sheet 记录写入 `object@misc$extra_material$tables`。
- [ ] 启动 Builder，确认上传控件接受 XLSX。
