# 多 Spatial 图片公开 API 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers-zh:subagent-driven-development（推荐）或 superpowers-zh:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法跟踪进度。

**目标：** 将空间背景图升级为 `dataset → Seurat/Cerebro spatial entry → 用户命名图片` 的完整公开函数接口，并使用扩充后的 Omnibus 数据验证真实的 `convertSeuratToCerebro()` → `createShinyApp()` H5 应用生成链路。

**架构：** 新建纯 R manifest 规范化层，统一验证路径图片、Seurat `misc` 内嵌图片、CRB canonical 图片和 Shiny App 配置。`Cerebro` 每个 spatial entry 存储具名 `histology_images`；转换器负责把外部路径编码为内嵌图片，App builder 负责复制不进入 CRB 的外部图片，Viewer 按当前 dataset、spatial entry 和 image label 精确解析背景及预设。

**技术栈：** R、R6、Seurat/SeuratObject、Matrix、HDF5Array、base64enc、png、Shiny、testthat、shinytest2。

---

## 文件结构

- 创建 `R/spatial_image_manifest.R`：图片格式、名称、bounds、路径描述符和内嵌 manifest 的纯验证/规范化函数。
- 修改 `R/spatial_image_payload.R`：把现有单图片 Seurat payload 验证迁移到多图片 canonical manifest，并保留旧形状兼容。
- 修改 `R/class-Cerebro.R`、`man/Cerebro.Rd`：`addSpatialData()`/`getSpatialData()` 的多图片 canonical 契约。
- 修改 `R/exportFromSeurat.R`：接受规范化 spatial 图片输入并为每个 spatial entry 写入多张内嵌图片。
- 修改 `R/convertSeuratToCerebro.R`、`man/convertSeuratToCerebro.Rd`、`man/exportFromSeurat.Rd`：增加公开 `spatial_images` 参数及示例。
- 修改 `R/createShinyApp.R`、`man/createShinyApp.Rd`：验证三级外部图片 manifest、复制资源、写入 per-image settings。
- 修改 `inst/viewer/spatial/func_spatial_helpers.R`：按 dataset/spatial/image 解析嵌入和外部背景。
- 修改 `inst/viewer/spatial/UI_projection_main_parameters.R`、`inst/viewer/spatial/obj_projection_parameters_plot.R`、`inst/viewer/spatial/obj_projection_background_controls.R`、`inst/viewer/spatial/func_projection_update_plot.R`：绑定当前 spatial entry、图片和变换预设。
- 修改 `tests/testthat/test-spatial-image-payload.R`：多图片、legacy 和错误边界测试。
- 创建 `tests/testthat/test-spatial-image-manifest.R`：公开路径 manifest 和格式验证测试。
- 修改 `tests/testthat/test-spatial.R`、`tests/testthat/test-spatial-preset-resolver.R`、`tests/testthat/test-createShinyApp-sibling.R`、`tests/testthat/test-createShinyApp-http-privacy.R`、`tests/testthat/test-smoke-production.R`：App bundle、隔离、兼容与安全回归。
- 修改 `data-raw/build_omnibus_demo.R`：生成三个 spatial entries、三张内嵌图片、一个外部图片和 marker CSV。
- 修改 `inst/extdata/examples/demo_omnibus_seurat.rds`、`inst/extdata/examples/demo_omnibus.crb`：重新生成的 canonical 工件。
- 创建 `inst/extdata/examples/demo_omnibus_markers.csv`、`inst/extdata/examples/demo_omnibus_donorB_if.png`、`inst/extdata/examples/demo_omnibus_donorC_review.png`：公开 API 集成输入。
- 修改 `tests/testthat/test-omnibus-pipeline.R`：完整数据工件和两函数调用链。
- 创建 `data-raw/verify_omnibus_public_api.R`：用户可直接运行的公开函数示例。
- 修改 `data-raw/omnibus.md`、`data-raw/spatial.md`、`data-raw/DATASETS.md`、`NEWS.md`：数据模型、平台差异、API 和兼容说明。

### 任务 1：建立 canonical 多图片 manifest 与 Cerebro 类契约

**文件：**
- 创建：`R/spatial_image_manifest.R`
- 修改：`R/spatial_image_payload.R`
- 修改：`R/class-Cerebro.R:1197-1244`
- 修改：`man/Cerebro.Rd`
- 创建：`tests/testthat/test-spatial-image-manifest.R`
- 修改：`tests/testthat/test-spatial-image-payload.R`
- 修改：`tests/testthat/test-spatial.R`

- [ ] **步骤 1：编写多图片 canonical shape 的失败测试**

构造一个 spatial entry：

```r
images <- list(
  "H&E" = list(
    histology_image = "data:image/png;base64,AA==",
    histology_image_bounds = c(xmin = 0, xmax = 10, ymin = 0, ymax = 8)
  ),
  "DAPI" = list(
    histology_image = "data:image/jpeg;base64,AA==",
    histology_image_bounds = c(xmin = 0, xmax = 10, ymin = 0, ymax = 8)
  )
)
cerebro <- Cerebro$new()
cerebro$addSpatialData("donorA tissue", list(
  coordinates = data.frame(x = c(1, 9), y = c(1, 7)),
  expression = Matrix::Matrix(matrix(1, 1, 2), sparse = TRUE),
  histology_images = images
))
expect_identical(
  names(cerebro$getSpatialData("donorA tissue")$histology_images),
  c("H&E", "DAPI")
)
```

增加空/重复 image label、非法 data URI、错误 bounds、坐标越界和
`histology_images = list()` 的用例。预期当前实现 FAIL，因为类不规范化或验证
`histology_images`。

- [ ] **步骤 2：运行红灯测试**

运行：

```bash
R -q -e 'devtools::test(filter = "spatial-image-manifest|spatial-image-payload|spatial$")'
```

预期：FAIL，失败信息指向缺少多图片 manifest 验证，而不是 fixture 构造错误。

- [ ] **步骤 3：实现纯规范化函数**

在 `R/spatial_image_manifest.R` 提供：

```r
.spatialImageBounds <- function(bounds, coordinates, context) {
  # NULL 时由 coordinates 的 x/y range 生成；否则验证四个有限命名值、
  # 有序区间和坐标 containment。返回 xmin/xmax/ymin/ymax 顺序的 numeric。
}

.normalizeEmbeddedSpatialImages <- function(images, coordinates, context) {
  # 返回 uniquely named list，每个 leaf 只含 histology_image 与
  # histology_image_bounds。空 list 合法。
}

.normalizeSpatialDataImages <- function(data, spatial_name) {
  # canonical histology_images 优先；把 legacy singular fields 变成
  # list("Tissue background" = payload)，并移除 singular fields。
}
```

`R/spatial_image_payload.R` 的 Seurat 顶层 validator 改为
`spatial_name → image_label → payload`，同时识别旧的
`spatial_name → payload` 形状。

- [ ] **步骤 4：让 `Cerebro` 类写入和返回 canonical 结构**

`addSpatialData()` 在保存前调用 `.normalizeSpatialDataImages()`；
`getSpatialData()` 对已序列化的 legacy singular entry 做按需规范化后返回，但不
原地修改只读调用者未请求的其他 entry。更新 roxygen/Rd，明确 spatial 名和图片
label 是不同身份。

- [ ] **步骤 5：验证绿灯和旧 CRB 读取**

运行：

```bash
R -q -e 'devtools::test(filter = "spatial-image-manifest|spatial-image-payload|spatial$")'
```

预期：新多图片和 legacy singular 测试全部 PASS；现有真实 spatial CRB 均可读取。

- [ ] **步骤 6：提交**

```bash
git add R/spatial_image_manifest.R R/spatial_image_payload.R R/class-Cerebro.R \
  man/Cerebro.Rd tests/testthat/test-spatial-image-manifest.R \
  tests/testthat/test-spatial-image-payload.R tests/testthat/test-spatial.R
git commit -m "feat: add canonical multi-spatial image manifest"
```

### 任务 2：扩充 Seurat 转换公开函数接口

**文件：**
- 修改：`R/convertSeuratToCerebro.R:330-455,900-945`
- 修改：`R/exportFromSeurat.R:430-560,1665-1790`
- 修改：`R/spatial_image_manifest.R`
- 修改：`man/convertSeuratToCerebro.Rd`
- 修改：`man/exportFromSeurat.Rd`
- 修改：`tests/testthat/test-spatial-image-manifest.R`
- 修改：`tests/testthat/test-spatial-image-payload.R`

- [ ] **步骤 1：编写路径 shorthand 与 descriptor 的失败测试**

对最小多 FOV Seurat fixture 调用：

```r
exportFromSeurat(
  object,
  file = output,
  assay = "RNA",
  slot = "data",
  experiment_name = "multi spatial",
  organism = "Human",
  groups = "sample_id",
  spatial_images = list(
    sliceA = c("H&E" = png_path, "DAPI" = jpeg_path),
    sliceB = list(
      "IF" = list(
        path = svg_path,
        bounds = c(xmin = 0, xmax = 20, ymin = 0, ymax = 10)
      )
    )
  ),
  verbose = FALSE
)
```

断言 CRB 的 sliceA 有 H&E/DAPI、sliceB 有 IF、coordinates-only sliceC 是空
`histology_images`。当前预期 FAIL，因为函数没有 `spatial_images` 参数。

- [ ] **步骤 2：增加错误边界红灯用例**

分别验证 unknown spatial、空/重复 image label、丢失路径、目录路径、`.tiff`、
非法 bounds，以及函数参数与 `object@misc$cerebro_spatial_images` 的同名冲突。
每条错误必须包含 `spatial/image` 路径。

- [ ] **步骤 3：运行红灯测试**

运行：

```bash
R -q -e 'devtools::test(filter = "spatial-image-manifest|spatial-image-payload")'
```

预期：FAIL，首要原因是公开参数和路径 manifest normalizer 尚不存在。

- [ ] **步骤 4：实现路径图片规范化与编码**

在 manifest 文件增加：

```r
.normalizeSpatialImagePaths <- function(images, spatial_names, context) {
  # 接受 named character shorthand 或 list(path=, bounds=) descriptor；
  # 只允许 png/jpg/jpeg/svg；返回 spatial → label → descriptor。
}

.encodeSpatialImageDescriptor <- function(descriptor, coordinates, context) {
  # base64enc::base64encode(path)，按扩展名生成 MIME，验证/推导 bounds，
  # 返回 canonical embedded leaf。
}
```

函数不接受未知格式的 JPEG fallback。

- [ ] **步骤 5：接入 `exportFromSeurat()`**

增加 `spatial_images = NULL` 参数。在坐标提取成功后，合并：

```r
declared <- .mergeSpatialImageSources(
  misc_images,
  argument_images,
  spatial_name = image_name
)
spatial_data$histology_images <- lapply(
  declared,
  .materializeSpatialImage,
  coordinates = spatial_data$coordinates
)
```

坐标提取继续维持 warning boundary；显式图片声明错误在 CRB 发布前 hard fail。

- [ ] **步骤 6：接入 `convertSeuratToCerebro()`**

增加同名参数并原样传给 `exportFromSeurat()`。不得把路径 manifest 写回输入 Seurat
对象。更新两个函数的 roxygen、Rd 和 `\dontrun{}` 示例。

- [ ] **步骤 7：运行转换层测试**

运行：

```bash
R -q -e 'devtools::test(filter = "spatial-image-manifest|spatial-image-payload|exportFromSeurat")'
```

预期：PASS，PNG/JPEG/SVG、legacy misc、direct export、wrapper conversion 均覆盖。

- [ ] **步骤 8：提交**

```bash
git add R/convertSeuratToCerebro.R R/exportFromSeurat.R \
  R/spatial_image_manifest.R man/convertSeuratToCerebro.Rd \
  man/exportFromSeurat.Rd tests/testthat/test-spatial-image-manifest.R \
  tests/testthat/test-spatial-image-payload.R
git commit -m "feat: accept named spatial images during Seurat conversion"
```

### 任务 3：扩充 `createShinyApp()` 三级外部图片接口

**文件：**
- 修改：`R/createShinyApp.R:1680-1800,1813-2100,2328-2375,2515-2550`
- 修改：`R/spatial_image_manifest.R`
- 修改：`man/createShinyApp.Rd`
- 修改：`tests/testthat/test-createShinyApp-sibling.R`
- 修改：`tests/testthat/test-createShinyApp-http-privacy.R`
- 修改：`tests/testthat/test-smoke-production.R`

- [ ] **步骤 1：编写三级 manifest 打包红灯测试**

创建包含 sliceA/sliceB/sliceC 的 CRB，调用：

```r
createShinyApp(
  cerebro_data = c(Atlas = crb),
  spatial_images = list(
    Atlas = list(
      sliceA = c("H&E 2" = second_png),
      sliceB = c("IF" = if_svg)
    )
  ),
  spatial_image_settings = list(
    Atlas = list(
      sliceA = list(
        "H&E 2" = list(offset_x = 10, flip_y = TRUE)
      )
    )
  ),
  result_dir = app,
  launch_browser = FALSE,
  verbose = FALSE
)
```

断言配置保留三级 names、相对路径位于 `spatial-assets/`、两个文件存在，并且
private CRB 不在公共资源映射中。当前预期 FAIL。

- [ ] **步骤 2：增加身份和兼容性红灯测试**

覆盖：unknown dataset、unknown spatial、内嵌/外部 label 冲突、重复 target basename、
setting 指向不存在图片；旧 `c(Atlas = path)` 在单 spatial CRB 成功，在多 spatial
CRB 报出所有可选名称。

- [ ] **步骤 3：运行红灯测试**

运行：

```bash
R -q -e 'devtools::test(filter = "createShinyApp-sibling|createShinyApp-http-privacy|smoke-production")'
```

预期：FAIL，原因是当前 builder 将 `spatial_images[[dataset]]` 当成无归属路径向量。

- [ ] **步骤 4：实现 App manifest validation**

在读取每个 CRB 的 preflight 阶段缓存 `availableSpatial()` 和各 entry 的 embedded
labels。规范化结果固定为：

```r
list(
  Atlas = list(
    sliceA = list(
      "H&E 2" = list(path = "/absolute/source.png")
    )
  )
)
```

所有 dataset/spatial/image/settings 引用必须在 staged copy 前通过。

- [ ] **步骤 5：复制图片并冻结 nested 配置**

资源目标包含稳定、冲突安全的层级：

```text
spatial-assets/{dataset-key}/{spatial-key}/{original-basename}
```

继续使用现有 `claim_target()` 防止两个源声明同一目标。配置仅存相对路径和经过
验证的 setting scalars。

- [ ] **步骤 6：更新 roxygen/Rd 与 legacy 参数说明**

加入 `spatial_image_settings` 正式参数；保留六个旧变换参数并标明只在 target 唯一
时规范化，不删除旧参数。

- [ ] **步骤 7：运行 App builder 聚焦测试**

运行：

```bash
R -q -e 'devtools::test(filter = "createShinyApp-sibling|createShinyApp-http-privacy|smoke-production")'
```

预期：PASS，且没有新增 HTTP exposure、transaction 或 path collision 回归。

- [ ] **步骤 8：提交**

```bash
git add R/createShinyApp.R R/spatial_image_manifest.R man/createShinyApp.Rd \
  tests/testthat/test-createShinyApp-sibling.R \
  tests/testthat/test-createShinyApp-http-privacy.R \
  tests/testthat/test-smoke-production.R
git commit -m "feat: bundle spatial images by dataset and spatial entry"
```

### 任务 4：让 Viewer 按当前 spatial entry 隔离图片和设置

**文件：**
- 修改：`inst/viewer/spatial/func_spatial_helpers.R`
- 修改：`inst/viewer/spatial/UI_projection_main_parameters.R`
- 修改：`inst/viewer/spatial/obj_projection_parameters_plot.R`
- 修改：`inst/viewer/spatial/obj_projection_background_controls.R`
- 修改：`inst/viewer/spatial/func_projection_update_plot.R`
- 修改：`tests/testthat/test-spatial-preset-resolver.R`
- 修改：`tests/testthat/test-spatial.R`
- 修改：`tests/testthat/test-app-inst.R`

- [ ] **步骤 1：编写 resolver 红灯测试**

固定配置：

```r
options <- list(
  spatial_images = list(
    Atlas = list(
      sliceA = c("H&E" = "a.png", "DAPI" = "a_dapi.png"),
      sliceB = c("IF" = "b.svg")
    )
  ),
  spatial_image_settings = list(
    Atlas = list(
      sliceA = list("DAPI" = list(scale_x = 1.2, flip_y = TRUE))
    )
  )
)
```

断言 sliceA 只得到 H&E/DAPI，sliceB 只得到 IF，sliceC 为空；设置只作用于
`Atlas/sliceA/DAPI`。当前预期 FAIL。

- [ ] **步骤 2：编写 embedded/external 合并与 selection reset 红灯测试**

embedded `H&E` 与 external `DAPI` 应按 label 合并；冲突由 builder 阻止。当前选中
`DAPI` 后切到 sliceB，应规范化为 sliceB 的第一张图片或 `No Background`，不得继续
渲染 sliceA 的 data URI。

- [ ] **步骤 3：运行红灯测试**

运行：

```bash
R -q -e 'devtools::test(filter = "spatial-preset-resolver|spatial$")'
```

预期：FAIL，旧 helper 只按 dataset 解析。

- [ ] **步骤 4：实现纯 resolver**

将 helper 接口改为显式接收 identity：

```r
configured_spatial_images(
  options,
  dataset,
  spatial_name
)

resolve_spatial_image_setting(
  options,
  dataset,
  spatial_name,
  image_label,
  setting,
  fallback
)

embedded_spatial_images(spatial_data)
```

Viewer 读取 legacy singular CRB 时以 `Tissue background` label 暴露。

- [ ] **步骤 5：把 spatial 和 image identity 贯穿 UI/server**

图片 selector 的 choices 使用显示 label，内部 value 使用 source-tagged stable key，
避免 embedded 与 external path 混淆。background control 和 plot parameters 使用当前
`spatial_projection_to_display`，per-image settings 在选择变化时重置。

- [ ] **步骤 6：验证坐标和图片解耦**

更新测试，确认切换背景不改变 x/y ranges、cells-to-show 或 spatial selection；切换
spatial entry 会更换图片 choices 但保留该 entry 的坐标。

- [ ] **步骤 7：运行 Viewer 测试**

运行：

```bash
R -q -e 'devtools::test(filter = "spatial-preset-resolver|spatial$|app-inst")'
```

预期：PASS，shinytest2 Spatial 页面没有 stale input 或旧图片残留。

- [ ] **步骤 8：提交**

```bash
git add inst/viewer/spatial tests/testthat/test-spatial-preset-resolver.R \
  tests/testthat/test-spatial.R tests/testthat/test-app-inst.R
git commit -m "feat: isolate backgrounds by spatial entry in Viewer"
```

### 任务 5：扩充完整 Omnibus Seurat 数据和可复现输入工件

**文件：**
- 修改：`data-raw/build_omnibus_demo.R`
- 修改：`inst/extdata/examples/demo_omnibus_seurat.rds`
- 修改：`inst/extdata/examples/demo_omnibus.crb`
- 创建：`inst/extdata/examples/demo_omnibus_markers.csv`
- 创建：`inst/extdata/examples/demo_omnibus_donorB_if.png`
- 创建：`inst/extdata/examples/demo_omnibus_donorC_review.png`
- 修改：`tests/testthat/test-omnibus-pipeline.R`

- [ ] **步骤 1：先把工件测试改成完整多 spatial 预期**

断言：

```r
expect_identical(
  sort(crb$availableSpatial()),
  sort(c("donorA tissue", "donorB tissue", "donorC tissue"))
)
expect_identical(
  names(crb$getSpatialData("donorA tissue")$histology_images),
  c("H&E", "DAPI")
)
expect_identical(
  names(crb$getSpatialData("donorB tissue")$histology_images),
  "H&E"
)
expect_length(
  crb$getSpatialData("donorC tissue")$histology_images,
  0L
)
```

还要验证每个 entry 恰好 40 个不同细胞、坐标布局/bounds 不同、metadata 含有
`condition`、marker CSV 与两张外部 PNG 可读。当前预期 FAIL。

- [ ] **步骤 2：运行红灯测试**

运行：

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline")'
```

预期：FAIL，因为当前只有 `omnibus_fov` 和一张图片。

- [ ] **步骤 3：扩充 deterministic builder**

固定 seed 继续使用 80 genes/120 cells。按 `orig.ident` 将细胞分成 donorA/B/C，
增加跨 donor 的 `condition` 分组，
为三个 entry 构造互不相同的圆形、矩形和三角形坐标。为 donorA 生成 H&E/DAPI，
为 donorB 生成 H&E；donorC 不声明图片。所有图片内容和 bounds 由脚本生成，无网络
输入。

- [ ] **步骤 4：生成公开外部输入**

builder 同时写出：

```text
demo_omnibus_markers.csv
demo_omnibus_donorB_if.png
demo_omnibus_donorC_review.png
```

CSV 覆盖所有 cell_type marker；两张 PNG 分别与 donorB、donorC bounds 对齐但不写入
源 Seurat misc。donorB 图片验证 converter 路径输入，donorC 图片验证
`createShinyApp()` 外部 manifest。

- [ ] **步骤 5：只通过 converter 生成 CRB**

builder 保存 staged Seurat 后调用公开 `convertSeuratToCerebro()`。禁止 readRDS CRB 后
补字段。重新读取 Seurat、CRB、CSV、PNG，完成 semantic validation 后原子替换五个
工件。

- [ ] **步骤 6：重新生成并跑工件测试**

运行：

```bash
Rscript data-raw/build_omnibus_demo.R
R -q -e 'devtools::test(filter = "omnibus-pipeline|spatial-image")'
```

预期：PASS，重复运行不残留 stage 目录。

- [ ] **步骤 7：提交**

```bash
git add data-raw/build_omnibus_demo.R inst/extdata/examples/demo_omnibus_seurat.rds \
  inst/extdata/examples/demo_omnibus.crb \
  inst/extdata/examples/demo_omnibus_markers.csv \
  inst/extdata/examples/demo_omnibus_donorB_if.png \
  inst/extdata/examples/demo_omnibus_donorC_review.png \
  tests/testthat/test-omnibus-pipeline.R
git commit -m "feat: expand Omnibus to multiple spatial entries and images"
```

### 任务 6：增加用户要求的两个公开函数完整调用测试

**文件：**
- 创建：`data-raw/verify_omnibus_public_api.R`
- 修改：`tests/testthat/test-omnibus-pipeline.R`

- [ ] **步骤 1：编写 H5 完整链路红灯测试**

测试从 committed RDS/CSV/PNG 复制到临时目录，执行与用户文档相同的调用：

```r
convertSeuratToCerebro(
  seurat_file = "inputs/demo_omnibus_seurat.rds",
  result_dir = "output",
  assay = "RNA",
  slot = "data",
  experiment_name = "Synthetic Omnibus",
  organism = "Human",
  groups = c("orig.ident", "condition", "cell_type"),
  groups_naming = list(
    "orig.ident" = "sample",
    "cell_type" = "cluster"
  ),
  marker_file = "inputs/demo_omnibus_markers.csv",
  marker_method = "Synthetic markers",
  spatial_images = list(
    "donorB tissue" = c(
      "IF panel" = "inputs/demo_omnibus_donorB_if.png"
    )
  ),
  expression_matrix_mode = "h5",
  verbose = FALSE
)

createShinyApp(
  cerebro_data = c(
    Omnibus = "output/cerebro_demo_omnibus_seurat.crb"
  ),
  spatial_images = list(
    Omnibus = list(
      "donorC tissue" = c(
        "Pathology review" = "inputs/demo_omnibus_donorC_review.png"
      )
    )
  ),
  result_dir = "my_app",
  welcome_message = "<h2>Synthetic Omnibus Atlas</h2>",
  port = 8080,
  host = "127.0.0.1",
  max_request_size = 8000,
  overwrite = TRUE,
  launch_browser = FALSE,
  verbose = FALSE
)
```

这里 donorB 图片通过 conversion 参数进入 CRB；donorC 图片通过
`createShinyApp(spatial_images = ...)` 进入 `spatial-assets/`，在同一字面调用链中证明
两种公开路径。

- [ ] **步骤 2：运行红灯并确认失败边界**

运行：

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline")'
```

预期：在任务 2/3 尚未全部联通时 FAIL；不得因为缺少测试输入或错误工作目录失败。

- [ ] **步骤 3：实现可直接运行的验证脚本**

`data-raw/verify_omnibus_public_api.R` 解析 repo root、创建临时或用户传入 output root，
按上面的字面调用执行，并用公开 getter 验证结果。脚本不得使用内部 `:::` 函数或
转换后 CRB mutation。

- [ ] **步骤 4：验证生成物和运行时**

测试断言：

- output CRB 与 sibling `.h5` 存在，backend location 为相对路径；
- sample/cluster 重命名和 marker import 生效；
- 三个 spatial entries、所有 embedded labels 和 coordinates-only entry 保留；
- generated app 的 private-data 同时包含 CRB/H5；
- external asset/config 保持 dataset/spatial/image 三层 identity；
- generated app 可在不调用 `CerebroNexus::` 的 Viewer source 下读取 CRB；
- `shiny::runApp()` 所需 `app.R`、config、viewer 和 extdata 均存在。

- [ ] **步骤 5：运行完整集成测试**

运行：

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline|export-data-integrity|smoke-production")'
```

预期：PASS；若 HDF5Array 不可用，只跳过专门的 H5 分支，embedded 链路仍必须执行。

- [ ] **步骤 6：提交**

```bash
git add data-raw/verify_omnibus_public_api.R tests/testthat/test-omnibus-pipeline.R
git commit -m "test: prove public Seurat to Shiny app workflow"
```

### 任务 7：同步文档和可复制示例

**文件：**
- 修改：`data-raw/omnibus.md`
- 修改：`data-raw/spatial.md`
- 修改：`data-raw/DATASETS.md`
- 修改：`NEWS.md`
- 修改：`R/convertSeuratToCerebro.R`
- 修改：`R/createShinyApp.R`
- 修改：对应 `man/*.Rd`

- [ ] **步骤 1：更新 API 文档契约测试**

在 `test-omnibus-pipeline.R` 或新 contract test 中断言帮助源包含：

```text
spatial_images
dataset -> spatial entry -> image label
expression_matrix_mode = "h5"
convertSeuratToCerebro(
createShinyApp(
```

预期在文档修改前 FAIL。

- [ ] **步骤 2：更新数据和平台说明**

明确说明 Visium slice、Xenium/MERFISH FOV、Slide-seq puck 均通过 Seurat
`Images(object)` 名称统一；donor 不是结构键的定义，只是可能的用户命名。

- [ ] **步骤 3：写入完整可复制调用**

文档先展示用户要求的 conversion 参数，再展示 `createShinyApp()`。示例使用真实
Omnibus 文件名、marker CSV、H5 mode、nested spatial images，不写抽象占位调用。

- [ ] **步骤 4：更新 NEWS 和生成文档**

运行：

```bash
R -q -e 'devtools::document()'
```

只保留与本计划公开接口有关的生成差异；恢复 roxygen 版本差异导致的无关变更。

- [ ] **步骤 5：运行文档和 API 测试**

运行：

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline|spatial-image|package-check-contract")'
```

预期：PASS，函数签名、Rd usage 和示例一致。

- [ ] **步骤 6：提交**

```bash
git add NEWS.md R man data-raw/omnibus.md data-raw/spatial.md data-raw/DATASETS.md \
  tests/testthat/test-omnibus-pipeline.R
git commit -m "docs: document multi-spatial image public API"
```

### 任务 8：最终规格审查和全量验证

**文件：**
- 审查：`docs/superpowers/specs/2026-08-10-multi-spatial-image-api-design.md`
- 审查：本计划列出的所有实现、测试、工件和文档文件

- [ ] **步骤 1：运行 focused specification matrix**

```bash
R -q -e 'devtools::test(filter = "spatial|omnibus-pipeline|createShinyApp-sibling|createShinyApp-http-privacy|smoke-production|export-data-integrity")'
```

预期：0 FAIL；只有仓库已知且与本修改无关的 warning/skip 可保留。

- [ ] **步骤 2：运行完整测试**

```bash
R -q -e 'result <- devtools::test(); summary <- as.data.frame(result); stopifnot(sum(summary$failed) == 0L, sum(summary$error) == 0L)'
```

预期：0 FAIL、0 ERROR。

- [ ] **步骤 3：安装并做静态包检查**

```bash
R CMD INSTALL .
check_tmp=$(mktemp -d /tmp/cerebro-multi-spatial-check.XXXXXX)
(cd "$check_tmp" && R CMD build --no-build-vignettes /Users/nuioi/projects/shiny/_wt_versionless_class)
(cd "$check_tmp" && R CMD check --no-tests --no-examples --no-manual --no-build-vignettes CerebroNexus_*.tar.gz)
```

预期：安装成功；源码、namespace、R/Rd、依赖和加载检查无本变更新增问题。仓库既有
vignette 构建告警单独记录，不归因于本计划。

- [ ] **步骤 4：执行工件和兼容审计**

```bash
git diff --check
R -q -e 'files <- list.files("inst/extdata/examples", pattern="[.]crb$", full.names=TRUE); stopifnot(all(vapply(files, function(x) inherits(readRDS(x), "Cerebro"), logical(1))))'
Rscript data-raw/verify_omnibus_public_api.R
```

确认 builder/coord-views 工作树未被写入，Omnibus builder 无 stage 残留，旧单图和
coordinates-only 数据均通过读取测试。

- [ ] **步骤 5：执行最终代码质量和规格覆盖审查**

逐项映射 design acceptance criteria 到测试名称；审查错误是否包含完整 identity、
是否有静默 fallback、是否复制 raw private data 到 public namespace、是否存在重复
normalizer。发现问题时先补失败测试，再修改实现。

- [ ] **步骤 6：提交最终审查修正**

若审查产生变更，使用对应任务中已经列出的精确文件清单执行 `git add`，然后：

```bash
git commit -m "fix: close multi-spatial image review findings"
```

最终要求 `git status --short` 无输出。
