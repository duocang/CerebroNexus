# Builder 冷启动优化设计

## 目标

让 CerebroNexus Builder 的页面在后台工作区尚未完成初始化时即可建立 Shiny 连接并响应用户，同时缩短 Worker 的真实冷启动时间和初始内存占用。优化必须保持现有上传、Open Project、数据集切换、Spatial FOV 切换和构建流程的行为不变。

## 已确认的瓶颈

首页 HTML 和静态资源不是主要瓶颈：本地热启动时 HTML 首字节约为 16 ms，Builder 自有前端资源总量约为 292 KB。长时间加载发生在浏览器收到页面之后。

当前会话会启动独立的 `callr::r_session`。在源码工作树模式下，子进程执行 `pkgload::load_all()`，随后又显式 source 多个 Builder 和 Viewer 模块。这个同步初始化链同时带来三类成本：

1. Shiny 会话在 Worker 可用前无法完成正常启动反馈。
2. 子进程重复加载整个包和大量暂未使用的 R 模块。
3. Spatial、Immune、HLA/TCR、Analysis 和 Build 能力在空白项目阶段就占用启动时间和内存。

现有 `session$onFlushed()` 加 `later::later()` 只延迟了启动调用时机；一旦回调进入 `builder_session_start()`，完整 Worker 初始化仍在当前 Shiny R 进程的事件循环中同步等待。因此它改善了首次 HTML flush，却没有消除随后出现的长时间不可响应阶段，也没有减少 Worker 自身的加载量。

## 方案

采用三层组合方案：非阻塞 Worker 生命周期、最小 bootstrap、能力按需加载。

### 非阻塞 Worker 生命周期

Worker 生命周期使用明确状态：`idle`、`starting`、`ready`、`failed`、`stopped`。首次 UI flush 后只发起进程创建和初始化请求，不等待初始化结果；现有轮询循环负责观察启动完成或失败。

依赖 Worker 的用户请求在 `starting` 期间进入现有服务端请求队列。Worker 变为 `ready` 后按原顺序派发。启动失败时队列不被静默丢弃，UI 显示失败原因并允许沿用现有重试入口。

Shiny 主事件循环不得执行等待 Worker 完成的阻塞调用。启动 API 必须快速返回一个可轮询的 Worker 句柄。

### 最小 Worker bootstrap

增加单一职责的 Worker bootstrap 模块。它只负责：

- 建立 Worker 协议和任务队列；
- 初始化数据集与快照注册表；
- 注册能力加载器；
- 接受、执行和返回请求；
- 报告启动状态与错误。

源码工作树模式不再在每个 Worker 内调用 `pkgload::load_all()`。bootstrap 使用明确的文件清单加载基础运行时，避免同时加载整个包命名空间后再重复 source Builder 文件。安装包模式和源码模式必须暴露相同的 Worker 协议。

### 能力按需加载

将 Worker 功能划分为可独立初始化且幂等的能力：

- `core`：导入、基础数据访问、快照与删除；
- `spatial`：坐标、图像预览、section bounds 和 Spatial 变换；
- `immune`：Immune repertoire、HLA 和 TCR；
- `analysis`：分析和扩展预览；
- `build`：计划校验、App bundle 和最终构建。

每种 Worker 请求声明所需能力。请求执行前调用 `ensure_capability(name)`；首次调用加载对应文件并记录为 ready，后续调用直接复用。能力加载失败只使当前请求失败，并保留可诊断的错误；不得把半初始化能力标记为 ready。

普通数据集切换和 FOV 切换只使用已经加载的 `core` 或 `spatial` 状态，不重新加载模块或重启 Worker。新上传数据仍沿用现有导入队列与快照身份规则。

## 状态与数据流

```text
浏览器收到页面
    -> Shiny 首次 flush
    -> 创建 starting Worker 句柄（立即返回）
    -> 后台 bootstrap 完成
    -> 轮询观察 ready
    -> 派发启动期间排队的请求

请求进入 Worker
    -> 映射所需 capability
    -> capability 已加载：直接执行
    -> capability 未加载：加载一次，再执行
    -> 返回现有响应 contract
```

服务端的 UI 状态只读取 Worker 生命周期，不根据计时器猜测 readiness。Worker 的数据集、快照和请求序列仍以现有 Worker 进程为唯一所有者，避免异步启动引入第二份业务状态。

## 兼容性与错误处理

- 保持现有 `builder_session_*()` 请求和响应 contract；允许内部 Worker 句柄增加启动状态。
- 在 Worker `starting` 时，调用方不得拿到伪造的 ready 进程；请求统一排队。
- session 结束时，无论 Worker 处于 `starting` 还是 `ready`，都必须终止子进程并清理轮询任务。
- bootstrap、能力加载和请求执行错误分别记录阶段信息，便于 UI 和测试区分。
- 不改变项目保存格式、Spatial 配置格式、快照身份或 Build 输出格式。

## 测试策略

测试遵循先失败再实现，聚焦真实生命周期而非静态字符串断言：

1. 使用可控的慢初始化替身证明首次 flush 不等待 Worker ready，启动 API在短时间内返回。
2. 验证 `starting` 期间提交的导入和 Open Project 恢复请求在 ready 后按顺序执行。
3. 验证 capability 首次请求加载一次，同类后续请求不重复加载。
4. 验证 Spatial 请求加载 `spatial`，基础导入不加载 `spatial`、`immune` 或 `build`。
5. 验证 bootstrap 或 capability 失败进入可观察的失败状态，不丢请求且不污染 ready 标记。
6. 运行现有 Worker、异步导入、Builder loading 和 Spatial 聚焦测试，确认公共 contract 不回退。

不把前端压缩作为本次主要工作。只有在结构优化完成后仍能测得显著的静态资源瓶颈，才单独处理资源打包；fixtures 清理也不进入本次启动生命周期修改。

## 成功标准

- 首页首次 flush 不等待完整 Worker 初始化。
- Worker 启动 API 不在 Shiny 主事件循环同步等待 bootstrap 完成。
- 源码模式 Worker 不调用 `pkgload::load_all()`。
- 空白项目不会加载 Spatial、Immune、Analysis 或 Build 能力。
- 首次相关请求自动加载所需能力，后续请求不重复加载。
- 启动期间提交的请求不丢失，现有数据集、FOV、Open Project 和 Build 行为保持兼容。
- 聚焦生命周期测试和相关现有测试通过。
