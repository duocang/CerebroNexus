# HLA/TCR 可复现论文案例

完整方法、结果和科学边界分别在两篇文章中：

- `vignettes/hla_tcr_antigen_selected.Rmd`：主要生物学案例，按严格 TRB
  `V/J/CDR3` 定义 clonotype。
- `vignettes/hla_tcr_main_case.Rmd`：次要交互案例，按 Viewer 的 `CTgene`
  定义扩增 clone，演示 Linked views、motif 和分享。

两者使用同一个真实、抗原筛选的 10x 单细胞 Cerebro 对象，但不能合并为
同一个生物学结论。Dextramer 字段是 reagent binder call，不是已验证的肽段
特异性；4 个供者也不足以支持群体层面的 HLA 关联推断。

## 证据产物

统一入口从 `.crb` 重新计算两个案例，并生成：

- `demo_hla_tcr_publication.manifest.json`：数据指纹、核心结果和全部哈希；
- `demo_hla_tcr_publication.cohort.csv`：供者级队列与选择计数；
- `demo_hla_tcr_publication.hla.csv`：发表的供者 HLA typing；
- `demo_hla_tcr_publication.sequences.csv`：两个案例的 TRB 序列计数；
- 两个 Linked views JSON 和严格案例 barcode TSV；
- 论文 UMAP、motif 和 genotype-context SVG 图。

文章中的核心数字通过 inline R 从这些产物读取，不再手工维护第三份结果。

## 日常复现

仅使用 Git 中约 10 MB 的完整 Cerebro 对象，不需要 Seurat `.rds` 或原始缓存：

```bash
Rscript data-raw/build_hla_tcr_publication.R --from-crb --verify
Rscript -e 'devtools::test(filter="hla-tcr-publication")'
```

去掉 `--verify` 会重新发布统一证据产物。

## 发布级原始数据审计

发布前可从已固定字节数和 SHA-256 的 12 个 10x 文件重建 `.crb`，再走完全
相同的证据生成路径：

```bash
Rscript data-raw/build_hla_tcr_publication.R --from-raw
Rscript data-raw/build_hla_tcr_publication.R --from-crb --verify
```

原始下载和解压缓存约 2.7 GB，位于 Git 忽略的
`data-raw/vdj_10x_dextramer/`。它不是日常测试前提，也不会进入 Git。

## Viewer 验收

导入 `demo_hla_tcr_dextramer.linked-views.json` 验收严格案例；导入
`demo_hla_tcr_main_case.linked-view.json` 验收次要 Viewer 案例。长期标准答案
是 JSON、barcode、CSV 和 manifest，不是 90 天后过期的分享 URL。
