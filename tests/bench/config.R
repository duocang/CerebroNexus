BENCH_CONFIG <- list(
  schema_version = 1L,
  source = list(
    key = "human_pfc_mssm",
    url = paste0(
      "https://datasets.cellxgene.cziscience.com/",
      "0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad"
    ),
    expected_bytes = 36092176654,
    expected_sha256 = paste0(
      "c62456941372b90bcf0df38e8cb1c34d",
      "d060bc5a507270ab1d068cbe6f1dfd54"
    ),
    n_cells = 4140453L,
    group = "X",
    organism = "hg38",
    slot = "data"
  ),
  comparison_fixed_tiers = c(
    tier_1k = 1000L,
    tier_5k = 5000L,
    tier_10k = 10000L,
    tier_25k = 25000L,
    tier_100k = 100000L,
    tier_250k = 250000L
  ),
  common_target = 500000L,
  common_min_exclusive = 250000L,
  full_scale_fixed_tiers = c(
    tier_1m = 1000000L,
    tier_2m = 2000000L,
    full = 4140453L
  ),
  comparison_backends = c("embedded", "bpcells", "h5"),
  comparison_repeats = 3L,
  full_scale_repeats = 4L,
  query_genes = 5L,
  warmed_iterations = 5L,
  rss_poll_ms = 500L,
  sparse_index_limit = .Machine$integer.max
)
