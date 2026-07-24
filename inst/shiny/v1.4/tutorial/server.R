##----------------------------------------------------------------------------##
## Tab: Tutorial.
##
## The end-to-end pipeline (raw data -> Seurat -> .crb -> app -> explore) as a
## styled stepper, an index of every tutorial, plus a screenshot gallery of the
## result. The screenshots are base64-inlined from www/tutorial/, so the
## section is fully self-contained and renders in exported standalone bundles
## too.
##
## All CSS references the app's :root design tokens (--c-*, --shadow-*, --r-*,
## --dur, --ease) defined in www/custom.css, so the tutorial page stays
## visually coherent with the console theme.
##----------------------------------------------------------------------------##
output[["tutorial_hub"]] <- renderText({
  root <- Cerebro.options[["cerebro_root"]]
  if (is.null(root) || !length(root) || !nzchar(as.character(root)[1])) {
    root <- "."
  }
  img_uri <- function(fname) {
    p <- file.path(root, "shiny/v1.4/www/tutorial", fname)
    if (!file.exists(p) || !requireNamespace("base64enc", quietly = TRUE)) {
      return("")
    }
    paste0("data:image/jpeg;base64,", base64enc::base64encode(p))
  }
  ## Link to the vignettes rendered to formatted HTML and served in-app at
  ## /cerebro_docs (see app.R addResourcePath). Opens a full, readable tutorial
  ## with copyable code — works offline and travels with exported bundles. Falls
  ## back to nothing gracefully if a file wasn't rendered.
  docs <- "cerebro_docs/"
  rendered <- character(0)
  tut_dir <- file.path(root, "shiny/v1.4/www/tutorials")
  if (dir.exists(tut_dir)) {
    rendered <- sub("\\.html$", "", list.files(tut_dir, pattern = "\\.html$"))
  }
  art <- function(slug, text) {
    if (!slug %in% rendered) {
      return("")
    }
    paste0(
      '<a class="cb-link" href="',
      docs,
      slug,
      '.html" target="_blank" rel="noopener">',
      text,
      "</a>"
    )
  }
  chip <- function(slug, text, cat = "") {
    if (!slug %in% rendered) {
      return("")
    }
    paste0(
      '<a class="cb-chip" data-cat="',
      cat,
      '" href="',
      docs,
      slug,
      '.html" target="_blank" rel="noopener">',
      text,
      "</a>"
    )
  }
  paste0(
    '<div class="cb-tut">
<style>
/* ---- Tutorial hub theme — references console :root tokens from custom.css -- */
.cb-tut{margin:0;color:var(--c-text);font-family:var(--font-sans)}

/* Hero: full-width banner flush to content edges, matching .cerebro-welcome */
.cb-hero{margin:-18px -26px 26px;padding:30px 26px 24px;background:linear-gradient(180deg,var(--c-white),var(--c-bg));border-bottom:1px solid var(--c-border)}
.cb-hero h1{font-size:26px;font-weight:700;letter-spacing:-0.02em;color:var(--c-text);margin:0 0 6px}
.cb-hero .cb-hero-sub{font-size:15px;color:var(--c-text-2);margin:0 0 16px;max-width:720px}
.cb-hero .cb-pipe{display:flex;gap:8px;flex-wrap:wrap;align-items:center}
.cb-hero .cb-pstep{padding:5px 13px;border-radius:var(--r-pill);font-size:12px;font-weight:700;white-space:nowrap;color:var(--c-text-2);border:1px solid var(--c-border);background:var(--c-surface)}
.cb-hero .cb-pstep.on{color:var(--c-white);background:var(--c-amber);border-color:var(--c-amber)}
.cb-hero .cb-parrow{color:var(--c-text-3);font-size:13px;align-self:center;flex-shrink:0}

/* 5-step cards grid — responsive, console-card styling */
.cb-steps{display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));gap:16px;margin-bottom:32px}
.cb-step{background:var(--c-surface);border:1px solid var(--c-border);border-radius:var(--r-lg);padding:20px 18px 16px;position:relative;box-shadow:var(--shadow-1);transition:box-shadow var(--dur) var(--ease),transform var(--dur) var(--ease);display:flex;flex-direction:column;min-width:0}
.cb-step:hover{box-shadow:var(--shadow-2);transform:translateY(-2px)}
.cb-num{position:absolute;top:-11px;left:15px;width:24px;height:24px;border-radius:50%;background:var(--c-amber);color:var(--c-white);font-size:12px;font-weight:700;display:flex;align-items:center;justify-content:center}
.cb-ic{font-size:22px;line-height:1;margin-top:2px}
.cb-step h3{margin:8px 0 4px;font-size:15px;font-weight:650;color:var(--c-text);letter-spacing:-0.01em}
.cb-step p{font-size:12.5px;color:var(--c-text-2);margin:6px 0 10px;line-height:1.55}
.cb-links{margin-top:auto;padding-top:2px}
.cb-links a.cb-link{display:block}
.cb-links a.cb-link+.cb-link{margin-top:6px}

/* Code blocks — console light-surface style with amber accent border */
.cb-code{background:var(--c-surface-2);color:var(--c-text);font-family:var(--font-mono);font-size:11.5px;line-height:1.55;padding:11px 13px;border-radius:var(--r-sm);border:1px solid var(--c-border);border-left:3px solid var(--c-amber);overflow-x:auto;white-space:pre;margin:0 0 11px}

/* Inline code — amber tint */
.cb-inline{font-family:var(--font-mono);font-size:.92em;color:var(--c-amber-700);background:var(--c-amber-50);padding:1px 5px;border-radius:4px}

/* Links — amber accent, underline on hover */
.cb-link{font-size:12px;font-weight:600;color:var(--c-amber);text-decoration:none;white-space:nowrap}
.cb-link:hover{color:var(--c-amber-700);text-decoration:underline;text-underline-offset:2px}

/* Tutorial index — chip grid with category color hints */
.cb-modules{margin-top:8px}
.cb-group{margin-bottom:18px}
.cb-grp{display:flex;align-items:center;gap:7px;font-size:11.5px;font-weight:800;text-transform:uppercase;letter-spacing:.06em;color:var(--c-text-2);margin:0 0 9px}
.cb-grp::before{content:"";display:inline-block;width:7px;height:7px;border-radius:50%;flex-shrink:0;background:var(--c-amber)}
.cb-grp[data-cat="data"]::before{background:#3b82f6}
.cb-grp[data-cat="workflow"]::before{background:var(--c-amber)}
.cb-grp[data-cat="deploy"]::before{background:#10b981}
.cb-grp[data-cat="analysis"]::before{background:#8b5cf6}
.cb-grp[data-cat="reference"]::before{background:#64748b}
.cb-subgrp{display:block;font-size:11px;font-weight:650;color:var(--c-text-3);margin:12px 0 8px;padding-left:1px}
.cb-chips{display:flex;flex-wrap:wrap;gap:9px}
.cb-chip{font-size:13px;padding:7px 15px;border:1px solid var(--c-border);border-radius:var(--r-pill);color:var(--c-text);text-decoration:none;background:var(--c-surface);cursor:pointer;position:relative;transition:all var(--dur) var(--ease)}
.cb-chip:hover{color:var(--c-amber-700);background:var(--c-amber-50);border-color:var(--c-amber);transform:translateY(-1px) scale(1.03);box-shadow:0 4px 14px rgba(249,115,22,.15)}
.cb-chip:active{transform:translateY(0) scale(.98)}
/* Per-category hover state — each group gets its own accent color */
.cb-chip[data-cat="data"]:hover{color:#1d4ed8;background:#eff6ff;border-color:#3b82f6;box-shadow:0 4px 14px rgba(59,130,246,.15)}
.cb-chip[data-cat="deploy"]:hover{color:#047857;background:#ecfdf5;border-color:#10b981;box-shadow:0 4px 14px rgba(16,185,129,.15)}
.cb-chip[data-cat="analysis"]:hover{color:#6d28d9;background:#f5f3ff;border-color:#8b5cf6;box-shadow:0 4px 14px rgba(139,92,246,.15)}
.cb-chip[data-cat="reference"]:hover{color:#334155;background:#f8fafc;border-color:#64748b;box-shadow:0 4px 14px rgba(100,116,139,.15)}

/* Gallery — screen-shot cards with zoom-on-hover */
.cb-gallery{margin-top:32px}
.cb-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:16px}
.cb-shot{border:1px solid var(--c-border);border-radius:var(--r-lg);overflow:hidden;background:var(--c-surface);box-shadow:var(--shadow-1);transition:box-shadow var(--dur) var(--ease),transform var(--dur) var(--ease)}
.cb-shot:hover{box-shadow:var(--shadow-2);transform:translateY(-2px)}
.cb-shot img{width:100%;display:block;border-bottom:1px solid var(--c-border);transition:transform .3s var(--ease)}
.cb-shot:hover img{transform:scale(1.03)}
.cb-cap{padding:10px 14px;font-size:12.5px;color:var(--c-text-2);line-height:1.5}
.cb-cap b{color:var(--c-text)}

/* Section headings */
.cb-tut h2{font-size:20px;font-weight:700;margin:0 0 4px;color:var(--c-text);letter-spacing:-0.012em}
.cb-tut h3{font-size:16px;font-weight:650;margin:0;color:var(--c-text)}
.cb-tut .cb-tut-sub{color:var(--c-text-2);font-size:13px;margin:0 0 20px}

/* Staggered fade-in animation for step cards */
.cb-steps .cb-step{animation:cb-step-in .4s var(--ease) backwards}
.cb-steps .cb-step:nth-child(1){animation-delay:0s}
.cb-steps .cb-step:nth-child(2){animation-delay:.1s}
.cb-steps .cb-step:nth-child(3){animation-delay:.2s}
.cb-steps .cb-step:nth-child(4){animation-delay:.3s}
.cb-steps .cb-step:nth-child(5){animation-delay:.4s}
@keyframes cb-step-in{from{opacity:0;transform:translateY(14px)}to{opacity:1;transform:translateY(0)}}

/* Responsive: on narrow screens, bring hero padding in */
@media (max-width:767px){.cb-hero{margin:-18px -14px 20px;padding:22px 14px 20px}}
</style>

<!-- ===== Hero ===== -->
<div class="cb-hero">
<h1>Build your own Cerebro</h1>
<p class="cb-hero-sub">From raw single-cell data to an interactive, shareable web app — five steps, one pipeline.</p>
<div class="cb-pipe">
<span class="cb-pstep on">1 &middot; Raw data</span><span class="cb-parrow">&rarr;</span>
<span class="cb-pstep">2 &middot; Seurat</span><span class="cb-parrow">&rarr;</span>
<span class="cb-pstep">3 &middot; Export .crb</span><span class="cb-parrow">&rarr;</span>
<span class="cb-pstep">4 &middot; Build app</span><span class="cb-parrow">&rarr;</span>
<span class="cb-pstep">5 &middot; Explore</span>
</div>
</div>

<!-- ===== 5-step pipeline cards ===== -->
<div class="cb-steps">
<div class="cb-step"><span class="cb-num">1</span><div class="cb-ic">&#128229;</div>
<h3>Download raw data</h3>
<p>Get count matrices or FASTQs from a public repository (10x Genomics, GEO) or your own sequencing run.</p>
<div class="cb-links"><a class="cb-link" href="https://www.10xgenomics.com/datasets" target="_blank" rel="noopener">10x datasets &rarr;</a><a class="cb-link" href="https://www.ncbi.nlm.nih.gov/geo/" target="_blank" rel="noopener">GEO &rarr;</a></div>
</div>
<div class="cb-step"><span class="cb-num">2</span><div class="cb-ic">&#129516;</div>
<h3>Process in Seurat</h3>
<p>QC, normalise, cluster, run UMAP and annotate cell types &mdash; the standard pipeline.</p>
<div class="cb-code">seu &lt;- CreateSeuratObject(counts) |&gt;
  NormalizeData() |&gt;
  FindVariableFeatures() |&gt;
  ScaleData() |&gt; RunPCA() |&gt;
  RunUMAP(dims = 1:30) |&gt;
  FindNeighbors() |&gt; FindClusters()</div>
<div class="cb-links"><a class="cb-link" href="https://satijalab.org/seurat/articles/pbmc3k_tutorial" target="_blank" rel="noopener">Seurat guided clustering &rarr;</a></div>
</div>
<div class="cb-step"><span class="cb-num">3</span><div class="cb-ic">&#128230;</div>
<h3>Export to Cerebro (.crb)</h3>
<p>One call packs projections, metadata, markers, immune repertoire and spatial data into one portable <span class="cb-inline">.crb</span> file.</p>
<div class="cb-code">library(CerebroNexus)
exportFromSeurat(seu,
  file = "dataset.crb",
  experiment_name = "my_experiment",
  organism = "hg",
  groups = c("sample", "cell_type"))</div>
<div class="cb-links">',
    art("cerebroApp_workflow_Seurat", "Seurat &rarr; Cerebro workflow &rarr;"),
    '</div>
</div>
<div class="cb-step"><span class="cb-num">4</span><div class="cb-ic">&#128295;</div>
<h3>Build the app</h3>
<p>Bundle one or more <span class="cb-inline">.crb</span> files into a standalone Shiny app &mdash; run it locally or host it online. Viewers need no package install.</p>
<div class="cb-code">createShinyApp(
  cerebro_data = "dataset.crb",
  result_dir   = "my_cerebro_app")
# then: shiny::runApp(result_dir)</div>
<div class="cb-links">',
    art("create_a_self_contained_shiny_app", "Self-contained app &rarr;"),
    art("host_cerebro_on_shinyapps", "Host online &rarr;"),
    '</div>
</div>
<div class="cb-step"><span class="cb-num">5</span><div class="cb-ic">&#128269;</div>
<h3>Explore &amp; present</h3>
<p>Open the app and explore every modality &mdash; dimensional reduction, immune repertoire, spatial maps and coordinated linked views.</p>
<div class="cb-links"><a class="cb-link" href="#cb-gallery">See the gallery &darr;</a></div>
</div>
</div>

<!-- ===== All tutorials index ===== -->
<div class="cb-modules">
<h3>All tutorials</h3>
<p class="cb-tut-sub" style="margin:2px 0 15px">Full step-by-step guides &mdash; click any to open the formatted tutorial (copyable code, figures, run instructions), served in-app.</p>
<div class="cb-group"><span class="cb-grp" data-cat="data">Data</span><div class="cb-chips">',
    chip("demo_datasets", "Demo datasets: overview", "data"),
    chip("demo_dataset_omnibus", "Omnibus (all modalities)", "data"),
    '</div>
<span class="cb-subgrp">Spatial platforms</span><div class="cb-chips">',
    chip("demo_dataset_visium", "10x Visium", "data"),
    chip("demo_dataset_slideseq", "Slide-seq v2", "data"),
    chip("demo_dataset_merfish", "MERFISH", "data"),
    chip("demo_dataset_xenium", "10x Xenium", "data"),
    chip("demo_dataset_trekker", "Trekker spatial mapping", "data"),
    '</div>
<span class="cb-subgrp">Other modalities</span><div class="cb-chips">',
    chip("demo_dataset_pbmc_tcr_bcr", "PBMC - Full (T+B)", "data"),
    chip("demo_dataset_hla_tcr_dextramer", "HLA &amp; TCR dextramer", "data"),
    chip("demo_dataset_trajectory", "Trajectory (B cells)", "data"),
    '</div></div>
<div class="cb-group"><span class="cb-grp" data-cat="workflow">Workflow</span><div class="cb-chips">',
    chip("cerebroApp_workflow_Seurat", "Seurat &rarr; Cerebro", "workflow"),
    chip(
      "export_a_data_set_in_SCE_format",
      "From SingleCellExperiment",
      "workflow"
    ),
    chip(
      "create_expression_matrix_in_h5_format",
      "Expression matrix (h5)",
      "workflow"
    ),
    '</div></div>
<div class="cb-group"><span class="cb-grp" data-cat="deploy">Build &amp; deploy</span><div class="cb-chips">',
    chip("create_a_self_contained_shiny_app", "Self-contained app", "deploy"),
    chip("host_cerebro_on_shinyapps", "Host on shinyapps.io", "deploy"),
    chip(
      "launch_cerebro_with_pre-loaded_data_set",
      "Pre-loaded data set",
      "deploy"
    ),
    chip("multi_crb", "Multiple data sets", "deploy"),
    chip("control_access_to_cerebro_with_a_login_page", "Login page", "deploy"),
    '</div></div>
<div class="cb-group"><span class="cb-grp" data-cat="analysis">Analysis modules</span><div class="cb-chips">',
    chip("immune_repertoire_analysis", "Immune repertoire", "analysis"),
    chip("spatial_analysis", "Spatial", "analysis"),
    chip("trajectory_analysis", "Trajectory", "analysis"),
    chip("trekker_spatial_mapping", "Trekker mapping", "analysis"),
    chip("hla_tcr_motifs", "HLA &amp; TCR motifs", "analysis"),
    chip("hla_bulk_tcr_associations", "HLA&ndash;TCR associations", "analysis"),
    chip("hla_tcr_antigen_selected", "Antigen-selected TCRs", "analysis"),
    chip("enriched_pathways", "Enriched pathways", "analysis"),
    chip("most_expressed_genes", "Most-expressed genes", "analysis"),
    chip(
      "export_and_visualize_custom_tables_and_plots",
      "Custom tables &amp; plots",
      "analysis"
    ),
    chip("extra_material", "Extra material", "analysis"),
    '</div></div>
<div class="cb-group"><span class="cb-grp" data-cat="reference">Reference</span><div class="cb-chips">',
    chip(
      "overview_of_cerebro_v1.3_class",
      "Cerebro object (v1.3)",
      "reference"
    ),
    chip("Cerebro_interface_v1.2", "Interface tour", "reference"),
    chip("release_notes_v1.3", "Release notes", "reference"),
    '</div></div>
</div>

<!-- ===== Screenshot gallery ===== -->
<div class="cb-gallery" id="cb-gallery">
<h2>The result &mdash; pages your users explore</h2>
<div class="cb-grid">
<div class="cb-shot"><img src="',
    img_uri("page_projection.jpeg"),
    '" alt="Dimensional reduction" loading="lazy"><div class="cb-cap"><b>Dimensional reduction</b> &mdash; UMAP / tSNE coloured by any variable</div></div>
<div class="cb-shot"><img src="',
    img_uri("page_immune.jpeg"),
    '" alt="Immune repertoire" loading="lazy"><div class="cb-cap"><b>Immune repertoire</b> &mdash; clonal expansion mapped onto the cell UMAP</div></div>
<div class="cb-shot"><img src="',
    img_uri("page_linked.jpeg"),
    '" alt="Linked views" loading="lazy"><div class="cb-cap"><b>Linked views</b> &mdash; one selection, highlighted across every modality</div></div>
<div class="cb-shot"><img src="',
    img_uri("page_spatial.jpeg"),
    '" alt="Spatial" loading="lazy"><div class="cb-cap"><b>Spatial</b> &mdash; cells in their physical tissue coordinates</div></div>
</div>
</div>
</div>'
  )
})
