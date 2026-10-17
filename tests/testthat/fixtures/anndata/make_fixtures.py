"""Regenerate the tiny h5ad fixtures used by test-h5ad.R.

Written with the real anndata/scanpy so the tests exercise the on-disk format
as the Python ecosystem produces it (not a hand-written imitation):

    python -m venv .venv && .venv/Scripts/pip install anndata scanpy
    .venv/Scripts/python make_fixtures.py

Fixtures were generated with anndata 0.13.4, scanpy 1.12.4, h5py 3.16.0.

Everything is deterministic and self-describing so the tests need no external
truth: X is log1p(counts normalised to 1e4), cell_type is blocked 40/40/40,
batch_note is NaN on every 4th cell, n_flag is missing on every 7th cell.
"""
import warnings; warnings.filterwarnings("ignore")
from pathlib import Path
import numpy as np, pandas as pd, scipy.sparse as sp, anndata as ad, scanpy as sc

here = Path(__file__).parent
rng = np.random.default_rng(7)
n_per, types = 40, ["T cell", "B cell", "Monocyte"]
n_obs, n_vars = n_per * len(types), 60
genes = ["CD3D", "CD3E", "MS4A1", "CD79A", "LYZ", "S100A8"] + [f"GENE{i:03d}" for i in range(n_vars - 6)]
cells = [f"cell{i:03d}" for i in range(n_obs)]
cell_type = np.repeat(types, n_per)

mu = np.full((n_obs, n_vars), 0.6)
for t, cols in zip(types, ([0, 1], [2, 3], [4, 5])):
    mu[np.ix_(cell_type == t, cols)] = 12.0
counts = sp.csr_matrix(rng.poisson(mu).astype("float32"))
lib = np.asarray(counts.sum(1)).ravel()
logx = sp.csr_matrix(counts.multiply(1e4 / lib[:, None]).log1p().astype("float32"))

def build():
    obs = pd.DataFrame({
        "cell_type": pd.Categorical(cell_type, categories=types),
        "donor": [f"d{i % 3}" for i in range(n_obs)],
        "batch_note": pd.Categorical([None if i % 4 == 0 else "ok" for i in range(n_obs)]),
        "is_flagged": np.arange(n_obs) % 5 == 0,
        "n_flag": pd.array([None if i % 7 == 0 else i % 5 for i in range(n_obs)], dtype="Int64"),
        "total_counts": lib.astype("float32"),
        "n_genes_by_counts": np.asarray((counts > 0).sum(1)).ravel().astype("int64"),
    }, index=cells)
    a = ad.AnnData(X=logx.copy(), obs=obs,
                   var=pd.DataFrame({"gene_ids": [f"ENSG{i:05d}" for i in range(n_vars)]}, index=genes))
    a.layers["counts"] = counts.copy()
    ang = np.linspace(0, 2 * np.pi, n_obs)
    a.obsm["X_umap"] = np.c_[np.cos(ang), np.sin(ang)] * 5
    a.obsm["X_pca"] = rng.normal(size=(n_obs, 5))
    a.obsm["X_single"] = rng.normal(size=(n_obs, 1))          # 1 column: must be skipped
    return a

a = build()
sc.tl.rank_genes_groups(a, "cell_type", method="t-test", n_genes=8)
a.write_h5ad(here / "small_csr.h5ad")

c = build(); c.X = sp.csc_matrix(c.X); c.write_h5ad(here / "small_csc.h5ad")
d = build(); d.X = np.asarray(d.X.todense()); d.layers.clear(); d.write_h5ad(here / "small_dense.h5ad")
for f in sorted(here.glob("*.h5ad")): print(f.name, f.stat().st_size, "bytes")
