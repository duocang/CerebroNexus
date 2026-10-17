"""Regenerate the tiny h5ad fixtures used by test-h5ad.R.

Written with the real anndata/scanpy so the tests exercise the on-disk format
as the Python ecosystem produces it (not a hand-written imitation):

    python -m venv .venv && .venv/Scripts/pip install anndata scanpy
    .venv/Scripts/python make_fixtures.py          # small_csr / csc / dense
    .venv/Scripts/python make_fixtures.py extra    # small_ensembl / spatial / mudata
    .venv/Scripts/pip install awkward
    .venv/Scripts/python make_fixtures.py airr     # small_airr (h5mu, h5ad) / small_awkward

Fixtures were generated with anndata 0.13.4, scanpy 1.12.4, h5py 3.16.0
(the airr ones also with awkward 2.14.0).

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

import sys
which = sys.argv[1] if len(sys.argv) > 1 else "base"

if which == "base":
    a = build()
    sc.tl.rank_genes_groups(a, "cell_type", method="t-test", n_genes=8)
    a.write_h5ad(here / "small_csr.h5ad")

    c = build(); c.X = sp.csc_matrix(c.X); c.write_h5ad(here / "small_csc.h5ad")
    d = build(); d.X = np.asarray(d.X.todense()); d.layers.clear(); d.write_h5ad(here / "small_dense.h5ad")

if which == "extra":
    # CELLxGENE-style: Ensembl IDs as var_names and the symbols in var["feature_name"]
    e = build()
    e.var["feature_name"] = list(e.var_names)
    e.var_names = [f"ENSG{i:011d}" for i in range(e.n_vars)]
    e.write_h5ad(here / "small_ensembl.h5ad")

    # two tissue sections in one coordinate frame: obsm["spatial"] + obs["section"]
    sp_ad = build()
    rng2 = np.random.default_rng(11)
    sp_ad.obs["section"] = pd.Categorical(["s1" if i % 2 == 0 else "s2" for i in range(sp_ad.n_obs)])
    sp_ad.obsm["spatial"] = rng2.uniform(0, 1, size=(sp_ad.n_obs, 2))
    sp_ad.write_h5ad(here / "small_spatial.h5ad")

    # MuData-style .h5mu: modalities are AnnData groups under mod/<name>
    import h5py
    from anndata.io import write_elem
    prot = ad.AnnData(X=np.abs(rng.normal(size=(n_obs, 4))).astype("float32"),
                      obs=pd.DataFrame(index=cells), var=pd.DataFrame(index=[f"P{i}" for i in range(4)]))
    with h5py.File(here / "small_mudata.h5mu", "w") as f:
        mod = f.create_group("mod")
        write_elem(mod, "gex", build())
        write_elem(mod, "prot", prot)

if which == "airr":
    # scirpy-style immune receptor data: obsm["airr"] is an awkward array, one
    # (possibly empty) list of AIRR chains per cell.
    import awkward as ak
    from awkward.contents import (NumpyArray, ListArray, ListOffsetArray, RecordArray,
                                  IndexedArray, ByteMaskedArray, RegularArray)
    from awkward.index import Index64, Index8

    AA = "ACDEFGHIKLMNPQRSTVWY"

    def chain(locus, v, d, j, c, aa, count, productive=True, junction_aa="auto"):
        ja = f"C{aa}F" if junction_aa == "auto" else junction_aa
        return dict(locus=locus, v_call=v, d_call=d, j_call=j, c_call=c, junction_aa=ja,
                    junction=None if ja is None else "TGT" + "GCC" * len(aa) + "TTT",
                    consensus_count=count * 100, duplicate_count=count, productive=productive,
                    sequence_id=None)

    def tcr(clone, extra_alpha=False):
        a = AA[clone % 20] * 3
        out = [chain("TRA", f"TRAV{clone % 4 + 1}", None, f"TRAJ{clone % 3 + 1}", "TRAC", "AV" + a, 5),
               chain("TRB", f"TRBV{clone % 5 + 1}", f"TRBD{clone % 2 + 1}", f"TRBJ{clone % 3 + 1}", "TRBC1", "SS" + a, 8)]
        if extra_alpha:
            out.insert(1, chain("TRA", "TRAV9", None, "TRAJ9", "TRAC", "AV" + AA[(clone + 7) % 20] * 3, 2))
        return out

    def bcr(clone):
        a = AA[clone % 20] * 4
        light = ("IGK", "IGKV1", "IGKJ1", "IGKC") if clone % 2 == 0 else ("IGL", "IGLV2", "IGLJ2", "IGLC1")
        return [chain("IGH", f"IGHV{clone % 3 + 1}", "IGHD3", "IGHJ4", "IGHM", "AR" + a, 9),
                chain(light[0], light[1], None, light[2], light[3], "QQ" + a, 4)]

    chains = {}
    for i, name in enumerate(cells):
        if i % 6 == 5:
            chains[name] = []                                  # no receptor found
        elif i < 40:                                           # T cells
            chains[name] = tcr(i % 8, extra_alpha=(i % 10 == 1))
            if i == 3: chains[name] = [chain("TRB", "TRBV1", None, "TRBJ1", "TRBC1", "SSXX", 3, productive=False)]
            if i == 7: chains[name][0]["junction_aa"] = chains[name][0]["junction"] = None
            if i == 9: chains[name] = chains[name][1:] + [chain("Multi", None, None, None, None, "ZZ", 1)]
        elif i < 80:                                           # B cells
            chains[name] = bcr(i % 5)
            if i == 44: chains[name] = tcr(2) + bcr(2)[:1]     # 2 TCR chains beat 1 BCR chain
            if i == 46:                                        # tie in chains, BCR has more UMIs
                chains[name] = [chain("IGH", "IGHV1", None, "IGHJ1", "IGHM", "ARTT", 10),
                                chain("TRB", "TRBV1", None, "TRBJ1", "TRBC1", "SSTT", 2)]
        else:                                                  # monocytes: no receptors
            chains[name] = []
            if i == 82: chains[name] = [chain("TRB", "TRBV1", None, "TRBJ1", "TRBC1", "SSYY", 1, productive=False)]
    for ghost in ("ghost0", "ghost1"):
        chains[ghost] = tcr(1)

    def airr_array(order):
        return ak.Array([chains[c] for c in order])

    gex = build()
    gex.obs["sample"] = pd.Categorical(["S1" if i % 2 == 0 else "S2" for i in range(gex.n_obs)])

    # MuData as scirpy writes it: gex + airr modalities. The airr modality lists
    # its cells in another order, lacks the last two gex cells, and has two cells
    # (ghost*) that are not in the expression data.
    import h5py
    from anndata.io import write_elem
    airr_cells = list(reversed(cells[:-2])) + ["ghost0", "ghost1"]
    airr_mod = ad.AnnData(X=np.zeros((len(airr_cells), 0)),
                          obs=pd.DataFrame({"is_cell": True}, index=airr_cells),
                          var=pd.DataFrame(index=[]))
    airr_mod.obsm["airr"] = airr_array(airr_cells)
    with h5py.File(here / "small_airr.h5mu", "w") as f:
        mod = f.create_group("mod")
        write_elem(mod, "gex", gex)
        write_elem(mod, "airr", airr_mod)

    # plain h5ad with obsm["airr"] (no sample column)
    plain = build()
    plain.obsm["airr"] = airr_array(cells)
    plain.write_h5ad(here / "small_airr.h5ad")

    # the awkward layouts the reader supports, as a list of records per row
    record = RecordArray(
        [ak.to_layout(["TRA", "TRB", "TRA", "TRB", "IGH", "IGK", "TRA", "TRB"]),
         ak.to_layout(["V1", "V2", None, "V4µ", "V5", "V6", "V7", "V8"]),
         ak.to_layout([5, None, 3, 4, 9, 8, 7, 6]),
         ByteMaskedArray(Index8(np.array([1, 0, 1, 1, 1, 1, 0, 1], dtype="int8")),
                         NumpyArray(np.arange(8) * 10), valid_when=True),
         NumpyArray(np.array([True, False, True, True, False, True, False, True]))],
        ["locus", "v_call", "count", "masked", "flag"])
    layouts = {
        "offsets": ListOffsetArray(Index64(np.array([0, 2, 2, 5, 5, 8, 8])), record),
        "listarray": ListArray(Index64(np.array([4, 0, 6, 2, 7, 2])), Index64(np.array([6, 2, 6, 4, 8, 2])), record),
        "indexed": ListOffsetArray(Index64(np.array([0, 2, 3, 3, 6, 8, 8])),
                                   IndexedArray(Index64(np.arange(7, -1, -1)), record)),
        "regular": RegularArray(IndexedArray(Index64(np.array([0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3])), record), 2),
    }
    tiny = ad.AnnData(X=np.zeros((6, 2), dtype="float32"),
                      obs=pd.DataFrame(index=[f"c{i}" for i in range(6)]), var=pd.DataFrame(index=["g1", "g2"]))
    for name, layout in layouts.items():
        tiny.obsm[name] = ak.Array(layout)
    tiny.write_h5ad(here / "small_awkward.h5ad")
    print("chains in small_airr:", sum(len(v) for v in chains.values()))

for f in sorted(here.glob("*.h5*")): print(f.name, f.stat().st_size, "bytes")
