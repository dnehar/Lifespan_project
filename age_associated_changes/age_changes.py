import os
import re
from pathlib import Path
import numpy as np
import pandas as pd

# ------------------------------------------------------------------
# User parameters
# ------------------------------------------------------------------
DATESTAMP = "02202026"
SIGNIF_P   = 0.05
SIGNIF_LFC = 0.3

OUT_DIR_COUNTS = Path("./LS_L3/counts_forHM")
OUT_DIR_DE     = Path("./LS_L3/age_changes")

OUT_DIR_COUNTS.mkdir(parents=True, exist_ok=True)
OUT_DIR_DE.mkdir(parents=True, exist_ok=True)

# Helper: safe subset label for filenames
def _slugify(x: str) -> str:
    x = str(x)
    x = x.strip().replace(" ", "_")
    x = re.sub(r"[^A-Za-z0-9_\-\.]+", "_", x)
    x = re.sub(r"_+", "_", x)
    return x

# Helper: quick sample check before attempting DE
def _has_enough_samples(ad, sample_col="sample_id", min_samples=2):
    if sample_col not in ad.obs.columns:
        return False
    return ad.obs[sample_col].nunique() >= min_samples

# Summary collector
summary_rows = []

# ------------------------------------------------------------------
# 1) Split all_pbmcs by LS_L3 (expecting ~18 subsets)
# ------------------------------------------------------------------
if "LS_L3" not in all_pbmcs.obs.columns:
    raise KeyError("`LS_L3` not found in all_pbmcs.obs.")

sub_levels = pd.Index(all_pbmcs.obs["LS_L3"].dropna().unique())
print(f"Found {len(sub_levels)} LS_L3 subsets.")

for lvl in sorted(map(str, sub_levels)):
    subset_label = lvl
    subset_slug  = _slugify(subset_label)
    print(f"\n=== Processing subset: {subset_label} ===")

    # Subset AnnData
    ad_sub = all_pbmcs[all_pbmcs.obs["LS_L3"] == subset_label].copy()

    # Skip tiny subsets
    if not _has_enough_samples(ad_sub, sample_col="sample_id", min_samples=2):
        print(f"[skip] Not enough samples in subset: {subset_label}")
        summary_rows.append({
            "subset": subset_label,
            "n_samples": ad_sub.obs["sample_id"].nunique() if "sample_id" in ad_sub.obs else np.nan,
            "n_genes_sig": 0,
            "de_csv": None,
            "counts_csv": None,
            "pseudo_bulk_counts_csv": None,
            "notes": "Too few samples"
        })
        continue

    # ------------------------------------------------------------------
    # 2) Run your pseudobulk + DESeq2 (continuous age) function
    # ------------------------------------------------------------------
    try:
        de, dds, meta_df, counts_df = gputils.run_pseudobulk_deseq_age(
            ad_sub,
            age_col='Age_in_yrs',
            platform_col='Platform',
            batch_col='Batch',
            sample_col='sample_id',
            min_total_counts=10,
            min_samples_detected=3,
            scale_age_per_decade=True,  # effect is per 10y
            collapse_rare_levels=False,
            min_samples_per_level=2,
            verbose=True
        )
    except Exception as e:
        print(f"[error] DE run failed for subset {subset_label}: {e}")
        summary_rows.append({
            "subset": subset_label,
            "n_samples": ad_sub.obs["sample_id"].nunique(),
            "n_genes_sig": 0,
            "de_csv": None,
            "counts_csv": None,
            "pseudo_bulk_counts_csv": None,
            "notes": f"DE error: {e}"
        })
        continue

    # ------------------------------------------------------------------
    # 2b) NEW: Save pseudobulk counts (samples × genes)
    # ------------------------------------------------------------------
    pseudo_bulk_counts_path = OUT_DIR_DE / f"pseudo_bulk_counts_{subset_slug}_{DATESTAMP}.csv"
    counts_df.to_csv(pseudo_bulk_counts_path)
    print(f"[ok] Wrote pseudobulk counts {counts_df.shape} to: {pseudo_bulk_counts_path}")

    # ------------------------------------------------------------------
    # 3) Build log1p(normed_counts) layer and extract significant genes
    # ------------------------------------------------------------------
    if "normed_counts" not in dds.layers:
        print(f"[warn] 'normed_counts' not present; skipping counts_forHM export for this subset.")
        normed = None
    else:
        # Create a log1p layer for easy downstream plotting
        normed = np.asarray(dds.layers["normed_counts"])
        dds.layers["log1p"] = np.log1p(normed)  # shape: samples × genes

    res = de  # (alias for readability)
    sigs = res[(res["padj"] < SIGNIF_P) & (res["log2FoldChange"].abs() > SIGNIF_LFC)].copy()
    n_sig = sigs.shape[0]
    print(f"Significant genes (padj<{SIGNIF_P}, |LFC|>{SIGNIF_LFC}): {n_sig}")

    counts_csv_path = None
    if (n_sig > 0) and (normed is not None):
        # Ensure overlap between sig genes and dds.var_names
        sig_gene_list = list(sigs.index.intersection(dds.var_names))
        if len(sig_gene_list) == 0:
            print("[warn] No overlap between significant genes and dds.var_names after filtering.")
        else:
            dds_sig = dds[:, sig_gene_list]

            # genes × samples matrix of log1p(normed_counts)
            arr = np.asarray(dds_sig.layers["log1p"]).T
            grapher = pd.DataFrame(arr, index=dds_sig.var_names, columns=dds_sig.obs_names)

            counts_csv_path = OUT_DIR_COUNTS / f"counts_{subset_slug}_{DATESTAMP}.csv"
            grapher.to_csv(counts_csv_path)
            print(f"[ok] Wrote counts heatmap matrix: {counts_csv_path}")

    # ------------------------------------------------------------------
    # 4) Save DE results
    # ------------------------------------------------------------------
    de_csv_path = OUT_DIR_DE / f"age_changes_{subset_slug}_{DATESTAMP}.csv"
    de.to_csv(de_csv_path)
    print(f"[ok] Wrote DE results: {de_csv_path}")

    # Summary row
    summary_rows.append({
        "subset": subset_label,
        "n_samples": ad_sub.obs["sample_id"].nunique(),
        "n_genes_sig": int(n_sig),
        "de_csv": str(de_csv_path),
        "counts_csv": (str(counts_csv_path) if counts_csv_path is not None else None),
        "pseudo_bulk_counts_csv": str(pseudo_bulk_counts_path),
        "notes": None
    })

# ------------------------------------------------------------------
# Final summary
# ------------------------------------------------------------------
summary_df = pd.DataFrame(summary_rows).sort_values("subset")
print("\n=== Batch summary ===")
display(summary_df)
