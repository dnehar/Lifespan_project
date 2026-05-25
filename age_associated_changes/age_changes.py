import os
import re
from pathlib import Path
import numpy as np
import pandas as pd

# ------------------------------------------------------------------
# User-configurable parameters
# ------------------------------------------------------------------

DATESTAMP = "02202026"         # Used in filenames to make outputs reproducible/auditable
SIGNIF_P   = 0.05              # padj threshold for significance
SIGNIF_LFC = 0.3               # |log2FoldChange| threshold for significance

# Output locations
OUT_DIR_COUNTS = Path("./LS_L3/counts_forHM")  # per-subset matrices for heatmaps (log1p counts of sig genes)
OUT_DIR_DE     = Path("./LS_L3/age_changes")   # per-subset DE result tables and pseudobulk counts

# Ensure output directories exist
OUT_DIR_COUNTS.mkdir(parents=True, exist_ok=True)
OUT_DIR_DE.mkdir(parents=True, exist_ok=True)

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

def _slugify(x: str) -> str:
    """
    Make a filesystem-safe slug from a label (e.g., subset names).
    - Replaces spaces with underscores.
    - Collapses repeated underscores.
    - Keeps only [A-Za-z0-9_.-] characters.
    """
    x = str(x)
    x = x.strip().replace(" ", "_")
    x = re.sub(r"[^A-Za-z0-9_\-\.]+", "_", x)
    x = re.sub(r"_+", "_", x)
    return x


def _has_enough_samples(ad, sample_col: str = "sample_id", min_samples: int = 2) -> bool:
    """
    Return True if the AnnData `ad` has at least `min_samples` unique donors/samples.
    This prevents attempting DE on very small or malformed subsets.
    """
    if sample_col not in ad.obs.columns:
        return False
    return ad.obs[sample_col].nunique() >= min_samples


# Collected per-subset summary rows to print a final batch table
summary_rows = []


# ------------------------------------------------------------------
# 1) Split all_pbmcs by LS_L3 (expecting ~18 subsets)
#    Assumptions:
#      - `all_pbmcs` is an AnnData available in scope.
#      - `all_pbmcs.obs["LS_L3"]` contains subset labels (e.g., cell types).
#      - Required columns for downstream steps include:
#            "sample_id", "Age_in_yrs", "Platform", "Batch"
# ------------------------------------------------------------------

if "LS_L3" not in all_pbmcs.obs.columns:
    raise KeyError("`LS_L3` not found in all_pbmcs.obs.")

sub_levels = pd.Index(all_pbmcs.obs["LS_L3"].dropna().unique())
print(f"Found {len(sub_levels)} LS_L3 subsets.")

for lvl in sorted(map(str, sub_levels)):
    subset_label = lvl
    subset_slug  = _slugify(subset_label)
    print(f"\n=== Processing subset: {subset_label} ===")

    # Subset the AnnData to the current LS_L3 level
    ad_sub = all_pbmcs[all_pbmcs.obs["LS_L3"] == subset_label].copy()

    # Sanity check: prevent DE on tiny or malformed subsets
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
    # 2) Run pseudobulk + DESeq2 (continuous age)
    #    This function is expected to:
    #      - Aggregate counts to sample-level (pseudobulk) per subset.
    #      - Fit DESeq2 with age as a continuous covariate.
    #      - Return:
    #          de: DataFrame indexed by gene, with at least ["padj", "log2FoldChange"]
    #          dds: an AnnData-like object with layers["normed_counts"] (samples x genes)
    #          meta_df: sample-level metadata used in the model
    #          counts_df: pseudobulk counts (samples x genes), saved for reproducibility
    #    Notes:
    #      - scale_age_per_decade=True means the age effect is per 10 years.
    #      - Adjust min_total_counts/min_samples_detected per data sparsity.
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
            scale_age_per_decade=True,  # interpret effect as per 10y
            collapse_rare_levels=False,
            min_samples_per_level=2,
            verbose=True
        )
    except Exception as e:
        # Catch and record DE failures for a subset, then continue with others
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
    # 2b) Save pseudobulk counts (samples × genes)
    #     This captures the exact input to DESeq2 per subset to ensure
    #     full reproducibility and downstream auditing.
    # ------------------------------------------------------------------
    pseudo_bulk_counts_path = OUT_DIR_DE / f"pseudo_bulk_counts_{subset_slug}_{DATESTAMP}.csv"
    counts_df.to_csv(pseudo_bulk_counts_path)
    print(f"[ok] Wrote pseudobulk counts {counts_df.shape} to: {pseudo_bulk_counts_path}")

    # ------------------------------------------------------------------
    # 3) Build log1p(normed_counts) layer and extract significant genes
    #    We'll use log1p-normalized counts to generate heatmap-friendly
    #    matrices for only the significant features.
    # ------------------------------------------------------------------
    if "normed_counts" not in dds.layers:
        # Some pipelines might not provide this layer; we degrade gracefully.
        print(f"[warn] 'normed_counts' not present; skipping counts_forHM export for this subset.")
        normed = None
    else:
        # dds.layers["normed_counts"] is expected to be a 2D array: (samples × genes)
        normed = np.asarray(dds.layers["normed_counts"])
        if normed.ndim != 2:
            raise ValueError("Expected dds.layers['normed_counts'] to be 2D (samples × genes).")
        # Add a convenient log1p layer for plotting heatmaps
        dds.layers["log1p"] = np.log1p(normed)  # shape: samples × genes

    # Filter significant genes by adjusted P and effect size
    res = de  # alias for readability
    if not {"padj", "log2FoldChange"}.issubset(res.columns):
        raise KeyError("DE results must include 'padj' and 'log2FoldChange' columns.")

    sigs = res[(res["padj"] < SIGNIF_P) & (res["log2FoldChange"].abs() > SIGNIF_LFC)].copy()
    n_sig = sigs.shape[0]
    print(f"Significant genes (padj<{SIGNIF_P}, |LFC|>{SIGNIF_LFC}): {n_sig}")

    counts_csv_path = None
    if (n_sig > 0) and (normed is not None):
        # Ensure overlap between significant genes and the dds var index
        # Note: assumes de.index and dds.var_names are gene IDs in the same namespace.
        sig_gene_list = list(sigs.index.intersection(dds.var_names))
        if len(sig_gene_list) == 0:
            print("[warn] No overlap between significant genes and dds.var_names after filtering.")
        else:
            # Subset to significant genes, preserve original sample ordering
            dds_sig = dds[:, sig_gene_list]

            # Build a genes × samples DataFrame of log1p(normed_counts)
            # We transpose because layers are samples × genes; heatmaps often want genes as rows.
            arr = np.asarray(dds_sig.layers["log1p"]).T
            grapher = pd.DataFrame(arr, index=dds_sig.var_names, columns=dds_sig.obs_names)

            counts_csv_path = OUT_DIR_COUNTS / f"counts_{subset_slug}_{DATESTAMP}.csv"
            grapher.to_csv(counts_csv_path)
            print(f"[ok] Wrote counts heatmap matrix: {counts_csv_path}")

    # ------------------------------------------------------------------
    # 4) Save DE results (all genes, not only significant)
    # ------------------------------------------------------------------
    de_csv_path = OUT_DIR_DE / f"age_changes_{subset_slug}_{DATESTAMP}.csv"
    de.to_csv(de_csv_path)
    print(f"[ok] Wrote DE results: {de_csv_path}")

    # Track per-subset summary for a final consolidated report
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
# Final summary across all LS_L3 subsets
# ------------------------------------------------------------------
summary_df = pd.DataFrame(summary_rows).sort_values("subset")
print("\n=== Batch summary ===")
display(summary_df)
