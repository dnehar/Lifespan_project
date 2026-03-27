import os, glob
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import gseapy as gp

# --------------------------
# USER CONFIG
# --------------------------
input_dir  = "./age_changes/LS_L3/Deqseq_outputs/"             # folder containing DESeq2 CSVs (age associated genes or peaks)
pattern    = "*.csv"                       # glob pattern for files
gene_sets  = "MSigDB_Hallmark_2020"        # OR a path to a .gmt file, e.g., "RTE_Signature_with_DN.gmt"
outdir     = "./age_changes/LS_L3/gsea_across_subsets"       # where results/plots will be written
min_size   = 10    # minimum gene set size to include in analysis
max_size   = 500   # maximum gene set size to include in analysis
perms      = 1000  # number of permutations for significance estimation
seed       = 6     # random seed for reproducibility

# Plotting controls
fdr_filter_for_plot = 0.05    # show terms significant in at least 1 subset
top_terms = 30                # limit plot to top-N most supported terms (by max -log10(FDR)) | set None to keep all

os.makedirs(outdir, exist_ok=True)

# --------------------------
# Helpers
# --------------------------
def detect_gene_col(df: pd.DataFrame) -> str:
    """
    Detect the gene identifier column in a DESeq2 DataFrame.
    Checks a list of common column name conventions (e.g., 'gene', 'SYMBOL',
    'Unnamed: 0'). Falls back to the first column if none match.
    """
    for c in ["gene","Gene","symbol","SYMBOL","GeneSymbol","Gene.name","GENE","Unnamed: 0"]:
        if c in df.columns:
            return c
    return df.columns[0]

def build_rnk_from_deseq(df: pd.DataFrame) -> pd.Series:
    """
    Build a gene ranking Series suitable for preranked GSEA.

    Strategy:
      1. Primary:  Use DESeq2 Wald test statistic ('stat') directly — the most
                   statistically principled ranking metric.
      2. Fallback: Compute signed -log10(p-value), using the sign of log2FoldChange
                   to indicate up- vs. down-regulation. P-values are clipped at
                   1e-300 to avoid log(0).

    Duplicate gene entries are resolved by keeping the entry with the largest
    absolute rank score. Inf/-Inf and NaN values are dropped before returning.
    """
    gene_col = detect_gene_col(df)
    df = df.dropna(subset=[gene_col]).copy()
    df[gene_col] = df[gene_col].astype(str).str.strip()

    if "stat" in df.columns:
        s = pd.to_numeric(df["stat"], errors="coerce")
        rnk = pd.Series(s.values, index=df[gene_col].values, name="Rank")
    else:
        pcol = None
        for c in ["pvalue","padj","FDR","adj.P.Val"]:
            if c in df.columns:
                pcol = c
                break
        if pcol is None:
            raise ValueError("No 'stat' and no p-value column found; cannot build RNK.")

        lfc_col = None
        for c in ["log2FoldChange","log2FC","logFC","LFC","log2fc"]:
            if c in df.columns:
                lfc_col = c
                break
        if lfc_col is None:
            raise ValueError("No log2FC column found; cannot build signed RNK.")

        p = pd.to_numeric(df[pcol], errors="coerce").clip(lower=1e-300)
        lfc = pd.to_numeric(df[lfc_col], errors="coerce").fillna(0.0)
        rank_vals = -np.log10(p) * np.sign(lfc)
        rnk = pd.Series(rank_vals.values, index=df[gene_col].values, name="Rank")

    # Deduplicate genes, keep the largest |score|
    rnk = rnk.groupby(level=0).apply(lambda x: x.iloc[np.argmax(np.abs(x.values))])
    rnk = rnk.replace([np.inf, -np.inf], np.nan).dropna()
    rnk = rnk.sort_values(ascending=False)
    return rnk

def prerank_one(csv_path: str) -> pd.DataFrame:
    """
    Run gseapy prerank GSEA on a single DESeq2 CSV file.

    Derives the subset name from the filename, builds a ranked gene list,
    runs GSEA, and saves both the full gseapy result table (res2d) and a
    compact summary CSV to a per-subset subdirectory under `outdir`.

    Returns a DataFrame with columns: Term, fdr, es, nes, lead_genes, subset.
    """
    subset = os.path.splitext(os.path.basename(csv_path))[0]
    df = pd.read_csv(csv_path)
    rnk = build_rnk_from_deseq(df)

    sub_outdir = os.path.join(outdir, subset)
    os.makedirs(sub_outdir, exist_ok=True)

    pre_res = gp.prerank(
        rnk=rnk,                      # pandas Series
        gene_sets=gene_sets,          # string (msigdb key) or path to local GMT, or dict
        min_size=min_size,
        max_size=max_size,
        permutation_num=perms,
        seed=seed,
        outdir=sub_outdir,
        format="png",
        threads=1,
    )

    # Build a compact summary DataFrame from the gseapy results dict, adding the subset label
    out = []
    for term in list(pre_res.results):
        r = pre_res.results[term]
        out.append([
            term,
            r.get('fdr', np.nan),
            r.get('es', np.nan),
            r.get('nes', np.nan),
            r.get('lead_genes', "")
        ])
    out_df = pd.DataFrame(out, columns=['Term','fdr','es','nes','lead_genes'])
    out_df['subset'] = subset
    out_df = out_df.sort_values('fdr').reset_index(drop=True)

    # Save per-subset tables (both res2d and your out_df)
    pre_res.res2d.to_csv(os.path.join(sub_outdir, f"{subset}_res2d.csv"))
    out_df.to_csv(os.path.join(sub_outdir, f"{subset}_summary.csv"), index=False)
    return out_df

# --------------------------
# Main loop: run prerank GSEA for every CSV and collect results
# --------------------------
all_csvs = sorted(glob.glob(os.path.join(input_dir, pattern)))
if not all_csvs:
    raise FileNotFoundError(f"No CSV files found in {input_dir!r} matching {pattern!r}")

combined = []
for fp in all_csvs:
    print(f"[GSEA] {fp}")
    try:
        combined.append(prerank_one(fp))
    except Exception as e:
        print(f"[WARN] Skipping {fp}: {e}")

if not combined:
    raise RuntimeError("No successful GSEA results to aggregate.")

combined_df = pd.concat(combined, ignore_index=True)
combined_df.to_csv(os.path.join(outdir, "combined_gsea_across_subsets.csv"), index=False)

# --------------------------
# Combined bubble plot across all subsets:
#   Each dot = one (Term, subset) pair
#   x-axis  : cell subset (one column per CSV input)
#   y-axis  : enriched gene set term
#   color   : NES (Normalized Enrichment Score); red = enriched, blue = depleted
#   dot size: -log10(FDR); larger = more significant
# --------------------------
dfp = combined_df.copy()
dfp = dfp[dfp['fdr'].notna()]

# Keep terms significant in at least one subset
sig_any = dfp[dfp['fdr'] <= fdr_filter_for_plot].copy()
if sig_any.empty:
    # If none pass, just take the best 30 overall by lowest FDR
    sig_any = dfp.sort_values('fdr', ascending=True).head(30).copy()

# Choose top-N terms by max -log10(FDR) across subsets (optional)
sig_any['_neglog10fdr'] = -np.log10(sig_any['fdr'].clip(lower=1e-300))
if top_terms is not None:
    keep = (
        sig_any.groupby('Term')['_neglog10fdr'].max()
        .sort_values(ascending=False)
        .head(top_terms).index
    )
    sig_any = sig_any[sig_any['Term'].isin(keep)]

# Order axes for readability
subset_order = list(sig_any['subset'].drop_duplicates())
term_order = (
    sig_any.groupby('Term')['_neglog10fdr'].max()
    .sort_values(ascending=False)
    .index.tolist()
)

sig_any['subset'] = pd.Categorical(sig_any['subset'], categories=subset_order, ordered=True)
sig_any['Term']   = pd.Categorical(sig_any['Term'],   categories=term_order,   ordered=True)

# Plot
plt.figure(figsize=(1.5 + 0.7*len(subset_order), 0.6*len(term_order)))
ax = plt.gca()

sizes = sig_any['_neglog10fdr']
# Rescale sizes to something legible
size_min, size_max = 30, 600
if sizes.max() == sizes.min():
    s_vals = np.full(len(sizes), (size_min + size_max)/2.0)
else:
    s_vals = size_min + (sizes - sizes.min())/(sizes.max() - sizes.min()) * (size_max - size_min)

sc = ax.scatter(
    x=sig_any['subset'],
    y=sig_any['Term'],
    c=sig_any['nes'],
    s=s_vals,
    cmap='RdBu_r',
    vmin=-max(2, sig_any['nes'].abs().max()),
    vmax= max(2, sig_any['nes'].abs().max()),
    edgecolor='k',
    linewidth=0.3,
    alpha=0.9
)

# Colorbar for NES
cbar = plt.colorbar(sc, ax=ax, pad=0.02)
cbar.set_label("NES", rotation=270, labelpad=12)

# Construct a size legend for –log10(FDR)
for val in [0.7, 1, 2, 3]:
    ax.scatter([], [], s=size_min + (min(val, sizes.max()) - sizes.min())/max(sizes.max() - sizes.min(), 1e-9)*(size_max - size_min),
               c="gray", alpha=0.6, edgecolor="k", linewidth=0.3,
               label=f"-log10(FDR)≈{val}")
leg = ax.legend(scatterpoints=1, frameon=True, bbox_to_anchor=(1.05, 1), loc="upper left", title="Dot size")

ax.set_xlabel("Subset")
ax.set_ylabel("Enriched Term")
ax.set_title("GSEA (prerank) across subsets\nColor = NES, Size = -log10(FDR)")
plt.tight_layout()

# Save plot + combined table
plot_pdf = os.path.join(outdir, "enriched_terms_across_subsets.pdf")
plot_png = os.path.join(outdir, "enriched_terms_across_subsets.png")
plt.savefig(plot_pdf, bbox_inches="tight", dpi=300)
plt.savefig(plot_png, bbox_inches="tight", dpi=300)
plt.close()

print(f"[OK] Combined table: {os.path.join(outdir, 'combined_gsea_across_subsets_LS_L3.csv')}")
print(f"[OK] Plot saved:     {plot_pdf}")
print(f"[OK] Plot saved:     {plot_png}")
