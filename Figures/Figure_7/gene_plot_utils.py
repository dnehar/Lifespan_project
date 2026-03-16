# gene_plot_utils.py
# Project: Lifespan 

import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
from scipy.stats import mannwhitneyu
from scipy.stats import pearsonr



##########################################################################
############ 1- Boxplot_gene_expression ##################
###########################################################################


import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import mannwhitneyu

def Boxplot_one_gene(
    adata,
    gene,
    sample_col="sample_id",
    group_col="Age_groups",
    group_order=('Infants', 'Child', 'Adolescent', 'Young',
                 'Middle_aged', 'Older', 'Oldest_old'),
    custom_palette=None,
    # ---- data source ----
    source="layer",              # "layer" | "raw" | "X"
    layer_name="counts",         # only used if source == "layer"
    log_transform=True,          # apply log1p to counts for plotting
    # ---- saving/automation ----
    save_path=None,              # e.g., "outputs/GENE_boxplot.pdf" or .png/.svg
    save_dpi=300,
    transparent=False,
    show=True,
    # ---- stats control ----
    consecutive_only=True,       # test only adjacent groups in present order
    # ---- visual consistency controls ----
    show_box_fliers=False,       # hide boxplot outlier glyphs by default
    point_marker="o",            # marker for strip points (and fliers if enabled)
    point_size=4,                # size for strip points (and fliers)
    point_alpha=1.0,             # solid (no gray wash-out)
    point_color="black",         # solid black dots
    # ---- box/line styling ----
    box_line_color="black",      # outlines (box, whisker, cap, median) in black
    box_linewidth=1.2            # thickness for box/whisker/cap/median lines
):
    """
    Boxplot of per-sample mean gene expression across groups.
    Returns (fig, ax, agg_df).

    Parameters
    ----------
    - consecutive_only: If True, only compare adjacent groups in group_order that are present.
    - show_box_fliers: If False, suppress boxplot's outlier glyphs (keeps shapes consistent).
                       If True, outliers are drawn using the same style as strip points.
    - point_*: Controls for strip points (and box fliers if enabled).
    - box_line_color/box_linewidth: Force box/whisker/cap/median lines to black with given width.
    """

    # --- palette defaults
    if custom_palette is None:
        custom_palette = {
         "Infants":     "#0072B2",
            "Child":       "#56B4E9",
            "Adolescent":  "#009E73",
            "Young":       "#F0E442",
            "Middle_aged": "#E69F00",
            "Older":       "#D55E00",
            "Oldest_old":  "#CC79A7",
        }

    # --- sanity checks
    for col in (sample_col, group_col):
        if col not in adata.obs.columns:
            raise KeyError(f"Column '{col}' not found in adata.obs.")

    # --- choose matrix & names
    if source == "layer":
        if layer_name not in adata.layers:
            raise KeyError(
                f"Layer '{layer_name}' not found in adata.layers. "
                "Set source='raw' to read from adata.raw.X or source='X' to use adata.X."
            )
        mat = adata.layers[layer_name]
        var_names = adata.var_names
    elif source == "raw":
        if adata.raw is None:
            raise KeyError("adata.raw is None; cannot use source='raw'.")
        mat = adata.raw.X
        var_names = adata.raw.var_names
    elif source == "X":
        mat = adata.X
        var_names = adata.var_names
    else:
        raise ValueError("Parameter 'source' must be one of: 'layer', 'raw', or 'X'.")

    # --- gene index (case-sensitive first, then case-insensitive)
    if gene in var_names:
        gene_idx = int(np.where(var_names == gene)[0][0])
    else:
        lower_map = {g.lower(): i for i, g in enumerate(var_names)}
        if gene.lower() in lower_map:
            gene_idx = lower_map[gene.lower()]
        else:
            where_from = "adata.layers['{}']".format(layer_name) if source == "layer" else f"adata.{source}"
            raise KeyError(f"Gene '{gene}' not found in {where_from} var names.")

    # --- slice expression
    try:
        col = mat[:, gene_idx]
    except Exception as e:
        raise RuntimeError(f"Failed to slice matrix for gene '{gene}' (source='{source}'): {e}")

    if hasattr(col, "toarray"):  # sparse
        expression = col.toarray().ravel()
    else:
        expression = np.asarray(col).ravel()

    if log_transform:
        expression = np.log1p(expression)

    # --- per-cell dataframe
    df = pd.DataFrame({
        "expression": expression,
        "sample": adata.obs[sample_col].values,
        "group": adata.obs[group_col].values
    })

    # --- per-sample aggregation
    agg_df = df.groupby(["sample", "group"], as_index=False)["expression"].mean()

    # --- present order
    present_groups = [g for g in group_order if g in agg_df["group"].unique()]
    if len(present_groups) == 0:
        present = sorted(agg_df["group"].unique())
        raise ValueError(
            "None of the specified groups in 'group_order' are present after aggregation. "
            f"Found groups: {present}"
        )

    # --- comparisons
    if consecutive_only:
        comparisons = list(zip(present_groups[:-1], present_groups[1:]))
    else:
        comparisons = [(g1, g2) for i, g1 in enumerate(present_groups) for g2 in present_groups[i+1:]]

    # --- stats
    p_values = []
    for g1, g2 in comparisons:
        data1 = agg_df.loc[agg_df["group"] == g1, "expression"].dropna()
        data2 = agg_df.loc[agg_df["group"] == g2, "expression"].dropna()
        if len(data1) >= 2 and len(data2) >= 2 and (data1.nunique() > 1 or data2.nunique() > 1):
            stat, p = mannwhitneyu(data1, data2, alternative='two-sided')
            if p < 0.05:
                p_values.append((g1, g2, p))

    # --- plot
    fig, ax = plt.subplots(figsize=(8, 6))

    # Boxplot fliers handling (match black style if enabled)
    if show_box_fliers:
        flierprops = dict(
            marker=point_marker,
            markerfacecolor=point_color,
            markeredgecolor=point_color,
            markersize=point_size,
            alpha=point_alpha
        )
        boxplot_kwargs = dict(showfliers=True, flierprops=flierprops)
    else:
        boxplot_kwargs = dict(showfliers=False)

    # Box/whisker/cap/median line styles in black
    boxprops     = dict(edgecolor=box_line_color, linewidth=box_linewidth)
    whiskerprops = dict(color=box_line_color, linewidth=box_linewidth)
    capprops     = dict(color=box_line_color, linewidth=box_linewidth)
    medianprops  = dict(color=box_line_color, linewidth=box_linewidth)

    sns.boxplot(
        x="group", y="expression", data=agg_df,
        order=present_groups, palette=custom_palette, ax=ax,
        boxprops=boxprops, whiskerprops=whiskerprops,
        capprops=capprops, medianprops=medianprops,
        **boxplot_kwargs
    )

    # Solid black dots (with black edge for crispness in PDF/PNG)
    sns.stripplot(
        x="group", y="expression", data=agg_df,
        order=present_groups,
        color=point_color, alpha=point_alpha,
        marker=point_marker, size=point_size,
        edgecolor="black", linewidth=0.25,
        ax=ax
    )

    # p-value bars & labels in black
    y_max = agg_df["expression"].max()
    step = max(y_max, 1e-6) * 0.05
    for i, (g1, g2, pval) in enumerate(p_values):
        x1, x2 = present_groups.index(g1), present_groups.index(g2)
        y = y_max + step * (i + 1)
        ax.plot([x1, x2], [y, y], lw=0.9, color='black')
        ax.text((x1 + x2) / 2, y + step * 0.1, f"p = {pval:.1e}",
                ha='center', va='bottom', fontsize=9, color='black')

    # labels
    src_label = (f"layers['{layer_name}']" if source == "layer"
                 else ("raw.X" if source == "raw" else "X"))
    ax.set_title(f"{gene}  ({src_label}{' | log1p' if log_transform else ''})")
    ax.set_xlabel("Group")
    ax.set_ylabel("Per-sample mean expression")
    fig.tight_layout()

    # save if requested
    if save_path is not None:
        fig.savefig(save_path, dpi=save_dpi, bbox_inches="tight", transparent=transparent)

    if show:
        plt.show()
    else:
        plt.close(fig)

    #return fig, ax, agg_df

###########################################################################
############### 2- Boxplot_multiple_genes ##################
###########################################################################

# Updated 03/02/2026

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import mannwhitneyu


def _get_expression(adata, gene, source, layer_name, log_transform):
    """Extract per-cell expression array for a single gene."""

    # --- choose matrix & var_names
    if source == "layer":
        if layer_name not in adata.layers:
            raise KeyError(
                f"Layer '{layer_name}' not found in adata.layers. "
                "Set source='raw' or source='X'."
            )
        mat = adata.layers[layer_name]
        var_names = adata.var_names
    elif source == "raw":
        if adata.raw is None:
            raise KeyError("adata.raw is None; cannot use source='raw'.")
        mat = adata.raw.X
        var_names = adata.raw.var_names
    elif source == "X":
        mat = adata.X
        var_names = adata.var_names
    else:
        raise ValueError("'source' must be one of: 'layer', 'raw', or 'X'.")

    # --- gene index (case-sensitive, then case-insensitive fallback)
    if gene in var_names:
        gene_idx = int(np.where(var_names == gene)[0][0])
    else:
        lower_map = {g.lower(): i for i, g in enumerate(var_names)}
        if gene.lower() in lower_map:
            gene_idx = lower_map[gene.lower()]
        else:
            where_from = (
                f"adata.layers['{layer_name}']" if source == "layer"
                else f"adata.{source}"
            )
            raise KeyError(f"Gene '{gene}' not found in {where_from} var names.")

    # --- slice expression
    try:
        col = mat[:, gene_idx]
    except Exception as e:
        raise RuntimeError(
            f"Failed to slice matrix for gene '{gene}' (source='{source}'): {e}"
        )

    if hasattr(col, "toarray"):
        expression = col.toarray().ravel()
    else:
        expression = np.asarray(col).ravel()

    if log_transform:
        expression = np.log1p(expression)

    return expression


def _build_agg_df(adata, gene, sample_col, group_col, source, layer_name, log_transform):
    """Return a per-sample mean-expression DataFrame for one gene."""
    expression = _get_expression(adata, gene, source, layer_name, log_transform)
    df = pd.DataFrame({
        "expression": expression,
        "sample":     adata.obs[sample_col].values,
        "group":      adata.obs[group_col].values,
    })
    return df.groupby(["sample", "group"], as_index=False)["expression"].mean()


def _compute_pvalues(agg_df, present_groups, consecutive_only):
    """Return list of (g1, g2, p) for significant comparisons."""
    if consecutive_only:
        comparisons = list(zip(present_groups[:-1], present_groups[1:]))
    else:
        comparisons = [
            (g1, g2)
            for i, g1 in enumerate(present_groups)
            for g2 in present_groups[i + 1:]
        ]

    p_values = []
    for g1, g2 in comparisons:
        d1 = agg_df.loc[agg_df["group"] == g1, "expression"].dropna()
        d2 = agg_df.loc[agg_df["group"] == g2, "expression"].dropna()
        if len(d1) >= 2 and len(d2) >= 2 and (d1.nunique() > 1 or d2.nunique() > 1):
            _, p = mannwhitneyu(d1, d2, alternative="two-sided")
            if p < 0.05:
                p_values.append((g1, g2, p))
    return p_values


def _draw_single_gene(
    ax, agg_df, gene, present_groups, p_values,
    custom_palette, source, layer_name, log_transform,
    show_box_fliers, point_marker, point_size, point_alpha, point_color,
    box_line_color, box_linewidth,
):
    """Draw one gene's boxplot + stripplot + p-value bars onto *ax*."""

    flierprops = dict(
        marker=point_marker,
        markerfacecolor=point_color,
        markeredgecolor=point_color,
        markersize=point_size,
        alpha=point_alpha,
    )
    boxplot_kwargs = dict(
        showfliers=show_box_fliers,
        **({"flierprops": flierprops} if show_box_fliers else {}),
    )

    sns.boxplot(
        x="group", y="expression", data=agg_df,
        order=present_groups, palette=custom_palette, ax=ax,
        boxprops=dict(edgecolor=box_line_color, linewidth=box_linewidth),
        whiskerprops=dict(color=box_line_color, linewidth=box_linewidth),
        capprops=dict(color=box_line_color, linewidth=box_linewidth),
        medianprops=dict(color=box_line_color, linewidth=box_linewidth),
        **boxplot_kwargs,
    )

    sns.stripplot(
        x="group", y="expression", data=agg_df,
        order=present_groups,
        color=point_color, alpha=point_alpha,
        marker=point_marker, size=point_size,
        edgecolor="black", linewidth=0.25,
        ax=ax,
    )

    # p-value bars
    y_max = agg_df["expression"].max()
    step  = max(y_max, 1e-6) * 0.05
    for i, (g1, g2, pval) in enumerate(p_values):
        x1, x2 = present_groups.index(g1), present_groups.index(g2)
        y = y_max + step * (i + 1)
        ax.plot([x1, x2], [y, y], lw=0.9, color="black")
        ax.text(
            (x1 + x2) / 2, y + step * 0.1,
            f"p = {pval:.1e}",
            ha="center", va="bottom", fontsize=9, color="black",
        )

    src_label = (
        f"layers['{layer_name}']" if source == "layer"
        else ("raw.X" if source == "raw" else "X")
    )
    ax.set_title(
        f"{gene}  ({src_label}{' | log1p' if log_transform else ''})"
    )
    ax.set_xlabel("Group")
    ax.set_ylabel("Per-sample mean expression")


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def Boxplot_multiple_genes(
    adata,
    genes,                              # str  OR  list[str]
    sample_col="sample_id",
    group_col="Age_groups",
    group_order=('Infants', 'Child', 'Adolescent', 'Young',
                 'Middle_aged', 'Older', 'Oldest_old'),
    custom_palette=None,
    # ---- data source ----
    source="layer",
    layer_name="counts",
    log_transform=True,
    # ---- multi-gene layout ----
    ncols=2,                            # columns when plotting multiple genes
    subplot_figsize=(8, 6),             # (w, h) per subplot panel
    # ---- saving/automation ----
    save_path=None,      # use "{gene}" placeholder for per-gene files, e.g.
                         # "outputs/{gene}_boxplot.pdf"
    save_dpi=300,
    transparent=False,
    show=True,
    # ---- stats control ----
    consecutive_only=True,
    # ---- visual consistency controls ----
    show_box_fliers=False,
    point_marker="o",
    point_size=4,
    point_alpha=1.0,
    point_color="black",
    # ---- box/line styling ----
    box_line_color="black",
    box_linewidth=1.2,
):
    """
    Boxplot of per-sample mean gene expression across age groups.

    Parameters
    ----------
    genes : str or list of str
        One gene name **or** a list of gene names.

    ncols : int
        Number of subplot columns when multiple genes are plotted in a single
        figure.  Rows are computed automatically.

    subplot_figsize : (float, float)
        Width × height **per panel** when building the multi-gene grid.
        Ignored for single-gene plots (uses (8, 6)).

    save_path : str or None
        • Single gene  – plain path, e.g. ``"out/GENE.pdf"``.
        • Multiple genes, one file  – plain path, e.g. ``"out/all_genes.pdf"``.
        • Multiple genes, one file **per gene** – include ``{gene}`` in the
          path, e.g. ``"out/{gene}_boxplot.pdf"``.

    Returns
    -------
    Single gene  → (fig, ax, agg_df)
    Multiple genes → list of (fig, ax, agg_df)   if save_path contains {gene}
                   → (fig, axes, agg_dfs)         otherwise (shared figure)
    """

    # ---- normalise input -------------------------------------------------
    if isinstance(genes, str):
        genes = [genes]
    genes = list(genes)

    if custom_palette is None:
        custom_palette = {
            "Infants":     "#0072B2",
            "Child":       "#56B4E9",
            "Adolescent":  "#009E73",
            "Young":       "#F0E442",
            "Middle_aged": "#E69F00",
            "Older":       "#D55E00",
            "Oldest_old":  "#CC79A7",
        }

    for col in (sample_col, group_col):
        if col not in adata.obs.columns:
            raise KeyError(f"Column '{col}' not found in adata.obs.")

    # ---- decide layout ---------------------------------------------------
    single_gene       = len(genes) == 1
    per_gene_files    = (not single_gene) and (save_path is not None) and ("{gene}" in str(save_path))
    shared_figure     = not per_gene_files

    # ====================================================================
    # CASE A – one figure per gene  (per_gene_files=True)
    # ====================================================================
    if per_gene_files:
        results = []
        for gene in genes:
            agg_df = _build_agg_df(
                adata, gene, sample_col, group_col, source, layer_name, log_transform
            )
            present_groups = [g for g in group_order if g in agg_df["group"].unique()]
            if not present_groups:
                raise ValueError(
                    f"No groups from group_order present for gene '{gene}'. "
                    f"Found: {sorted(agg_df['group'].unique())}"
                )
            p_values = _compute_pvalues(agg_df, present_groups, consecutive_only)

            fig, ax = plt.subplots(figsize=(8, 6))
            _draw_single_gene(
                ax, agg_df, gene, present_groups, p_values,
                custom_palette, source, layer_name, log_transform,
                show_box_fliers, point_marker, point_size,
                point_alpha, point_color, box_line_color, box_linewidth,
            )
            fig.tight_layout()

            path = save_path.format(gene=gene)
            fig.savefig(path, dpi=save_dpi, bbox_inches="tight", transparent=transparent)
            print(f"Saved: {path}")

            if show:
                plt.show()
            else:
                plt.close(fig)

            results.append((fig, ax, agg_df))
        return results

    # ====================================================================
    # CASE B – shared figure (single gene OR multi-gene grid)
    # ====================================================================
    n        = len(genes)
    n_cols   = min(ncols, n)
    n_rows   = int(np.ceil(n / n_cols))
    fig_w    = subplot_figsize[0] * n_cols if not single_gene else 8
    fig_h    = subplot_figsize[1] * n_rows if not single_gene else 6

    fig, axes = plt.subplots(
        n_rows, n_cols,
        figsize=(fig_w, fig_h),
        squeeze=False,          # always 2-D array
    )

    agg_dfs = []
    for idx, gene in enumerate(genes):
        row, col = divmod(idx, n_cols)
        ax = axes[row][col]

        agg_df = _build_agg_df(
            adata, gene, sample_col, group_col, source, layer_name, log_transform
        )
        present_groups = [g for g in group_order if g in agg_df["group"].unique()]
        if not present_groups:
            raise ValueError(
                f"No groups from group_order present for gene '{gene}'. "
                f"Found: {sorted(agg_df['group'].unique())}"
            )
        p_values = _compute_pvalues(agg_df, present_groups, consecutive_only)

        _draw_single_gene(
            ax, agg_df, gene, present_groups, p_values,
            custom_palette, source, layer_name, log_transform,
            show_box_fliers, point_marker, point_size,
            point_alpha, point_color, box_line_color, box_linewidth,
        )
        agg_dfs.append(agg_df)

    # hide any unused axes in the last row
    for idx in range(n, n_rows * n_cols):
        row, col = divmod(idx, n_cols)
        axes[row][col].set_visible(False)

    fig.tight_layout()

    if save_path is not None:
        fig.savefig(save_path, dpi=save_dpi, bbox_inches="tight", transparent=transparent)
        print(f"Saved: {save_path}")

    if show:
        plt.show()
    else:
        plt.close(fig)

    # return consistent with single-gene behaviour
    if single_gene:
        return fig, axes[0][0], agg_dfs[0]
    #return fig #, axes, agg_dfs

###########################################################################
############### 3- cor_plot_one_gene ##################
###########################################################################


import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.sparse import issparse
from scipy.stats import pearsonr, spearmanr

def cor_plot_one_gene(
    adata,
    gene,
    sample_col="sample_id",
    group_col="Age_groups",
    age_col="Age_in_months",
    group_order=None,
    custom_palette=None,
    # ---- data source (aligned to Boxplot_one_gene) ----
    source="layer",              # "layer" | "raw" | "X"
    layer_name="counts",         # used only if source == "layer"
    log_transform=True,          # log1p before aggregation/correlation
    # ---- aggregation / correlation ----
    agg_func="mean",             # "mean" or "median" per sample
    corr_method="pearson",       # "pearson" or "spearman"
    fit_line="overall",          # "overall" | "per_group" | "none"
    # ---- plotting ----
    show_legend=True,
    figsize=(8, 6),
    point_size=40,               # scatterpoint size
    line_kwargs=None             # dict, e.g., {"linewidth":1.2}
):
    """
    Scatter + regression plot of per-sample aggregated expression vs age.

    Parameters
    ----------
    adata : AnnData
    gene : str
        Gene symbol (case-insensitive allowed).
    sample_col : str
        Per-sample identifier in adata.obs (e.g., "sample_id", "donor").
    group_col : str
        Grouping column for color/hue (e.g., "Age_groups").
    age_col : str
        Numeric age column (e.g., "Age_in_months" or "Age_in_yrs").
    group_order : list/tuple or None
        Optional order for groups on legend/semantic mapping.
    custom_palette : dict or None
        Mapping from group -> color hex/RGB. If None, auto palette is used.
    source : {"layer","raw","X"}
        Where to read expression from (mirrors Boxplot_one_gene).
    layer_name : str
        Layer name when source=="layer".
    log_transform : bool
        Apply np.log1p to expression prior to aggregation/correlation.
    agg_func : {"mean","median"}
        Aggregation function to get per-sample expression.
    corr_method : {"pearson","spearman"}
        Correlation method computed across ALL samples.
    fit_line : {"overall","per_group","none"}
        Whether to draw a global regression line, per-group lines, or none.
    show_legend : bool
        Show/Hide legend (groups).
    figsize : tuple
        Figure size (width, height) in inches.
    point_size : int/float
        Scatter point size.
    line_kwargs : dict or None
        Extra kwargs passed to regplot lines (e.g., {"line_kws": {"lw":2}} or {"color":"black"}).

    Returns
    -------
    agg_df : pd.DataFrame
        Columns: ["sample","expression","age","group"] (per-sample).
    stats : dict
        Overall correlation stats: {"method","r","p","n"}.
        If fit_line == "per_group", includes "by_group": {group: {"r","p","n"}}.
    fig, ax : matplotlib Figure and Axes
    """
    # --- palette defaults (same style you used before) ---
    if custom_palette is None:
        custom_palette = {
               "Infants":     "#0072B2",
            "Child":       "#56B4E9",
            "Adolescent":  "#009E73",
            "Young":       "#F0E442",
            "Middle_aged": "#E69F00",
            "Older":       "#D55E00",
            "Oldest_old":  "#CC79A7",
        }

    # --- sanity checks ---
    for col in (sample_col, group_col, age_col):
        if col not in adata.obs.columns:
            raise KeyError(f"Column '{col}' not found in adata.obs.")

    # --- choose matrix & var_names, mirroring Boxplot_one_gene ---
    if source == "layer":
        if layer_name not in adata.layers:
            raise KeyError(
                f"Layer '{layer_name}' not found in adata.layers. "
                "Set source='raw' or 'X' to choose from those matrices."
            )
        mat = adata.layers[layer_name]
        var_names = adata.var_names
        src_label = f"layers['{layer_name}']"
    elif source == "raw":
        if adata.raw is None:
            raise KeyError("adata.raw is None; cannot use source='raw'.")
        mat = adata.raw.X
        var_names = adata.raw.var_names
        src_label = "raw.X"
    elif source == "X":
        mat = adata.X
        var_names = adata.var_names
        src_label = "X"
    else:
        raise ValueError("Parameter 'source' must be one of: 'layer', 'raw', or 'X'.")

    # --- gene index (case-sensitive first, then case-insensitive) ---
    if gene in var_names:
        gene_idx = int(np.where(var_names == gene)[0][0])
        gene_used = gene
    else:
        lower_map = {g.lower(): i for i, g in enumerate(var_names)}
        if gene.lower() in lower_map:
            gene_idx = lower_map[gene.lower()]
            # keep the exact var_names entry to preserve original case
            gene_used = list(var_names)[gene_idx]
        else:
            raise KeyError(f"Gene '{gene}' not found in {src_label} var names.")

    # --- slice expression (cells x 1) and convert to 1D array ---
    try:
        col = mat[:, gene_idx]
    except Exception as e:
        raise RuntimeError(f"Failed to slice matrix for gene '{gene_used}' (source='{source}'): {e}")

    if issparse(col):
        expression = col.toarray().ravel()
    else:
        expression = np.asarray(col).ravel()

    # --- optional log1p transform ---
    if log_transform:
        expression = np.log1p(expression)

    # --- per-cell dataframe ---
    df = pd.DataFrame({
        "expression": expression,
        "sample": adata.obs[sample_col].values,
        "group": adata.obs[group_col].astype(str).values,
        "age": pd.to_numeric(adata.obs[age_col].values, errors="coerce")
    }).dropna(subset=["age"])  # ensure age is numeric

    # --- per-sample aggregation (expression and age) ---
    if agg_func not in {"mean", "median"}:
        raise ValueError("agg_func must be 'mean' or 'median'.")
    agg_op = np.mean if agg_func == "mean" else np.median

    agg_df = (df
              .groupby("sample", as_index=False)
              .agg(expression=("expression", agg_op),
                   age=("age", "first")))

    # add group info per sample (first occurrence)
    group_info = df.drop_duplicates(subset=["sample"])[["sample", "group"]]
    agg_df = agg_df.merge(group_info, on="sample", how="left")

    # optional group order
    if group_order is not None:
        agg_df["group"] = pd.Categorical(agg_df["group"], categories=list(group_order), ordered=True)

    # --- guards for correlation ---
    n = len(agg_df)
    if n < 3:
        raise ValueError(f"Not enough samples ({n}) for correlation. Need at least 3.")
    if agg_df["expression"].nunique() <= 1:
        raise ValueError("Expression has no variability after aggregation; correlation undefined.")
    if agg_df["age"].nunique() <= 1:
        raise ValueError("Age has no variability across samples; correlation undefined.")

    # --- compute overall correlation ---
    x = agg_df["age"].values
    y = agg_df["expression"].values
    if corr_method == "pearson":
        r, p = pearsonr(x, y)
    elif corr_method == "spearman":
        r, p = spearmanr(x, y)
    else:
        raise ValueError("corr_method must be 'pearson' or 'spearman'.")

    stats = {"method": corr_method, "r": float(r), "p": float(p), "n": int(n)}

    # --- plotting ---
    if line_kwargs is None:
        line_kwargs = {}

    fig, ax = plt.subplots(figsize=figsize)

    # Scatter with groups
    sns.scatterplot(
        data=agg_df, x="age", y="expression", hue="group",
        palette=custom_palette, legend=show_legend, s=point_size, ax=ax
    )

    # Regression lines
    if fit_line == "overall":
        sns.regplot(
            data=agg_df, x="age", y="expression", scatter=False, color="black", ax=ax,
            **line_kwargs
        )
    elif fit_line == "per_group":
        # Compute per-group regression lines and (optionally) per-group stats
        by_group_stats = {}
        for g, sub in agg_df.groupby("group", observed=True, sort=False):
            if len(sub) >= 3 and sub["expression"].nunique() > 1 and sub["age"].nunique() > 1:
                c = custom_palette.get(g, None) if isinstance(custom_palette, dict) else None
                sns.regplot(data=sub, x="age", y="expression", scatter=False, color=c, ax=ax, **line_kwargs)
                # per-group corr
                if corr_method == "pearson":
                    rg, pg = pearsonr(sub["age"].values, sub["expression"].values)
                else:
                    rg, pg = spearmanr(sub["age"].values, sub["expression"].values)
                by_group_stats[str(g)] = {"r": float(rg), "p": float(pg), "n": int(len(sub))}
            else:
                by_group_stats[str(g)] = {"r": np.nan, "p": np.nan, "n": int(len(sub))}
        stats["by_group"] = by_group_stats

    # Labels & title
    src_txt = (f"layers['{layer_name}']" if source == "layer" else ("raw.X" if source == "raw" else "X"))
    log_txt = " | log1p" if log_transform else ""
    title = f"{gene_used} vs age — r={r:.2f}, p={p:.1e}, n={n}  ({src_txt}{log_txt}, {agg_func})"
    ax.set_title(title)
    ax.set_xlabel("Age")
    ax.set_ylabel(f"Per-sample {agg_func} expression")

    # Make axis/legend text black for print clarity
    ax.tick_params(colors="black")
    ax.xaxis.label.set_color("black")
    ax.yaxis.label.set_color("black")
    if show_legend and ax.get_legend() is not None:
        for text in ax.get_legend().get_texts():
            text.set_color("black")
        ax.get_legend().get_title().set_color("black")

    fig.tight_layout()
    plt.show()

    #return agg_df, stats, (fig, ax)



###########################################################################
############### 4- cor_plot_multiple_genes ##################
###########################################################################
# Updated 03/02/2026

import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
from scipy.stats import pearsonr, spearmanr


def cor_plot_multiple_genes(
    adata,
    genes,
    sample_col="sample_id",
    group_col="Age_groups",
    age_col="Age_in_months",
    custom_palette=None,
    ncols=5,
    # ---- data source (matches Boxplot_genes) ----
    source="layer",              # "layer" | "raw" | "X"
    layer_name="counts",         # only used if source == "layer"
    log_transform=True,          # apply log1p before aggregation/correlation
    # ---- aggregation & correlation ----
    agg_func="mean",             # "mean" or "median" per-sample aggregation
    corr_method="pearson",       # "pearson" or "spearman"
    # ---- visual ----
    show_legend=True,
    figsize_per_plot=(6, 5),
    # ---- saving/automation ----
    save_path=None,              # e.g. "outputs/cor_plot.pdf"
    save_dpi=300,
    transparent=False,
    show=True,
    # ---- point styling (matches Boxplot_genes) ----
    point_marker="o",
    point_size=4,
    point_alpha=1.0,
    # ---- regression line styling ----
    reg_line_color="black",
    reg_line_width=1.5,
):
    """
    Scatter + regression plots of per-sample aggregated expression vs age
    for multiple genes.

    Mirrors the source/layer/palette/styling conventions of Boxplot_genes.

    Parameters
    ----------
    genes : list or tuple of str
        Gene names to plot.
    source : str
        Where to read expression from: ``"layer"`` (default), ``"raw"``, or ``"X"``.
    layer_name : str
        Layer key — only used when ``source == "layer"``.
    log_transform : bool
        Apply log1p to expression values before aggregation.
    agg_func : str
        ``"mean"`` or ``"median"`` — per-sample aggregation method.
    corr_method : str
        ``"pearson"`` or ``"spearman"``.
    save_path : str or None
        File path to save the figure.  Use ``"{gene}"`` placeholder to save
        one file per gene, e.g. ``"outputs/{gene}_cor.pdf"``.
    point_marker / point_size / point_alpha
        Scatter point aesthetics — mirrors Boxplot_genes parameters.

    Returns
    -------
    results : dict
        ``gene -> {"agg_df": DataFrame, "stats": {"method","r","p","n"}}``
    """

    # ---- palette defaults (same as Boxplot_genes) -----------------------
    if custom_palette is None:
        custom_palette = {
            "Infants":     "#0072B2",
            "Child":       "#56B4E9",
            "Adolescent":  "#009E73",
            "Young":       "#F0E442",
            "Middle_aged": "#E69F00",
            "Older":       "#D55E00",
            "Oldest_old":  "#CC79A7",
        }

    # ---- sanity checks --------------------------------------------------
    if not isinstance(genes, (list, tuple)) or len(genes) == 0:
        raise ValueError("genes must be a non-empty list or tuple of gene names.")
    if agg_func not in {"mean", "median"}:
        raise ValueError("agg_func must be 'mean' or 'median'.")
    if corr_method not in {"pearson", "spearman"}:
        raise ValueError("corr_method must be 'pearson' or 'spearman'.")
    for col in (sample_col, group_col, age_col):
        if col not in adata.obs.columns:
            raise KeyError(f"Column '{col}' not found in adata.obs.")

    # ---- choose matrix & var_names (same logic as Boxplot_genes) --------
    if source == "layer":
        if layer_name not in adata.layers:
            raise KeyError(
                f"Layer '{layer_name}' not found in adata.layers. "
                "Set source='raw' or source='X'."
            )
        mat = adata.layers[layer_name]
        var_names = adata.var_names
    elif source == "raw":
        if adata.raw is None:
            raise KeyError("adata.raw is None; cannot use source='raw'.")
        mat = adata.raw.X
        var_names = adata.raw.var_names
    elif source == "X":
        mat = adata.X
        var_names = adata.var_names
    else:
        raise ValueError("'source' must be one of: 'layer', 'raw', or 'X'.")

    src_label = (
        f"layers['{layer_name}']" if source == "layer"
        else ("raw.X" if source == "raw" else "X")
    )

    # ---- figure layout --------------------------------------------------
    per_gene_files = save_path is not None and "{gene}" in str(save_path)
    num_genes      = len(genes)
    n_cols         = min(ncols, num_genes)
    n_rows         = int(np.ceil(num_genes / n_cols))

    if not per_gene_files:
        fig, axes = plt.subplots(
            n_rows, n_cols,
            figsize=(figsize_per_plot[0] * n_cols, figsize_per_plot[1] * n_rows),
        )
        axes_flat = np.array(axes).flatten() if num_genes > 1 else [axes]

    results = {}
    agg_op  = agg_func  # "mean" or "median"

    for i, gene in enumerate(genes):

        # --- per-gene figure when saving individually --------------------
        if per_gene_files:
            fig, ax = plt.subplots(figsize=figsize_per_plot)
        else:
            ax = axes_flat[i]

        # --- gene index (case-sensitive → case-insensitive fallback) -----
        if gene in var_names:
            gene_idx = int(np.where(var_names == gene)[0][0])
        else:
            lower_map = {g.lower(): idx for idx, g in enumerate(var_names)}
            if gene.lower() in lower_map:
                gene_idx = lower_map[gene.lower()]
            else:
                ax.text(0.5, 0.5, f"Gene '{gene}' not found in {src_label}",
                        ha="center", va="center", fontsize=9)
                ax.axis("off")
                results[gene] = {
                    "agg_df": pd.DataFrame(),
                    "stats": {"method": corr_method, "r": np.nan, "p": np.nan, "n": 0},
                }
                continue

        # --- extract expression ------------------------------------------
        try:
            col = mat[:, gene_idx]
        except Exception as e:
            ax.text(0.5, 0.5, f"Error slicing '{gene}': {e}",
                    ha="center", va="center", fontsize=9)
            ax.axis("off")
            results[gene] = {
                "agg_df": pd.DataFrame(),
                "stats": {"method": corr_method, "r": np.nan, "p": np.nan, "n": 0},
            }
            continue

        if hasattr(col, "toarray"):
            expression = col.toarray().ravel()
        else:
            expression = np.asarray(col).ravel()

        if log_transform:
            expression = np.log1p(expression)

        # --- per-cell → per-sample aggregation --------------------------
        df = pd.DataFrame({
            "expression": expression,
            "sample":     adata.obs[sample_col].values,
            "group":      adata.obs[group_col].values,
            "age":        adata.obs[age_col].values,
        })

        agg_df = df.groupby("sample", as_index=False).agg(
            expression=("expression", agg_op),
            age=("age", "first"),       # donor-level age
        )
        group_info = df.drop_duplicates("sample")[["sample", "group"]]
        agg_df     = pd.merge(agg_df, group_info, on="sample", how="left")

        # --- correlation -------------------------------------------------
        n = len(agg_df)
        if n < 3 or agg_df["expression"].nunique() <= 1 or agg_df["age"].nunique() <= 1:
            r, p = np.nan, np.nan
            ax.text(0.5, 0.92, "Insufficient data for correlation",
                    ha="center", va="center", fontsize=8,
                    transform=ax.transAxes)
        else:
            if corr_method == "pearson":
                r, p = pearsonr(agg_df["expression"], agg_df["age"])
            else:
                r, p = spearmanr(agg_df["expression"], agg_df["age"])

        # ---- scatter (styled like Boxplot_genes strip points) -----------
        sns.scatterplot(
            data=agg_df, x="age", y="expression",
            hue="group", palette=custom_palette,
            marker=point_marker,
            s=point_size ** 2,          # seaborn uses area units
            alpha=point_alpha,
            edgecolor="black",
            linewidth=0.25,
            ax=ax,
            legend=show_legend,
        )

        # ---- regression line -------------------------------------------
        try:
            sns.regplot(
                data=agg_df, x="age", y="expression",
                scatter=False,
                line_kws={"color": reg_line_color, "linewidth": reg_line_width},
                ax=ax,
            )
        except Exception:
            pass

        # ---- labels & title --------------------------------------------
        r_str = f"{r:.2f}" if not np.isnan(r) else "nan"
        p_str = f"{p:.1e}" if not np.isnan(p) else "nan"

        ax.set_title(
            f"{gene}  (r={r_str}, p={p_str}, n={n})\n"
            f"[{src_label}{' | log1p' if log_transform else ''}, {agg_op}]",
            fontsize=10,
        )
        ax.set_xlabel("Age (months)")
        ax.set_ylabel(
            f"Per-sample {agg_op} expression"
            + (" (log1p)" if log_transform else "")
        )

        # ---- save per-gene file if requested ---------------------------
        if per_gene_files:
            fig.tight_layout()
            path = save_path.format(gene=gene)
            fig.savefig(path, dpi=save_dpi, bbox_inches="tight",
                        transparent=transparent)
            print(f"Saved: {path}")
            if show:
                plt.show()
            else:
                plt.close(fig)

        results[gene] = {
            "agg_df": agg_df,
            "stats": {
                "method": corr_method,
                "r": float(r) if not np.isnan(r) else np.nan,
                "p": float(p) if not np.isnan(p) else np.nan,
                "n": int(n),
            },
        }

    # ---- shared figure: hide unused panels, save, show -----------------
    if not per_gene_files:
        for j in range(num_genes, n_rows * n_cols):
            axes_flat[j].set_visible(False)

        fig.tight_layout()

        if save_path is not None:
            fig.savefig(save_path, dpi=save_dpi, bbox_inches="tight",
                        transparent=transparent)
            print(f"Saved: {save_path}")

        if show:
            plt.show()
        else:
            plt.close(fig)

    #return results


#############################################################################################
                ############### 5- cor_plot_one_age_group 
#############################################################################################



import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import pearsonr, spearmanr

def cor_plot_one_age_group(
    adata,
    gene,
    sample_col="sample_id",
    group_col="Age_groups",
    age_col="Age_in_months",
    selected_group="Infants",
    custom_palette=None,
    layer_name="counts",        # <-- raw counts expected in layers['counts']
    log_transform=True,         # apply log1p(counts) before aggregation/correlation
    agg_func="mean",            # "mean" or "median" per-sample aggregation
    corr_method="pearson",      # "pearson" or "spearman"
    show_legend=False,          # single-group legend typically unnecessary
    figsize=(8, 6)
):
    """
    Scatter + regression plot of per-sample aggregated expression vs age for one group,
    using raw counts from adata.layers['counts'].

    Parameters
    ----------
    adata : AnnData
        AnnData object with raw counts in adata.layers[layer_name].
    gene : str
        Gene name present in adata.var_names.
    sample_col : str
        Column in adata.obs identifying samples (e.g., pseudo-bulk sample IDs).
    group_col : str
        Column in adata.obs specifying age groups (e.g., 'Age_groups').
    age_col : str
        Column in adata.obs specifying age (e.g., 'Age_in_months').
    selected_group : str
        Group to filter for (e.g., 'Infants').
    custom_palette : dict or None
        Mapping from group name to color hex. If None, a default palette is used.
    layer_name : str
        Name of the layer containing raw counts (default: 'counts').
    log_transform : bool
        If True, apply log1p transform to counts before aggregation/correlation.
    agg_func : {"mean","median"}
        Aggregation function for per-sample expression.
    corr_method : {"pearson","spearman"}
        Correlation method to compute.
    show_legend : bool
        Whether to display the legend (single group usually False).
    figsize : tuple
        Figure size for the plot.

    Returns
    -------
    agg_df : pandas.DataFrame
        Per-sample aggregated dataframe with columns: ['sample','expression','age','group'].
    stats : dict
        Correlation stats: {'method','r','p','n'}.
    """

    # --- palette defaults
    if custom_palette is None:
        custom_palette = {
            'Infants': '#99d8c9',
            'Child': '#9ecae1',
            'Adolescent': '#fa9fb5',
            'Young': '#fec44f',
            'Middle_aged': '#807dba',
            'Older': '#bcbddc',
            'Oldest_old': '#fb6a4a'
        }

    # --- sanity checks
    if layer_name not in adata.layers:
        raise KeyError(
            f"Layer '{layer_name}' not found in adata.layers. "
            "This function expects raw counts in adata.layers['counts']."
        )
    if gene not in adata.var_names:
        raise KeyError(f"Gene '{gene}' not found in adata.var_names.")
    for col in (sample_col, group_col, age_col):
        if col not in adata.obs.columns:
            raise KeyError(f"Column '{col}' not found in adata.obs.")
    if agg_func not in {"mean", "median"}:
        raise ValueError("agg_func must be 'mean' or 'median'.")
    if corr_method not in {"pearson", "spearman"}:
        raise ValueError("corr_method must be 'pearson' or 'spearman'.")

    # --- Filter AnnData by selected group
    mask = (adata.obs[group_col] == selected_group)
    if mask.sum() == 0:
        raise ValueError(
            f"No cells found for selected_group='{selected_group}'. "
            f"Available groups: {sorted(adata.obs[group_col].unique())}"
        )
    adata_filtered = adata[mask, :]

    # --- extract raw counts for the gene from the specified layer
    layer_mat = adata_filtered.layers[layer_name]
    gene_idx = np.where(adata_filtered.var_names == gene)[0]
    if len(gene_idx) == 0:
        raise KeyError(f"Gene '{gene}' not found in adata_filtered.var_names.")
    gene_idx = gene_idx[0]

    try:
        gene_counts = layer_mat[:, gene_idx]
    except Exception as e:
        raise RuntimeError(
            f"Failed to slice layer '{layer_name}' for gene '{gene}': {e}"
        )

    # Convert to 1D numpy array; handle sparse and dense
    if hasattr(gene_counts, "toarray"):
        expression = gene_counts.toarray().flatten()
    else:
        expression = np.asarray(gene_counts).flatten()

    # Optional log1p transform (common for count data visualization/correlation)
    if log_transform:
        expression = np.log1p(expression)

    # --- build per-cell dataframe
    df = pd.DataFrame({
        "expression": expression,
        "sample": adata_filtered.obs[sample_col].values,
        "group": adata_filtered.obs[group_col].values,
        "age": adata_filtered.obs[age_col].values
    })

    # --- aggregate to per-sample expression (and age)
    agg_op = "mean" if agg_func == "mean" else "median"
    agg_df = df.groupby("sample", as_index=False).agg({
        "expression": agg_op,
        "age": "first"  # assumes age is donor-level; adjust if needed
    })

    # Merge group info (constant here but kept for clarity/hue)
    group_info = df.drop_duplicates(subset=["sample"])[["sample", "group"]]
    agg_df = pd.merge(agg_df, group_info, on="sample", how="left")

    # --- guards for correlation
    n = len(agg_df)
    if n < 3:
        raise ValueError(
            f"Not enough samples ({n}) in '{selected_group}' for correlation. Need at least 3."
        )
    if agg_df["expression"].nunique() <= 1:
        raise ValueError(
            "Expression has no variability after aggregation; correlation undefined."
        )
    if agg_df["age"].nunique() <= 1:
        raise ValueError(
            "Age has no variability within the selected group; correlation undefined."
        )

    # --- Compute correlation
    if corr_method == "pearson":
        r, p = pearsonr(agg_df['expression'], agg_df['age'])
    else:
        r, p = spearmanr(agg_df['expression'], agg_df['age'])

    stats = {"method": corr_method, "r": float(r), "p": float(p), "n": int(n)}

    # --- Plot
    plt.figure(figsize=figsize)
    sns.scatterplot(
        data=agg_df, x="age", y="expression",
        hue="group", palette=custom_palette, legend=show_legend
    )
    # Regression line (linear fit) for visualization; for Spearman we still show linear trend
    sns.regplot(
        data=agg_df, x="age", y="expression",
        scatter=False, color="black"
    )
    title = (
        f"Age vs {gene} in {selected_group} "
        f"(r={r:.2f}, p={p:.1e}, n={n}) "
        #f"[from layers['{layer_name}']{' log1p' if log_transform else ''}, {agg_op}]"
    )
    plt.title(title)
    plt.xlabel("Age")
    ylabel = f"Per-sample {agg_op.capitalize()} Counts" + (" (log1p)" if log_transform else "")
    plt.ylabel(ylabel)
    plt.tight_layout()
    plt.show()

    #print("Function 'cor_plot_one_age_group' has been defined (using layers['counts']).")
    #return agg_df, stats


####################################################################################
                        ###### 6- plot_deg_counts #### 
##########################################################################################

def plot_deg_counts(path_to_csv, 
                    padj_threshold=0.05, 
                    log2fc_threshold=0.6, 
                    figsize=(10, 6)):
    import os
    import pandas as pd
    import matplotlib.pyplot as plt

    # Initialize DEG counts
    deg_counts = {}

    # Loop through CSV files
    for file in os.listdir(path_to_csv):
        if file.endswith('.csv'):
            filepath = os.path.join(path_to_csv, file)
            de = pd.read_csv(filepath, index_col=0)

            # Extract category from filename
            parts = file.replace('.csv', '').split('_')
            category = '_'.join(parts[1:-3])

            # Count DEGs
            up = ((de['padj'] < padj_threshold) & (de['log2FoldChange'] > log2fc_threshold)).sum()
            down = ((de['padj'] < padj_threshold) & (de['log2FoldChange'] < -log2fc_threshold)).sum()

            deg_counts[f'UP_{category}'] = up
            deg_counts[f'DOWN_{category}'] = down

    # Group by category and calculate total DEGs
    category_totals = {}
    for key, value in deg_counts.items():
        category = key.split('_')[1]
        category_totals[category] = category_totals.get(category, 0) + value

    # Sort categories by total DEGs
    sorted_categories = sorted(category_totals, key=category_totals.get, reverse=True)

    # Reorder deg_counts based on sorted categories with spacing
    ordered_keys = []
    for cat in sorted_categories:
        ordered_keys.append(f'UP_{cat}')
        ordered_keys.append(f'DOWN_{cat}')
        ordered_keys.append('')  # spacer

    if ordered_keys and ordered_keys[-1] == '':
        ordered_keys.pop()

    ordered_values = [deg_counts.get(k, 0) if k else 0 for k in ordered_keys]
    colors = ['#99d8c9' if 'UP' in k else '#fec44f' if 'DOWN' in k else 'white' for k in ordered_keys]

    # Plot
    plt.figure(figsize=figsize)
    plt.bar(ordered_keys, ordered_values, color=colors)
    plt.ylabel('Number of DEGs')
    plt.title('Upregulated and Downregulated DEGs Across Cell Types')
    plt.xticks(rotation=90)
    plt.tight_layout()
    plt.show()




