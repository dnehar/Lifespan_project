# =============================================================================
# Figure 7a — CD38 and SOX4 expression in naïve CD4⁺ and CD8⁺ T cells across age groups
#
# This script plots pseudobulked gene expression (counts × samples) for CD38
# and SOX4 in naïve CD4⁺ and CD8⁺ T cell subsets across seven age groups.
# Batch correction (ComBat) is applied before plotting to remove study-level
# technical variation while preserving biological signals.
#
# Input:  naive_CD4.h5ad, naive_CD8.h5ad
#         AnnData objects (.h5ad) available at GSE233321:
#         https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE233321
# Output: Boxplots for CD38 and SOX4 in naïve CD4⁺ and CD8⁺ T cells
# =============================================================================

import numpy as np
import pandas as pd
import scanpy as sc
import seaborn as sns

import gene_plot_utils as gputils

# =============================================================================
# 1 — Naïve CD4⁺ T cells
# =============================================================================

# --- Load naïve CD4⁺ T cell AnnData object ---
naive_CD4 = sc.read('./naive_CD4.h5ad')

# --- Batch correction (ComBat) ---
# Removes study-level (Batch) technical variation while preserving
# Age_groups and LS_L4 (cell subtype) as biological covariates
sc.pp.combat(naive_CD4, key='Batch', covariates=['Age_groups', 'LS_L4'])  # Batch = Studies

# --- Plot CD38 and SOX4 expression across age groups ---
# source='raw' uses raw counts; log_transform=True applies log1p before plotting
gputils.Boxplot_one_gene(naive_CD4,
                 gene="CD38",
                 source='raw',
                 log_transform=True)

gputils.Boxplot_one_gene(naive_CD4,
                 gene="SOX4",
                 source='raw',
                 log_transform=True)

# =============================================================================
# 2 — Naïve CD8⁺ T cells
# =============================================================================

# --- Load naïve CD8⁺ T cell AnnData object ---
naive_CD8 = sc.read('./naive_CD8.h5ad')

# --- Batch correction (ComBat) ---
# Same covariates as CD4 to ensure comparable correction across subsets
sc.pp.combat(naive_CD8, key='Batch', covariates=['Age_groups', 'LS_L4'])  # Batch = Studies

# --- Plot CD38 and SOX4 expression across age groups ---
gputils.Boxplot_one_gene(naive_CD8,
                 gene="CD38",
                 source='raw',
                 log_transform=True)

gputils.Boxplot_one_gene(naive_CD8,
                 gene="SOX4",
                 source='raw',
                 log_transform=True)
