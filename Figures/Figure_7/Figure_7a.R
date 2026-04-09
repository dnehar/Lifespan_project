# ============================================================
# Figure_7a.R
# Pseudobulked gene expression boxplots (CD38 and SOX4)
# across age groups in naive CD4 and CD8 T cells.
# ============================================================

# --- Imports ---
import numpy as np
import pandas as pd
import scanpy as sc        # Single-cell analysis toolkit
import seaborn as sns      # Seaborn for plotting

import gene_plot_utils as gputils  # Custom utility module for gene-level plotting
import importlib
# importlib.reload(gputils)  # Uncomment to reload gputils during development without restarting kernel


# ============================================================
# 1. Naive CD4 T cells
# ============================================================

# Load the pre-processed naive CD4 T cell dataset (AnnData format)
naive_CD4 = sc.read('./naive_CD4.h5ad')

# Batch correction using ComBat
# key='Batch'      -> column in obs representing the batch (here: Studies)
# covariates       -> biological covariates to preserve during correction
sc.pp.combat(naive_CD4, key='Batch', covariates=['Age_groups', 'LS_L4'])

# Boxplot: CD38 expression across age groups in naive CD4 T cells
gputils.Boxplot_one_gene(naive_CD4,
                 gene="CD38",
                 source='raw',         # Use raw (unnormalized) counts
                 log_transform=True)   # Apply log transformation for visualization
                 # save_path='./CD38_CD4Tcells.pdf'  # Uncomment to save plot to file

# Boxplot: SOX4 expression across age groups in naive CD4 T cells
gputils.Boxplot_one_gene(naive_CD4,
                 gene="SOX4",
                 source='raw',
                 log_transform=True)
                 # save_path='./SOX4_CD4Tcells.pdf'  # Uncomment to save plot to file


# ============================================================
# 2. Naive CD8 T cells
# ============================================================

# Load the pre-processed naive CD8 T cell dataset (AnnData format)
naive_CD8 = sc.read('./naive_CD8.h5ad')

# Batch correction (same approach as CD4 above)
sc.pp.combat(naive_CD8, key='Batch', covariates=['Age_groups', 'LS_L4'])

# Boxplot: CD38 expression across age groups in naive CD8 T cells
gputils.Boxplot_one_gene(naive_CD8,
                 gene="CD38",
                 source='raw',
                 log_transform=True)
                 # save_path='./CD38_CD8Tcells.pdf'  # Uncomment to save plot to file

# Boxplot: SOX4 expression across age groups in naive CD8 T cells
gputils.Boxplot_one_gene(naive_CD8,
                 gene="SOX4",
                 source='raw',
                 log_transform=True)
                 # save_path='./SOX4_CD8Tcells.pdf'  # Uncomment to save plot to file

