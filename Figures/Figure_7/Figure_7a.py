import numpy as np
import pandas as pd
import scanpy as sc
import seaborn as sns

import gene_plot_utils as gputils

# plot pseudobulked counts (counts x samples) in naive CD4 and CD8 T cells across age groups:

# 1 - naive CD4 T cells
# load data
naive_CD4 = sc.read('./naive_CD4.h5ad')
# batch correct
sc.pp.combat(naive_CD4, key='Batch', covariates=['Age_groups', 'LS_L4'])  # Batch = Studies

# plots
gputils.Boxplot_one_gene(naive_CD4,
                 gene="CD38",  
                 source='raw',
                 log_transform=True)

gputils.Boxplot_one_gene(naive_CD4,
                 gene="SOX4", 
                 source='raw',
                 log_transform=True)


# 2 - naive CD8 T cells
# load data
naive_CD8 = sc.read('./naive_CD8.h5ad')
# batch correct
sc.pp.combat(naive_CD8, key='Batch', covariates=['Age_groups', 'LS_L4'])  # Batch = Studies

gputils.Boxplot_one_gene(naive_CD8,
                 gene="CD38",  # previously: CTLA4
                 source='raw',
                 log_transform=True)

gputils.Boxplot_one_gene(naive_CD8,
                 gene="SOX4",  # previously: CTLA4
                 source='raw',
                 log_transform=True)
