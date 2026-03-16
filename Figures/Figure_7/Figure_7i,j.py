import numpy as np
import pandas as pd
import scanpy as sc
import seaborn as sb
import seaborn as sns
import gene_plot_utils as gputils
import importlib
#importlib.reload(gputils)

cols={"CD4_naive_SOX4-" : "#193a1c",
   "CD4_naive_SOX4+" : "#a4de02ff",
   "CD8_naive_SOX4+" : "#ffdeadff",
   "CD8_naive_SOX4-" :"#f37421"}

# 1 - naive CD4 T cells 
naive_CD4=sc.read('./naive_CD4.h5ad')
sc.pp.combat(naive_CD4, key='Batch', covariates=['Age_groups','LS_L4'])


## Boxplot SOX4+ vs. SOX4-
gputils.Boxplot_one_gene(naive_CD4,
                 gene='STMN1', 
                 group_col='LS_L4', 
                 group_order=['CD4_naive_SOX4-','CD4_naive_SOX4+'],
                 source='raw', 
                 log_transform= False,
                custom_palette= cols) #save_path='./SOX4_CD8Tcells.pdf')

## Barplot Expression vs. age 
gputils.cor_plot_multiple_genes(naive_CD4, 
                                 genes=['SOX4','ID2','TOX2','SMC4','CHI3L2','CD38','STMN1','SMAD7','IGF2BP3','ITM2A'], 
                                source='raw', 
                                ncols=5)
# 2 - naive CD8 T cells 
naive_CD8=sc.read('./naive_CD8.h5ad')
sc.pp.combat(naive_CD4, key='Batch', covariates=['Age_groups','LS_L4'])

## Boxplot SOX4+ vs. SOX4-
gputils.Boxplot_one_gene(naive_CD8,
                 gene='STMN1', 
                 group_col='LS_L4', 
                 group_order=['CD4_naive_SOX4-','CD4_naive_SOX4+'],
                 source='raw', 
                 log_transform= False,
                custom_palette= cols) #save_path='./SOX4_CD8Tcells.pdf')

## Barplot Expression vs. age 
gputils.cor_plot_multiple_genes(naive_CD8, 
                                 genes=['SOX4','ID2','TOX2','SMC4','CHI3L2','CD38','STMN1','SMAD7','IGF2BP3','ITM2A'], 
                                source='raw', 
                                ncols=5)
