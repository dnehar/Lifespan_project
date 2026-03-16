
import gene_plot_utils as gputils
import importlib
#importlib.reload(gputils)

# Boxplot SOX4+ vs. SOX4-
gputils.Boxplot_one_gene(naive_CD4,
                 gene='STMN1', 
                 group_col='LS_L4', 
                 group_order=['CD4_naive_SOX4-','CD4_naive_SOX4+'],
                 source='raw', 
                 log_transform= False,
                custom_palette= cols) #save_path='./SOX4_CD8Tcells.pdf')

# Barplot Expression vs. age 
gputils.cor_plot_multiple_genes(naive_CD4, 
                                 genes=['SOX4','ID2','TOX2','SMC4','CHI3L2','CD38','STMN1','SMAD7','IGF2BP3','ITM2A'], 
                                source='raw', 
                                ncols=5)
