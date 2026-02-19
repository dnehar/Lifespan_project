library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('Tmem_TH2'= '#1c7b3d',
            'Tmem_TH17'= '#3cb54a',
            'Tmem_CM'= '#74c168',
            'Tmem_HLA_DR'= '#a4de02ff',
            'Tmem_ISGhi'= '#697d35',
            'Tmem_CM_TOX'= '#edf8b1',
            'Tmem_GzK_TH1_like'='#a8ddb5',
            'Tmem_TEMRA'='#1c572b',
            'Tmem_CM_SOX4'= '#7fcdbb')
            
# subset to be plotted 
subset_to_be_plotted <- c("Tmem_CM", "Tmem_CM_SOX4","Tmem_CM_TOX", "Tmem_GzK_TH1_like","Tmem_TEMRA", "Tmem_TH2","Tmem_TH17","Tmem_HLA_DR","Tmem_ISGhi")

            
# plot umap B cells
p_umap_subset <- LifeSpan_ALL_MetaData %>% 
 dplyr::filter(subset_simple_clustering %in% c('CD4_CTL','CD4_T_Memory')) %>% 
ggplot(aes(x=X_umap1, y=X_umap2,  color=Final_annotations)) +
geom_point(size=0.5) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)
