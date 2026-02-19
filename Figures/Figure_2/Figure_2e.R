library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('CD14_mo_ISGhi'= '#f15d64',
          'CD14_mo'= '#f6a2a7',
          'CD16_mo'= '#f9d3d7')


# plot umap monocytes

p_umap_subset <- meta_all %>% 
dplyr::filter(Lineage %in% 'Monocytes') %>% 
ggplot(aes(x=SC_umap1, y=SC_umap2,  color=Final_annotations)) +
geom_point(size=0.2) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)
