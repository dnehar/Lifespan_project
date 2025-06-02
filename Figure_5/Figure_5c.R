library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('naive_Tregs'= '#137d82',
            'mem_Tregs'= '#56bbbf')
# subset to be plotted 
subset_to_be_plotted <- c("naive_Tregs", "mem_Tregs")
           
# plot umap B cells
p_umap_subset <- LifeSpan_ALL_MetaData %>% 
 dplyr::filter(subset_simple_clustering %in% c('CD4_Tregs')) %>% 
ggplot(aes(x=X_umap1, y=X_umap2,  color=Final_annotations)) +
geom_point(size=0.5) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)
