library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('cDC2'= '#d84598',
          'cDC1'= '#771215',
          'AXL_DC'= '#a41e21',
          'moDC'= '#ed2024',
          'pDC'= '#a5a4a4')
            

# plot umap DCs
p_umap_subset <- meta_all %>% 
dplyr::filter(Lineage %in% 'DCs') %>% 
ggplot(aes(x=SC_umap1, y=SC_umap2,  color=Final_annotations)) +
geom_point(size=0.5) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)

