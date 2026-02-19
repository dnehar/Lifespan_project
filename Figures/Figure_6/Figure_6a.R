library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


#color 
cols <- c("CD8_T_Naive"="#f37421",
            'gd_Tcells'= '#80622f',
            'CD8_Naive'= '#f37421',
            'CD8_TEMRA'= '#d28529',
            'CD8_GZMK'= '#fba919',
            'CD8_MAIT'= '#fbb36a',
            'CD8_cycling'='#7f7f7f',
            'CD8_GD'= '#80622f',
            'CD8_Naive_SOX4'= '#ffdeadff')
            

# plot umap CD4 T cells

p_umap_subset <- LifeSpan_ALL_MetaData %>% 
dplyr::filter(Lineage %in% c('CD8_Tcells')) %>% 
#mutate(subset_simple_clustering = gsub(pattern = "CD4_CTL", replacement = "CD4_T_Memory", x = subset_simple_clustering)) %>%
ggplot(aes(x=SC_umap1, y=SC_umap2,  color=Final_annotations)) +
geom_point(size=0.1) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)
