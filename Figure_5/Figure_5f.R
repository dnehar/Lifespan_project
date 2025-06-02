library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


#color 
cols <- c("CD4_T_Naive"="#193a1c",
            "CD4_T_ISGhi"="#697d35",
            "CD4_Tregs"="#137d82",
            "CD4_T_Memory"="#1c572b",
            'CD4_ISGhi'= '#697d35',
            'CD4_T_ISGhi'='#697d35',
            #CD4_CTL'= '#90aa3c',
            'CD4_Naive'= '#193a1c')
            

# plot umap CD4 T cells

p_umap_subset <- LifeSpan_ALL_MetaData %>% 
dplyr::filter(Lineage %in% c('CD4_Tcells')) %>% 
mutate(subset_simple_clustering = gsub(pattern = "CD4_CTL", replacement = "CD4_T_Memory", x = subset_simple_clustering)) %>%
ggplot(aes(x=SC_umap1, y=SC_umap2,  color=subset_simple_clustering)) +
geom_point(size=0.05) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)
