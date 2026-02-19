library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('NK_CD16'= '#fee000',
            'NK_XCL1'= '#f2e4a0',
            'NK_cycling'= '#ccb72d',
            'NK_CD16_KLRC2'='#feb24c')
            

# plot umap NK cells 
p_umap_subset <- meta_all %>% 
dplyr::filter(Lineage %in% 'NK_cells') %>% 
ggplot(aes(x=SC_umap1, y=SC_umap2,  color=Final_annotations)) +
geom_point(size=0.5) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)
