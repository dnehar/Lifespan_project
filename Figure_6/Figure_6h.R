
library(dplyr)
library(ggplot2)
library(Seurat)

# load data: 
 #https://www.science.org/doi/10.1126/sciimmunol.abf0125?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed
  GD_T <- readRDS('/GSE149356_Human_GDT_Seurat.rds')
  GD_T <- UpdateSeuratObject(GD_T)



# Figure 7h ########

# Violin plots 
    VP_gd <- VlnPlot(GD_T, 
                     features = c('TRDC','SOX4'),
                     group.by = 'group', ncol = 2, 
                     cols=c('Adult'='#4c459c','Newborn'='#96daf7'),
                     pt.size = 0) & theme(legend.position="none", 
                                            axis.ticks.x=element_blank(),
                                            axis.ticks.y=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank())
    print(VP_gd)

  
