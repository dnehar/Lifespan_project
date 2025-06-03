library(dplyr)
library(ggplot2)
library(reshape2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['meta_small']] %>% as.data.frame()

  MetaData <- LifeSpan_ALL_MetaData
  HI <- MetaData %>% filter (Groups =="HI") %>% dplyr::select(n_genes) 
  HC <- MetaData %>% filter (Groups =="HC") %>% dplyr::select(n_genes)  
  HY <- MetaData %>% filter (Groups =="HY") %>% dplyr::select(n_genes) 
  HO <- MetaData %>% filter (Groups =="HO") %>% dplyr::select(n_genes)  

mat <-  MetaData %>% dplyr::select(Groups,n_genes)
mat1 <- melt(mat)
  
# reorder age groups  
  mat1$Groups <- factor(mat1$Groups, levels = c("HI","HC","HY","HO"))
# plot 
  K <- ggplot(mat1, aes(x=value, fill=Groups)) + 
    geom_histogram(position="identity") +
    geom_vline(aes(xintercept=mean(value)), color="black",
               linetype="dashed")+
    facet_wrap(~Groups,ncol = 4 ,scales = "free") + 
    scale_fill_manual(values=col_age_gp) + #***
    scale_color_grey()+
    theme(legend.position="none", 
          axis.text.y=element_text(size=18), 
          axis.text.x=element_text(size=18, angle = 90),
          axis.title.x = element_text(face="bold", size=18),
          axis.title.y = element_text(face="bold", size=18),
          plot.title = element_text(hjust = 0.5,face='bold',size=14),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black', size=1))+
    theme(strip.text.x = element_text(size = 16),
          strip.background = element_rect(colour = 'black',fill='#C0C0C0')) +
    xlab("number of genes in age groups")
  
print(K)
  ggsave("../PANELS/Number_of_genes_per_age_groups.pdf",  
         K , width=3.5, height=1.2,  units="in", scale=3)
  
