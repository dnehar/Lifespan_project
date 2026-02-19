library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['pheno']] %>% as.data.frame()
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

ordered_ids <- as.character(pheno$Names)

p_tonic <- LifeSpan_ALL_MetaData %>% 
     
     mutate(Groups = factor(Groups, levels = c("HI", "HC", "HY", "HO"))) %>%
     mutate(ReCluster = factor(Final_annotations)) %>%
     group_by(Groups, Names, ReCluster) %>%
     summarise(n = n()) %>% #, Set = first(Set)
     mutate(freq = n / sum(n) *100) %>%
     ungroup() %>%
     as.data.frame() %>%
     filter(ReCluster %in% c( 'B_ISGhi')) %>% 
     
     #plot the number of cells
     ggplot(aes(x=Names, y=n)) + 
     geom_bar(stat="identity",fill="#4c459c", color="black") + #CD4 T cells:  #697d35 #CD14 mo: #f15d64,  B cell: 
     ylab("Number of cells") +
     xlab("Individuals") +
     scale_x_discrete(limits=ordered_ids) +
     theme(axis.text.y=element_text(size=16), 
           axis.text.x=element_text(size=16, angle = 90),
           axis.title.x = element_text(face="bold", size=18),
           axis.title.y = element_text(face="bold", size=18),
           legend.position = "none") +

     ggtitle("ISGhi B cells ")  +
     theme(plot.title = element_text(hjust = 0.5, vjust = 2, size=20, face="bold"))
   
   p_tonic

