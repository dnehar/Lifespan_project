library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('CD4_Naive'= '#193a1c',
            'CD4_Naive_SOX4'='#a4de02ff')



