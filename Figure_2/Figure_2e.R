library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('CD14_mo_ISGhi'= '#f15d64',
          'CD14_mo'= '#f6a2a7',
          'CD16_mo'= '#f9d3d7')

