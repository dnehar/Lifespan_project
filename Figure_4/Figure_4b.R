library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('B_ISGhi'='#9ecae1',
            'B_memory'='#283779',
            'B_ABC'='#41b8ea',
            'PCs'='#8856a7',
            'B_transitional'='#756bb1',
            'B_naive'='#1c9099')
