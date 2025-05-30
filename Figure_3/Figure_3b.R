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



