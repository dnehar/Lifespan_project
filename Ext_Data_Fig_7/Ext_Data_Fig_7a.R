library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['pheno']] %>% as.data.frame()
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
 cols <- c('CD4_Naive'= '#193a1c',
            'CD4_Naive_SOX4'='#a4de02ff')

# subset to be plotted 
subset_to_be_plotted <-  c('CD4_Naive', 'CD4_Naive_SOX4')



