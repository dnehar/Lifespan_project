library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['pheno']] %>% as.data.frame()
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('cDC2'= '#d84598',
          'cDC1'= '#771215',
          'AXL_DC'= '#a41e21',
          'moDC'= '#ed2024',
          'pDC'= '#a5a4a4')


age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

# subset to be plotted 
subset_to_be_plotted <-  c('moDC','cDC1','cDC2', 'AXL_DC', 'pDC')
