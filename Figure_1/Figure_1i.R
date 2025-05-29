library(dplyr)
library(ggplot2)
  

# load sample informatiom
pheno <- read.csv('../LS95_sample_info_03262024.csv')
head(pheno)

ordered_names <- unique(pheno$Names)

# load metadata

