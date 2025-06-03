library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['meta_small']] %>% as.data.frame()

# colors
my_col <- c('F'= '#a8ddb5', M='#feb24c')

# plot pie chart 
pPC <- pheno %>% group_by(Groups, Sex) %>% summarise(n = n()) %>%   mutate(freq = n / sum(n) *100) %>%  
  mutate(Groups = factor(Groups, levels = age_groups)) %>%
  ggplot(aes(x = '', y = freq, fill = Sex, group = Sex)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  facet_wrap(.~Groups, scales = "free_y", nrow = 2) + 
  scale_fill_manual(values=my_col) + #***
  #scale_x_discrete(limits=c("HY_F","HO_F",'HY_M','HO_M')) + #labels= labels
  theme_void()  # remove background, grid, numeric labels

print(pPC)
