library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['pheno']] %>% as.data.frame()
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


#color 
cols <- c("CD8_T_Naive"="#f37421",
            'gd_Tcells'= '#80622f',
            'CD8_Naive'= '#f37421',
            'CD8_TEMRA'= '#d28529',
            'CD8_GZMK'= '#fba919',
            'CD8_MAIT'= '#fbb36a',
            'CD8_cycling'='#7f7f7f',
            'CD8_GD'= '#80622f',
            'CD8_Naive_SOX4'= '#ffdeadff')
age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

# subset to be plotted 
subset_to_be_plotted <- c('CD8_Naive','CD8_Naive_SOX4',"CD8_TEMRA","CD8_GZMK","CD8_MAIT","CD8_cycling","CD8_GD")


plt_cor1 <- LifeSpan_ALL_MetaData %>%
  
  mutate(Groups = factor(Groups, levels = c("HI", "HC", "HY", "HO"))) %>%
  mutate(ReCluster = factor(Final_annotations, levels = subset_to_be_plotted)) %>% #*****
  mutate(Age_days = Age_months*30) %>% 
  group_by(Groups, Names,Age_months,Age_days, ReCluster) %>%
  mutate(ReCluster = gsub(pattern = "CD8_Naive_SOX4", replacement = "CD8_Naive", x = ReCluster)) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(ReCluster = factor(ReCluster, levels = subset_to_be_plotted)) %>% #*****

  ggplot(aes(x = Age_months, y = freq, fill=ReCluster)) +
  geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
  geom_point(aes(shape = Groups, color=ReCluster)) +
  scale_fill_manual(values=cols) + 
  scale_color_manual(values = cols)+
  ggpubr::stat_cor() +
  theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 14)) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 1) +
  theme(axis.text.y=element_text(size=12, colour = 'black'), 
        axis.text.x=element_text(size=12, colour = 'black'),
        axis.title.x = element_text(face="bold", size=14, colour = 'black'),
        axis.title.y = element_text(face="bold", size=14, colour = 'black'), 
        strip.text.x = element_text(size = 14, face ='bold', colour = 'black')) +#    ylab('% PBMC') + xlab('Age groups') #    ylab('% PBMC') + xlab('Age groups'
  ylab('% of CD8 T cells') + xlab('Age (months)')
plt_cor1


