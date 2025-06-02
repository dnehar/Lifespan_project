library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['pheno']] %>% as.data.frame()
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()



#color 
cols <- c('Tmem_TH2'= '#1c7b3d',
            'Tmem_TH17'= '#3cb54a',
            'Tmem_CM'= '#74c168',
            'Tmem_HLA_DR'= '#a4de02ff',
            'Tmem_ISGhi'= '#697d35',
            'Tmem_CM_TOX'= '#edf8b1',
            'Tmem_GzK_TH1_like'='#a8ddb5',
            'Tmem_TEMRA'='#1c572b',
            'Tmem_CM_SOX4'= '#7fcdbb')
            

age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

# subset to be plotted 
subset_to_be_plotted <- c("Tmem_CM", "Tmem_CM_SOX4","Tmem_CM_TOX", "Tmem_GzK_TH1_like","Tmem_TEMRA", "Tmem_TH2","Tmem_TH17","Tmem_HLA_DR","Tmem_ISGhi")


plt_cor1 <- LifeSpan_ALL_MetaData %>%
  
  mutate(Groups = factor(Groups, levels = c("HI", "HC", "HY", "HO"))) %>%
  mutate(ReCluster = factor(Final_annotations, levels = ordered_SC)) %>% #*****
  mutate(Age_days = Age_months*30) %>% 
  group_by(Groups, Names,Age_months,Age_days, ReCluster) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>%   
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
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
  ylab('% PBMCs') + xlab('Age (months)')
plt_cor1

