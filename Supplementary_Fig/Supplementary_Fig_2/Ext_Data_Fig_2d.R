library(dplyr)
library(ggplot2)

# colors 
  cols_L2 = c('B_naive' = '#41b8ea',
    'B_memory' = '#283779',
    'CD4_T_ISGhi' = '#697d35',
    'CD4_T_Memory' = '#1c572b',
    'CD4_T_Naive' = '#193a1c',
    'CD4_Tregs'='#137d82',
    'CD8_T_Effector'='#fba919',
    'CD8_T_Naive'='#f37421', 
    'CD14_mo'='#f6a2a7', 
    'CD16_mo'='#f9d3d7',
    'DCs'='#ed2024', 
    'HSPC'='#b0479a',
    'Mgk'='#932169', 
    'NK_CD16'='#fee000', 
    'NK_XCL1'='#f2e4a0', 
    'PCs'='#232323ff', 
    'gd_Tcells'='#80622f', 
    'pDC'='#a5a4a4')
   
# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['meta_small']] %>% as.data.frame()

meta <-  data.frame(dplyr::select(MetaData, pbmc_simple_clustering, Groups, Names, Age_months))

# plot cell frequencies (Level 2: 18 clusters) vs. age 
p_corr <- meta %>%
  mutate(ReCluster = factor(pbmc_simple_clustering)) %>%
  mutate(Groups = factor(Groups, levels = c('HI','HC','HY','HO'))) %>%
  group_by(Groups, Names, Age_months, ReCluster) %>%
  
  #filter(Groups %in% c("HI")) %>% 
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  #summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  
  filter(Groups %in% c('HI','HC','HY','HO')) %>% 
  
  ggplot(aes(x = Age_months, y = freq, fill=ReCluster)) +
  geom_point(aes(shape = Groups, color=ReCluster)) +
  geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color=ReCluster)) +
  scale_fill_manual(values=cols_L2) + 
  scale_color_manual(values = cols_L2)+
  ggpubr::stat_cor() +
  theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 11, face ='bold')) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 3) +
  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18), 
        strip.text.x = element_text(size = 11, face ='bold')) + #    ylab('% PBMC') + xlab('Age groups') #    ylab('% PBMC') + xlab('Age groups'
  ylab('% in PBMCs') + xlab('Age (months)')
print(p_corr)
