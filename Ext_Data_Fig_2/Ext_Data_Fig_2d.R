
library(dplyr)
library(ggplot2)

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
pheno <- MetaData[['pheno']] %>% as.data.frame()
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


meta <-  data.frame(dplyr::select(LifeSpan_ALL_MetaData, pbmc_simple_clustering, Groups, Names, Age_months))
ordered_ids <- as.character(pheno$Names)

# print bar plot
BP <- meta %>% 
  mutate(Groups = factor(Groups, levels = c("HI", "HC", "HY", "HO"))) %>%
  mutate(ReCluster = factor(pbmc_simple_clustering)) %>%
  group_by(Groups, Names, ReCluster) %>%
  summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(x = Names, y = freq, fill = ReCluster, group = Groups)) +
  geom_bar(stat = "identity") + #, color = "black"
  scale_fill_manual(values=cols) + #***
  scale_x_discrete(limits=ordered_ids) + #labels= labels
  theme(axis.text.y=element_text(size=18), 
        axis.text.x=element_blank(),#, angle = 90
        axis.title.x = element_text(size=18),
        axis.ticks.x=element_blank(),
        #axis.ticks.y=element_blank(),
        axis.title.y = element_text(size=18),
        #panel.border = element_rect(fill=NA, color = 'black', size=1),
        legend.direction = "horizontal", 
        legend.position = 'bottom',
        legend.text.align = 1,
        plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
  ylab('% in PBMC') + 
  xlab('Individuals (n=95; ordered based on their age)') + 
  
  ggtitle('Individuals')

print(BP) 


