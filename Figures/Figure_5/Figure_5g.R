library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
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
            
# subset to be plotted 
subset_to_be_plotted <- c("Tmem_CM", "Tmem_CM_SOX4","Tmem_CM_TOX", "Tmem_GzK_TH1_like","Tmem_TEMRA", "Tmem_TH2","Tmem_TH17","Tmem_HLA_DR","Tmem_ISGhi")

BP_gp <- LifeSpan_ALL_MetaData %>% 
  mutate(Groups = factor(Groups, levels = c("HI", "HC", "HY", "HO"))) %>%
  mutate(ReCluster = factor(Final_annotations)) %>%
  group_by(Groups, ReCluster) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  
  summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  
  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = ReCluster)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values=cols) + #***
  scale_x_discrete(limits=c("HI", "HC", "HY", "HO")) + #labels= labels
  theme(axis.text.y=element_text(size=18), 
        axis.text.x=element_text(size=18, angle = 0),
        axis.title.x = element_text(size=0),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size=0),
        #legend.position = "none",
        plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
  ggtitle('Age groups')

BP_gp

