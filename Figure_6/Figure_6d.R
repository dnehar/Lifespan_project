library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
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

# subset to be plotted 
subset_to_be_plotted <- c('CD8_T_Naive','CD8_Naive_SOX4',"CD8_TEMRA","CD8_GZMK","CD8_MAIT","CD8_cycling","CD8_GD")

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
