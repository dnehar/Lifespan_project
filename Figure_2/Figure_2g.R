library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('CD14_mo_ISGhi'= '#f15d64',
          'CD14_mo'= '#f6a2a7',
          'CD16_mo'= '#f9d3d7')

# subset to be plotted 
subset_to_be_plotted <-  c('CD14_mo', 'CD14_mo_ISGhi', 'CD16_mo')

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
        legend.position = "none",
        plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
  ggtitle('Age groups')

BP_gp

