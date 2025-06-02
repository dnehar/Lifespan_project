
library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
pheno <- MetaData[['pheno']] %>% as.data.frame()
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 

 cols <- c('CD4_Naive'= '#193a1c',
            'CD4_Naive_SOX4'='#a4de02ff')

# subset to be plotted 
subset_to_be_plotted <-  c('CD4_Naive', 'CD4_Naive_SOX4')

ordered_ids <- as.character(pheno$Names)

# print bar plot
BP2 <- LifeSpan_ALL_MetaData %>% 
  mutate(Groups = factor(Groups, levels = c("HI", "HC", "HY", "HO"))) %>%
  mutate(ReCluster = factor(Final_annotations)) %>%
  group_by(Groups, Names, ReCluster) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(x = Names, y = freq, fill = ReCluster, group = Groups)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=cols) + #***
  scale_x_discrete(limits=ordered_ids) + #labels= labels
  theme(axis.text.y=element_text(size=18),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        #axis.text.x=element_text(size=16, angle = 90),
        axis.title.x = element_text(size=18),
        axis.ticks.y=element_blank(),
        axis.title.y = element_text( size=18), #face="bold",
        #panel.border = element_rect(fill=NA, color = 'black', size=1),
        legend.direction = "horizontal", 
        legend.position = 'bottom',
        legend.text.align = 1, 
        plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
  ylab('% in naive CD4 T cells') + 
  xlab('Individuals (n=95; ordered based on their age)') +
  ggtitle('Individuals')

print(BP2)

