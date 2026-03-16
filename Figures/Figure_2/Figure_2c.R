library(dplyr); library(ggplot2); 

cols <- c(  "moDC" = "#ed2024",
            "cDC1" = "#771215",
            "cDC2" = "#d84598",
            "AXL_DC" = "#a41e21",
            "pDC" = "#a5a4a4")

# load metadata and sample informatiom
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

subset_to_be_plotted <- c('moDC', 'cDC1', 'cDC2', 'AXL_DC', 'pDC')

age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')
my_comparisons <- list (c('Infants', 'Child'),
                        c('Child','Adolescent'),
                        c('Adolescent', 'Young'),
                        c('Young', 'Middle_aged'),
                        c('Middle_aged', 'Older'),
                        c('Older', 'Oldest_old'))



BP_1 <- LifeSpan_ALL_MetaData %>% 
  
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L4, levels = subset_to_be_plotted)) %>% #***
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  mutate(ReCluster = factor(LS_L4, levels = subset_to_be_plotted)) %>% #***
  group_by(Groups, ReCluster) %>%
  summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  
  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = ReCluster)) +
  geom_bar(stat = "identity", color = "black") +
  
  scale_fill_manual(values=cols) + #***
  scale_x_discrete(limits=age_groups) + #labels= labels
  theme(axis.text.y=element_text(size=18, colour = 'black'), 
        axis.text.x=element_text(size=18, colour = 'black'), 
        axis.title.x = element_text(face="bold", size=20, colour = 'black'),
        axis.title.y = element_text(face="bold", size=20, colour = 'black'),
        plot.title = element_text(hjust = 0.5,face='bold',size=16, colour = 'black'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        panel.border = element_rect(fill=NA, color = 'black', size=1), 
        legend.position = "none") + #    ylab('% PBMC') + xlab('Age groups')
  
  ylab('% of Lineage') + xlab('')

BP_1 

ggsave("./Figure_2026/Barplot_DCs_age_groups_01062026.pdf", BP_1,
       width=1.7, height=1.8,  units="in", scale=3)
