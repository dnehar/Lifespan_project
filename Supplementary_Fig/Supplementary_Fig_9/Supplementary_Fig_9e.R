


### CD4+ T cells
subset_to_be_plotted <- c('CD4_naive','CD4_ISGhi', 'CD4_Tregs','CD4_memory','CD4_Proliferating')

box_plot_lineage_CD4 <- LifeSpan_ALL_MetaData %>% 
  mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% #head()
  group_by(Groups, sample_id,  LS_L4) %>% 
  summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  #filter(LS_L4 %in% c('CD8_naive_SOX4-', 'CD8_naive_SOX4+')) %>% 
  filter(LS_L4 %in% c('CD4_naive_SOX4-', 'CD4_naive_SOX4+')) %>% 
  #filter(Groups %in% c('HI')) %>% 
  ggplot(aes(x = Groups, y = freq, fill = LS_L4, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.2) +
  theme_bw()  +  #THEME +
  #ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  #ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.signif") +
  ggpubr::stat_compare_means(comparisons = my_comparisons,  label = "p.format", hide.ns = T, vjust = 0.5) + 
  theme(legend.position = "none", 
        strip.text = element_text(size = 10, face='bold')) +
  facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) + 
  
  scale_fill_manual(values=cols) + #**
  theme(axis.text.y=element_text(size=12, colour = 'black'), 
        axis.text.x=element_text(size=12, colour = 'black',angle = 90),
        axis.title.x = element_text(face="bold", size=14, colour = 'black'),
        axis.title.y = element_text(face="bold", size=14, colour = 'black'), 
        strip.text.x = element_text(size = 14, face ='bold', colour = 'black')) + #    ylab('% PBMC') + xlab('Age groups')
  ylab('% of lineage') + xlab('Age groups')

box_plot_lineage_CD4


### CD8+ T cells 
subset_to_be_plotted <- c('CD8_naive', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT','CD8_TEMRA', 
                          'CD8_gdT', 'CD8aa')


box_plot_lineage_CD8 <- LifeSpan_ALL_MetaData %>% 
  mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% #head()
  group_by(Groups, sample_id,  LS_L4) %>% 
  summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(LS_L4 %in% c('CD8_naive_SOX4-', 'CD8_naive_SOX4+')) %>% 
  #filter(Groups %in% c('HI')) %>% 
  ggplot(aes(x = Groups, y = freq, fill = LS_L4, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.2) +
  theme_bw()  +  #THEME +
  #ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  #ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.signif") +
  ggpubr::stat_compare_means(comparisons = my_comparisons,  label = "p.format", hide.ns = T, vjust = 0.5) + 
  theme(legend.position = "none", 
        strip.text = element_text(size = 10, face='bold')) +
  facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) + 
  
  scale_fill_manual(values=cols) + #**
  theme(axis.text.y=element_text(size=12, colour = 'black'), 
        axis.text.x=element_text(size=12, colour = 'black',angle = 90),
        axis.title.x = element_text(face="bold", size=14, colour = 'black'),
        axis.title.y = element_text(face="bold", size=14, colour = 'black'), 
        strip.text.x = element_text(size = 14, face ='bold', colour = 'black')) + #    ylab('% PBMC') + xlab('Age groups')
  ylab('% of lineage') + xlab('Age groups')

box_plot_lineage_CD8


