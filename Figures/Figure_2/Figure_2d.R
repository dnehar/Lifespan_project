library(dplyr); library(ggplot2); 

cols <- c(  "moDC" = "#ed2024",
            "cDC1" = "#771215",
            "cDC2" = "#d84598",
            "AXL_DC" = "#a41e21",
            "pDC" = "#a5a4a4")

# load metadata and sample informatiom
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')
my_comparisons <- list (c('Infants', 'Child'),
                        c('Child','Adolescent'),
                        c('Adolescent', 'Young'),
                        c('Young', 'Middle_aged'),
                        c('Middle_aged', 'Older'),
                        c('Older', 'Oldest_old'))

box_plot_lineage <- LifeSpan_ALL_MetaData %>% 
  mutate(ReCluster = factor(LS_L4, levels = order_LS_L4)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  group_by(Groups, sample_id, ReCluster) %>%
  summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.2) +
  theme_bw()  +  #THEME +
  ggpubr::stat_compare_means(comparisons = my_comparisons,  label = "p.format", hide.ns = T, vjust = 0.5) + 
  theme(legend.position = "none", 
        strip.text = element_text(size = 10, face='bold')) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 1) + 
  
  scale_fill_manual(values=cols) + #**
  theme(axis.text.y=element_text(size=12, colour = 'black'), 
        axis.text.x=element_text(size=12, colour = 'black',angle = 90),
        axis.title.x = element_text(face="bold", size=14, colour = 'black'),
        axis.title.y = element_text(face="bold", size=14, colour = 'black'), 
        strip.text.x = element_text(size = 14, face ='bold', colour = 'black')) + #    ylab('% PBMC') + xlab('Age groups')
  ylab('% of lineage') + xlab('Age groups')

box_plot_lineage

# --- Save figure as PDF ---
# Note: corrected plot object name from box_plot_pbmc_L2 to box_plot_pbmc_L4
ggsave("./Boxplot_DCs_in_lineage_03132026.pdf", box_plot_lineage,
       width = 4.2, height = 3, units = "in", scale = 3)
