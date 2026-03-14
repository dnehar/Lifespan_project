library(dplyr); library(ggplot2); 

cols <- c(
  # Level 2
  "B_naive" = "#1c9099",
  "CD4_ISGhi" = "#697d35",
  "CD4_memory" = "#90aa3c",
  "CD4_naive" = "#193a1c",
  "CD8_TEMRA" = "#d28529",
  "CD8_naive" = "#f37421",
  "CD14_mono" = "#f6a2a7",
  "CD56bright_NK" = "#f2e4a0",
  "CD56dim_NK" = "#fee000"
)

# load metadata  
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

subset_to_be_plotted <- c('CD14_mono','CD56dim_NK','CD4_memory','CD8_TEMRA',
                          'CD4_naive', 'CD8_naive', 'B_naive', 'CD4_ISGhi' )

box_plot_pbmc_L3 <- LifeSpan_ALL_MetaData %>% 
  
  mutate(ReCluster = factor(LS_L3, levels = subset_to_be_plotted)) %>% #***
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  group_by(Groups, sample_id, ReCluster) %>%
  summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  filter(ReCluster %in% subset_to_be_plotted) %>%  
  mutate(ReCluster = factor(ReCluster, levels = subset_to_be_plotted)) %>% #order_LS_L3
  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.2) +
  theme_bw()  +  #THEME +
  #ggpubr::stat_compare_means(ref.group = 'Infants', label = "p.signif", method = "t.test",  hide.ns = T, vjust = 0.5) + # label = "p.format",
  ggpubr::stat_compare_means(comparisons = my_comparisons,  label = "p.format", hide.ns = F, vjust = 0.5) + 
  #ggpubr::stat_compare_means(method = "kruskal.test", label = "p.format") +
  theme(legend.position = "none", 
        strip.text = element_text(size = 13, face ='bold')) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 2) + 
  
  scale_fill_manual(values=cols) + #**
  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16, angle =90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18)) + #    ylab('% PBMC') + xlab('Age groups')
  ylab('% PBMC') + xlab('Age groups')

box_plot_pbmc_L3


ggsave("./boxplot_Fig1f_01022026.pdf", box_plot_pbmc_L3,
       width=3.4, height=2.4,  units="in", scale=3)


