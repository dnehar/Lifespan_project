library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


#color 
cols <- c('GD_T_SOX4'= '#56bbbf',           
          'CD8_GD'= '#80622f')
age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

# subset to be plotted 
subset_to_be_plotted <- c('GD_T_SOX4','CD8_GD')


  plt_age <- LifeSpan_ALL_MetaData %>% 
    mutate(ReCluster = factor(Final_annotations)) %>% #, levels = ordered_SC
    mutate(Groups = factor(Groups, levels = age_groups)) %>%
    group_by(Groups, Names, ReCluster) %>%
    summarise(n = n()) %>% #, Set = first(Set)
    mutate(freq = n / sum(n) *100) %>%
    ungroup() %>%
    as.data.frame() %>%
    
    filter(ReCluster %in% subset_to_be_plotted) %>% 
    ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size = 0.2) +
    theme_bw()  +  #THEME +
    ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") + #label = "p.signif"
    theme(legend.position = "none", 
          strip.text = element_text(size = 14, face='bold')) +
    facet_wrap(.~ReCluster, scales = "free_y", nrow = 1) + 
    
    scale_fill_manual(values=cols) + #**
    theme(axis.text.y=element_text(size=12, colour = 'black'), 
          axis.text.x=element_text(size=12, colour = 'black'),
          axis.title.x = element_text(face="bold", size=14, colour = 'black'),
          axis.title.y = element_text(face="bold", size=14, colour = 'black'), 
          strip.text.x = element_text(size = 14, face ='bold', colour = 'black')) + #    ylab('% PBMC') + xlab('Age groups')
    ylab('% in PBMCs') + xlab('Age groups')
  
  plt_age
  
