library(dplyr)
library(ggplot2)

# colors 
  cols_L2 <-  c('B_naive' = '#41b8ea',
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

# load meta data   
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


# level 2 (n=18 clusters)
  order_pbmc_simple_clustering <- c('CD14_mo', 'CD16_mo', 'DCs', 'pDC', 'Mgk','HSPC',
                                    'NK_CD16', 'NK_XCL1', 'gd_Tcells',
                                    'B_naive', 'B_memory','PCs', 
                                    'CD4_T_Naive','CD4_T_ISGhi', 'CD4_T_Memory', 'CD4_Tregs', 
                                    'CD8_T_Naive', 'CD8_T_Effector')
    
# age groups 
 age_groups <- c("HI", "HC", "HY", "HO")

  my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)
  
  # Subsets to plot 
  subset_to_plot <- c("CD14_mo",'CD8_T_Effector',"CD4_T_Memory",'NK_CD16',"CD4_T_Naive",'CD8_T_Naive','B_naive','CD4_T_ISGhi')

  plt_age1 <- LifeSpan_ALL_MetaData %>% 
    
    mutate(ReCluster = factor(pbmc_simple_clustering, levels = order_pbmc_simple_clustering)) %>%
    mutate(Groups = factor(Groups, levels = age_groups)) %>%
    group_by(Groups, Names, ReCluster) %>%
    summarise(n = n()) %>% #, Set = first(Set)
    mutate(freq = n / sum(n) *100) %>%
    ungroup() %>%
    as.data.frame() %>%
    filter(ReCluster %in% subset_to_plot) %>% 
    mutate(ReCluster = factor(ReCluster, levels = subset_to_plot)) %>%
    ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size = 0.2) +
    theme_bw()  +  #THEME +
    #ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") +
    ggpubr::stat_compare_means(comparisons = my_comparisons,  label = "p.signif", hide.ns = F, vjust = 0.5) + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 13)) +
    facet_wrap(.~ReCluster, scales = "free_y", nrow = 2) + 
    
    scale_fill_manual(values=cols_L2) + #**
    theme(axis.text.y=element_text(size=16), 
          axis.text.x=element_text(size=16, angle =90),
          axis.title.x = element_text(face="bold", size=18),
          axis.title.y = element_text(face="bold", size=18)) + #    ylab('% PBMC') + xlab('Age groups')
    ylab('% PBMC') + xlab('Age groups')
  
  print(plt_age1)
