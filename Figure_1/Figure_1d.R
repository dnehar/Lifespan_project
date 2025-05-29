library(dplyr)
library(ggplot2)

# colors
col <- c("Monocytes"="#f6a2a7",
             "B_cells"="#96daf7",
             "DCs"= "#ed2024",
             "CD4_Tcells"="#193a1c",
             "CD8_Tcells"="#f37421",
             "NK_cells"="#fee000",
             "PCs"="#232323ff",
             "HSPC"= '#b0479a',
             'gd_Tcells'= '#80622f')

# order clusters
 order_Lineage <- c('Monocytes','DCs',   'NK_cells',
                       'B_cells', 'PCs', 'HSPC',
                       'CD4_Tcells', 'CD8_Tcells', 'gd_Tcells')

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# plot 
p_corr <- LifeSpan_ALL_MetaData %>%        
    mutate(ReCluster = factor(Lineage, levels = order_Lineage)) %>%
    mutate(Groups = factor(Groups, levels = age_groups)) %>%
    group_by(Groups, Names,Gender, Age_months, ReCluster) %>%
    #filter(Groups %in% c("HI")) %>% 
    summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
    #summarise(n = n()) %>% #, Set = first(Set)
    mutate(freq = n / sum(n) *100) %>%
    ungroup() %>%
    as.data.frame() %>%
    ggplot(aes(x = Age_months, y = freq, fill=ReCluster)) +
    geom_point(aes(shape = Groups, color=ReCluster)) +
    geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
    #geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color=ReCluster)) +
    scale_fill_manual(values=cols_Lineage) + 
    scale_color_manual(values = cols_Lineage)+
    ggpubr::stat_cor() +
    theme_bw() +
    theme(legend.position = "none", 
          strip.text = element_text(size = 11, face ='bold')) +
    facet_wrap(.~ReCluster, scales = "free_y", nrow = 2) +
    theme(axis.text.y=element_text(size=16), 
          axis.text.x=element_text(size=16),
          axis.title.x = element_text(face="bold", size=18),
          axis.title.y = element_text(face="bold", size=18), 
          strip.text.x = element_text(size = 11, face ='bold')) + #    ylab('% PBMC') + xlab('Age groups') #    ylab('% PBMC') + xlab('Age groups'
    #    ylab('% PBMC') + xlab('Age groups')
    ylab('% in PBMCs') + xlab('Age (months)') 
print(p_corr)
  
