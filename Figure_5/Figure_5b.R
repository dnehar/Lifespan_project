library(dplyr)
library(ggplot2)

#color 
cols <- c("CD4_T_Naive"="#193a1c",
            "CD4_T_ISGhi"="#697d35",
            "CD4_Tregs"="#137d82",
            "CD4_T_Memory"="#1c572b",
            'CD4_ISGhi'= '#697d35',
            'CD4_T_ISGhi'='#697d35',
            #CD4_CTL'= '#90aa3c',
            'CD4_Naive'= '#193a1c')
            

# plot umap CD4 T cells

p_umap_subset <- LifeSpan_ALL_MetaData %>% 
dplyr::filter(Lineage %in% c('CD4_Tcells')) %>% 
mutate(subset_simple_clustering = gsub(pattern = "CD4_CTL", replacement = "CD4_T_Memory", x = subset_simple_clustering)) %>%
ggplot(aes(x=SC_umap1, y=SC_umap2,  color=subset_simple_clustering)) +
geom_point(size=0.05) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)

age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

# subset to be plotted 
subset_to_be_plotted <- c("B_naive","B_transitional","B_memory","B_ABC","B_ISGhi","PCs")

# plot box plot - age groups 

  plt_age <- LifeSpan_ALL_MetaData %>% 
            mutate(ReCluster = factor(subset_simple_clustering)) %>% #, levels = ordered_SC
            mutate(subset_simple_clustering = gsub(pattern = "CD4_CTL", replacement = "CD4_T_Memory", x = subset_simple_clustering)) %>%
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
    #ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") +
    ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") + #label = "p.signif"
    #ggpubr::stat_compare_means(comparisons = my_comparisons, label = "p.signif", hide.ns = F, vjust = 0.5) + 
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
