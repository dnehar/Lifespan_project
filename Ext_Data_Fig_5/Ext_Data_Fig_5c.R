
#color 
cols <- c("CD4_T_Naive"="#193a1c",
            "CD4_T_ISGhi"="#697d35",
            "CD4_Tregs"="#137d82",
            "CD4_T_Memory"="#1c572b")
            

age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

# subset to be plotted 
subset_to_be_plotted <- c("CD4_T_Memory","CD4_Tregs","CD4_T_Naive","CD4_T_ISGhi" ) #"CD4_CTL",

# Scatter plot - age groups 
plt_cor1<- LifeSpan_ALL_MetaData %>% 
            mutate(ReCluster = factor(subset_simple_clustering)) %>% #
            mutate(subset_simple_clustering = gsub(pattern = "CD4_CTL", replacement = "CD4_T_Memory", x = subset_simple_clustering)) %>%
            mutate(Groups = factor(Groups, levels = age_groups)) %>%
            group_by(Groups, Names,Age_months, ReCluster) %>%
            summarise(n = n()) %>% #, Set = first(Set)
            mutate(freq = n / sum(n) *100) %>%
            ungroup() %>%
            as.data.frame() %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  mutate(ReCluster = factor(ReCluster, levels = subset_to_be_plotted)) %>%
  filter(Groups %in% c('HI')) %>% 

  ggplot(aes(x = Age_months, y = freq, fill=ReCluster)) +
  geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
  geom_point(aes(shape = Groups, color=ReCluster)) +
  scale_fill_manual(values=cols) + 
  scale_color_manual(values = cols)+
  ggpubr::stat_cor() +
  theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 14)) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 1) +
  theme(axis.text.y=element_text(size=12, colour = 'black'), 
        axis.text.x=element_text(size=12, colour = 'black'),
        axis.title.x = element_text(face="bold", size=14, colour = 'black'),
        axis.title.y = element_text(face="bold", size=14, colour = 'black'), 
        strip.text.x = element_text(size = 14, face ='bold', colour = 'black')) +#    ylab('% PBMC') + xlab('Age groups') #    ylab('% PBMC') + xlab('Age groups'
  ylab('% PBMCs') + xlab('Age (months)')
plt_cor1
