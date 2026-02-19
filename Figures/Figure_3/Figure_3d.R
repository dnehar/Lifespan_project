library(dplyr)
library(ggplot2)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

#color 
cols <- c('NK_CD16'= '#fee000',
            'NK_XCL1'= '#f2e4a0',
            'NK_cycling'= '#ccb72d',
            'NK_CD16_KLRC2'='#feb24c')

# subset to be plotted 
subset_to_be_plotted <-  c('NK_CD16', 'NK_XCL1','NK_CD16_KLRC2', 'NK_cycling')


plt_cor1 <- LifeSpan_ALL_MetaData %>%
  
  mutate(Groups = factor(Groups, levels = c("HI", "HC", "HY", "HO"))) %>%
  mutate(ReCluster = factor(Final_annotations, levels = ordered_SC)) %>% #*****
  mutate(Age_days = Age_months*30) %>% 
  group_by(Groups, Names,Age_months,Age_days, ReCluster) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  
  # subsets to be plotted   
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  # infants only 
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
  ylab('% of NK cells') + xlab('Age (months)')
plt_cor1

