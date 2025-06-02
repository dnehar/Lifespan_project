library(dplyr)
library(ggplot2)
library(ggstream)



# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


#color 
cols <- c('naive_Tregs'= '#137d82',
            'mem_Tregs'= '#56bbbf')
            

age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

# subset to be plotted 

subset_to_be_plotted <- c("naive_Tregs", "mem_Tregs")

BP <- LifeSpan_ALL_MetaData %>% 
  mutate(ReCluster = factor(Final_annotations)) %>% #*****
  group_by(Groups,  Names, ReCluster) %>%
  summarise(n = n(), Age_months = first(Age_months), Gender = first(Sex)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  filter(ReCluster %in% subset_to_be_plotted ) %>% 
  ggplot(aes(x = Names, y = freq, fill = ReCluster, group=ReCluster)) +
  scale_fill_manual(values=cols) + #**
  
  scale_x_discrete(limits=ordered_names) + #labels= labels

  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16, angle = 90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        legend.position = "none") + #    ylab('% PBMC') + xlab('Age groups')
  
  ylab('% of lineage') + xlab('Individuals (n=95)')

# gg stream 
BP_Tregs <- BP +   ggstream::geom_stream(color = 'black', 
                                         lwd = 0.25,
                                         bw = 1,   
                                         type = "proportional")
print(BP_Tregs)


