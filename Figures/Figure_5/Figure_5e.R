library(dplyr)
library(ggplot2)
library(ggstream)



# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()
pheno <- MetaData[['pheno']] %>% as.data.frame()


#color 
cols <- c("Tregs_naive" = "#137d82",
          "Tregs_mem" = "#56bbbf")
            

age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

ordered_names <- unique(pheno$sample_id)


# subset to be plotted 
subset_to_be_plotted <- c( "Tregs_naive", "Tregs_mem")

ordered_names <- unique(pheno$sample_id)

BP <- LifeSpan_ALL_MetaData %>% 
  
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L4)) %>% #*****
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  summarise(n = n(), Age_months = first(Age_in_yrs)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  ggplot(aes(x = sample_id, y = freq, fill = ReCluster, group=ReCluster)) +
  scale_fill_manual(values=cols) + #**
  scale_x_discrete(limits=ordered_names) + #labels= labels
  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16, angle = 90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        legend.position = "none") + #    ylab('% PBMC') + xlab('Age groups')
  
  ylab('% of Lineage') + xlab('Individuals (n=167)')


# gg stream 
BP_Tregs <- BP +   ggstream::geom_stream(color = 'black', 
                                         lwd = 0.25,
                                         bw = 1,   
                                         type = "proportional")
print(BP_Tregs)


