library(dplyr); library(ggplot2); 

# load metadata  
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


proportions <- LifeSpan_ALL_MetaData %>%
  #filter(Groups %in% c('HO','HY','HC')) %>%  #& Lineage %in% c('Monocytes','DCs')) %>%
  #filter(!sample_id %in% c('sample6','sample12','sample13',
  #                        'sample14','sample15','sample47','donor9')) %>% #
  mutate(ReCluster = factor(LS_L2)) %>%
  
  #group_by(Names, Simple_Clustering) %>%
  group_by(sample_id, ReCluster) %>%
  
  summarise(n = n(), Groups = first(Age_groups), Age_months = first(Age_in_yrs),  Lineage = first(lifespan_L1)) %>%
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame()


t.proportions <- proportions %>%
  reshape2::dcast(sample_id + Groups + Age_months ~ ReCluster, value.var = "freq", fill = 0)
head(t.proportions)

pca <- prcomp(t.proportions[,4:ncol(t.proportions)], center = T) #***

d  <- round(pca$sdev^2/sum(pca$sdev^2)*100, digits=1)
xl <- sprintf("PC 1: %.1f %%", d[1])
yl <- sprintf("PC 2: %.1f %%", d[2])
zl <- sprintf("PC 3: %.1f %%", d[3])


# ├ ├Plot PC contributuons ####
contrib <- as.data.frame(pca$rotation)
contrib$celltypes <- rownames(contrib)

# PC1 
PC1 <- ggplot(contrib, aes(x =PC1, y = reorder(celltypes, PC1))) + #, group = Groups, color = Groups
  geom_point(colour='#9e9ac8', size=4) + 
  #geom_line(color="black") + 
  theme_bw() +
  geom_vline(xintercept = 0,linetype="dotted", size=1) +
  theme(axis.text.y=element_text(size=18), 
        axis.text.x=element_text(size=18),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20)) +
  ylab('Immune subsets') +
  xlab('PC1')
# ggtitle("PBMC state clusters contributing to PCs")  #+
PC1

ggsave("./LS_pca_contrib_PC1_LS_L2_123162025.pdf", PC1,
       width=2, height=3,  units="in", scale=3)

