library(dplyr)
library(ggplot2)
library(ggstream)


# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()
pheno <- MetaData[['pheno']] %>% as.data.frame()


#color 
cols <- c(
  "CD8_naive_SOX4+" = "#ffdeadff",  # pale yellow
  "CD8_naive_SOX4-" = "#f37421",    # orange
  "CD4_naive_SOX4-" = "#193a1c",    # dark green
  "CD4_naive_SOX4+" = "#a4de02ff"   # lime green
)


age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

ordered_names <- unique(pheno$sample_id)


# subset to be plotted 
subset_to_be_plotted <- c( "Tregs_naive", "Tregs_mem")

ordered_names <- unique(pheno$sample_id)



ordered_names <- unique(sample_info$sample_id)

# ============================================================================
# PART 1: CD4+ T CELL ANALYSIS
# ============================================================================

subset_to_be_plotted <- c('CD4_naive_SOX4+', 'CD4_naive_SOX4-')

BP <- LifeSpan_ALL_MetaData %>% 
  
  #mutate(ReCluster = factor(pbmc_simple_clustering, levels = order_pbmc_simple_clustering)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L4, levels=subset_to_be_plotted)) %>% #*****
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  
  #filter(Groups %in% c("HO_M",'HO_F')) %>% 
  summarise(n = n(), Age_months = first(Age_in_yrs)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  ggplot(aes(x = sample_id, y = freq, fill = ReCluster, group=ReCluster)) +
  #scale_fill_manual(values=col) + #***
  scale_fill_manual(values=cols) + #**
  #scale_fill_manual(values=cols_Lineage) + #***
  scale_x_discrete(limits=ordered_names) + #labels= labels
  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16, angle = 90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        legend.position = "none") + #    ylab('% PBMC') + xlab('Age groups')
  
  ylab('% of naive CD4 T cells') + xlab('Individuals (n=167)')

SOX4_CD4 <-  BP +   ggstream::geom_stream(color = 'black', 
                                         lwd = 0.25,
                                         bw = 1,   
                                         type = "proportional")
print(SOX4_CD4)

# ============================================================================
# PART 2: CD8+ T CELL ANALYSIS
# ============================================================================

subset_to_be_plotted <- c('CD8_naive_SOX4+', 'CD8_naive_SOX4-')

BP2 <- LifeSpan_ALL_MetaData %>% 
  
  #mutate(ReCluster = factor(pbmc_simple_clustering, levels = order_pbmc_simple_clustering)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L4, levels=subset_to_be_plotted)) %>% #*****
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  
  #filter(Groups %in% c("HO_M",'HO_F')) %>% 
  summarise(n = n(), Age_months = first(Age_in_yrs)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  ggplot(aes(x = sample_id, y = freq, fill = ReCluster, group=ReCluster)) +
  #scale_fill_manual(values=col) + #***
  scale_fill_manual(values=cols) + #**
  #scale_fill_manual(values=cols_Lineage) + #***
  scale_x_discrete(limits=ordered_names) + #labels= labels
  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16, angle = 90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        legend.position = "none") + #    ylab('% PBMC') + xlab('Age groups')
  
  ylab('% of naive CD8 T cells') + xlab('Individuals (n=167)')

SOX4_CD8 <-  BP2 +   ggstream::geom_stream(color = 'black', 
                                          lwd = 0.25,
                                          bw = 1,   
                                          type = "proportional")
print(SOX4_CD8)

print(SOX4_CD4 / SOX4_CD8)

