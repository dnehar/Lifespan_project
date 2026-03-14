library(dplyr); library(ggplot2); 


# load metadata and sample informatiom
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

pheno <- MetaData[['pheno']] %>% as.data.frame()
ordered_names <- unique(pheno$sample_id)

length(unique(LifeSpan_ALL_MetaData$lifespan_L2))
age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

order_pbmc_simple_clustering <- c('CD14_mono', 'CD16_mono', 'DCs', 'pDCs', 'Mgk','HSPC',
                                  'CD56bright_NK', 'CD56dim_NK', 'gd_Tcells',
                                  'B_naive', 'B_memory','PCs', 
                                  'CD4_naive','CD4_ISGhi', 'CD4_memory', 'CD4_Tregs', 
                                  'CD8_naive', 'CD8_memory')

length(order_pbmc_simple_clustering)

 cols <- c(
   # Level 2
   "B_naive" = "#1c9099",
   "B_memory" = "#283779",
   "CD4_naive" = "#193a1c",
   "CD4_memory" = "#90aa3c",
   "CD8_naive" = "#f37421",
   "CD8_memory" = "#fba919" )

my_comparisons <- list (c('Infants', 'Child'),
                        c('Child','Adolescent'),
                        c('Adolescent', 'Young'),
                        c('Young', 'Middle_aged'),
                        c('Middle_aged', 'Older'),
                        c('Older', 'Oldest_old'))

######################################################
# CD4 T cells 
######################################################

to_be_ploted <- c("CD4_memory", "CD4_naive")

BP <- LifeSpan_ALL_MetaData %>% 
  
  #mutate(ReCluster = factor(pbmc_simple_clustering, levels = order_pbmc_simple_clustering)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L2)) %>% #*****
  filter(ReCluster %in% to_be_ploted) %>% 
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
  
  ylab('% of PBMCs') + xlab('Individuals (n=167)')


CD4_T <-  BP +   ggstream::geom_stream(color = 'black', 
                                       lwd = 0.25,
                                       bw = 1,   
                                       type = "proportional")
print(CD4_T)

######################################################
# CD8 T cells 
######################################################

to_be_ploted <- c( 'CD8_T_Effector','CD8_T_Naive')

my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)


to_be_ploted <- c('CD8_naive', 'CD8_memory')

ordered_names <- unique(sample_info$sample_id)

BP <- LifeSpan_ALL_MetaData %>% 
  
  #mutate(ReCluster = factor(pbmc_simple_clustering, levels = order_pbmc_simple_clustering)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L2)) %>% #*****
  filter(ReCluster %in% to_be_ploted) %>% 
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
  
  ylab('% of PBMCs') + xlab('Individuals (n=167)')

BP

CD8_T <-  BP +   ggstream::geom_stream(color = 'black', 
                                       lwd = 0.25,
                                       bw = 1,   
                                       type = "proportional")
print(CD8_T)

######################################################
# B cells 
######################################################

to_be_ploted <- c('B_memory','B_naive')

my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

BP <- LifeSpan_ALL_MetaData %>% 
  
  #mutate(ReCluster = factor(pbmc_simple_clustering, levels = order_pbmc_simple_clustering)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L2)) %>% #*****
  filter(ReCluster %in% to_be_ploted) %>% 
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
  
  ylab('% of PBMCs') + xlab('Individuals (n=167)')


Bcells <-  BP +   ggstream::geom_stream(color = 'black', 
                                        lwd = 0.25,
                                        bw = 1,   
                                        type = "proportional")
print(Bcells) 

####
BPs <- CD4_T | CD8_T |  Bcells 
print(BPs)

ggsave("./Streamed_LS_L2_clutering_01062026.pdf", BPs,
       width=4, height=2,  units="in", scale=3)
