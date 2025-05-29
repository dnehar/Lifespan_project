library(dplyr)
library(ggplot2)
  

# load sample informatiom
pheno <- read.csv('../LS95_sample_info_03262024.csv')
head(pheno)

ordered_names <- unique(pheno$Names)

# load metadata
LifeSpan_ALL_MetaData <- read.csv("meta/LifeSpan_ALL_Annotated_MetaData_09122024.csv", row.names = 1) %>% as.data.frame()

######################################################
                # CD4 T cells 
######################################################

to_be_ploted <- c("CD4_T_Memory", "CD4_T_Naive")

age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

BP <- LifeSpan_ALL_MetaData %>% 
  
  mutate(Groups = factor(Groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(pbmc_simple_clustering, levels = to_be_ploted)) %>% #*****
  filter(ReCluster %in% to_be_ploted) %>% 
  
  group_by(Groups,  Names, ReCluster) %>%
  summarise(n = n(), Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  ggplot(aes(x = Names, y = freq, fill = ReCluster, group=ReCluster)) +
  scale_fill_manual(values=cols) + #**
  scale_x_discrete(limits=ordered_names) + #labels= labels
  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16, angle = 90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        legend.position = "none") + #    ylab('% PBMC') + xlab('Age groups')
  
  ylab('% of PBMCs') + xlab('Individuals (n=95)')

CD4_T <-  BP +   ggstream::geom_stream(color = 'black', 
                                       lwd = 0.25,
                                       bw = 1,   
                                       type = "proportional")
print(CD4_T)

######################################################
                # CD8 T cells 
######################################################

to_be_ploted <- c( 'CD8_T_Effector','CD8_T_Naive')

age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

BP <- LifeSpan_ALL_MetaData %>% 
  
  mutate(Groups = factor(Groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(pbmc_simple_clustering, levels = to_be_ploted)) %>% #*****
  filter(ReCluster %in% to_be_ploted) %>% 
  
  group_by(Groups,  Names, ReCluster) %>%
  summarise(n = n(), Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  ggplot(aes(x = Names, y = freq, fill = ReCluster, group=ReCluster)) +
  scale_fill_manual(values=cols) + #**
  scale_x_discrete(limits=ordered_names) + #labels= labels
  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16, angle = 90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        legend.position = "none") + #    ylab('% PBMC') + xlab('Age groups')
  
  ylab('% of PBMCs') + xlab('Individuals (n=95)')

CD8_T <-  BP +   ggstream::geom_stream(color = 'black', 
                                       lwd = 0.25,
                                       bw = 1,   
                                       type = "proportional")
print(CD8_T)

######################################################
                # B cells 
######################################################

to_be_ploted <- c('B_naive','B_memory')

age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

BP <- LifeSpan_ALL_MetaData %>% 
  
  mutate(Groups = factor(Groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(pbmc_simple_clustering, levels = to_be_ploted)) %>% #*****
  filter(ReCluster %in% to_be_ploted) %>% 
  
  group_by(Groups,  Names, ReCluster) %>%
  summarise(n = n(), Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  as.data.frame() %>% #head()
  ggplot(aes(x = Names, y = freq, fill = ReCluster, group=ReCluster)) +
  scale_fill_manual(values=cols) + #**
  scale_x_discrete(limits=ordered_names) + #labels= labels
  theme(axis.text.y=element_text(size=16), 
        axis.text.x=element_text(size=16, angle = 90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        legend.position = "none") + #    ylab('% PBMC') + xlab('Age groups')
  
  ylab('% of PBMCs') + xlab('Individuals (n=95)')

Bcells <-  BP +   ggstream::geom_stream(color = 'black', 
                                       lwd = 0.25,
                                       bw = 1,   
                                       type = "proportional")
print(Bcells) 

####
BPs <- CD4_T | CD8_T |  Bcells 
print(BPs)

