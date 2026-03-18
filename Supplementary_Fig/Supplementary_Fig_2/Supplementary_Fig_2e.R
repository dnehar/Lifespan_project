
# =============================================================================
# Supplementary Fig. 2e  —  Stream plots of immune subsets (Level 2; n=18)
#
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Streamed_Barplot_PBMCs_LS_L2_03172026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# MetaData is a list containing:
#   $meta_small : per-cell metadata (cell type annotations, sample IDs, age groups, etc.)
#   $pheno      : per-sample metadata (sample_id, age, sex, etc.)
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()
pheno <- MetaData[['pheno']] %>% as.data.frame()

age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# color palette ---
cols <- c( "B_naive" = "#1c9099",
  "B_memory" = "#283779",
  "CD4_ISGhi" = "#697d35",
  "CD4_memory" = "#90aa3c",
  "CD4_naive" = "#193a1c",
  "CD4_Tregs" = "#137d82",
  "CD8_memory" = "#fba919",
  "CD8_naive" = "#f37421",
  "CD14_mono" = "#f6a2a7",
  "CD16_mono" = "#f9d3d7",
  "Mgk" = "#932169",
  "CD56bright_NK" = "#f2e4a0",
  "CD56dim_NK" = "#fee000",
  "pDCs" = "#a5a4a4")

ordered_names 
# Level 2: LS_L2  (n=18 clusters)
order_LS_L2 <- c('CD14_mono', 'CD16_mono', 'DCs', 'pDCs', 'Mgk','HSPC',
                 'CD56bright_NK', 'CD56dim_NK', 'gd_Tcells',
                 'B_naive', 'B_memory','PCs', 
                 'CD4_naive','CD4_ISGhi', 'CD4_memory', 'CD4_Tregs', 
                 'CD8_naive', 'CD8_memory')

ordered_names <- unique(pheno$sample_id)

BP_all <- LifeSpan_ALL_MetaData %>% 
  
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L2, levels= order_LS_L2)) %>% #*****
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
  
  ylab('% of PBMCs') + xlab('Individuals (n=95)')

BP_all2 <-  BP_all +   ggstream::geom_stream(color = 'black', 
                                             lwd = 0.25,
                                             bw = 1,   
                                             type = "proportional")
BP_all2

ggsave("./Streamed_Barplot_PBMCs_LS_L2_03172026.pdf", BP_all2,
       width=3.5, height=2.8,  units="in", scale=3)

