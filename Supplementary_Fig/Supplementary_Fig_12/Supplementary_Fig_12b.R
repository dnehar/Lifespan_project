# =============================================================================
# Supplementary Figure 12b — Boxplots of PBMC subsets proportions across age groups (Level 4)
#
# This script computes per-sample frequencies of 46 subsets as a percentage of total PBMCs,
# and displays their distribution across seven age groups acording to infered CMV using boxplots with
# pairwise statistical comparisons between consecutive age groups.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./boxplot_PBMCs_LS_L4_CMV_differences_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per DC subtype (Level 4 annotation) ---


cmv_cols <- c('Pos'='#faa31b',
              'Neg'='#1eb8d4')

# Level 4: LS_L4  (n=47 clusters)
order_LS_L4 <- c('moDC',  'cDC1', 'cDC2','AXL_DC', 'pDC', 'CD14_mono','ISGhi_CD14_mono', 'CD16_mono',
                 'B_transitional','B_naive', 'B_ABC', 'B_ISGhi', 'B_memory',  'PCs','HSPC', 'Mgk',
                 'CD56bright_NK', 'CD56dim_NK', 'Adaptive_NK',  'Proliferating_NK',
                 'gdT_Vd1_Naive', 'gdT_Vd1_SOX4', 'gdT_Vd1_KLRF1',  'gdT_Vd2_GZMB', 'gdT_Vd2_GZMK',
                 'CD8_naive_SOX4+','CD8_naive_SOX4-', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT','CD8_TEMRA','CD8aa',
                 'CD4_naive_SOX4+', 'CD4_naive_SOX4-',  'CD4_ISGhi', 'CD4_Proliferating','Tregs_naive', 'Tregs_mem', 
                 'CXCR5+_TFH-like','GZMK_TH1_like','TH10', 'TH17', 'TH2', 'TH22', 'TPH',  'CD4_TEMRA')


# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


#age_groups <- c("Pos", "Neg")
cmv_comp <- list (c('Infants_Pos','Infants_Neg'),
                  c('Child_Pos','Child_Neg'),
                  c('Adolescent_Pos','Adolescent_Neg'),
                  c('Young_Pos','Young_Neg'),
                  c('Middle_aged_Pos','Middle_aged_Neg'),
                  c('Older_Pos','Older_Neg'),
                  c( 'Oldest_old_Pos','Oldest_old_Neg'))
LifeSpan_ALL_MetaData$infered_CMV <- LifeSpan_ALL_MetaData$CMVerify_prediction 
LifeSpan_ALL_MetaData$groups_cmv <- paste0(LifeSpan_ALL_MetaData$Age_groups, "_",LifeSpan_ALL_MetaData$infered_CMV)
head(LifeSpan_ALL_MetaData)


cmv_gps <- c('Infants_Pos','Infants_Neg','Child_Pos','Child_Neg','Adolescent_Pos','Adolescent_Neg',
             'Young_Pos','Young_Neg','Middle_aged_Pos','Middle_aged_Neg','Older_Pos','Older_Neg',
             'Oldest_old_Pos','Oldest_old_Neg')


plt_cmv <- LifeSpan_ALL_MetaData %>% 
  filter(!LS_L4 %in% c('doublets')) %>% 
  mutate(ReCluster = factor(LS_L4, levels= order_LS_L4)) %>% #, levels = ordered_SC
  mutate(groups_cmv = factor(groups_cmv, levels = cmv_gps)) %>%
  mutate(ReCluster = factor(ReCluster)) %>% #, levels = ordered_SC
  group_by(groups_cmv,  sample_id, Age_groups, infered_CMV, ReCluster) %>%
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(!ReCluster %in% c('doublets')) %>% 
  #filter(ReCluster %in% subset_to_be_plotted) %>% 
  ggplot(aes(x = groups_cmv, y = freq, fill = infered_CMV, group = groups_cmv)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.2) +
  theme_bw()  +  #THEME +
  ggpubr::stat_compare_means(comparisons = cmv_comp) + #, method = "t.test"
  scale_fill_manual(values=cmv_cols) + 
  scale_color_manual(values = cmv_cols)+
  
  theme(legend.position = "none", strip.text = element_text(size = 10, face='bold')) +
  
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 6) + 
  scale_fill_manual(values=cmv_cols) + 
  theme(axis.text.y=element_text(size=12, colour = 'black'), 
        axis.title.y = element_text(face="bold", size=14, colour = 'black'), 
        axis.text.x=element_text(size=12, colour = 'black', angle = 90),
        axis.title.x = element_text(face="bold", size=14, colour = 'black'),
        strip.text.x = element_text(size = 14, face ='bold', colour = 'black')) +#    ylab('% PBMC') + xlab('Age groups')
  ylab('% in PBMCs') + xlab(' ')

plt_cmv

ggsave("./boxplot_PBMCs_LS_L4_CMV_differences_03132026.pdf", 
       plt_cmv,
       width=8, height=6,  units="in", scale=3, dpi=100)
