# =============================================================================
# Supplementary Figure 11 — Boxplots of PBMC subsets proportions across age groups (Level 4)
#
# This script computes per-sample frequencies of 46 subsets as a percentage of total PBMCs,
# and displays their distribution across seven age groups acording to SEX using boxplots with
# pairwise statistical comparisons between consecutive age groups.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./boxplot_PBMCs_LS_L4_Sex_differences_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per DC subtype (Level 4 annotation) ---

cols <- c("B_naive" = "#1c9099",
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
  "pDCs" = "#a5a4a4",
  # Level 3
  "ISGhi_CD14_mono" = "#f15d64",
  "CD4_Proliferating" = "#2a9d8f",
  "CD8_CM" = "#f59e2f",
  "CD8_GZMK" = "#fba919",
  "CD8_MAIT" = "#fbb36a",
  "CD8_TEMRA" = "#d28529",
  "CD8_gdT" = "#80622f",
  "CD8aa" = "#c46b1c",
  "B_transitional" = "#756bb1",
  "B_ABC" = "#41b8ea",
  "B_ISGhi" = "#9ecae1",
  "moDC" = "#ed2024",
  "cDC1" = "#771215",
  "cDC2" = "#d84598",
  "AXL_DC" = "#a41e21",
  "pDC" = "#a5a4a4",
  "Adaptive_NK" = "#feb24c",
  "Proliferating_NK" = "#ccb72d",  
  "gdT_Vd2_GZMK" = "#d29734",
  "gdT_Vd2_GZMB" = "#d8bd93",
  "gdT_Vd1_SOX4" = "#56bbbf",
  "gdT_Vd1_KLRF1" = "#993404",
  "gdT_Vd1_Naive" = "#ffeda0",
  "Tregs_naive" = "#137d82",
  "Tregs_mem" = "#56bbbf",
  "CD4_naive_SOX4-" = "#193a1c",
  "CD4_naive_SOX4+" = "#a4de02ff",
  "CD8_naive_SOX4+" = "#ffdeadff",
  "CD8_naive_SOX4-" = "#f37421",
  'TH2'= '#1c7b3d',
  'TH17'= '#3cb54a',
  'CXCR5+_TFH-like'= '#74c168',
  'TH10'= '#a4de02ff',#'#a4de02ff', 
  'TPH'= '#697d35',
  'GZMK_TH1_like'= '#7fcdbb',
  'CD4_TEMRA'='#1c572b')

# Level 4: LS_L4  (n=47 clusters)
order_LS_L4 <- c('moDC',  'cDC1', 'cDC2','AXL_DC', 'pDC', 'CD14_mono','ISGhi_CD14_mono', 'CD16_mono',
                 'B_transitional','B_naive', 'B_ABC', 'B_ISGhi', 'B_memory',  'PCs','HSPC', 'Mgk',
                 'CD56bright_NK', 'CD56dim_NK', 'Adaptive_NK',  'Proliferating_NK',
                 'gdT_Vd1_Naive', 'gdT_Vd1_SOX4', 'gdT_Vd1_KLRF1',  'gdT_Vd2_GZMB', 'gdT_Vd2_GZMK',
                 'CD8_naive_SOX4+','CD8_naive_SOX4-', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT','CD8_TEMRA','CD8aa',
                 'CD4_naive_SOX4+', 'CD4_naive_SOX4-',  'CD4_ISGhi', 'CD4_Proliferating','Tregs_naive', 'Tregs_mem', 
                 'CXCR5+_TFH-like','GZMK_TH1_like','TH10', 'TH17', 'TH2', 'TH22', 'TPH',  'CD4_TEMRA')


MetaData <- readRDS('/Users/nehard/MyProjects/DU_lab/LS95/LS95_manuscript/SUBMITTED/NatCom_LS/Revisions/GitHub/pbmcs_v1.rds')

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Define pairwise comparisons between consecutive age groups ---
# Used by ggpubr::stat_compare_means to annotate p-values on the plot
my_comparisons <- list(c('Infants', 'Child'),
                       c('Child', 'Adolescent'),
                       c('Adolescent', 'Young'),
                       c('Young', 'Middle_aged'),
                       c('Middle_aged', 'Older'),
                       c('Older', 'Oldest_old'))


group_sex_order <- c("Infants_M","Infants_F","Child_M","Child_F",
                     "Adolescent_M", "Adolescent_F","Young_M","Young_F",      
                     "Middle_aged_F", "Middle_aged_M", "Older_M","Older_F", 
                     "Oldest_old_M","Oldest_old_F")

LifeSpan_ALL_MetaData$groups_sex <- paste0(LifeSpan_ALL_MetaData$Age_groups, "_",LifeSpan_ALL_MetaData$Sex)
head(LifeSpan_ALL_MetaData)

sex_pairs <- split(group_sex_order, ceiling(seq_along(group_sex_order)/2))
sex_com <- lapply(sex_pairs, function(x) c(x[1], x[2]))



plt_sex <- LifeSpan_ALL_MetaData %>% 
  filter(!LS_L4 %in% c('doublets')) %>% 
  mutate(ReCluster = factor(LS_L4, levels = order_LS_L4)) %>%
  mutate(Groups = factor(groups_sex, levels = group_sex_order)) %>%
  group_by(Groups, sample_id, Sex, ReCluster) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.2) +
  theme_bw()  +  #THEME +
  ggpubr::stat_compare_means(comparisons = sex_com) +
  
  theme(legend.position = "none", 
        strip.text = element_text(size = 10, face='bold')) +
  
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 6) + 

  scale_fill_manual(values=cols) + 
  theme(axis.text.y=element_text(size=12, colour = 'black'), 
        axis.text.x=element_text(size=12, colour = 'black', angle = 90),
        axis.title.x = element_text(face="bold", size=14, colour = 'black'),
        axis.title.y = element_text(face="bold", size=14, colour = 'black'), 
        strip.text.x = element_text(size = 14, face ='bold', colour = 'black')) +#    ylab('% PBMC') + xlab('Age groups')
  ylab('% in PBMCs') + xlab(' ')

plt_sex

# --- Save figure as PDF ---
ggsave("./boxplot_PBMCs_LS_L4_Sex_differences_03132026.pdf", plt_sex,
       width = 9.2, height = 6, units = "in", scale = 3)
 
