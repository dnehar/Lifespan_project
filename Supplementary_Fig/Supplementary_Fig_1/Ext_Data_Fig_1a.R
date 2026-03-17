
# =============================================================================
# Supplementary Fig. 1a — correlation plot on PBMC (Level 1, n=9 clusters)
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_PBMCs_LS_L1_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

 cols <- c(
   # Level 1
   "CD4_Tcells" = "#193a1c",
   "CD8_Tcells" = "#f37421",
   "gd_Tcells" = "#80622f",
   "NK_cells" = "#fee000",
   "B_cells" = "#1c9099",
   "PCs" = "#8856a7",
   "monocytes" = "#f6a2a7",
   "DCs" = "#ed2024",
   "HSPC" = "#b0479a",
   
   # Level 2
   "B_naive" = "#1c9099",
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
 
   # Level 4 additions
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
   
   #Tmem - helpers
   'TH2'= '#1c7b3d',
   'TH17'= '#3cb54a',
   'CXCR5+_TFH-like'= '#74c168',
   'TH10'= '#a4de02ff',#'#a4de02ff', 
   'TPH'= '#697d35',
   'GZMK_TH1_like'= '#7fcdbb',
   'doublets'='#a8ddb5',
   'CD4_TEMRA'='#1c572b',
   'TH22'= '#edf8b1',

   # Groups
   "Infants" = "#0072B2",
   "Child" = "#56B4E9",
   "Adolescent" = "#009E73", 
   "Young" = "#F0E442",
   "Middle_aged" = "#E69F00",
   "Older" ="#D55E00",
   "Oldest_old" = "#CC79A7"
 )



age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

p_corr_pbmc_L1 <- LifeSpan_ALL_MetaData %>%
  
  mutate(ReCluster = factor(LS_L1, levels = order_LS_L1)) %>% #***
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  #summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  #filter(ReCluster %in% subset_to_be_plotted) %>%  
  ggplot(aes(x = Age_in_yrs, y = freq, fill=ReCluster)) +
  geom_point(shape = 21, aes(fill = ReCluster), color = "black", size = 3, stroke = 0.5) + #stroke: thickness of the border
  geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color=ReCluster)) +
  scale_fill_manual(values=cols) + #**** 
  scale_color_manual(values = cols)+ #****
  ggpubr::stat_cor() +
  #theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 13, face ='bold')) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 2) + #***
  theme_bw() +
  # -------- ALL TEXT IN BLACK 
theme(
  legend.position = "none",
  
  # Facet strip labels
  strip.text = element_text(size = 13, face = "bold", colour = "black"),
  
  # Axis tick labels
  axis.text.y  = element_text(size = 16, colour = "black"),
  axis.text.x  = element_text(size = 16, colour = "black"),
  
  # Axis titles
  axis.title.x = element_text(face = "bold", size = 18, colour = "black"),
  axis.title.y = element_text(face = "bold", size = 18, colour = "black")
) +
ylab('% PBMC')  + xlab('Age (years)')

p_corr_pbmc_L1

ggsave("./Figure_2026/corplot_pbmcs_L1_01082026.pdf", p_corr_pbmc_L1,
       width=5, height=2,  units="in", scale=3)
