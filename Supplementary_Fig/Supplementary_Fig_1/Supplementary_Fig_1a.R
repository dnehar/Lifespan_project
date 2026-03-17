
# =============================================================================
# Supplementary Fig. 1a  — Scatter plots of PBMCs subset frequencies vs. age in Infants (Level 1 - clustering)
#
# This script computes frequencies of nine PBMC cell subtypes
# as scatter plots with linear regression fits and Pearson correlation coefficients.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./corplot_PBMCs_cells_LS_L1_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# Required columns from meta_small: Age_groups, Age_in_months, sample_id, LS_L4
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


# --- Color palette  cols <- c(
   "CD4_Tcells" = "#193a1c",
   "CD8_Tcells" = "#f37421",
   "gd_Tcells" = "#80622f",
   "NK_cells" = "#fee000",
   "B_cells" = "#1c9099",
   "PCs" = "#8856a7",
   "monocytes" = "#f6a2a7",
   "DCs" = "#ed2024",
   "HSPC" = "#b0479a")

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

ggsave("./corplot_PBMCs_cells_LS_L1_03132026.pdf", p_corr_pbmc_L1,
       width=5, height=2,  units="in", scale=3)
