

#=============================================================================
# Supplementary Fig. 6d— Scatter plots of memory CD4 T  subset frequencies vs. age in infants  
#
# This script computes frequencies of two memory CD4 T  subsets 
# as a percentage of memory CD4 T ,
# as scatter plots with linear regression fits and Pearson correlation coefficients.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./corplot_Tmemory_in_lineage_infants_03182026.pdf

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
cols <- c('TH2'= '#1c7b3d',
          'TH17'= '#3cb54a',
          'CXCR5+_TFH-like'= '#74c168',
          'TH10'= '#a4de02ff',#'#a4de02ff', 
          'TPH'= '#697d35',
          'GZMK_TH1_like'= '#7fcdbb',
          'CD4_TEMRA'='#1c572b',
          'TH22'= '#edf8b1')

subset_to_be_plotted <-  c('CXCR5+_TFH-like', 'TH22', 'TH17', 'GZMK_TH1_like', 'TH2', 'TH10',
                           'TPH', 'CD4_TEMRA')


p_corr_lineage <- LifeSpan_ALL_MetaData %>%
  
  mutate(ReCluster = factor(LS_L4, levels= subset_to_be_plotted)) %>% #***
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>%  
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(Groups %in% c('Infants')) %>% 
  ggplot(aes(x = Age_in_yrs, y = freq, fill=ReCluster)) +
  geom_point(shape = 21, aes(fill = ReCluster), color = "black", size = 3, stroke = 0.5) + #stroke: thickness of the border
  geom_smooth(method = "lm", aes(color=ReCluster)) + 
  scale_fill_manual(values=cols) + #**** 
  scale_color_manual(values = cols)+ #****
  ggpubr::stat_cor() +
  #theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 13, face ='bold')) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 1) + #***
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
  ylab('% Lineage')  + xlab('Age (years)')
p_corr_lineage


ggsave("./corplot_Tmemory_in_lineage_infants_03182026.pdf", p_corr_lineage,
       width=2, height=1.1,  units="in", scale=3)
