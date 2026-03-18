

#=============================================================================
  # Supplementary Fig. 5f— Scatter plots of Tregs subset frequencies vs. age 
  #
  # This script computes frequencies of two Tregs subsets 
  # as a percentage of Tregs,
  # as scatter plots with linear regression fits and Pearson correlation coefficients.
  # Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
  # Output: ./corplot_Tregs_in_lineage_03182026.pdf
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


cols <- c(  "Tregs_naive" = "#137d82",
            "Tregs_mem" = "#56bbbf")

subset_to_be_plotted <- c('Tregs_naive','Tregs_mem')

p_corr_lineage <- LifeSpan_ALL_MetaData %>%
  
  mutate(ReCluster = factor(LS_L4)) %>% #***
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>%  
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  #filter(Groups %in% c('Infants')) %>% 
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
  ylab('% PBMC')  + xlab('Age (years)')
p_corr_lineage


ggsave("./corplot_Tregs_in_lineage_03182026.pdf", p_corr_lineage,
       width=2, height=1.1,  units="in", scale=3)


