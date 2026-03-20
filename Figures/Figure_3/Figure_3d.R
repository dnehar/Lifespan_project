# =============================================================================
# Figure 3d — Scatter plots of NK cell subset frequencies vs. age in Infants (Level 4)
#
# This script computes frequencies of four NK cell subtypes
# (CD56dim_NK, CD56bright_NK, Adaptive_NK, Proliferating_NK) as a percentage of all NK cells,
# restricted to the Infants age group, and displays their correlation with age (in months)
# as scatter plots with linear regression fits and Pearson correlation coefficients.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./corplot_NK_cells_in_lineage_infants_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per NK cell subtype (Level 4 annotation) ---
cols <- c(
  "CD56bright_NK"    = "#f2e4a0",
  "CD56dim_NK"       = "#fee000",
  "Adaptive_NK"      = "#feb24c",
  "Proliferating_NK" = "#ccb72d"
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# Required columns from meta_small: Age_groups, Age_in_months, sample_id, LS_L4
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define the NK cell subtypes to plot (Level 4 annotation) ---
subset_to_be_plotted <- c('CD56dim_NK', 'CD56bright_NK', 'Adaptive_NK', 'Proliferating_NK')

# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Compute NK cell subtype proportions and build correlation scatter plots ---

p_corr_lineage_infants <- LifeSpan_ALL_MetaData %>%
  
  mutate(ReCluster = factor(LS_L4, levels = subset_to_be_plotted)) %>%           # Level 4 NK annotation (ordered)
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%           # ordered age groups
  filter(ReCluster %in% subset_to_be_plotted) %>%                        # keep NK subtypes only
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  summarise(n = n()) %>%                                                  # cell count per donor x cluster
  mutate(freq = n / sum(n) *100) %>%                                     # % of NK cells per donor
  ungroup() %>%
  as.data.frame() %>%
  filter(Groups %in% c('Infants')) %>%                                    # restrict to Infants only
  ggplot(aes(x = Age_in_yrs, y = freq, fill=ReCluster)) +
  geom_point(shape = 21, aes(fill = ReCluster), color = "black", size = 3, stroke = 0.5)+  # filled scatter points
  geom_smooth(method = "lm", aes(color=ReCluster)) +                     # linear regression fit per subtype
  scale_fill_manual(values=cols) +                                        # apply NK color palette (fill)
  scale_color_manual(values = cols)+                                      # apply NK color palette (line)
  ggpubr::stat_cor() +                                                    # add Pearson R and p-value
  theme(legend.position = "none", 
        strip.text = element_text(size = 13, face ='bold')) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 1) +                 # one panel per NK subtype
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

p_corr_lineage_infants

# --- Save output ---
ggsave("./corplot_NK_cells_in_lineage_infants_03132026.pdf", p_corr_lineage_infants,
       width=5, height=1.18,   units="in", scale=3)
