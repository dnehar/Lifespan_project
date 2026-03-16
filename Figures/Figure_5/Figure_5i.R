# =============================================================================
# Figure 5i — Stacked bar plot of memory CD4 T cell subset proportions across age groups (Level 4)
#
# This script computes frequencies of eight memory CD4 T cell subtypes
# (CXCR5+_TFH-like, TH22, TH17, GZMK_TH1_like, TH2, TH10, TPH, CD4_TEMRA)
# as a percentage of all memory CD4 T cells,
# and displays their composition across seven age groups as a stacked bar plot.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Barplot_memory_CD4_Tcells_age_groups_01062026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per memory CD4 T cell subtype (Level 4 annotation) ---
cols <- c(
  'TH2'             = '#1c7b3d',
  'TH17'            = '#3cb54a',
  'CXCR5+_TFH-like' = '#74c168',
  'TH10'            = '#a4de02ff',
  'TPH'             = '#697d35',
  'GZMK_TH1_like'   = '#7fcdbb',
  'CD4_TEMRA'       = '#1c572b',
  'TH22'            = '#edf8b1'
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# Required columns from meta_small: Age_groups, LS_L4
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define the memory CD4 T cell subtypes to plot (Level 4 annotation) ---
subset_to_be_plotted <- c('CXCR5+_TFH-like', 'TH22', 'TH17', 'GZMK_TH1_like',
                          'TH2', 'TH10', 'TPH', 'CD4_TEMRA')

# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Define pairwise comparisons between consecutive age groups ---
# NOTE: my_comparisons is defined here for reference but is not used in this
# stacked bar plot. It would be needed if adding stat_compare_means (e.g. in boxplot version).
my_comparisons <- list(c('Infants', 'Child'),
                       c('Child', 'Adolescent'),
                       c('Adolescent', 'Young'),
                       c('Young', 'Middle_aged'),
                       c('Middle_aged', 'Older'),
                       c('Older', 'Oldest_old'))

# --- Compute memory CD4 T cell subtype proportions and build stacked bar plot ---
# Step 1: assign ordered factor levels to age group (Groups) and T cell subtype (ReCluster)
# Step 2: filter to keep only the eight memory CD4 T cell subtypes of interest
# Step 3: count cells per age group x T cell subtype combination
# Step 4: compute frequency as % of total memory CD4 T cells within each age group
# Step 5: plot stacked bar chart — one bar per age group, filled by T cell subtype
BP_1 <- LifeSpan_ALL_MetaData %>%

  mutate(Groups    = factor(Age_groups, levels = age_groups)) %>%        # ordered age groups
  mutate(ReCluster = factor(LS_L4, levels = subset_to_be_plotted)) %>%   # Level 4 T cell annotation
  filter(ReCluster %in% subset_to_be_plotted) %>%                        # keep memory CD4 T cell subtypes only
  group_by(Groups, ReCluster) %>%
  summarise(n = n()) %>%                                                  # cell count per group x cluster
  mutate(freq = n / sum(n) * 100) %>%                                    # % of memory CD4 T cells per age group
  ungroup() %>%
  as.data.frame() %>%

  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = ReCluster)) +
  geom_bar(stat = "identity", color = "black") +                         # stacked bar, black outline

  scale_fill_manual(values = cols) +                                     # apply T cell color palette
  scale_x_discrete(limits = age_groups) +                                # enforce age group order on x-axis
  theme(
    axis.text.y       = element_text(size = 18, colour = 'black'),
    axis.text.x       = element_text(size = 18, colour = 'black'),
    axis.title.x      = element_text(face = "bold", size = 20, colour = 'black'),
    axis.title.y      = element_text(face = "bold", size = 20, colour = 'black'),
    plot.title        = element_text(hjust = 0.5, face = 'bold', size = 16, colour = 'black'),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_rect(fill = NA, color = 'black', size = 1),
    legend.position   = "none"
  ) +
  ylab('% of Lineage') + xlab('')

BP_1

# --- Save output ---
ggsave("./Barplot_memory_CD4_Tcells_age_groups_01062026.pdf", BP_1,
       width = 1.7, height = 1.8, units = "in", scale = 3)