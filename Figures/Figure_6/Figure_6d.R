# =============================================================================
# Figure 6d — Stacked bar plot of CD8 T cell subset proportions across age groups (Level 3)
#
# This script computes frequencies of seven CD8 T cell subtypes
# (CD8_naive, CD8_CM, CD8_GZMK, CD8_MAIT, CD8_TEMRA, CD8_gdT, CD8aa)
# as a percentage of all CD8 T cells,
# and displays their composition across seven age groups as a stacked bar plot.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Barplot_CD8_Tcells_age_groups_03162026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per CD8 T cell subtype (Level 3 annotation) ---
cols <- c(
  "CD8_CM"    = "#f59e2f",
  "CD8_GZMK"  = "#fba919",
  "CD8_MAIT"  = "#fbb36a",
  "CD8_TEMRA" = "#d28529",
  "CD8_gdT"   = "#80622f",
  "CD8aa"     = "#c46b1c",
  "CD8_naive" = "#f37421"
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# Required columns from meta_small: Age_groups, LS_L3
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define the CD8 T cell subtypes to plot (Level 3 annotation) ---
subset_to_be_plotted <- c('CD8_naive', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT', 'CD8_TEMRA',
                          'CD8_gdT', 'CD8aa')

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

# --- Compute CD8 T cell subtype proportions and build stacked bar plot ---
# Step 1: assign ordered factor levels to age group (Groups) and T cell subtype (ReCluster)
# Step 2: filter to keep only the seven CD8 T cell subtypes of interest
# Step 3: count cells per age group x T cell subtype combination
# Step 4: compute frequency as % of total CD8 T cells within each age group
# Step 5: plot stacked bar chart — one bar per age group, filled by T cell subtype
BP_1 <- LifeSpan_ALL_MetaData %>%

  mutate(Groups    = factor(Age_groups, levels = age_groups)) %>%        # ordered age groups
  mutate(ReCluster = factor(LS_L3, levels = subset_to_be_plotted)) %>%   # Level 3 CD8 T cell annotation
  filter(ReCluster %in% subset_to_be_plotted) %>%                        # keep CD8 T cell subtypes only
  group_by(Groups, ReCluster) %>%
  summarise(n = n()) %>%                                                  # cell count per group x cluster
  mutate(freq = n / sum(n) * 100) %>%                                    # % of CD8 T cells per age group
  ungroup() %>%
  as.data.frame() %>%

  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = ReCluster)) +
  geom_bar(stat = "identity", color = "black") +                         # stacked bar, black outline

  scale_fill_manual(values = cols) +                                     # apply CD8 T cell color palette
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
ggsave("./Barplot_CD8_Tcells_age_groups_03162026.pdf", BP_1,
       width = 1.7, height = 1.8, units = "in", scale = 3)