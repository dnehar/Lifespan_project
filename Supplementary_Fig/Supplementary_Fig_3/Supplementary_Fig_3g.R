# =============================================================================
# Figure 2d — Boxplot of CD14 and CD16 monocytes frequency as a percentage of monocytes across age groups
# This script computes the frequency of pDC cells as a percentage of all DC
# lineage cells (CD14 mo and CD16 mo) per donor, and displays their
# distribution across seven age groups as a boxplot with pairwise statistics.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Boxplot_mono_in_lineage_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)


# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# Required columns from meta_small: Age_groups, LS_L4, sample_id
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Define pairwise comparisons between consecutive age groups ---
# Used by ggpubr::stat_compare_means to add p-values to the boxplot
my_comparisons <- list(c('Infants', 'Child'),
                       c('Child', 'Adolescent'),
                       c('Adolescent', 'Young'),
                       c('Young', 'Middle_aged'),
                       c('Middle_aged', 'Older'),
                       c('Older', 'Oldest_old'))

# color palette ---
cols <- c( "CD14_mono" = "#f6a2a7",
           "CD16_mono" = "#f9d3d7", 
           "ISGhi_CD14_mono" = "#f15d64")

subset_to_be_plotted <- c('CD14_mono', 'CD16_mono')

box_plot_lineage <- LifeSpan_ALL_MetaData %>%
  mutate(ReCluster = factor(LS_L3)) %>%       # Level 4 DC annotation (ordered)
  mutate(Groups    = factor(Age_groups, levels = age_groups)) %>%    # ordered age groups
  filter(ReCluster %in% subset_to_be_plotted) %>%                    # keep DC lineage subtypes only
  group_by(Groups, sample_id, ReCluster) %>%
  summarise(n = n()) %>%                                             # cell count per donor x group x cluster
  mutate(freq = n / sum(n) * 100) %>%                                # pDC % of DC lineage per donor
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +                                 # boxplot; outliers shown via jitter below
  geom_jitter(size = 0.2) +                                          # individual donor-level data points
  theme_bw() +
  ggpubr::stat_compare_means(                                        # pairwise Wilcoxon tests
    comparisons = my_comparisons,
    label       = "p.format",
    hide.ns     = TRUE,                                              # suppress non-significant comparisons
    vjust       = 0.5
  ) +
  theme(
    legend.position = "none",
    strip.text      = element_text(size = 10, face = 'bold')
  ) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 1) +            # one panel per DC subtype (here: pDC only)
  scale_fill_manual(values = cols) +                                 # apply DC subtype color palette
  theme(
    axis.text.y    = element_text(size = 12, colour = 'black'),
    axis.text.x    = element_text(size = 12, colour = 'black', angle = 90),
    axis.title.x   = element_text(face = "bold", size = 14, colour = 'black'),
    axis.title.y   = element_text(face = "bold", size = 14, colour = 'black'),
    strip.text.x   = element_text(size = 14, face = 'bold', colour = 'black')
  ) +
  ylab('% of lineage') + xlab('Age groups')

box_plot_lineage

# --- Save figure as PDF ---
ggsave("./Boxplot_monocytes_in_lineage_03132026.pdf", box_plot_lineage,
       width = 4.2, height = 3, units = "in", scale = 3)
