
# =============================================================================
# Figure 2b — Boxplots of DC subset proportions across age groups (Level 2)
#
# This script computes per-sample frequencies of five DC subtypes
# (moDC, cDC1, cDC2, AXL_DC and pDC) as a percentage of total PBMCs,
# and displays their distribution across four age groups
# using boxplots with all pairwise t-test comparisons.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./boxplot_DCs_in_PBMCs_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per DC subtype (Level 2 annotation) ---
cols <- c(
  "moDC"   = "#ed2024",
  "cDC1"   = "#771215",
  "cDC2"   = "#d84598",
  "AXL_DC" = "#a41e21",
  "pDC"    = "#a5a4a4"
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


# --- Define all pairwise comparisons between age groups ---
# Used by ggpubr::stat_compare_means to annotate p-values on the plot
my_comparisons <- list (c('Infants', 'Child'),
                        c('Child','Adolescent'),
                        c('Adolescent', 'Young'),
                        c('Young', 'Middle_aged'),
                        c('Middle_aged', 'Older'),
                        c('Older', 'Oldest_old'))

# --- Define the DC subtypes to plot (Level 2 annotation) ---
subset_to_be_plotted <- c('moDC', 'cDC1', 'cDC2', 'AXL_DC', 'pDC')


# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Compute per-sample DC subtype proportions and build boxplot ---

box_plot_pbmc <- LifeSpan_ALL_MetaData %>%
  mutate(ReCluster = factor(LS_L4)) %>%               # Level 4 cluster annotation
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%        # ordered age groups
  group_by(Groups, sample_id, ReCluster) %>%   
  summarise(n = n()) %>%                                           # cell count per sample x cluster
  mutate(freq = n / sum(n) * 100) %>%                             # % of total PBMCs per sample
  ungroup() %>%
  as.data.frame() %>%
  filter(ReCluster %in% subset_to_be_plotted) %>%                 # keep monocyte subtypes only

  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +                              # boxplot without outlier symbols
  geom_jitter(size = 0.2) +                                       # overlay individual sample points
  theme_bw() +

  # Pairwise Wilcoxon between all age group combinations; p-values displayed above brackets
  ggpubr::stat_compare_means(comparisons = my_comparisons, aes(label = paste0("p = ", after_stat(p.format)))) +
  theme(legend.position = "none",                                 # legend redundant with facet labels
        strip.text = element_text(size = 14, face = 'bold')) +
  facet_wrap(. ~ ReCluster, scales = "free_y", nrow = 1) +       # one panel per monocyte subtype

  scale_fill_manual(values = cols) +                              # apply monocyte subtype color palette
  theme(axis.text.y  = element_text(size = 12, colour = 'black'),
        axis.text.x  = element_text(size = 12, colour = 'black'),
        axis.title.x = element_text(face = "bold", size = 14, colour = 'black'),
        axis.title.y = element_text(face = "bold", size = 14, colour = 'black'),
        strip.text.x = element_text(size = 14, face = 'bold', colour = 'black')) +
  ylab('% in PBMCs') + xlab('Age groups')

print(box_plot_pbmc)

# --- Save figure as PDF ---
ggsave("./boxplot_DCs_in_PBMCs_03132026.pdf", box_plot_pbmc,
       width = 4.2, height = 3, units = "in", scale = 3)
