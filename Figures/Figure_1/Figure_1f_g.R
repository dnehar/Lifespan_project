# =============================================================================
# Figure 1f & 1g — Cell type frequency across age groups (PBMC, Level 3)
#
# This script generates boxplots showing the relative frequency (% of PBMC)
# of selected immune cell subsets across seven age groups, from Infants to
# Oldest_old. Statistical comparisons between consecutive age groups are
# displayed using pairwise Wilcoxon tests. The plot is faceted by cell type
# and saved as a PDF.
#
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: boxplot_Fig1f_01022026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette for selected Level 3 cell types ---
cols <- c(
  "B_naive"       = "#1c9099",
  "CD4_ISGhi"     = "#697d35",
  "CD4_memory"    = "#90aa3c",
  "CD4_naive"     = "#193a1c",
  "CD8_TEMRA"     = "#d28529",
  "CD8_naive"     = "#f37421",
  "CD14_mono"     = "#f6a2a7",
  "CD56bright_NK" = "#f2e4a0",
  "CD56dim_NK"    = "#fee000"
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define ordered age groups ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Define pairwise comparisons between consecutive age groups ---
my_comparisons <- list(
  c('Infants', 'Child'),
  c('Child', 'Adolescent'),
  c('Adolescent', 'Young'),
  c('Young', 'Middle_aged'),
  c('Middle_aged', 'Older'),
  c('Older', 'Oldest_old')
)

# --- Cell subsets to display in the plot ---
subset_to_be_plotted <- c('CD14_mono', 'CD56dim_NK', 'CD4_memory', 'CD8_TEMRA',
                          'CD4_naive', 'CD8_naive', 'B_naive', 'CD4_ISGhi')

# --- Compute per-sample cell type frequencies and build boxplot ---
box_plot_pbmc_L3 <- LifeSpan_ALL_MetaData %>%
  mutate(ReCluster = factor(LS_L3, levels = subset_to_be_plotted)) %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  group_by(Groups, sample_id, ReCluster) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>%       # convert counts to % of PBMC
  ungroup() %>%
  as.data.frame() %>%
  filter(ReCluster %in% subset_to_be_plotted) %>%
  mutate(ReCluster = factor(ReCluster, levels = subset_to_be_plotted)) %>%
  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 0.2) +
  theme_bw() +
  # Pairwise Wilcoxon tests between consecutive age groups
  ggpubr::stat_compare_means(comparisons = my_comparisons, label = "p.format", hide.ns = F, vjust = 0.5) +
  theme(legend.position = "none",
        strip.text = element_text(size = 13, face = 'bold')) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = cols) +
  theme(axis.text.y  = element_text(size = 16),
        axis.text.x  = element_text(size = 16, angle = 90),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18)) +
  ylab('% PBMC') + xlab('Age groups')

box_plot_pbmc_L3

# --- Save figure as PDF ---
ggsave("./boxplot_Fig1f_01022026.pdf", box_plot_pbmc_L3,
       width = 3.4, height = 2.4, units = "in", scale = 3)