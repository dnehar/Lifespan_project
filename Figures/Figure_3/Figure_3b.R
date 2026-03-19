# =============================================================================
# Figure 3b — Boxplots of NK cell subset proportions across age groups (Level 4)
#
# This script computes per-sample frequencies of four NK cell subtypes
# (CD56dim_NK, CD56bright_NK, Adaptive_NK, Proliferating_NK) as a percentage
# of total PBMCs, and displays their distribution across four age groups
# (HI, HC, HY, HO) using boxplots with all pairwise t-test comparisons.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./boxplot_NK_cells_in_PBMCs_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per NK cell subtype (Level 2 annotation) ---
cols <- c(
  "CD56bright_NK"    = "#f2e4a0",
  "CD56dim_NK"       = "#fee000",
  "Adaptive_NK"      = "#feb24c",
  "Proliferating_NK" = "#ccb72d"
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Define all pairwise comparisons between age groups ---
# Used by ggpubr::stat_compare_means to annotate p-values on the plot
my_comparisons <- list (c('Infants', 'Child'),
                        c('Child','Adolescent'),
                        c('Adolescent', 'Young'),
                        c('Young', 'Middle_aged'),
                        c('Middle_aged', 'Older'),
                        c('Older', 'Oldest_old'))

# --- Define the NK cell subtypes to plot (Level 2 annotation) ---
subset_to_be_plotted <- c('CD56dim_NK', 'CD56bright_NK', 'Adaptive_NK', 'Proliferating_NK')

# --- Compute per-sample NK cell subtype proportions and build boxplot ---
# Step 1: assign ordered factor levels to cell type (ReCluster) and age group (Groups)
# Step 2: count cells per sample x cell type combination
# Step 3: compute frequency as % of all cells in that sample x age group
# Step 4: keep only the four NK cell subtypes of interest
# Step 5: plot one facet per NK cell subtype (free y-axis scale), with pairwise t-test p-values
box_plot_pbmc <- LifeSpan_ALL_MetaData %>%

  mutate(ReCluster = factor(LS_L4)) %>%               # Level 4 cluster annotation
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%        # ordered age groups
  group_by(Groups, sample_id, ReCluster) %>%
  summarise(n = n()) %>%                                           # cell count per sample x cluster
  mutate(freq = n / sum(n) * 100) %>%                             # % of total PBMCs per sample
  ungroup() %>%
  as.data.frame() %>%
  filter(ReCluster %in% subset_to_be_plotted) %>%                 # keep NK cell subtypes only

  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +                              # boxplot without outlier symbols
  geom_jitter(size = 0.2) +                                       # overlay individual sample points
  theme_bw() +

  # Pairwise t-test between all age group combinations; p-values displayed above brackets
  #ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") + #label = "p.signif"
  #ggpubr::stat_compare_means(comparisons = my_comparisons, label = "p.signif", hide.ns = F, vjust = 0.5) +

  theme(legend.position = "none",                                 # legend redundant with facet labels
        strip.text = element_text(size = 14, face = 'bold')) +
  facet_wrap(. ~ ReCluster, scales = "free_y", nrow = 1) +       # one panel per NK cell subtype

  scale_fill_manual(values = cols) +                              # apply NK cell subtype color palette

  theme(axis.text.y  = element_text(size = 12, colour = 'black'),
        axis.text.x  = element_text(size = 12, colour = 'black'),
        axis.title.x = element_text(face = "bold", size = 14, colour = 'black'),
        axis.title.y = element_text(face = "bold", size = 14, colour = 'black'),
        strip.text.x = element_text(size = 14, face = 'bold', colour = 'black')) +
  ylab('% in PBMCs') + xlab('Age groups')

box_plot_pbmc


# --- Save figure as PDF ---
ggsave("./boxplot_NK_cells_in_PBMCs_03132026.pdf", box_plot_pbmc,
       width = 4.2, height = 3, units = "in", scale = 3)
