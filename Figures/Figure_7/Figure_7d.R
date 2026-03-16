# =============================================================================
# Figure 7d — Boxplots of SOX4 CD8 T cell subset proportions across age groups (Level 4)
#
# This script computes per-sample frequencies of two SOX4 CD8 T cell subtypes
# (CD8_naive_SOX4-, CD8_naive_SOX4+) as a percentage of total PBMCs, and displays
# their distribution across four age groups (HI, HC, HY, HO) using boxplots with
# all pairwise t-test comparisons.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./boxplot_SOX4_CD8_T_cells_in_PBMCs_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per SOX4 CD8 T cell subtype (Level 4 annotation) ---
cols <- c(
  "CD8_naive_SOX4+" = "#ffdeadff",
  "CD8_naive_SOX4-" = "#f37421"
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c("HI", "HC", "HY", "HO")

# --- Define all pairwise comparisons between age groups ---
# Used by ggpubr::stat_compare_means to annotate p-values on the plot
my_comparisons <- combn(age_groups, 2, FUN = list, simplify = T)

# --- Define the SOX4 CD8 T cell subtypes to plot (Level 4 annotation) ---
subset_to_be_plotted <- c('CD8_naive_SOX4-', 'CD8_naive_SOX4+')

# --- Compute per-sample SOX4 CD8 T cell subtype proportions and build boxplot ---
# Step 1: assign ordered factor levels to cell type (ReCluster) and age group (Groups)
# Step 2: count cells per sample x cell type combination
# Step 3: compute frequency as % of all cells in that sample x age group
# Step 4: keep only the two SOX4 CD8 T cell subtypes of interest
# Step 5: plot one facet per subtype (free y-axis scale), with pairwise t-test p-values
box_plot_pbmc_L2 <- LifeSpan_ALL_MetaData %>%

  mutate(ReCluster = factor(LS_L4)) %>%                           # Level 4 cluster annotation
  mutate(Groups = factor(Groups, levels = age_groups)) %>%        # ordered age groups
  group_by(Groups, Names, ReCluster) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>%                 # keep SOX4 CD8 T cell subtypes only
  summarise(n = n()) %>%                                           # cell count per sample x cluster
  mutate(freq = n / sum(n) * 100) %>%                             # % of total PBMCs per sample
  ungroup() %>%
  as.data.frame() %>%

  ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +                              # boxplot without outlier symbols
  geom_jitter(size = 0.2) +                                       # overlay individual sample points
  theme_bw() +

  # Pairwise t-test between all age group combinations; p-values displayed above brackets
  ggpubr::stat_compare_means(comparisons = my_comparisons, method = "t.test") + #label = "p.signif"

  theme(legend.position = "none",                                 # legend redundant with facet labels
        strip.text = element_text(size = 14, face = 'bold')) +
  facet_wrap(. ~ ReCluster, scales = "free_y", nrow = 1) +       # one panel per SOX4 CD8 T cell subtype

  scale_fill_manual(values = cols) +                              # apply SOX4 CD8 T cell subtype color palette

  theme(axis.text.y  = element_text(size = 12, colour = 'black'),
        axis.text.x  = element_text(size = 12, colour = 'black'),
        axis.title.x = element_text(face = "bold", size = 14, colour = 'black'),
        axis.title.y = element_text(face = "bold", size = 14, colour = 'black'),
        strip.text.x = element_text(size = 14, face = 'bold', colour = 'black')) +
  ylab('% in PBMCs') + xlab('Age groups')

box_plot_pbmc_L2

# --- Save figure as PDF ---
ggsave("./boxplot_SOX4_CD8_T_cells_in_PBMCs_03132026.pdf", box_plot_pbmc_L2,
       width = 4.2, height = 3, units = "in", scale = 3)
