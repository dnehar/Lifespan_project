# =============================================================================
# Supplementary Fig. 9e — Scatter plots of D8 T cell, MAIT and gd T subset frequencies vs. age in Infants 
#
# This script computes frequencies of CD8 T cell, MAIT and gd T subsets 
# as a percentage of PBMCs,
# restricted to the Infants age group, and displays their correlation with age (in months)
# as scatter plots with linear regression fits and Pearson correlation coefficients.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./corplot_T_CD8_T_cells_in_pbmcs_infants_03182026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# MetaData is a list containing:
#   $meta_small : per-cell metadata (cell type annotations, sample IDs, age groups, etc.)
#   $pheno      : per-sample metadata (sample_id, age, sex, etc.)
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()
pheno <- MetaData[['pheno']] %>% as.data.frame()

# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Color palette — one color per SOX4 CD4 and CD8 T cell subtype (Level 4 annotation) ---
cols <- c(
  "CD8_naive_SOX4+" = "#ffdeadff",
  "CD8_naive_SOX4-" = "#f37421",
  "CD4_naive_SOX4-" = "#193a1c",
  "CD4_naive_SOX4+" = "#a4de02ff"
)

# --- Define all pairwise comparisons between consecutive age groups ---
# Used by ggpubr::stat_compare_means to annotate p-values on the plot
my_comparisons <- list (c('Infants', 'Child'),
                        c('Child','Adolescent'),
                        c('Adolescent', 'Young'),
                        c('Young', 'Middle_aged'),
                        c('Middle_aged', 'Older'),
                        c('Older', 'Oldest_old'))

# --- CD4+ T cells: compute per-sample SOX4 subtype proportions within CD4 lineage and build boxplot ---
subset_to_be_plotted <- c('CD4_naive','CD4_ISGhi', 'CD4_Tregs','CD4_memory','CD4_Proliferating')

box_plot_lineage_CD4 <- LifeSpan_ALL_MetaData %>%
  mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%         # Level 3 cluster annotation
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%         # ordered age groups
  filter(ReCluster %in% subset_to_be_plotted) %>%                      # keep CD4 lineage clusters only
  group_by(Groups, sample_id, LS_L4) %>%
  summarise(n = n()) %>%                                               # cell count per sample x cluster
  mutate(freq = n / sum(n) * 100) %>%                                  # % of CD4 lineage per sample
  ungroup() %>%
  as.data.frame() %>%
  filter(LS_L4 %in% c('CD4_naive_SOX4-', 'CD4_naive_SOX4+')) %>%      # keep naive CD4 SOX4 subtypes only
  ggplot(aes(x = Groups, y = freq, fill = LS_L4, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +                                   # boxplot without outlier symbols
  geom_jitter(size = 0.2) +                                            # overlay individual sample points
  theme_bw() +

  # Pairwise comparisons between consecutive age groups; non-significant p-values hidden
  ggpubr::stat_compare_means(comparisons = my_comparisons, label = "p.format", hide.ns = T, vjust = 0.5) +

  theme(legend.position = "none",                                      # legend redundant with facet labels
        strip.text = element_text(size = 10, face = 'bold')) +
  facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) +                  # one panel per CD4 SOX4 subtype

  scale_fill_manual(values = cols) +                                   # apply CD4 SOX4 subtype color palette
  theme(axis.text.y  = element_text(size = 12, colour = 'black'),
        axis.text.x  = element_text(size = 12, colour = 'black', angle = 90),
        axis.title.x = element_text(face = "bold", size = 14, colour = 'black'),
        axis.title.y = element_text(face = "bold", size = 14, colour = 'black'),
        strip.text.x = element_text(size = 14, face = 'bold', colour = 'black')) +
  ylab('% of lineage') + xlab('Age groups')

box_plot_lineage_CD4

# --- CD8+ T cells (incl. MAIT and γδ T): compute per-sample SOX4 subtype proportions within CD8 lineage and build boxplot ---
subset_to_be_plotted <- c('CD8_naive', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT','CD8_TEMRA',
                          'CD8_gdT', 'CD8aa')

box_plot_lineage_CD8 <- LifeSpan_ALL_MetaData %>%
  mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%         # Level 3 cluster annotation
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%         # ordered age groups
  filter(ReCluster %in% subset_to_be_plotted) %>%                      # keep CD8 lineage clusters only
  group_by(Groups, sample_id, LS_L4) %>%
  summarise(n = n()) %>%                                               # cell count per sample x cluster
  mutate(freq = n / sum(n) * 100) %>%                                  # % of CD8 lineage per sample
  ungroup() %>%
  as.data.frame() %>%
  filter(LS_L4 %in% c('CD8_naive_SOX4-', 'CD8_naive_SOX4+')) %>%      # keep naive CD8 SOX4 subtypes only
  ggplot(aes(x = Groups, y = freq, fill = LS_L4, group = Groups)) +
  geom_boxplot(outlier.shape = NA) +                                   # boxplot without outlier symbols
  geom_jitter(size = 0.2) +                                            # overlay individual sample points
  theme_bw() +

  # Pairwise comparisons between consecutive age groups; non-significant p-values hidden
  ggpubr::stat_compare_means(comparisons = my_comparisons, label = "p.format", hide.ns = T, vjust = 0.5) +

  theme(legend.position = "none",                                      # legend redundant with facet labels
        strip.text = element_text(size = 10, face = 'bold')) +
  facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) +                  # one panel per CD8 SOX4 subtype

  scale_fill_manual(values = cols) +                                   # apply CD8 SOX4 subtype color palette
  theme(axis.text.y  = element_text(size = 12, colour = 'black'),
        axis.text.x  = element_text(size = 12, colour = 'black', angle = 90),
        axis.title.x = element_text(face = "bold", size = 14, colour = 'black'),
        axis.title.y = element_text(face = "bold", size = 14, colour = 'black'),
        strip.text.x = element_text(size = 14, face = 'bold', colour = 'black')) +
  ylab('% of lineage') + xlab('Age groups')

box_plot_lineage_CD8
