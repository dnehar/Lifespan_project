# =============================================================================
# Figure 1h — Stream plots of CD4 T cells, CD8 T cells, and B cells
#              proportions per individual across the lifespan (Level 2 annotation)
#
# This script generates proportional stream plots showing how CD4 T cell,
# CD8 T cell, and B cell subtype compositions change across individuals,
# ordered by age. Each stream panel shows the relative proportion of two
# subtypes within the cell lineage (e.g. CD4_memory vs CD4_naive).
#
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Streamed_LS_L2_clutering_01062026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# MetaData is a list containing:
#   $meta_small : per-cell metadata (cell type annotations, sample IDs, age groups, etc.)
#   $pheno      : per-sample metadata (sample_id, age, sex, etc.)
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()
pheno <- MetaData[['pheno']] %>% as.data.frame()

# --- Sample order: individuals ordered as they appear in pheno (by age) ---
ordered_names <- unique(pheno$sample_id)

# --- Sanity check: number of unique Level 2 cell types ---
length(unique(LifeSpan_ALL_MetaData$lifespan_L2))

# --- Define ordered age groups (used for factor levels throughout) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Full Level 2 cell type order (reference, used for ordered x-axes if needed) ---
order_pbmc_simple_clustering <- c('CD14_mono', 'CD16_mono', 'DCs', 'pDCs', 'Mgk', 'HSPC',
                                  'CD56bright_NK', 'CD56dim_NK', 'gd_Tcells',
                                  'B_naive', 'B_memory', 'PCs',
                                  'CD4_naive', 'CD4_ISGhi', 'CD4_memory', 'CD4_Tregs',
                                  'CD8_naive', 'CD8_memory')

length(order_pbmc_simple_clustering)  # should be 18

# --- Color palette for Level 2 subtypes shown in the stream plots ---
cols <- c(
  "B_naive"    = "#1c9099",
  "B_memory"   = "#283779",
  "CD4_memory" = "#90aa3c",
  "CD8_memory" = "#fba919",
  "CD8_naive"  = "#f37421",
  "CD16_mono"  = "#f9d3d7"
)

# --- Pairwise comparisons between consecutive age groups ---
# Available for stat_compare_means() if statistical annotations are needed
my_comparisons <- list(c('Infants', 'Child'),
                       c('Child', 'Adolescent'),
                       c('Adolescent', 'Young'),
                       c('Young', 'Middle_aged'),
                       c('Middle_aged', 'Older'),
                       c('Older', 'Oldest_old'))

######################################################
# CD4 T cells
######################################################

# --- Select CD4 T cell subtypes to display ---
to_be_ploted <- c("CD4_memory", "CD4_naive")

# --- Compute per-sample proportions of CD4 subtypes, then plot ---
# freq = % of cells within the two CD4 subtypes per sample (sums to 100% within each sample)
# x-axis: individuals ordered by age (ordered_names from pheno)
# geom_stream: proportional stream plot (each band = relative contribution of one subtype)
BP <- LifeSpan_ALL_MetaData %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L2)) %>%                         # use Level 2 annotation
  filter(ReCluster %in% to_be_ploted) %>%
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  summarise(n = n(), Age_months = first(Age_in_yrs)) %>%
  mutate(freq = n / sum(n) * 100) %>%                           # proportion within CD4 subtypes
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(x = sample_id, y = freq, fill = ReCluster, group = ReCluster)) +
  scale_fill_manual(values = cols) +
  scale_x_discrete(limits = ordered_names) +                    # order individuals by age
  theme(axis.text.y  = element_text(size = 16),
        axis.text.x  = element_text(size = 16, angle = 90),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        legend.position = "none") +
  ylab('% of PBMCs') + xlab('Individuals (n=167)')

# --- Add stream geometry (bw = bandwidth for smoothing; type = "proportional") ---
CD4_T <- BP + ggstream::geom_stream(color = 'black',
                                    lwd   = 0.25,
                                    bw    = 1,
                                    type  = "proportional")
print(CD4_T)

######################################################
# CD8 T cells
######################################################

# --- Select CD8 T cell subtypes to display ---
to_be_ploted <- c('CD8_naive', 'CD8_memory')

# --- Compute per-sample proportions of CD8 subtypes, then plot ---
# freq = % of cells within the two CD8 subtypes per sample (sums to 100% within each sample)
BP <- LifeSpan_ALL_MetaData %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L2)) %>%                         # use Level 2 annotation
  filter(ReCluster %in% to_be_ploted) %>%
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  summarise(n = n(), Age_months = first(Age_in_yrs)) %>%
  mutate(freq = n / sum(n) * 100) %>%                           # proportion within CD8 subtypes
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(x = sample_id, y = freq, fill = ReCluster, group = ReCluster)) +
  scale_fill_manual(values = cols) +
  scale_x_discrete(limits = ordered_names) +                    # order individuals by age
  theme(axis.text.y  = element_text(size = 16),
        axis.text.x  = element_text(size = 16, angle = 90),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        legend.position = "none") +
  ylab('% of PBMCs') + xlab('Individuals (n=167)')

# --- Add stream geometry ---
CD8_T <- BP + ggstream::geom_stream(color = 'black',
                                    lwd   = 0.25,
                                    bw    = 1,
                                    type  = "proportional")
print(CD8_T)

######################################################
# B cells
######################################################

# --- Select B cell subtypes to display ---
to_be_ploted <- c('B_memory', 'B_naive')

# --- Compute per-sample proportions of B cell subtypes, then plot ---
# freq = % of cells within the two B cell subtypes per sample (sums to 100% within each sample)
BP <- LifeSpan_ALL_MetaData %>%
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  mutate(ReCluster = factor(LS_L2)) %>%                         # use Level 2 annotation
  filter(ReCluster %in% to_be_ploted) %>%
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  summarise(n = n(), Age_months = first(Age_in_yrs)) %>%
  mutate(freq = n / sum(n) * 100) %>%                           # proportion within B cell subtypes
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(x = sample_id, y = freq, fill = ReCluster, group = ReCluster)) +
  scale_fill_manual(values = cols) +
  scale_x_discrete(limits = ordered_names) +                    # order individuals by age
  theme(axis.text.y  = element_text(size = 16),
        axis.text.x  = element_text(size = 16, angle = 90),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        legend.position = "none") +
  ylab('% of PBMCs') + xlab('Individuals (n=167)')

# --- Add stream geometry ---
Bcells <- BP + ggstream::geom_stream(color = 'black',
                                     lwd   = 0.25,
                                     bw    = 1,
                                     type  = "proportional")
print(Bcells)

# --- Combine the three panels side by side using patchwork ---
BPs <- CD4_T | CD8_T | Bcells
print(BPs)

# --- Save the combined figure as PDF ---
ggsave("./Streamed_LS_L2_clutering_01062026.pdf", BPs,
       width = 4, height = 2, units = "in", scale = 3)