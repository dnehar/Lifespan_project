# ============================================================================
# Supplementary Fig. 9f — Scatter plots with linear regression of SOX4 
# CD4+ and CD8+ naive T cell subset proportions across age in infants
# ============================================================================
# NOTE: The title says "barplots" but the script creates scatter plots with
# linear regression lines and correlation coefficients.

library(dplyr); library(ggplot2)

# --- Color palette for SOX4 naive T cell subtypes (Level 4 annotation) ---
# Maps each cell subset to a specific color for consistent visualization
cols <- c(
  "CD8_naive_SOX4+" = "#ffdeadff",  # pale yellow
  "CD8_naive_SOX4-" = "#f37421",    # orange
  "CD4_naive_SOX4-" = "#193a1c",    # dark green
  "CD4_naive_SOX4+" = "#a4de02ff"   # lime green
)

# --- Load metadata from RDS file ---
# Extracts 'meta_small' nested list from the main pbmcs_v1.rds object
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define ordered age groups (youngest to oldest) ---
# Used for consistent factor ordering across visualizations
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Define pairwise comparisons (created but not used in this script) ---
# NOTE: This line defines 'my_comparisons' but it's never actually used in
# the plotting code. ggpubr::stat_compare_means is not called in the plots.
my_comparisons <- combn(age_groups, 2, FUN = list, simplify = T)

# ============================================================================
# PART 1: CD4+ T CELL ANALYSIS
# ============================================================================
# Filters for CD4 naive cells and displays only SOX4+/- subtypes in infants

subset_to_be_plotted <- c('CD4_naive', 'CD4_ISGhi', 'CD4_Tregs', 'CD4_memory', 'CD4_Proliferating')

p_corr_pbmcs_inf_CD4 <- LifeSpan_ALL_MetaData %>%
    # Create factor for level 3 clustering
    mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%
    # Create ordered factor for age groups
    mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
    # Keep only specified CD4 subsets
    filter(ReCluster %in% subset_to_be_plotted) %>%
    # Count cells per group, sample, age, and level 4 annotation
    group_by(Groups, sample_id, Age_in_yrs, LS_L4) %>%
    summarise(n = n()) %>%
    # Calculate frequency as percentage
    mutate(freq = n / sum(n) * 100) %>%
    ungroup() %>%
    as.data.frame() %>%
    # Filter to only SOX4+/- CD4 naive subsets
    filter(LS_L4 %in% c('CD4_naive_SOX4-', 'CD4_naive_SOX4+')) %>%
    # Restrict to infant age group
    filter(Groups %in% c('Infants')) %>%
    
    # Create scatter plot with linear regression
    ggplot(aes(x = Age_in_yrs, y = freq, fill = LS_L4)) +
    geom_point(shape = 21, aes(fill = LS_L4), color = "black", size = 3, stroke = 0.5) +
    geom_smooth(method = "lm", aes(color = LS_L4)) +  # Add linear regression lines
    scale_fill_manual(values = cols) +
    scale_color_manual(values = cols) +
    ggpubr::stat_cor() +  # Display correlation coefficients
    facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) +
    theme_bw() +
    # Customize text (all black)
    theme(
        legend.position = "none",
        strip.text = element_text(size = 13, face = "bold", colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 18, colour = "black"),
        axis.title.y = element_text(face = "bold", size = 18, colour = "black")
    ) +
    ylab('% PBMCs') + xlab('Age (years)')

p_corr_pbmcs_inf_CD4

ggsave("./corplot_SOX4_CD4_T_cells_in_PBMCs_infants_03132026.pdf", p_corr_pbmcs_inf_CD4,
       width = 5, height = 1.18, units = "in", scale = 3)

# ============================================================================
# PART 2: CD8+  T CELL ANALYSIS
# ============================================================================
# Identical workflow to Part 1, but for CD8+ naive T cells with SOX4+/- subtypes

subset_to_be_plotted <- c('CD8_naive', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT', 'CD8_TEMRA', 
                          'CD8_gdT', 'CD8aa')

p_corr_pbmcs_inf_CD8 <- LifeSpan_ALL_MetaData %>%
    mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%
    mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
    filter(ReCluster %in% subset_to_be_plotted) %>%
    group_by(Groups, sample_id, Age_in_yrs, LS_L4) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n) * 100) %>%
    ungroup() %>%
    as.data.frame() %>%
    filter(LS_L4 %in% c('CD8_naive_SOX4-', 'CD8_naive_SOX4+')) %>%
    filter(Groups %in% c('Infants')) %>%
    
    ggplot(aes(x = Age_in_yrs, y = freq, fill = LS_L4)) +
    geom_point(shape = 21, aes(fill = LS_L4), color = "black", size = 3, stroke = 0.5) +
    geom_smooth(method = "lm", aes(color = LS_L4)) +
    scale_fill_manual(values = cols) +
    scale_color_manual(values = cols) +
    ggpubr::stat_cor() +
    facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) +
    theme_bw() +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 13, face = "bold", colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 18, colour = "black"),
        axis.title.y = element_text(face = "bold", size = 18, colour = "black")
    ) +
    ylab('% PBMCs') + xlab('Age (years)')

p_corr_pbmcs_inf_CD8

ggsave("./corplot_SOX4_CD8_T_cells_in_PBMCs_infants_03132026.pdf", p_corr_pbmcs_inf_CD8,
       width = 5, height = 1.18, units = "in", scale = 3)
