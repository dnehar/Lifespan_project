# =============================================================================
# Supplementary Fig. 9e — Boxplots of CD4+ and CD8+ T cell SOX4 subtypes 
# across age groups
#
# This script computes frequencies of CD4+ and CD8+ T cell subtypes 
# (naive SOX4+ and naive SOX4-) as a percentage of their respective lineages,
# across all age groups, and displays these distributions as boxplots
# with statistical comparisons between consecutive age groups.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: visualization of CD4 and CD8 naive SOX4 subtype frequencies
# =============================================================================

# Load required libraries for data manipulation and visualization
library(dplyr)    # data wrangling
library(ggplot2)  # plotting framework

# --- LOAD DATA ---
# Load the metadata object from RDS file
# MetaData is a list containing:
#   $meta_small : per-cell metadata (cell type annotations, sample IDs, age groups, etc.)
#   $pheno      : per-sample metadata (sample_id, age, sex, etc.)
MetaData <- readRDS('./pbmcs_v1.rds')

# Extract per-cell metadata and convert to data frame
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# Extract per-sample phenotype metadata
pheno <- MetaData[['pheno']] %>% as.data.frame()

# --- SETUP: Define age groups ---
# Define age groups in chronological order from youngest to oldest
# This ensures proper ordering on the x-axis of plots
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- SETUP: Define color palette ---
# Assign distinct colors to each CD4 and CD8 naive SOX4 subtype
# Colors will be used to distinguish subtypes in the plots
cols <- c(
  "CD8_naive_SOX4+" = "#ffdeadff",  # pale yellow
  "CD8_naive_SOX4-" = "#f37421",    # orange
  "CD4_naive_SOX4-" = "#193a1c",    # dark green
  "CD4_naive_SOX4+" = "#a4de02ff"   # lime green
)

# --- SETUP: Define statistical comparisons ---
# List all pairwise comparisons between consecutive age groups
# These comparisons will be tested for statistical significance and annotated on plots
my_comparisons <- list(
  c('Infants', 'Child'),           # comparison 1
  c('Child', 'Adolescent'),        # comparison 2
  c('Adolescent', 'Young'),        # comparison 3
  c('Young', 'Middle_aged'),       # comparison 4
  c('Middle_aged', 'Older'),       # comparison 5
  c('Older', 'Oldest_old')         # comparison 6
)

# =============================================================================
# PART 1: CD4+ T CELL ANALYSIS
# =============================================================================
# Compute per-sample frequencies of CD4 naive SOX4 subtypes within CD4 lineage
# and generate boxplots with statistical annotations

# Specify which CD4 lineage clusters to include in the analysis
subset_to_be_plotted <- c('CD4_naive', 'CD4_ISGhi', 'CD4_Tregs', 'CD4_memory', 'CD4_Proliferating')

box_plot_lineage_CD4 <- LifeSpan_ALL_MetaData %>%
  # Convert Level 3 cluster annotation to factor with predefined order
  mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%
  
  # Convert age groups to ordered factor (ensures correct x-axis ordering)
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  
  # Keep only cells from CD4 lineage clusters
  filter(ReCluster %in% subset_to_be_plotted) %>%
  
  # Group by age group, sample ID, and Level 4 cluster annotation
  group_by(Groups, sample_id, LS_L4) %>%
  
  # Count cells per group/sample/cluster combination
  summarise(n = n()) %>%
  
  # Calculate frequency as percentage of total cells in CD4 lineage per sample
  mutate(freq = n / sum(n) * 100) %>%
  
  # Ungroup and convert to data frame
  ungroup() %>%
  as.data.frame() %>%
  
  # Retain only naive CD4 SOX4 subtypes (filter out other CD4 clusters)
  filter(LS_L4 %in% c('CD4_naive_SOX4-', 'CD4_naive_SOX4+')) %>%
  
  # --- BEGIN GGPLOT VISUALIZATION ---
  # Map age groups to x-axis, frequency to y-axis, color by SOX4 subtype
  ggplot(aes(x = Groups, y = freq, fill = LS_L4, group = Groups)) +
  
  # Draw boxplot without displaying outlier points
  geom_boxplot(outlier.shape = NA) +
  
  # Overlay individual sample data points with slight horizontal jitter
  geom_jitter(size = 0.2) +
  
  # Use black and white theme
  theme_bw() +
  
  # Add statistical annotations: perform Wilcoxon test for each comparison,
  # display p-values in formatted notation, hide non-significant comparisons
  ggpubr::stat_compare_means(comparisons = my_comparisons, 
                             label = "p.format", 
                             hide.ns = TRUE, 
                             vjust = 0.5) +
  
  # Customize theme: remove legend (redundant with facet labels)
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = 'bold')) +
  
  # Create separate panels for each CD4 SOX4 subtype with independent y-axes
  facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) +
  
  # Apply custom color palette for CD4 subtypes
  scale_fill_manual(values = cols) +
  
  # Customize text and axis styling for publication quality
  theme(axis.text.y  = element_text(size = 12, colour = 'black'),
        axis.text.x  = element_text(size = 12, colour = 'black', angle = 90),
        axis.title.x = element_text(face = "bold", size = 14, colour = 'black'),
        axis.title.y = element_text(face = "bold", size = 14, colour = 'black'),
        strip.text.x = element_text(size = 14, face = 'bold', colour = 'black')) +
  
  # Add axis labels
  ylab('% of lineage') + xlab('Age groups')

# Display the CD4 boxplot
box_plot_lineage_CD4

# =============================================================================
# PART 2: CD8+ T CELL ANALYSIS (including MAIT and γδ T cells)
# =============================================================================
# Compute per-sample frequencies of CD8 naive SOX4 subtypes within CD8 lineage
# and generate boxplots with statistical annotations

# Specify which CD8 lineage clusters to include in the analysis
# Includes: naive, central memory (CM), GZMK+, MAIT, TEMRA, gamma-delta, and αα T cells
subset_to_be_plotted <- c('CD8_naive', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT', 'CD8_TEMRA',
                          'CD8_gdT', 'CD8aa')

box_plot_lineage_CD8 <- LifeSpan_ALL_MetaData %>%
  # Convert Level 3 cluster annotation to factor with predefined order
  mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%
  
  # Convert age groups to ordered factor (ensures correct x-axis ordering)
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  
  # Keep only cells from CD8 lineage clusters
  filter(ReCluster %in% subset_to_be_plotted) %>%
  
  # Group by age group, sample ID, and Level 4 cluster annotation
  group_by(Groups, sample_id, LS_L4) %>%
  
  # Count cells per group/sample/cluster combination
  summarise(n = n()) %>%
  
  # Calculate frequency as percentage of total cells in CD8 lineage per sample
  mutate(freq = n / sum(n) * 100) %>%
  
  # Ungroup and convert to data frame
  ungroup() %>%
  as.data.frame() %>%
  
  # Retain only naive CD8 SOX4 subtypes (filter out other CD8 clusters)
  filter(LS_L4 %in% c('CD8_naive_SOX4-', 'CD8_naive_SOX4+')) %>%
  
  # --- BEGIN GGPLOT VISUALIZATION ---
  # Map age groups to x-axis, frequency to y-axis, color by SOX4 subtype
  ggplot(aes(x = Groups, y = freq, fill = LS_L4, group = Groups)) +
  
  # Draw boxplot without displaying outlier points
  geom_boxplot(outlier.shape = NA) +
  
  # Overlay individual sample data points with slight horizontal jitter
  geom_jitter(size = 0.2) +
  
  # Use black and white theme
  theme_bw() +
  
  # Add statistical annotations: perform Wilcoxon test for each comparison,
  # display p-values in formatted notation, hide non-significant comparisons
  ggpubr::stat_compare_means(comparisons = my_comparisons, 
                             label = "p.format", 
                             hide.ns = TRUE, 
                             vjust = 0.5) +
  
  # Customize theme: remove legend (redundant with facet labels)
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = 'bold')) +
  
  # Create separate panels for each CD8 SOX4 subtype with independent y-axes
  facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) +
  
  # Apply custom color palette for CD8 subtypes
  scale_fill_manual(values = cols) +
  
  # Customize text and axis styling for publication quality
  theme(axis.text.y  = element_text(size = 12, colour = 'black'),
        axis.text.x  = element_text(size = 12, colour = 'black', angle = 90),
        axis.title.x = element_text(face = "bold", size = 14, colour = 'black'),
        axis.title.y = element_text(face = "bold", size = 14, colour = 'black'),
        strip.text.x = element_text(size = 14, face = 'bold', colour = 'black')) +
  
  # Add axis labels
  ylab('% of lineage') + xlab('Age groups')

# Display the CD8 boxplot
box_plot_lineage_CD8
