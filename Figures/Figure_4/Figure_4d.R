# =============================================================================
# Figure 4d — Scatter plots of B cell subset frequencies vs. age in Infants (Level 4)
#
# This script computes frequencies of four B cell subsets as a percentage of all B cells,
# restricted to the Infants age group, and displays their correlation with age (in months)
# as scatter plots with linear regression fits and Pearson correlation coefficients.
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./corplot_B_cells_in_lineage_infants_03132026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per B cell subtype (Level 4 annotation) ---
cols <- c(
  'B_ISGhi'       = '#9ecae1',
  'B_memory'      = '#283779',
  'B_ABC'         = '#41b8ea',
  'PCs'           = '#8856a7',
  'B_transitional'= '#756bb1',
  'B_naive'       = '#1c9099'
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# Required columns from meta_small: Age_groups, Age_in_months, sample_id, LS_L4
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define the B cell subtypes to plot (Level 4 annotation) ---
subset_to_be_plotted <- c('B_naive', 'B_transitional', 'B_ABC', 'B_memory', 'B_ISGhi', 'PCs')

# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Compute B cell subtype proportions and build correlation scatter plots ---
# Step 1: assign ordered factor levels to B cell subtype (ReCluster) and age group (Groups)
# Step 2: filter to keep only the four B cell subtypes of interest
# Step 3: count cells per age group x donor x age in months x B cell subtype combination
# Step 4: compute frequency as % of B cells within each donor x age group
# Step 5: restrict to Infants age group only
# Step 6: plot scatter with linear regression line and Pearson correlation per B cell subtype


p_corr_lineage_infants <- LifeSpan_ALL_MetaData %>%
  
  mutate(ReCluster = factor(LS_L4, levels = subset_to_be_plotted)) %>% #***
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  summarise(n = n()) %>% 
  #summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(Groups %in% c('Infants')) %>% 
  ggplot(aes(x = Age_in_yrs, y = freq, fill=ReCluster)) +
  geom_point(shape = 21, aes(fill = ReCluster), color = "black", size = 3, stroke = 0.5)+
  geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color=ReCluster)) +
  scale_fill_manual(values=cols) + #**** 
  scale_color_manual(values = cols)+ #****
  ggpubr::stat_cor() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 13, face ='bold')) +
  facet_wrap(.~ReCluster, scales = "free_y", nrow = 1) + #***
  theme_bw() +
  # -------- ALL TEXT IN BLACK 
  theme(
    legend.position = "none",
    
    # Facet strip labels
    strip.text = element_text(size = 13, face = "bold", colour = "black"),
    
    # Axis tick labels
    axis.text.y  = element_text(size = 16, colour = "black"),
    axis.text.x  = element_text(size = 16, colour = "black"),
    
    # Axis titles
    axis.title.x = element_text(face = "bold", size = 18, colour = "black"),
    axis.title.y = element_text(face = "bold", size = 18, colour = "black")
  ) +
  ylab('% Lineage')  + xlab('Age (years)')

p_corr_lineage_infants

ggsave("./corplot_B_cells_in_lineage_infants_03132026.pdf", p_corr_lineage_infants,
       width=5, height=1.18,   units="in", scale=3)
