# =============================================================================
# Figure 7e — Barplots of SOX4 CD4 T cell subset proportions across age in infants
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per SOX4 CD4 T cell subtype (Level 4 annotation) ---
cols <- c(
  "CD4_naive_SOX4-" = "#193a1c",
  "CD4_naive_SOX4+" = "#a4de02ff"
)

# Note: CD8 SOX4 subtypes commented out below for reference
#"CD8_naive_SOX4+" = "#ffdeadff",
#"CD8_naive_SOX4-" = "#f37421",

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Define all pairwise comparisons between age groups ---
# Used by ggpubr::stat_compare_means to annotate p-values on the plot
my_comparisons <- combn(age_groups, 2, FUN = list, simplify = T)

# --- Define the SOX4 CD4 T cell subtypes to plot (Level 4 annotation) ---
subset_to_be_plotted <- c('CD4_naive_SOX4-', 'CD4_naive_SOX4+')

p_corr_pbmcs_infants <- LifeSpan_ALL_MetaData %>%
  
  mutate(ReCluster = factor(LS_L4, levels = subset_to_be_plotted)) %>% #***
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
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
  ylab('% PBMCs')  + xlab('Age (years)')

p_corr_pbmcs_infants

ggsave("./corplot_SOX4_CD4_T_cells__in_PBMCs_infants_03132026.pdf", p_corr_pbmcs_infants,
       width=5, height=1.18,   units="in", scale=3)
