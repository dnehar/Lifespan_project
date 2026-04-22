# =============================================================================
# Supplementary Fig. 9f — Barplots of SOX4 CD8 T cell subset proportions across age in infants
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per SOX4 naive T cell subtype (Level 4 annotation) ---
cols <- c(
  "CD8_naive_SOX4+" = "#ffdeadff",  # pale yellow
  "CD8_naive_SOX4-" = "#f37421",    # orange
  "CD4_naive_SOX4-" = "#193a1c",    # dark green
  "CD4_naive_SOX4+" = "#a4de02ff"   # lime green
)


# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Define all pairwise comparisons between age groups ---
# Used by ggpubr::stat_compare_means to annotate p-values on the plot
my_comparisons <- combn(age_groups, 2, FUN = list, simplify = T)


# =============================================================================
# PART 1: CD4+ T CELL ANALYSIS
# =============================================================================
subset_to_be_plotted <- c('CD4_naive', 'CD4_ISGhi', 'CD4_Tregs', 'CD4_memory', 'CD4_Proliferating')

p_corr_pbmcs_inf_CD4 <- LifeSpan_ALL_MetaData %>%
    mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%
    mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
    filter(ReCluster %in% subset_to_be_plotted) %>%
    group_by(Groups, sample_id, Age_in_yrs, LS_L4) %>%
    summarise(n = n()) %>% #, Age_months = first(Age_in_yrs)) %>% #, Set = first(Set)
    mutate(freq = n / sum(n) *100) %>%
    ungroup() %>%
    as.data.frame() %>%
    filter(LS_L4 %in% c('CD4_naive_SOX4-', 'CD4_naive_SOX4+')) %>%
    filter(Groups %in% c('Infants')) %>% 
    ggplot(aes(x = Age_in_yrs, y = freq, fill=LS_L4)) +
    geom_point(shape = 21, aes(fill = LS_L4), color = "black", size = 3, stroke = 0.5)+
    geom_smooth(method = "lm", aes(color=LS_L4)) +
    scale_fill_manual(values=cols) + #**** 
    scale_color_manual(values = cols)+ #****
    ggpubr::stat_cor() +
    theme(legend.position = "none", 
          strip.text = element_text(size = 13, face ='bold')) +
    facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) + #***
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

p_corr_pbmcs_inf_CD4

ggsave("./corplot_SOX4_CD4_T_cells__in_PBMCs_infants_03132026.pdf", p_corr_pbmcs_inf_CD4,
       width=5, height=1.18,   units="in", scale=3)

# =============================================================================
# PART 2: CD8+ T CELL ANALYSIS
# =============================================================================

subset_to_be_plotted <- c('CD8_naive', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT','CD8_TEMRA', 
                          'CD8_gdT', 'CD8aa')
p_corr_pbmcs_inf_CD8 <- LifeSpan_ALL_MetaData %>%
    mutate(ReCluster = factor(LS_L3, levels = order_LS_L3)) %>%
    mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
    filter(ReCluster %in% subset_to_be_plotted) %>%
    group_by(Groups, sample_id, Age_in_yrs, LS_L4) %>%
    summarise(n = n()) %>% #, Age_months = first(Age_in_yrs)) %>% #, Set = first(Set)
    mutate(freq = n / sum(n) *100) %>%
    ungroup() %>%
    as.data.frame() %>%
    filter(LS_L4 %in% c('CD8_naive_SOX4-', 'CD8_naive_SOX4+')) %>%
    filter(Groups %in% c('Infants')) %>% 
    ggplot(aes(x = Age_in_yrs, y = freq, fill=LS_L4)) +
    geom_point(shape = 21, aes(fill = LS_L4), color = "black", size = 3, stroke = 0.5)+
    geom_smooth(method = "lm", aes(color=LS_L4)) +
    scale_fill_manual(values=cols) + #**** 
    scale_color_manual(values = cols)+ #****
    ggpubr::stat_cor() +
    theme(legend.position = "none", 
          strip.text = element_text(size = 13, face ='bold')) +
    facet_wrap(.~LS_L4, scales = "free_y", nrow = 1) + #***
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

p_corr_pbmcs_inf_CD8

ggsave("./corplot_SOX4_CD8_T_cells__in_PBMCs_infants_03132026.pdf", p_corr_pbmcs_inf_CD8,
       width=5, height=1.18,   units="in", scale=3)

