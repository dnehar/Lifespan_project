
# =============================================================================
# Supplementary Fig. 1b  — Pie char showing platform (3' and 5' 10X platforms) information across age groups
#
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Piechart_Platform_ageGroups_03182026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# MetaData is a list containing:
#   $meta_small : per-cell metadata (cell type annotations, sample IDs, age groups, etc.)
#   $pheno      : per-sample metadata (sample_id, age, sex, etc.)
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()
pheno <- MetaData[['pheno']] %>% as.data.frame()

age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# color palette ---
col_plat <- c('#a8dde3','#fbb36a')

p_plaform <- donor_table %>%
  group_by(Age_groups, Platform) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(freq = n / sum(n) * 100) %>%
  mutate(Age_groups = factor(Age_groups, levels = age_groups)) %>%
  ggplot(aes(x = "", y = freq, fill = Platform)) +
  
  # Pie slices
  geom_bar(stat = "identity", width = 1, color = "white") +
  
  # --- NEW: Add counts in the middle of each slice ---
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 4, color = "black", fontface = "bold") +
  
  coord_polar("y", start = 0) +
  
  facet_wrap(
    . ~ Age_groups,
    scales = "free_y",
    nrow = 2,
    labeller = labeller(Age_groups =
                          c(
                            'Infants'='Infants (n=36): 2m-2y',
                            'Child'='Child (n=26): 2y-12y',
                            'Adolescent'='Adolescent (n=20): 12y-18y',
                            'Young'='Young (n=24): 18y-40y',
                            'Middle_aged'='Middle_aged (n=16): 40y-65y',
                            'Older'='Older (n=33): 65y-85y',
                            'Oldest_old'='Oldest_old (n=12): 85y-105y'
                          ))
  ) +
  scale_fill_manual(values = col_plat) +
  theme_void() +
  theme(strip.text = element_text(size = 14, face = "bold"))

print(p_plaform)

ggsave("./Piechart_Platform_ageGroups_03182026.pdf", p_plaform,
       width=6.2, height=2.2,  units="in", scale=3)
