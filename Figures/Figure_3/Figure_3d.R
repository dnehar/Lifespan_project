
library(dplyr); library(ggplot2)

# --- Color palette — one color per NK cell subtype (Level 4 annotation) ---
cols <- c(
  "CD56bright_NK"    = "#f2e4a0",
  "CD56dim_NK"       = "#fee000",
  "Adaptive_NK"      = "#feb24c",
  "Proliferating_NK" = "#ccb72d"
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# Required columns from meta_small: Age_groups, LS_L4
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define the NK cell subtypes to plot (Level 4 annotation) ---
subset_to_be_plotted <- c('CD56dim_NK', 'CD56bright_NK', 'Adaptive_NK', 'Proliferating_NK')


# --- Define ordered age groups (youngest to oldest) ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')


p_corr_lineage_infants <- LifeSpan_ALL_MetaData %>%
  
  mutate(ReCluster = factor(LS_L4, levels = order_LS_L4)) %>% #***
  mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
  filter(ReCluster %in% subset_to_be_plotted) %>% 
  group_by(Groups, sample_id, Age_in_months, ReCluster) %>%
  summarise(n = n()) %>% #, Age_months = first(Age_months), Gender = first(Gender)) %>% #, Set = first(Set)
  #summarise(n = n()) %>% #, Set = first(Set)
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(Groups %in% c('Infants')) %>% 
  ggplot(aes(x = Age_in_months, y = freq, fill=ReCluster)) +
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

ggsave("./corplot_NK_cells_in_lineage_infants_03132026.pdf", p_corr_lineage_infants,
       width=5, height=1.18,   units="in", scale=3)
