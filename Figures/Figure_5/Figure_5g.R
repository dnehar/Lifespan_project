
# =============================================================================
# Figure 5g — UMAP plot T helper subsets
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_T_helpers_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['T_helpers']]

# --- Color palette — one color per memory CD4 T cell subtype (Level 4 annotation) ---
cols <- c(
  'TH2'              = '#1c7b3d',
  'TH17'             = '#3cb54a',
  'CXCR5+_TFH-like'  = '#74c168',
  'TH10'             = '#a4de02ff',
  'TPH'              = '#697d35',
  'GZMK_TH1_like'    = '#7fcdbb',
  'CD4_TEMRA'        = '#1c572b',
  'TH22'             = '#edf8b1'
)

p <- df %>%
  ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = cols, drop = FALSE) +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(title = "T_helper subsets")

print(p)

# save plot
ggsave('./UMAP_T_helpers_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
