# =============================================================================
# Figure 6e — UMAP plot gd T cell subsets
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_gd_Tcells_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['gammadelta_T']]

# --- Color palette — one color per memory CD4 T cell subtype (Level 4 annotation) ---
cols <- c(
  "gdT_Vd2_GZMK"  = "#d29734",
  "gdT_Vd2_GZMB"  = "#d8bd93",
  "gdT_Vd1_SOX4"  = "#56bbbf",
  "gdT_Vd1_KLRF1" = "#993404",
  "gdT_Vd1_Naive" = "#ffeda0"
)

p <- df %>%
  ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = cols, drop = FALSE) +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(title = "gd T cell subsets")

print(p)

# save plot
ggsave('./UMAP_gd_Tcells_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
