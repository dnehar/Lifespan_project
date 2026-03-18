# =============================================================================
# Figure 2e — UMAP plot monocyte subsets
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_DC_subsets_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['Monocytes']]

 cols <- c("moDC" = "#ed2024",
  "cDC1" = "#771215",
  "cDC2" = "#d84598",
  "AXL_DC" = "#a41e21",
  "pDC" = "#a5a4a4")
     
p <- df %>%
    ggplot(aes(x = X_umap1, y = X_umap2, color = Final_annotations)) +
    geom_point(size = 0.1) +
    scale_color_manual(values = cols, drop = FALSE) +
    theme_void() +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(title = "DC subsets")

print(p)

# save plot
ggsave('./UMAP_monocyte_subsets_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
