# =============================================================================
# Figure 3a — UMAP plot NK subsets
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_NK_subsets_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['NK_cells']]

cols <- c("CD56bright_NK" = "#f2e4a0",
          "CD56dim_NK" = "#fee000",
          "Adaptive_NK" = "#feb24c",
           "Proliferating_NK" = "#ccb72d")

p <- df %>%
  ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = cols, drop = FALSE) +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(title = "NK cell subsets")

print(p)

# save plot
ggsave('./UMAP_NK_subsets_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
