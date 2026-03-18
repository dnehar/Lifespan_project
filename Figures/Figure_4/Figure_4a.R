# =============================================================================
# Figure 4a — UMAP plot B cell subsets
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_B_cell_subsets_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['Bcells']]

cols <- c("B_transitional" = "#756bb1",
          "B_ABC" = "#41b8ea",
          "B_ISGhi" = "#9ecae1", 
          "B_naive" = "#1c9099",
          "B_memory" = "#283779",
          "PCs" = "#8856a7")

p <- df %>%
  ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = cols, drop = FALSE) +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(title = "NK cell subsets")

print(p)

# save plot
ggsave('./UMAP_B_cell_subsets_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
