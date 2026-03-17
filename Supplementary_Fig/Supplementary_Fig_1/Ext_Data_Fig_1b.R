# =============================================================================
# Supplementary Figure 1b — UMAP plot on PBMC (Level 2, n=18 clusters)
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_PBMCs_LS_L2_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['pbmc']]

cols <- c(
   "B_naive" = "#1c9099",
   "B_memory" = "#283779",
   "CD4_ISGhi" = "#697d35",
   "CD4_memory" = "#90aa3c",
   "CD4_naive" = "#193a1c",
   "CD4_Tregs" = "#137d82",
   "CD8_memory" = "#fba919",
   "CD8_naive" = "#f37421",
   "CD14_mono" = "#f6a2a7",
   "CD16_mono" = "#f9d3d7",
   "Mgk" = "#932169",
   "CD56bright_NK" = "#f2e4a0",
   "CD56dim_NK" = "#fee000",
   "pDCs" = "#a5a4a4")
     
p <- df %>%
    ggplot(aes(x = X_umap1, y = X_umap2, color = LS_L2)) +
    geom_point(size = 0.1) +
    scale_color_manual(values = cols, drop = FALSE) +
    theme_void() +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(title = "PBMCs - Level 2")

print(p)

# save plot
ggsave('./UMAP_PBMCs_LS_L2_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
