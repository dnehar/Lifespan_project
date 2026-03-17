
# =============================================================================
# Figure 1b — UMAP plot on PBMC (Level 1, n=9 clusters)
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_PBMCs_LS_L1_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['pbmc']]

 cols <- c(
   "CD4_Tcells" = "#193a1c",
   "CD8_Tcells" = "#f37421",
   "gd_Tcells" = "#80622f",
   "NK_cells" = "#fee000",
   "B_cells" = "#1c9099",
   "PCs" = "#8856a7",
   "monocytes" = "#f6a2a7",
   "DCs" = "#ed2024",
   "HSPC" = "#b0479a")
     
p <- df %>%
    ggplot(aes(x = X_umap1, y = X_umap2, color = LS_L1)) +
    geom_point(size = 0.1) +
    scale_color_manual(values = cols, drop = FALSE) +
    theme_void() +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(title = "PBMCs - Level 1")

print(p)

# save plot
ggsave('./UMAP_PBMCs_LS_L1_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
