# =============================================================================
# Figure 5a — UMAP plot CD4 T cell subsets
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_CD4_T_cell_subsets_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['CD4_Tcells']]

cols <- c(
  "CD4_ISGhi"        = "#697d35",
  "CD4_memory"       = "#90aa3c",
  "CD4_naive"        = "#193a1c",
  "CD4_Tregs"        = "#137d82",
  "CD4_Proliferating" = "#2a9d8f"
)


p <- df %>%
  ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = cols, drop = FALSE) +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(title = "CD4 T cell subsets")

print(p)

# save plot
ggsave('./UMAP_CD4_T_cell_subsets_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
