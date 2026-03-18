# =============================================================================
# Figure 5c — UMAP plot Treg subsets
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_Treg_subsets_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['Tregs']]

# --- Color palette — one color per Treg subtype (Level 4 annotation) ---
cols <- c(
  "Tregs_naive" = "#137d82",
  "Tregs_mem"   = "#56bbbf"
)


p <- df %>%
  ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = cols, drop = FALSE) +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(title = "Treg subsets")

print(p)

# save plot
ggsave('./UMAP_Treg_subsets_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)
