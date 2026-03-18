
# =============================================================================
# Figure 6a — UMAP plot CD8 T cell subsets
#
# Input:  umaps_coordinates.rds  — available at dnehar/Lifespan_project/umaps_coordinates.rds
# Output: UMAP_CD8_Tcells_13022026.pdf
# =============================================================================

library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['CD8_Tcells']]

# --- Color palette — one color per memory CD4 T cell subtype (Level 4 annotation) ---
# --- Color palette — one color per CD8 T cell subtype (Level 3 annotation) ---
cols <- c(
  "CD8_CM"     = "#f59e2f",
  "CD8_GZMK"   = "#fba919",
  "CD8_MAIT"   = "#fbb36a",
  "CD8_TEMRA"  = "#d28529",
  "CD8_gdT"    = "#80622f",
  "CD8aa"      = "#c46b1c",
  "CD8_naive"  = "#f37421"
)
p <- df %>%
  ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = cols, drop = FALSE) +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(title = "CD8 Tcell subsets")

print(p)

# save plot
ggsave('./UMAP_CD8_Tcells_13022026.pdf', 
       p, width = 7, height = 6, dpi = 300)

