# =============================================================================
# Supplementary Fig. 7f–7g — γδ T cells (GSE149356): SOX4 expression,
#   UMAP density plots by group (Adult vs. Newborn), and violin plots
#
# Data source:
#   Tan et al. (2021) Science Immunology
#   https://www.science.org/doi/10.1126/sciimmunol.abf0125
#   Dataset: GSE149356 — Human γδ T cell Seurat object
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Libraries
# -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(Seurat)    # FeaturePlot, DimPlot, VlnPlot, UpdateSeuratObject
library(cetcolor)  # cet_pal — perceptually uniform color palettes


# -----------------------------------------------------------------------------
# 1. Load & update Seurat object
#    Download GSE149356_Human_GDT_Seurat.rds from GEO (GSE149356) first
# -----------------------------------------------------------------------------
GD_T <- readRDS("GSE149356_Human_GDT_Seurat.rds")
GD_T <- UpdateSeuratObject(GD_T)


# -----------------------------------------------------------------------------
# 2. Supplementary Fig. 7f — Left panel: SOX4 feature plot
# -----------------------------------------------------------------------------

# UMAP colored by SOX4 expression (grey → orange gradient)
gd_FP <- FeaturePlot(
  GD_T,
  features = "SOX4",
  pt.size  = 0.5,
  cols     = c("lightgrey", "#e6550d")
) &
  theme_void() &
  theme(
    plot.title = element_text(
      size   = 20,
      face   = "italic",
      hjust  = 0.5,
      family = "Helvetica"
    )
  )

ggsave(
  "../PANELS/FP_SOX4_dgTcells_04172025.pdf",
  gd_FP,
  width  = 2.5,
  height = 2,
  units  = "in",
  scale  = 3,
  dpi    = 100
)


# -----------------------------------------------------------------------------
# 3. Supplementary Fig. 7f — Right panel: UMAP density plot by group
# -----------------------------------------------------------------------------

# Base DimPlot colored by group (Adult vs. Newborn)
DP_sox4 <- DimPlot(
  GD_T,
  cols     = c("Adult" = "#4c459c", "Newborn" = "#96daf7"),
  group.by = "group"
)

# Overlay 2D kernel density contours, faceted by group
ppp <- DP_sox4[[1]] &
  stat_density_2d(
    aes_string(x = "UMAP_1", y = "UMAP_2", fill = "group"),
    linewidth = 0.3,
    geom      = "density_2d_filled",
    colour    = "black",
    alpha     = 0.6,
    n         = 150,
    h         = c(1.2, 1.2)
  ) &
  facet_wrap(vars(group), nrow = 1) &
  scale_fill_manual(values = c("Adult" = "#4c459c", "Newborn" = "#96daf7")) &
  xlab("UMAP_1") &
  ylab("UMAP_2") &
  theme(
    legend.position  = "none",
    axis.ticks.x     = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    plot.title       = element_blank(),
    strip.text.x     = element_text(size = 14, face = "bold")
  )

ppp


# -----------------------------------------------------------------------------
# 4. Supplementary Fig. 7g — Violin plots: TRDC and SOX4 by group
# -----------------------------------------------------------------------------

# Side-by-side violin plots for TRDC (γδ TCR marker) and SOX4 (transcription factor)
VP_gd <- VlnPlot(
  GD_T,
  features = c("TRDC", "SOX4"),
  group.by = "group",
  ncol     = 2,
  cols     = c("Adult" = "#4c459c", "Newborn" = "#96daf7"),
  pt.size  = 0   # hide individual data points for clarity
) &
  theme(
    legend.position = "none",
    axis.ticks.x    = element_blank(),
    axis.ticks.y    = element_blank(),
    axis.title.x    = element_blank(),
    axis.title.y    = element_blank()
  )

VP_gd

ggsave(
  "../PANELS/violinplot_gdTcells_GSE149356_04172025.pdf",
  VP_gd,
  width  = 1.5,
  height = 1,
  units  = "in",
  scale  = 3
)
