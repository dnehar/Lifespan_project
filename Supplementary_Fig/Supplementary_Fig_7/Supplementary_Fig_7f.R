
# =============================================================================
# Supplementary Fig. 7f & 7g — SOX4 expression in human γδ T cells
# Dataset: GSE149356 (Human γδ T cells, Seurat object)
# Reference: https://www.science.org/doi/10.1126/sciimmunol.abf0125
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# --- Data Loading ------------------------------------------------------------
# Load the pre-processed Seurat object for human γδ T cells (GSE149356)
# Download from GEO: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE149356
GD_T <- readRDS('/GSE149356_Human_GDT_Seurat.rds')

# Update the Seurat object to be compatible with the current version of Seurat
GD_T <- UpdateSeuratObject(GD_T)


# =============================================================================
# Supplementary Figure 7f — Left Panel: SOX4 Feature Plot
# =============================================================================

# FeaturePlot: visualize SOX4 gene expression overlaid on UMAP embedding
# Color scale: light grey (low) → orange (#e6550d, high)
gd_FP <- FeaturePlot(GD_T,
                     features = c('SOX4'),
                     pt.size = 0.5,
                     cols = c("lightgrey", "#e6550d")) &
  theme_void() &
  theme(plot.title = element_text(size = 20,
                                  face = 'italic',
                                  hjust = 0.5,
                                  family = 'Helvetica'))

# Save the FeaturePlot to PDF
ggsave("../PANELS/FP_SOX4_dgTcells_04172025.pdf",
       gd_FP,
       width = 2.5, height = 2, units = "in", scale = 3, dpi = 100)


# =============================================================================
# Supplementary Figure 7f — Right Panel: UMAP Density DimPlot
# =============================================================================

# Base DimPlot colored by group (Adult vs. Newborn)
# Adult: dark purple (#4c459c), Newborn: light blue (#96daf7)
DP_sox4 <- DimPlot(GD_T,
                   cols = c('Adult' = '#4c459c', 'Newborn' = '#96daf7'),
                   group.by = 'group')

# (Unused) Custom color palette using cetcolor "fire" scale — 16 colors
scale.col <- cet_pal(16, name = "fire")

# Overlay 2D kernel density contours on the UMAP, faceted by group
ppp <- DP_sox4[[1]] &
  stat_density_2d(
    aes_string(x = "UMAP_1", y = "UMAP_2", fill = 'group'),
    linewidth = 0.3,
    geom = "density_2d_filled",  # filled contour polygons
    colour = "black",            # contour line color
    alpha = 0.6,                 # transparency of filled contours
    n = 150,                     # grid resolution for density estimation
    h = c(1.2, 1.2)              # bandwidth for kernel density estimation (x, y)
  ) &
  facet_wrap(vars(group), nrow = 1) &       # separate panel per group
  ylab('UMAP_2') & xlab('UMAP_1') &
  scale_fill_manual(values = c('Adult' = '#4c459c', 'Newborn' = '#96daf7')) &
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    strip.text.x = element_text(size = 14, face = 'bold')  # facet label styling
  )

ppp  # display plot


# =============================================================================
# Supplementary Figure 7g — Violin Plots: TRDC and SOX4 Expression
# =============================================================================

# VlnPlot: compare TRDC and SOX4 expression between Adult and Newborn γδ T cells
# - ncol = 2: display two genes side by side
# - pt.size = 0: hide individual data points for a cleaner violin plot
VP_gd <- VlnPlot(GD_T,
                 features = c('TRDC', 'SOX4'),
                 group.by = 'group',
                 ncol = 2,
                 cols = c('Adult' = '#4c459c', 'Newborn' = '#96daf7'),
                 pt.size = 0) &
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

VP_gd  # display plot

# Save the violin plot to PDF
ggsave("./violinplot_gdTcells_GSE149356_04172025.pdf",
       VP_gd,
       width = 1.5, height = 1, units = "in", scale = 3)
