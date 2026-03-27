# =============================================================================
# Supplementary Figure 8 — GSEA NES Barplot across Cell Subsets (Level 3)
# =============================================================================
# Description:
#   This script visualises Gene Set Enrichment Analysis (GSEA) results across
#   multiple Level-3 cell subsets from the Lifespan project.
#   For each subset, the top N gene sets ranked by FDR are selected, and their
#   Normalised Enrichment Scores (NES) are displayed as a faceted horizontal
#   bar chart, coloured by -log10(FDR).
#
# Input:
#   - combined_gsea_across_LS_L3_subsets.csv
#     Expected columns: Term, fdr, es, nes, lead_genes, subset
#     (See: dnehar/Lifespan_project/GSEA_analysis/ for how this file is created)
#
# Output:
#   - PDF barplot saved to ./Figure_2026/GSEA/age_changes/
#
# Dependencies:
#   - tidyverse  : data wrangling (dplyr, tidyr, ggplot2, etc.)
#   - ggplot2    : plotting
#   - viridis    : colour scale for FDR
#   - tidytext   : reorder_within() / scale_x_reordered() for per-facet axis ordering
#
# Usage:
#   Set `subset_to_be_plotted` to a character vector of subset names to display.
#   Example:
#     subset_to_be_plotted <- c("CD4_Tmem_1", "CD4_Tmem_2", "CD4_Tmem_3")
# =============================================================================

# ── 0. Load required packages ─────────────────────────────────────────────────
library(tidyverse)   # dplyr, ggplot2, stringr, etc.
library(viridis)     # viridis colour palettes for fill scale
library(tidytext)    # reorder_within() and scale_x_reordered() for faceted axes

# ── 1. Define subsets to be plotted ───────────────────────────────────────────
# Specify which Level-3 cell subsets to include in the figure.
# These names must match the 'subset' column in the input CSV (after cleaning).
# Example:
#   subset_to_be_plotted <- c("CD4_Tmem_1", "CD4_Tmem_2", "CD4_Tmem_3")
# NOTE: The order here controls the left-to-right facet order in the plot.
# subset_to_be_plotted <- c(...)   # <-- define before running the script

# ── 2. Parameters ─────────────────────────────────────────────────────────────
# Number of top gene sets per subset to display (ranked by ascending FDR)
top_n <- 10

# ── 3. Read GSEA results ──────────────────────────────────────────────────────
# Input: combined GSEA results across all Level-3 subsets.
# Columns expected: Term, fdr, es, nes, lead_genes, subset
f  <- "./analysis/gsea/combined_gsea_across_LS_L3_subsets.csv"
df <- read.csv(f, check.names = FALSE)

# Quick sanity checks
str(df)   # verify column names and types
head(df)  # preview first few rows

# ── 4. Clean subset labels ────────────────────────────────────────────────────
# Remove the 'age_changes_' prefix and any trailing date stamp (_YYYYMMDD)
# that may have been appended by upstream processing pipelines.
df <- df %>%
  mutate(
    subset = gsub("^age_changes_", "", subset),          # strip prefix
    subset = gsub("_[0-9]{8}$",    "", subset)           # strip trailing date stamp
  )

head(df)  # confirm cleaning looks correct

# ── 5. Filter, rank, and prepare data for plotting ───────────────────────────
# Steps:
#   (a) Retain only the subsets listed in subset_to_be_plotted
#   (b) Re-apply label cleaning to subset_clean (defensive, in case of residuals)
#   (c) Convert subset_clean to a factor to enforce facet order
#   (d) Compute -log10(FDR) for use as the fill colour; floor at 1e-300 to
#       avoid -Inf from p-values of exactly 0
#   (e) For each subset, keep only the top_n pathways by lowest FDR
#   (f) Reorder Terms within each facet by NES for cleaner visualisation

df_fac <- df %>%
  filter(subset %in% subset_to_be_plotted) %>%
  mutate(
    subset_clean = gsub("^age_changes_", "", subset),
    subset_clean = gsub("_[0-9]{8}$",    "", subset_clean),
    subset_clean = factor(subset_clean, levels = subset_to_be_plotted),  # enforce facet order
    neglog10_fdr = -log10(pmax(fdr, 1e-300))                             # colour scale metric
  ) %>%
  group_by(subset_clean) %>%
  slice_min(order_by = fdr, n = top_n, with_ties = FALSE) %>%   # top N by FDR per subset
  ungroup() %>%
  mutate(Term_re = reorder_within(Term, nes, subset_clean))      # per-facet term ordering by NES

# ── 6. Build the faceted barplot ──────────────────────────────────────────────
# Horizontal bar chart (coord_flip) showing NES per pathway, coloured by
# -log10(FDR). Each facet = one cell subset. Facets share the Y axis scale
# to allow independent per-subset pathway lists.

p_fac <- ggplot(df_fac, aes(x = Term_re, y = nes, fill = neglog10_fdr)) +
  geom_col(width = 0.75, color = "black", linewidth = 0.2) +           # bar outline
  coord_flip() +                                                         # horizontal bars
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +       # zero reference line
  scale_fill_viridis(
    option    = "C",
    direction = 1,
    name      = expression(-log[10]~FDR)                                 # legend title
  ) +
  facet_wrap(
    ~ subset_clean,
    scales = "free_y",   # each facet has its own set of pathways on the Y axis
    nrow   = 1           # all facets in a single row
  ) +
  scale_x_reordered() +  # strip reorder_within() suffix from axis labels
  labs(
    title = "GSEA NES across subsets (Level 3)",
    x     = "Pathway / Term",
    y     = "Normalised Enrichment Score (NES)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold",  colour = "black"),
    axis.text.y      = element_text(size = 8,        colour = "black"),
    axis.text.x      = element_text(size = 8,        colour = "black"),
    axis.title.x     = element_text(face = "bold", size = 8, colour = "black"),
    axis.title.y     = element_text(face = "bold", size = 8, colour = "black")
  )

# ── 7. Display the plot ───────────────────────────────────────────────────────
p_fac

# ── 8. Save to PDF ────────────────────────────────────────────────────────────
# Width scales with the number of subsets so each facet has enough space.
# Adjust `scale` or `height` if labels are clipped.
ggsave(
  filename = "./Figure_2026/GSEA/age_changes/Barplot_gsea_age_changes_CD4_Tmem_top10.pdf",
  plot     = p_fac,
  width    = length(subset_to_be_plotted),  # one unit of width per facet
  height   = 0.8,
  units    = "in",
  scale    = 3,
  dpi      = 100
)