# =============================================================================
# Figure 1d — PCA on PBMC cell type proportions (Level 2, n=18 clusters)
#
# This script performs a principal component analysis (PCA) on the
# per-sample proportions of PBMC cell types at Level 2 annotation,
# then plots PC1 vs PC2 with 40% confidence ellipses per age group.
#
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Figure_2026/LS_pca_plot_L2_012562026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Color palette — one color per age group ---
cols <- c(
  "Infants"     = "#0072B2",
  "Child"       = "#56B4E9",
  "Adolescent"  = "#009E73",
  "Young"       = "#F0E442",
  "Middle_aged" = "#E69F00",
  "Older"       = "#D55E00",
  "Oldest_old"  = "#CC79A7"
)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# --- Define ordered age groups ---
age_groups <- c('Infants', 'Child', 'Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# --- Compute per-sample cell type proportions at Level 2 (LS_L2) ---
# Frequencies are expressed as % of total cells per sample
proportions <- LifeSpan_ALL_MetaData %>%
  mutate(ReCluster = factor(LS_L2)) %>%
  group_by(sample_id, ReCluster) %>%
  summarise(n = n(), Groups = first(Age_groups), Age_months = first(Age_in_yrs), Lineage = first(lifespan_L1)) %>%
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame()

# --- Reshape to wide format: one row per sample, one column per cell type ---
# Missing combinations are filled with 0
t.proportions <- proportions %>%
  reshape2::dcast(sample_id + Groups + Age_months ~ ReCluster, value.var = "freq", fill = 0)
head(t.proportions)

# --- PCA on cell type proportion matrix ---
# Columns 4:ncol skip the sample_id, Groups, Age_months metadata columns
# center = TRUE: mean-centers each variable before PCA
pca <- prcomp(t.proportions[,4:ncol(t.proportions)], center = T)

# --- Compute variance explained (%) for each PC — used as axis labels ---
d  <- round(pca$sdev^2/sum(pca$sdev^2)*100, digits=1)
xl <- sprintf("PC 1: %.1f %%", d[1])
yl <- sprintf("PC 2: %.1f %%", d[2])
zl <- sprintf("PC 3: %.1f %%", d[3])  # available if needed for PC3 plot

# --- Add PC coordinates back to the sample metadata table ---
t.proportions <- cbind(t.proportions, data.frame("PC1" = as.numeric(pca$x[,1]), "PC2"= as.numeric(pca$x[,2]), "PC3"= as.numeric(pca$x[,3])))

# Factor-order Groups for consistent color/legend ordering
t.proportions$Groups <- factor(t.proportions$Groups, levels = age_groups)
head(t.proportions)

# --- Plot PC1 vs PC2 ---
# Each dot = one sample, colored by age group
# Ellipses show 40% normal-distribution confidence regions per group
plt_pca_L2 <- t.proportions %>%
  ggplot(aes(x = PC1, y = PC2, group = Groups, color = Groups)) +
  geom_point(aes(color = Groups), size = 3) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20)) +
  stat_ellipse(type = "norm", linetype = 1, level = 0.4) +  # 40% normal ellipses
  labs(x=xl,y=yl) +
  ggtitle("PBMCs - Level 2 (n=18) ") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size=20, face="bold"))

plt_pca_L2

# --- Save figure as PDF ---
ggsave("./Figure_2026/LS_pca_plot_L2_012562026.pdf", plt_pca_L2,
       width=3, height=3, units="in", scale=3)