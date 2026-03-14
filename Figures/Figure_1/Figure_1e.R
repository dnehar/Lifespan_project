# =============================================================================
# Figure 1e — PCA loadings: cell type contributions to PC1 (Level 2, n=18 clusters)
#
# This script visualises the contribution (rotation/loading) of each
# Level 2 PBMC cell type to the first principal component (PC1).
# The PCA is performed on per-sample cell type proportions (same as Figure 1d).
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./LS_pca_contrib_PC1_LS_L2_123162025.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

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

# --- Extract PCA rotation matrix (loadings) ---
# pca$rotation contains the contribution of each cell type to each PC
# Each row is a cell type; each column is a PC
contrib <- as.data.frame(pca$rotation)
contrib$celltypes <- rownames(contrib)  # add cell type names as a column

# --- Plot PC1 loadings: cell type contributions to PC1 ---
# Each dot = one cell type, sorted by PC1 loading value
# Positive values push samples to the right along PC1; negative values to the left
# Vertical dashed line at 0 separates positive vs negative contributors
PC1 <- ggplot(contrib, aes(x = PC1, y = reorder(celltypes, PC1))) +
  geom_point(colour='#9e9ac8', size=4) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype="dotted", size=1) +  # reference line at zero loading
  theme(axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20)) +
  ylab('Immune subsets') +
  xlab('PC1')
# ggtitle("PBMC state clusters contributing to PCs")

PC1

# --- Save figure as PDF ---
ggsave("./LS_pca_contrib_PC1_LS_L2_123162025.pdf", PC1,
       width=2, height=3, units="in", scale=3)