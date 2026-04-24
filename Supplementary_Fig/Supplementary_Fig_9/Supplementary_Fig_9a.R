# =============================================================================
# Supplementary Figure 9a 
# =============================================================================
# This script is designed to process and analyze data for Supplementary Figure 9a.
# It includes various sections, each with distinct purposes and functionalities.

# Load necessary libraries
library(ggplot2) # For data visualization
library(dplyr)  # For data manipulation

summary_counts <- read.csv('./summary_DEG_L3.csv') # file available here : '/Lifespan_project/age_associated_changes/'

# Barplot grouped by file (NS hidden by default; flip coord for readability)
plot_df <- summary_counts %>% filter(dir %in% c("Up","Down"))

p_all <- ggplot(plot_df, aes(x = fct_reorder(file, n, .fun = max), y = n, fill = dir)) +
  geom_col(width = 0.7, )+
  #position = position_dodge(width = 0.8)) +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(Down = "#4575b4", Up = "#d73027")) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    title = "Differentially expressed genes per contrast",
    subtitle = paste0("padj ≤ ", padj_threshold,
                      ", |log2FC| ≥ ", log2fc_threshold),
    x = NULL, y = "Gene count", fill = "Direction"
  ) +
  theme_minimal(base_size = 12)

print(p_all)

ggsave('../../../Figure_2026/DE/number_DEG_LS_L303022026.pdf', 
       p_all, width = 7, height = 6, dpi = 300)

