library(dplyr)
library(ggplot2)

LS_list <- readRDS("./umaps_coordinates.rds")
df <- LS_list[['NK_cells']]

p <- df %>%
    ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
    geom_point(size = point_size) +
    scale_color_manual(values = cols_vec, drop = FALSE) +
    theme_void() +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(title = subset_name, color = "Final annotations")
  
  # Save if requested
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p, width = width, height = height, dpi = dpi)
    message(sprintf("Saved plot to: %s", save_path))
  }
  
return(p)
