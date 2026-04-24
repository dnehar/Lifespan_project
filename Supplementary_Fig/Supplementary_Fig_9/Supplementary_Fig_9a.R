
# ---- User settings
padj_threshold    <- 0.05
log2fc_threshold  <- 0.3

in_dir  <- "./LS_L3/Deqseq_outputs/"     # folder containing your DESeq2 CSV files
out_dir <- "./LS_L3/Deqseq_outputs/"    # where to save plots
dir.create(out_dir, showWarnings = FALSE)

# Vector of files; adjust pattern if needed
files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)


suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(tidyr)
  library(forcats)
})

files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)

sum_list <- lapply(files, function(f) {
  df <- readr::read_csv(f, show_col_types = FALSE)
  nm <- names(df)
  col_padj <- nm[which(tolower(nm) %in% c("padj","adjp","fdr","qvalue"))][1]
  col_lfc  <- nm[which(tolower(nm) %in% c("log2foldchange","log2fc","lfc"))][1]
  if (is.na(col_padj) || is.na(col_lfc))
    stop("Couldn't find padj/log2FoldChange columns in: ", f)
  
  label <- f %>% basename() %>% str_replace("\\.csv$","") %>% str_replace_all("_"," ")
  
  df %>%
    transmute(
      file = label,
      padj = .data[[col_padj]],
      lfc  = .data[[col_lfc]],
      dir  = case_when(
        is.na(padj) ~ "NA",
        padj <= padj_threshold & lfc >=  log2fc_threshold ~ "Up",
        padj <= padj_threshold & lfc <= -log2fc_threshold ~ "Down",
        TRUE ~ "NS"
      )
    ) %>%
    count(file, dir)
})

summary_counts <- bind_rows(sum_list) %>%
  mutate(dir = factor(dir, levels = c("Down","NS","Up")))

# Save the table
write_csv(summary_counts, file.path(out_dir, "summary_counts_all_files.csv"))

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

