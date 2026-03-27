
# Example of Level 3 clustering: can be found here: dnehar/Lifespan_project/GSEA_analysis/
f <- "./analysis/gsea/combined_gsea_across_LS_L3_subsets.csv"

df <- read.csv(f, check.names = FALSE)
str(df)  # should show columns: Term, fdr, es, nes, lead_genes, subset
head(df)

df <- df %>%
  mutate(
    subset = gsub("^age_changes_", "", subset),
    subset = gsub("_[0-9]{8}$", "", subset)   # remove any trailing _YYYYMMDD
  )
head(df)


top_n <- 10

#df_plot %>% filter(Term %in% c('TGF-beta Signaling', 'Interferon Alpha Response',#'IL-2/STAT5 Signaling',
                            #   'TNF-alpha Signaling via NF-kB','Interferon Gamma Response')) -> df_plot #


#df <- df %>% filter(!subset %in% c('doublets')) 
# Desired facet order
#b_order <- subset_to_be_plotted

df_fac <- df %>%
  filter(subset %in% subset_to_be_plotted) %>%
  mutate(
    subset_clean = gsub("^age_changes_", "", subset),
    subset_clean = gsub("_[0-9]{8}$", "", subset_clean),
    # <-- enforce facet order here
    subset_clean = factor(subset_clean, levels = subset_to_be_plotted),
    neglog10_fdr = -log10(pmax(fdr, 1e-300))
  ) %>%
  group_by(subset_clean) %>%
  slice_min(order_by = fdr, n = top_n, with_ties = FALSE) %>%
  ungroup() %>%
  # reorder terms within each facet by NES
  mutate(Term_re = reorder_within(Term, nes, subset_clean))

p_fac <- ggplot(df_fac, aes(x = Term_re, y = nes, fill = neglog10_fdr)) +
  geom_col(width = 0.75, color = "black", linewidth = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
  scale_fill_viridis(option = "C", direction = 1, name = expression(-log[10]~FDR)) +
  facet_wrap(~ subset_clean, scales = "free_y", nrow = 1) + #***
  scale_x_reordered() +
  labs(
    title = "GSEA NES across subsets",
    x = "Pathway / Term",
    y = "NES"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", colour = 'black'),
    axis.text.y      = element_text(size = 8, colour = 'black'),
    axis.text.x      = element_text(size = 8, colour = 'black'),
    axis.title.x     = element_text(face = "bold", size = 8, colour = 'black'),
    axis.title.y     = element_text(face = "bold", size = 8, colour = 'black')
  )

p_fac


ggsave("./Figure_2026/GSEA/age_changes/Barplot_gsea_age_changes_CD4_Tmem_top10.pdf", 
       p_fac, width=length(subset_to_be_plotted), height=0.8,  units="in", scale=3, dpi=100)

