# =============================================================================
# Figure 7a — GSEA plots 
# get sets used: 'GSE1460_CD4_THYMOCYTE_VS_NAIVE_CD4_TCELL_ADULT_BLOOD_UP' and 
# 'HALLMARK_TGF_BETA_SIGNALING'
# Input:  prerank_data_GEX_Naive_CD8_Tcells.rnk  — available at dnehar/Lifespan_project/GSEA_analysis/
# Output: ./Enrichment_plot_TGFb_naive_CD8_Tcells.pdf' or 'Enrichment_plot_THYMOCYTE_naive_CD8_Tcells.pdf'
# =============================================================================

library(data.table); library(fgsea)

msigdb.hs = getMsigdb(org = 'hs', id = 'SYM', version = '7.4')

# --- Paths to your prerank files (Naive_CD4_Tcells or Naive_CD8_Tcells)
rnk_file <- "./analysis/gsea/prerank_data_GEX_Naive_CD8_Tcells.rnk" # see. 'dnehar/Lifespan_project/GSEA_analysis/'
#rnk_file <- "./analysis/gsea/prerank_data_GEX_Naive_CD4_Tcells.rnk" # see. 'dnehar/Lifespan_project/GSEA_analysis/'

# Read .rnk; works whether or not there is a header
rnk_dt <- fread(rnk_file, header = TRUE)
if (ncol(rnk_dt) < 2) stop("The .rnk file must have at least 2 columns: gene, score/stat.")

# If column names are not as expected, standardize them
setnames(rnk_dt, old = names(rnk_dt)[1:2], new = c("gene", "stat"))

# Remove missing values and duplicates (keep the max magnitude stat for duplicates)
rnk_dt <- rnk_dt[!is.na(gene) & !is.na(stat)]
rnk_dt <- rnk_dt[order(abs(stat), decreasing = TRUE)]
rnk_dt <- rnk_dt[!duplicated(gene)]

# Build the named, decreasing‑sorted rank vector
ranks <- rnk_dt[order(stat, decreasing = TRUE)]
ranks <- setNames(ranks$stat, ranks$gene)

# Quick sanity checks
stopifnot(is.numeric(ranks), !is.null(names(ranks)), length(ranks) > 0)

#================================================================================#
# ├├ HALLMARKS #### 
#================================================================================#

set.seed(123)  # for reproducibility
msigdb_ids = geneIds(subsetCollection(msigdb.hs, 'h'))

fgsea_res <- fgsea(
  pathways = msigdb_ids,
  stats    = ranks,
  #minSize  = 15,      # tune: typical 15–500
  #maxSize  = 500,
  #nperm    = 10000    # increase for stable p/FDR; 10k is a common starting point
)

# Order by FDR (ascending) and write results
fgsea_res <- fgsea_res[order(padj, NES, decreasing = c(FALSE, TRUE))]
#fwrite(fgsea_res, file.path(out_dir, "fgsea_results.tsv"), sep = "\t")


# enrichment plot 
pathway_name <- c('HALLMARK_TGF_BETA_SIGNALING')

row <- subset(fgsea_res, pathway == pathway_name)[1, ]

p_txt   <- paste0("p = ", format(row$pval, digits = 3, scientific = TRUE))
fdr_txt <- paste0("FDR = ", format(row$padj, digits = 3, scientific = TRUE))
nes_txt <- paste0("NES = ", round(row$NES, 3))

p2 <- plotEnrichment(
  pathway = msigdb_ids$HALLMARK_TGF_BETA_SIGNALING, #**** #pathways[[pathway_name]],
  stats   = ranks
) + ggtitle(pathway_name) + labs( subtitle = paste(nes_txt, p_txt, fdr_txt, sep = "  |  "))

print(p2)
ggsave("./Enrichment_plot_TGFb_naive_CD8_Tcells.pdf", 
       p2, width=2.5, height=1,  units="in", scale=3, dpi=100

#================================================================================#
# GSE1460_CD4_THYMOCYTE_VS_NAIVE_CD4_TCELL_ADULT_BLOOD_UP
#================================================================================#

# ├├ C7 #### 
set.seed(123)  # for reproducibility
msigdb_ids = geneIds(subsetCollection(msigdb.hs, 'c7'))

fgsea_res <- fgsea(
  pathways = msigdb_ids,
  stats    = ranks,
  #minSize  = 15,      # tune: typical 15–500
  #maxSize  = 500,
  #nperm    = 10000    # increase for stable p/FDR; 10k is a common starting point
)

pathway_name <- 'GSE1460_CD4_THYMOCYTE_VS_NAIVE_CD4_TCELL_ADULT_BLOOD_UP'
row <- subset(fgsea_res, pathway == pathway_name)[1, ]

p_txt   <- paste0("p = ", format(row$pval, digits = 3, scientific = TRUE))
fdr_txt <- paste0("FDR = ", format(row$padj, digits = 3, scientific = TRUE))
nes_txt <- paste0("NES = ", round(row$NES, 3))

p33 <- plotEnrichment(
  pathway = msigdb_ids$GSE1460_CD4_THYMOCYTE_VS_NAIVE_CD4_TCELL_ADULT_BLOOD_UP, #pathways[[pathway_name]],
  stats   = ranks
) + ggtitle(pathway_name) + labs( subtitle = paste(nes_txt, p_txt, fdr_txt, sep = "  |  "))

print(p33)
ggsave("./Enrichment_plot_THYMOCYTE_naive_CD8_Tcells.pdf", 
       p33, width=2.5, height=1,  units="in", scale=3, dpi=100)

