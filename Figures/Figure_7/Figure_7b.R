library(data.table); library(fgsea)

msigdb.hs = getMsigdb(org = 'hs', id = 'SYM', version = '7.4')

# --- Paths to your files (edit as needed) ---
rnk_file <- "./analysis/gsea/prerank_data_GEX_Naive_CD8_Tcells.rnk" # after runing pyGSEA
#rnk_file <- "./analysis/gsea/prerank_data_CD8_Tcells.rnk" # after runing pyGSEA

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


# ├├ HALLMARKS #### 
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
#pathway_name <- c('HALLMARK_TGF_BETA_SIGNALING')
pathway_name <- c('HALLMARK_TNFA_SIGNALING_VIA_NFKB')
#pathway_name <- c('HALLMARK_TNFA_SIGNALING_VIA_NFKB')


row <- subset(fgsea_res, pathway == pathway_name)[1, ]

p_txt   <- paste0("p = ", format(row$pval, digits = 3, scientific = TRUE))
fdr_txt <- paste0("FDR = ", format(row$padj, digits = 3, scientific = TRUE))
nes_txt <- paste0("NES = ", round(row$NES, 3))

p2 <- plotEnrichment(
  pathway = msigdb_ids$HALLMARK_TNFA_SIGNALING_VIA_NFKB, #**** #pathways[[pathway_name]],
  stats   = ranks
) + ggtitle(pathway_name) + labs( subtitle = paste(nes_txt, p_txt, fdr_txt, sep = "  |  "))

print(p2)

