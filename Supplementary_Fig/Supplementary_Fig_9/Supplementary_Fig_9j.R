
library(fgsea)
library(msigdb)
library(ggplot2)


ncd4_de = read.csv('NaiveCD4_DE.csv')
ncd8_de = read.csv('NaiveCD8_DE.csv')

msigdb.hs = getMsigdb(org = 'hs', id = 'SYM', version = '7.4')
cd4rank = sign(ncd4_de$logFC)*-log(ncd4_de$PValue)
cd8rank = sign(ncd8_de$logFC)*-log(ncd8_de$PValue)

ordered_ncd4de = ncd4_de[order(cd4rank),]
ordered_ncd8de = ncd8_de[order(cd8rank),]

ordered_ncd4de_names <- setNames(cd4rank[order(cd4rank)], ordered_ncd4de$X)
ordered_ncd8de_names <- setNames(cd8rank[order(cd8rank)], ordered_ncd8de$X)


set.seed(929)
msigdb_ids = geneIds(subsetCollection(msigdb.hs, 'h'))

ncd4_fgseaRes <- fgsea(pathways = msigdb_ids, 
                       stats    = ordered_ncd4de_names)

ncd8_fgseaRes <- fgsea(pathways = msigdb_ids, 
                       stats    = ordered_ncd8de_names)




orderedresult = ncd4_fgseaRes[order(ncd4_fgseaRes$pval),]
orderedresult$leadingEdge = sapply(orderedresult$leadingEdge, paste, collapse=",")
write.csv(orderedresult, "NCD4_DE_GSEA_HALLMARK.csv")

orderedresult = ncd8_fgseaRes[order(ncd8_fgseaRes$pval),]
orderedresult$leadingEdge = sapply(orderedresult$leadingEdge, paste, collapse=",")
write.csv(orderedresult, "NCD8_DE_GSEA_HALLMARK.csv")

pdf("NCD4_DE_EnrichmentPlot_TGFBeta.pdf", width = 6, height = 4)
plotEnrichment(msigdb_ids$HALLMARK_TGF_BETA_SIGNALING, ordered_ncd4de_names)+labs(title="HALLMARK_TGF_BETA_SIGNALING")
dev.off()

pdf("NCD8_DE_EnrichmentPlot_TGFBeta.pdf", width = 6, height = 4)
plotEnrichment(msigdb_ids$HALLMARK_TGF_BETA_SIGNALING, ordered_ncd8de_names)+labs(title="HALLMARK_TGF_BETA_SIGNALING")
dev.off()


set.seed(929)
msigdb_ids = geneIds(subsetCollection(msigdb.hs, 'c7'))

ncd4_fgseaRes <- fgsea(pathways = msigdb_ids, 
                       stats    = ordered_ncd4de_names)

ncd8_fgseaRes <- fgsea(pathways = msigdb_ids, 
                       stats    = ordered_ncd8de_names)

orderedresult = ncd4_fgseaRes[order(ncd4_fgseaRes$pval),]
orderedresult$leadingEdge = sapply(orderedresult$leadingEdge, paste, collapse=",")
write.csv(orderedresult, "NCD4_DE_GSEA_c7.csv")

orderedresult = ncd8_fgseaRes[order(ncd8_fgseaRes$pval),]
orderedresult$leadingEdge = sapply(orderedresult$leadingEdge, paste, collapse=",")
write.csv(orderedresult, "NCD8_DE_GSEA_c7.csv")

pdf("NCD4_DE_EnrichmentPlot.pdf", width = 6, height = 4)
plotEnrichment(msigdb_ids$GSE1460_CD4_THYMOCYTE_VS_NAIVE_CD4_TCELL_ADULT_BLOOD_UP, ordered_ncd4de_names)+labs(title="GSE1460_CD4_THYMOCYTE_VS_NAIVE_CD4_TCELL_ADULT_BLOOD_UP")
dev.off()

pdf("NCD8_DE_EnrichmentPlot.pdf", width = 6, height = 4)
plotEnrichment(msigdb_ids$GSE1460_CD4_THYMOCYTE_VS_NAIVE_CD4_TCELL_ADULT_BLOOD_UP, ordered_ncd8de_names)+labs(title="GSE1460_CD4_THYMOCYTE_VS_NAIVE_CD4_TCELL_ADULT_BLOOD_UP")
dev.off()


