library(chromVAR); library(SummarizedExperiment); library(Matrix)
library(motifmatchr); library(BSgenome.Hsapiens.UCSC.hg38); library(TFBSTools)

args = commandArgs(trailingOnly = TRUE)
countmatrix = read.table(args[1])
jasparfile = args[2]
outfile = args[3]

#New Code reading from jaspar file
pfm_list <- readJASPARMatrix(jasparfile, matrixClass = "PFM")
jaspar_motifs  <- toPWM(pfm_list)

peaksdf = read.table(text=rownames(countmatrix), sep="_")
colnames(peaksdf) = c("chr", "start", "end")
peakgranges = makeGRangesFromDataFrame(peaksdf)

countmatrix <- as.matrix(countmatrix)

fragment_counts <- SummarizedExperiment(assays = 
                                          list(counts = countmatrix),
                                        rowRanges = peakgranges, colData = DataFrame(celltype = colnames(countmatrix)))

fragment_counts

fragment_counts <- addGCBias(fragment_counts, genome = BSgenome.Hsapiens.UCSC.hg38)

fragment_counts_filtered = filterPeaks(fragment_counts, non_overlapping = TRUE)

motif_ix <- matchMotifs(jaspar_motifs, fragment_counts_filtered, genome = BSgenome.Hsapiens.UCSC.hg38)

dev <- computeDeviations(object = fragment_counts_filtered, annotations = motif_ix)
barcode_deviations = t(dev@assays@data$deviations)

#outputs
write.csv(barcode_deviations, outfile, sep = '\t')

