# chromVAR Deviation Analysis Script
# This script computes chromatin accessibility deviations for transcription factor (TF) motifs
# across ATAC-seq samples using the chromVAR package

# Load required libraries
library(chromVAR); library(SummarizedExperiment); library(Matrix)
library(motifmatchr); library(BSgenome.Hsapiens.UCSC.hg38); library(TFBSTools)

# Parse command-line arguments
args = commandArgs(trailingOnly = TRUE)
countmatrix = read.table(args[1])      # ATAC peak count matrix (peaks x cells)
jasparfile = args[2]                    # JASPAR motif database file
outfile = args[3]                       # Output file path

# Step 1: Load and convert JASPAR motif database from PFM to PWM format
pfm_list <- readJASPARMatrix(jasparfile, matrixClass = "PFM")
jaspar_motifs  <- toPWM(pfm_list)

# Step 2: Parse peak coordinates from row names (format: chr_start_end) and create GRanges object
peaksdf = read.table(text=rownames(countmatrix), sep="_")
colnames(peaksdf) = c("chr", "start", "end")
peakgranges = makeGRangesFromDataFrame(peaksdf)

# Step 3: Convert count matrix to standard matrix format
countmatrix <- as.matrix(countmatrix)

# Step 4: Create SummarizedExperiment object with peak counts and genomic ranges
fragment_counts <- SummarizedExperiment(assays = 
                                          list(counts = countmatrix),
                                        rowRanges = peakgranges, colData = DataFrame(celltype = colnames(countmatrix)))

fragment_counts

# Step 5: Add GC bias corrections based on hg38 genome
fragment_counts <- addGCBias(fragment_counts, genome = BSgenome.Hsapiens.UCSC.hg38)

# Step 6: Filter out overlapping peaks (keep non-overlapping peaks only)
fragment_counts_filtered = filterPeaks(fragment_counts, non_overlapping = TRUE)

# Step 7: Match JASPAR motifs to peaks in the filtered dataset
motif_ix <- matchMotifs(jaspar_motifs, fragment_counts_filtered, genome = BSgenome.Hsapiens.UCSC.hg38)

# Step 8: Compute chromVAR deviations - quantifies how much each cell's chromatin accessibility
# deviates from the expected pattern for each TF motif
dev <- computeDeviations(object = fragment_counts_filtered, annotations = motif_ix)
barcode_deviations = t(dev@assays@data$deviations)  # Transpose to cells x motifs format

# Output: Write deviation scores to TSV file (cells x motifs matrix)
write.csv(barcode_deviations, outfile, sep = '\t')
