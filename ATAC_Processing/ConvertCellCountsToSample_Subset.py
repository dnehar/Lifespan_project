import numpy as np
import pandas as pd
import argparse

# Set up command-line argument parser
parser = argparse.ArgumentParser(description='Produces count matrix from multiple samples for select barcodes and peaks')
parser.add_argument("cellcounts")
parser.add_argument("barcodes")
parser.add_argument("outputfile")

# Parse command-line arguments
args = parser.parse_args()
barcodesfile = args.cellcounts
cellcounts = args.cellcounts
outfile = args.outputfile

def convertCellToSampleMatrix(file, barcodesfile):
    """
    Convert per-cell count matrix to per-sample count matrix.
    
    Parameters:
        file: TSV file with cell counts (rows=peaks, columns=cell barcodes)
        barcodesfile: File with list of valid barcodes to filter by
    
    Returns:
        rv1: DataFrame with summed counts per sample
        rv2: DataFrame with fraction of cells with non-zero counts per sample
    """
    # Load cell count data and valid barcodes
    data = pd.read_csv(file, sep='\t', index_col=0)
    barcodes = pd.read_csv(barcodesfile, header=None).values
    
    # Group columns (cell barcodes) by sample ID
    # Assumes column names format: "prefix-1-sampleID"
    #Determine the columns belonging to the same sample
    samplecols = dict()
    for curcol in data.columns:
        if curcol in barcodes:
            sample = curcol.split("-1-")[1]
            if sample not in samplecols:
                samplecols[sample] = []
            samplecols[sample].append(curcol)
        
    # Initialize output matrices: rows=peaks, columns=samples
    rv1 = np.zeros((len(data), len(samplecols.keys())))
    rv2 = np.zeros((len(data), len(samplecols.keys())))

    # Aggregate counts by sample
    colindex = 0
    for cursample in samplecols.keys():
        # rv1: Sum of raw counts across all cells in the sample
        rv1[:,colindex] = data[samplecols[cursample]].sum(1).values
        # rv2: Fraction of cells with non-zero counts in the sample
        rv2[:,colindex] = (data[samplecols[cursample]] > 0).sum(1).values/len(samplecols[cursample])
        colindex += 1
        
    # Convert numpy arrays to DataFrames with proper labels
    rv1 = pd.DataFrame(rv1, columns=samplecols.keys(), index=data.index)
    rv2 = pd.DataFrame(rv2, columns=samplecols.keys(), index=data.index)

    return rv1, rv2


# Main execution: Run conversion and save results
rv1,rv2 = convertCellToSampleMatrix(cellcounts,barcodes)

# Save aggregated counts to output file
rv1.to_csv(outfile, sep="\t")
# Save cell frequency percentages to separate file
rv2.to_csv(outfile+"_percents.txt", sep="\t")
