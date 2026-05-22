# Import required libraries
import numpy as np
import pandas as pd
import argparse

# Set up command-line argument parser
parser = argparse.ArgumentParser(description='Produces count matrix from multiple samples for select barcodes and peaks')
parser.add_argument("cellcounts")  # Input: count matrix file (tab-separated, sample name and fragment file path)
parser.add_argument("barcodes")    # Input: file containing cell barcodes to partition
parser.add_argument("outputfile")  # Output: base filename for output files

# Parse and assign command-line arguments
args = parser.parse_args()
cellcounts = args.cellcounts
barcodes = args.barcodes
outfile = args.outputfile

def partitionMatrix(file, barcodes):
    """
    Partition a count matrix by cell barcodes and summarize cells per sample.
    
    Args:
        file: Path to tab-separated count matrix (rows=peaks, columns=barcodes)
        barcodes: Path to file containing barcodes to extract
    
    Returns:
        Tuple of (filtered count matrix, summary dataframe with cell counts per sample)
    """
    
    # Read the count matrix (first column becomes index, typically peak names)
    data = pd.read_csv(file, sep='\t', index_col=0)
    
    # Read the list of barcodes to partition (no header, single column)
    barcodes = pd.read_csv(barcodes, sep='\t', header=None)
    
    # Extract sample information from barcodes and group by sample
    # Assumes barcode format: "....-1-{SAMPLE_ID}"
    samplecols = dict()  # Dictionary to store barcodes grouped by sample
    for curcol in barcodes[0]:
        # Extract sample ID from barcode (part after "-1-")
        sample = curcol.split("-1-")[1]
        # Initialize list for new samples
        if sample not in samplecols:
            samplecols[sample] = []
        # Add barcode to the corresponding sample
        samplecols[sample].append(curcol)
    
    # Create summary statistics dictionary
    summary = dict()
    summary['Sample'] = []
    summary['Cells'] = []
    
    # Populate summary with sample names and cell counts
    for curkey in samplecols.keys():
        summary['Sample'].append(curkey)
        summary['Cells'].append(len(samplecols[curkey]))

    # Print debug information to verify data dimensions
    print(len(data.columns))      # Total number of columns in count matrix
    print(len(barcodes))           # Total number of barcodes provided
    print(len(data))               # Total number of peaks in count matrix
    print(len(data[barcodes[0]]))  # Number of peaks after filtering to selected barcodes
    
    # Return filtered count matrix (only selected barcodes) and summary dataframe
    return data[barcodes[0]], pd.DataFrame(summary)

# Call partitionMatrix function and unpack results
rv1, rv2 = partitionMatrix(cellcounts, barcodes)

# Save filtered count matrix to output file (tab-separated)
rv1.to_csv(outfile, sep="\t")

# Save partition summary (cell counts per sample) to separate file
rv2.to_csv(outfile + "_partitionSummary.txt", sep="\t")
