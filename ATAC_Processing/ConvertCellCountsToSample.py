# Import required libraries
import numpy as np
import pandas as pd
import argparse

# Set up command-line argument parser
parser = argparse.ArgumentParser(description='Produces count matrix from multiple samples for select barcodes and peaks')
parser.add_argument("cellcounts")  # Input: cell-level count matrix (rows=peaks, columns=barcodes)
parser.add_argument("outputfile")  # Output: base filename for sample-level output files

# Parse and assign command-line arguments
args = parser.parse_args()
cellcounts = args.cellcounts
outfile = args.outputfile

def convertCellToSampleMatrix(file):
    """
    Convert a cell-level count matrix to a sample-level matrix by aggregating counts.
    
    Args:
        file: Path to tab-separated count matrix (rows=peaks, columns=barcodes)
    
    Returns:
        Tuple of (aggregated count matrix, accessibility frequency matrix)
        - First matrix: sum of counts across all cells in each sample
        - Second matrix: fraction of cells with accessibility (counts > 0) per sample
    """
    
    # Read the cell-level count matrix (first column becomes index, typically peak names)
    data = pd.read_csv(file, sep='\t', index_col=0)
    
    # Extract sample information from cell barcodes and group cells by sample
    # Assumes barcode format: "....-1-{SAMPLE_ID}"
    samplecols = dict()  # Dictionary to store barcodes grouped by sample
    for curcol in data.columns:
        # Extract sample ID from barcode (part after "-1-")
        sample = curcol.split("-1-")[1]
        # Initialize list for new samples
        if sample not in samplecols:
            samplecols[sample] = []
        # Add barcode to the corresponding sample
        samplecols[sample].append(curcol)
    
    # Initialize numpy arrays to store aggregated results
    # Dimensions: (number of peaks) x (number of samples)
    rv1 = np.zeros((len(data), len(samplecols.keys())))  # Summed counts per sample
    rv2 = np.zeros((len(data), len(samplecols.keys())))  # Accessibility frequency per sample

    # Iterate through each sample and aggregate cell-level data to sample level
    colindex = 0
    for cursample in samplecols.keys():
        # Sum all counts across cells in this sample (total accessibility)
        rv1[:, colindex] = data[samplecols[cursample]].sum(1).values
        
        # Calculate fraction of cells with non-zero counts (accessibility frequency)
        # (data > 0) creates boolean matrix, .sum(1) counts accessible cells, divide by total cells
        rv2[:, colindex] = (data[samplecols[cursample]] > 0).sum(1).values / len(samplecols[cursample])
        colindex += 1
    
    # Convert numpy arrays back to pandas DataFrames with sample names as column headers
    rv1 = pd.DataFrame(rv1, columns=samplecols.keys(), index=data.index)
    rv2 = pd.DataFrame(rv2, columns=samplecols.keys(), index=data.index)

    return rv1, rv2

# Call convertCellToSampleMatrix function and unpack results
rv1, rv2 = convertCellToSampleMatrix(cellcounts)

# Save aggregated count matrix (total counts per sample) to output file (tab-separated)
rv1.to_csv(outfile, sep="\t")

# Save accessibility frequency matrix (fraction of cells with counts > 0) to separate file
rv2.to_csv(outfile + "_percents.txt", sep="\t")
