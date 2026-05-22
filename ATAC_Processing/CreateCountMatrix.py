import numpy as np
import pandas as pd
import pysam
import argparse
import tabix
import peakoverlap

# ============================================================================
# ARGUMENT PARSING
# ============================================================================
# Parse command-line arguments for input/output files
parser = argparse.ArgumentParser(description='Produces count matrix from multiple samples for select barcodes and peaks')
parser.add_argument("sampledatafile") #sample name, fragment file path
parser.add_argument("barcodefile")
parser.add_argument("peakfile")
parser.add_argument("exclusionfile")
parser.add_argument("outputfile")

args = parser.parse_args()
sampledatafile = args.sampledatafile
peakfile = args.peakfile
exclusionfile = args.exclusionfile
barcodefile = args.barcodefile
outfile = args.outputfile

# Load sample metadata (sample names and corresponding fragment file paths)
sampledata = pd.read_csv(sampledatafile, header=None, sep="\t")#TODO 2 column, 1) Name, 2) path to fragment file


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

def getPeaks(peakfile):
    """
    Load peaks from a BED-format file and organize them by chromosome.
    Returns a DataFrame with columns [chromosome, start, end] sorted by position.
    """
    peaks = pd.read_csv(peakfile, header=None, sep="\t", comment='#')

    # Build list of chromosomes to process (chr1-chr22, chrX)
    chromosomes = []
    for i in np.arange(1,23):
        chromosomes.append("chr"+str(i))
    chromosomes.append("chrX")

    rv = []
    
    # Process each chromosome and sort peaks by start position
    for curchr in chromosomes:
        curselection = peaks[peaks[0] == curchr]
        curnum = len(curselection)
        if curnum > 0:
            curselection = curselection.sort_values(by=[1])
        
        # Add each peak to output list
        for currow in range(curnum):
            curstart = curselection.iloc[currow, 1]
            curend = curselection.iloc[currow, 2]
            rv.append([curchr, curstart, curend])
    return pd.DataFrame(rv)


def filterExclusionRegions(peaks, exclusionfile):
    """
    Remove peaks that overlap with exclusion regions (e.g., blacklisted regions).
    Returns a DataFrame containing only non-overlapping peaks.
    """
    exclusionlist_sites = pd.read_csv(exclusionfile, sep='\t', header=None)
    counts,_ =  peakoverlap.getOverlapCount(peaks.values, [exclusionlist_sites.values])
    exclusionselection = (counts == 0)
    rv = peaks.loc[exclusionselection]
    return rv


def getBarcodeDictionary(barcodefile, samplenames):
    """
    Create a dictionary mapping barcodes to column indices for each sample.
    Filters barcodes that contain "-1-" (cell barcode identifier format).
    
    Returns:
        - Dictionary mapping sample name -> {barcode -> column index}
        - List of filtered barcodes in order
    """
    barcodes = list(pd.read_csv(barcodefile, header=None).values.ravel())
    rv = dict()
    rv2 = []
    for cursample in samplenames:
        rv[cursample] = dict()
    bcidx = 0
    for curbarcode in barcodes:
        if "-1-" not in curbarcode:
            continue
        rv2.append(curbarcode)
        # Parse barcode format: "barcode-1-samplename"
        cursplit = curbarcode.split("-1-")
        cursample = cursplit[1]
        curbarcode2 = cursplit[0]+"-1"
        rv[cursample][curbarcode2] = bcidx
        
        bcidx += 1

    return rv,rv2


def getCountsByBarcode(records, barcodes):
    """
    Count fragment occurrences for each barcode in a given peak region.
    
    @records - Iterator yielding [chr, start, end, barcode] for each fragment
    @barcodes - Dictionary mapping barcode -> column index
    
    @return - List of [column_index, count] pairs, sorted by column index
    """
    countdict = dict()
    for currecord in records:
        curbc = currecord[3]
        if curbc in barcodes:
            colidx = barcodes[curbc]
            if colidx not in countdict:
                countdict[colidx] = 0
            countdict[colidx] = countdict[colidx]+1
            
    colsortedcounts = sorted(countdict.keys())
    rv = []
    for curcol in colsortedcounts:
        rv.append([curcol, countdict[curcol]])
    return rv


# ============================================================================
# MAIN PROCESSING PIPELINE
# ============================================================================

# Step 1: Load and filter peaks
peaks = filterExclusionRegions(getPeaks(peakfile), exclusionfile)
print(peaks)

# Step 2: Load barcodes and create sample-barcode mapping
barcodesampledict,colnames =  getBarcodeDictionary(barcodefile,sampledata.values[:,0])
nrow = np.shape(peaks)[0]
ncol = len(colnames)

# Print column names (barcodes) for verification
for curcolname in colnames:
    print(curcolname)

# Step 3: Initialize count matrix (rows=peaks, columns=barcodes)
countmat = np.zeros((nrow,ncol), dtype=int)
rownames = []
for curpeak in peaks.values:
    rownames.append(curpeak[0]+"_"+str(curpeak[1])+"_"+str(curpeak[2]))

# Step 4: Populate count matrix by querying fragment files for each sample
curcolindex = 0
for cursampledata in sampledata.values:
    cursample = cursampledata[0]
    curfragmentfile = cursampledata[1]
    curbcsampledict = barcodesampledict[cursample]    
    
    # Open tabix-indexed fragment file for efficient region queries
    tb = tabix.open(curfragmentfile)
    
    # Query each peak region and count fragments per barcode
    rowindex = 0
    for curpeak in peaks.values:
        curchr = curpeak[0]
        curstart = curpeak[1]
        curend = curpeak[2]

        try:
            # Get fragment counts for this peak region
            curcounts = getCountsByBarcode(tb.query(curchr, curstart, curend), curbcsampledict)

            # Populate matrix with counts
            for curcount in curcounts:
                countmat[rowindex,curcount[0]] = curcount[1]
        except:
            pass
        
        rowindex += 1

# Step 5: Write count matrix to output file
pd.DataFrame(countmat, index=rownames, columns=colnames).to_csv(outfile, sep="\t")
