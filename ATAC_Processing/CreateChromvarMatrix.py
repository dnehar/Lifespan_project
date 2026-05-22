import numpy as np
import pandas as pd
import pysam
import argparse
import tabix
import peakoverlap

# Parse command-line arguments for input/output files
parser = argparse.ArgumentParser(description='Produces count matrix from multiple samples for select barcodes and peaks')
parser.add_argument("sampledatafile") #sample name, fragment file path
parser.add_argument("barcodefile")
parser.add_argument("peakfile")
parser.add_argument("exclusionfile")
parser.add_argument("outputfile")

# Store parsed arguments into variables
args = parser.parse_args()
sampledatafile = args.sampledatafile
peakfile = args.peakfile
exclusionfile = args.exclusionfile
barcodefile = args.barcodefile
outfile = args.outputfile

##
# Load sample data (sample names and paths to fragment files)
sampledata = pd.read_csv(sampledatafile, header=None, sep="\t")#TODO 2 column, 1) Name, 2) path to fragment file
#peaks = pd.read_csv(peakfile, header=None, sep="\t", comment="#")


def getPeaks(summitfile):
    """
    Extract peaks from summit file and create peak regions (±250bp around summit).
    Returns DataFrame with columns: chromosome, start, end, signal value
    """
    summits = pd.read_csv(summitfile, header=None, sep="\t")
    
    # Define chromosomes 1-22 and X
    chromosomes = []
    for i in np.arange(1,23):
        chromosomes.append("chr"+str(i))
    chromosomes.append("chrX")

    rv = []
    
    # Process each chromosome
    for curchr in chromosomes:
        # Filter summits for current chromosome and sort by position
        curselection = summits[summits[0] == curchr]
        curselection = curselection.sort_values(by=[1])
        
        # Create 500bp peaks (±250bp) centered at each summit
        curnum = len(curselection)
        rv.append([curchr, curselection.iloc[0,1]-250, curselection.iloc[0,1]+250, curselection.iloc[0,4]])
        for currow in range(1,curnum):
            cursummit = curselection.iloc[currow, 1]
            cursignal = curselection.iloc[currow, 4]
            curstart = cursummit-250
            curend = cursummit+250
            rv.append([curchr, curstart, curend, cursignal])
    return pd.DataFrame(rv)


def filterExclusionRegions(peaks, exclusionfile):
    """
    Remove peaks that overlap with exclusion regions (e.g., blacklisted regions).
    Returns filtered peak DataFrame.
    """
    exclusionlist_sites = pd.read_csv(exclusionfile, sep='\t', header=None)
    counts,_ =  peakoverlap.getOverlapCount(peaks.values, [exclusionlist_sites.values])
    # Keep only peaks with no overlaps (counts == 0)
    exclusionselection = (counts == 0)
    rv = peaks.loc[exclusionselection]
    return rv

def getBarcodeDictionary(barcodefile, samplenames):
    """
    Create a dictionary mapping barcodes to column indices for each sample.
    Returns:
    - rv: nested dict with structure {sample: {barcode: column_index}}
    - rv2: list of barcodes in order (used as column names)
    """
    barcodes = list(pd.read_csv(barcodefile, header=None).values.ravel())
    rv = dict()
    rv2 = []
    for cursample in samplenames:
        rv[cursample] = dict()
    bcidx = 0
    for curbarcode in barcodes:
        # Only process barcodes containing "-1-" (cell barcodes from 10x)
        if "-1-" not in curbarcode:
            continue
        rv2.append(curbarcode)
        # Extract sample name from barcode format "barcode-1-samplename"
        cursplit = curbarcode.split("-1-")
        cursample = cursplit[1]
        curbarcode2 = cursplit[0]+"-1"
        rv[cursample][curbarcode2] = bcidx
        
        bcidx += 1


    return rv,rv2

def getCountsByBarcode(records, barcodes):
    """
    Count fragments per barcode within a peak region.
    
    @records -  Iterator providing [chr, start, end, barcode] for each fragment
    @barcodes - Dictionary mapping barcodes to column indices
        
    @return - List of [column_index, fragment_count] pairs
    """
    countdict = dict()
    for currecord in records:
        curbc = currecord[3]
        # Only count fragments with barcodes in our barcode dictionary
        if curbc in barcodes:
            colidx = barcodes[curbc]
            if colidx not in countdict:
                countdict[colidx] = 0
            countdict[colidx] = countdict[colidx]+1
            
    # Sort and format results        
    colsortedcounts = sorted(countdict.keys())
    rv = []
    for curcol in colsortedcounts:
        rv.append([curcol, countdict[curcol]])
    return rv


# ============ MAIN EXECUTION ============

# Load and filter peaks: get peaks from summit file, then remove blacklisted regions
peaks = filterExclusionRegions(getPeaks(peakfile), exclusionfile)

# Build barcode dictionary mapping each barcode to its column index
barcodesampledict,colnames =  getBarcodeDictionary(barcodefile,sampledata.values[:,0])

# Get matrix dimensions: rows = peaks, columns = barcodes
nrow = np.shape(peaks)[0]
ncol = len(colnames)

# Print column names (barcodes) for verification
for curcolname in colnames:
    print(curcolname)

# Initialize output count matrix (peaks x barcodes)
countmat = np.zeros((nrow,ncol), dtype=int)

# Create peak names as row identifiers (format: "chr_start_end")
rownames = []
for curpeak in peaks.values:
    rownames.append(curpeak[0]+"_"+str(curpeak[1])+"_"+str(curpeak[2]))

# Process each sample's fragment file
curcolindex = 0
for cursampledata in sampledata.values:
    cursample = cursampledata[0]
    curfragmentfile = cursampledata[1]
    # Get barcodes dictionary for current sample
    curbcsampledict = barcodesampledict[cursample]    
    # Open tabix-indexed fragment file for fast region queries
    tb = tabix.open(curfragmentfile)
    
    # For each peak, query fragments and count by barcode
    rowindex = 0
    for curpeak in peaks.values:
        curchr = curpeak[0]
        curstart = curpeak[1]
        curend = curpeak[2]

        try:
            # Query fragments overlapping this peak region
            curcounts = getCountsByBarcode(tb.query(curchr, curstart, curend), curbcsampledict)

            # Fill in count matrix for this peak
            for curcount in curcounts:
                countmat[rowindex,curcount[0]] = curcount[1]
        except:
            pass
        
        rowindex += 1
        
    
    
# Write output matrix as tab-separated file with peak names (rows) and barcode names (columns)
pd.DataFrame(countmat, index=rownames, columns=colnames).to_csv(outfile, sep="\t")


