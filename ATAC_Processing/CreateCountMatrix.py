import numpy as np
import pandas as pd
import pysam
import argparse
import tabix
import peakoverlap

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

##
sampledata = pd.read_csv(sampledatafile, header=None, sep="\t")#TODO 2 column, 1) Name, 2) path to fragment file
#peaks = pd.read_csv(peakfile, header=None, sep="\t", comment="#")


def getPeaks(peakfile):
    peaks = pd.read_csv(peakfile, header=None, sep="\t", comment='#')

    chromosomes = []
    for i in np.arange(1,23):
        chromosomes.append("chr"+str(i))
    chromosomes.append("chrX")

    rv = []
    
    for curchr in chromosomes:
        curselection = peaks[peaks[0] == curchr]
        curnum = len(curselection)
        if curnum > 0:
            curselection = curselection.sort_values(by=[1])
        
        for currow in range(curnum):
            curstart = curselection.iloc[currow, 1]
            curend = curselection.iloc[currow, 2]
            rv.append([curchr, curstart, curend])
    return pd.DataFrame(rv)

def filterExclusionRegions(peaks, exclusionfile):
    exclusionlist_sites = pd.read_csv(exclusionfile, sep='\t', header=None)
    counts,_ =  peakoverlap.getOverlapCount(peaks.values, [exclusionlist_sites.values])
    exclusionselection = (counts == 0)
    rv = peaks.loc[exclusionselection]
    return rv



def getBarcodeDictionary(barcodefile, samplenames):
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
        cursplit = curbarcode.split("-1-")
        cursample = cursplit[1]
        curbarcode2 = cursplit[0]+"-1"
        rv[cursample][curbarcode2] = bcidx
        
        bcidx += 1


    return rv,rv2

def getCountsByBarcode(records, barcodes):
    """
    @records -  The record iterator that provides a list of chr, start, end, barcode for each entry
    @barcode - Dictionary of column indices. 
        
    @return - List of [colindex, count]
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


peaks = filterExclusionRegions(getPeaks(peakfile), exclusionfile)

print(peaks)

barcodesampledict,colnames =  getBarcodeDictionary(barcodefile,sampledata.values[:,0])
nrow = np.shape(peaks)[0]
ncol = len(colnames)

for curcolname in colnames:
    print(curcolname)

countmat = np.zeros((nrow,ncol), dtype=int)
rownames = []
for curpeak in peaks.values:
    rownames.append(curpeak[0]+"_"+str(curpeak[1])+"_"+str(curpeak[2]))

curcolindex = 0
for cursampledata in sampledata.values:
    cursample = cursampledata[0]
    curfragmentfile = cursampledata[1]
    curbcsampledict = barcodesampledict[cursample]    
    tb = tabix.open(curfragmentfile)
    
    rowindex = 0
    for curpeak in peaks.values:
        curchr = curpeak[0]
        curstart = curpeak[1]
        curend = curpeak[2]

        try:
            curcounts = getCountsByBarcode(tb.query(curchr, curstart, curend), curbcsampledict)

            for curcount in curcounts:
                countmat[rowindex,curcount[0]] = curcount[1]
        except:
            pass
        
        rowindex += 1
        
    
    
pd.DataFrame(countmat, index=rownames, columns=colnames).to_csv(outfile, sep="\t")



