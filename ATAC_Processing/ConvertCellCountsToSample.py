import numpy as np
import pandas as pd
import argparse

parser = argparse.ArgumentParser(description='Produces count matrix from multiple samples for select barcodes and peaks')
parser.add_argument("cellcounts") #sample name, fragment file path
parser.add_argument("outputfile")

args = parser.parse_args()
cellcounts = args.cellcounts
outfile = args.outputfile

def convertCellToSampleMatrix(file):
    data = pd.read_csv(file, sep='\t', index_col=0)
    
    #Determine the columns belonging to the same sample
    samplecols = dict()
    for curcol in data.columns:
        sample = curcol.split("-1-")[1]
        if sample not in samplecols:
            samplecols[sample] = []
        samplecols[sample].append(curcol)
        
    rv1 = np.zeros((len(data), len(samplecols.keys())))
    rv2 = np.zeros((len(data), len(samplecols.keys())))

    colindex = 0
    for cursample in samplecols.keys():
        rv1[:,colindex] = data[samplecols[cursample]].sum(1).values
        rv2[:,colindex] = (data[samplecols[cursample]] > 0).sum(1).values/len(samplecols[cursample])
        colindex += 1
        
    rv1 = pd.DataFrame(rv1, columns=samplecols.keys(), index=data.index)
    rv2 = pd.DataFrame(rv2, columns=samplecols.keys(), index=data.index)

    return rv1, rv2

rv1,rv2 = convertCellToSampleMatrix(cellcounts)

rv1.to_csv(outfile, sep="\t")
rv2.to_csv(outfile+"_percents.txt", sep="\t")




