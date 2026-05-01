import numpy as np
import pandas as pd
import argparse

parser = argparse.ArgumentParser(description='Produces count matrix from multiple samples for select barcodes and peaks')
parser.add_argument("cellcounts") #sample name, fragment file path
parser.add_argument("barcodes")
parser.add_argument("outputfile")

args = parser.parse_args()
cellcounts = args.cellcounts
barcodes = args.barcodes
outfile = args.outputfile

def partitionMatrix(file, barcodes):
    data = pd.read_csv(file, sep='\t', index_col=0)
    barcodes = pd.read_csv(barcodes, sep='\t', header=None)
    #Determine the columns belonging to the same sample
    samplecols = dict()
    for curcol in barcodes[0]:
        sample = curcol.split("-1-")[1]
        if sample not in samplecols:
            samplecols[sample] = []
        samplecols[sample].append(curcol)
    
    summary = dict()
    summary['Sample'] = []
    summary['Cells'] = []
    
    for curkey in samplecols.keys():
        summary['Sample'].append(curkey)
        summary['Cells'].append(len(samplecols[curkey]))

    print(len(data.columns))
    print(len(barcodes))

    print(len(data))
    print(len(data[barcodes[0]]))
    
    return data[barcodes[0]], pd.DataFrame(summary)

rv1,rv2 = partitionMatrix(cellcounts, barcodes)

rv1.to_csv(outfile, sep="\t")
rv2.to_csv(outfile+"_partitionSummary.txt", sep="\t")



