Imports & setup
import seaborn as sb
from matplotlib import rcParams
import matplotlib
import matplotlib.pyplot as plt
matplotlib.style.use('default')   # reset to default matplotlib style
import warnings
warnings.filterwarnings('ignore')  # suppress non-critical warnings
import celltypist
from celltypist import models

#Load data
########################## load data - all pbmcs ##########################
# AnnData object after integration, doublet removal (Scrublet), and batch correction (Harmony)
harm_pbmc = sc.read('./h5ad/subsets/T_cell_Harmonized_regress_12262025.h5ad')

#CellTypist annotation — model 1 (Gong et al. 2025)
########################### Gong et al. Nature 2025 (PMID: 41162704) ##########################
# Path to custom CellTypist model trained on AIFI level-3 PBMC annotations
model_path = './models/Gong_et_al/ref_pbmc_clean_celltypist_model_AIFI_L3_2024-04-19.pkl'

print("🔍 Annotating target dataset with custom model...")

# Run CellTypist: classifies each cell, then applies majority voting within clusters
# for a more robust label per group rather than per individual cell
predictions = celltypist.annotate(harm_pbmc, model=model_path, majority_voting=True)

# Store the majority-voted labels as a new metadata column in the AnnData object
harm_pbmc.obs['AIFI_L3'] = predictions.predicted_labels['majority_voting']


# CellTypist annotation — model 2 (Balasubramanian et al. 2025)
########################### Balasubramanian et al. Nature Immunology 2025 (PMID: 38134877) ##########################
# Path to a second custom model, focused on CD4+ T cell subsets (Pascual lab)
model_path = './models/cd4_sorted_pascual_celltypist_model.pkl'

print("🔍 Annotating target dataset with custom model...")

# Same annotation approach — adds an independent set of labels from a different reference
predictions = celltypist.annotate(harm_pbmc, model=model_path, majority_voting=True)

# Store Pascual lab labels as a separate metadata column for comparison
harm_pbmc.obs['Pascual_lab'] = predictions.predicted_labels['majority_voting']

########################## save data ##########################
# Write the annotated AnnData object (with both label columns) back to disk
harm_pbmc.write('./h5ad/T_cell_Harmonized_celltypist_12262025.h5ad')

