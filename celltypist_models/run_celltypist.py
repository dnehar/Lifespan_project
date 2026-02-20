import seaborn as sb
from matplotlib import rcParams
import matplotlib
import matplotlib.pyplot as plt
matplotlib.style.use('default')  
import warnings
warnings.filterwarnings('ignore')
import celltypist
from celltypist import models

########################## load data - all pbmcs ##########################

# data after integration, scrublet and harmony 
harm_pbmc=sc.read('./h5ad/subsets/T_cell_Harmonized_regress_12262025.h5ad')

########################### Gong et al. Nature 2025 (PMID: 41162704)  ##########################

# AIFI_L3 (level 3 annotations)
model_path = './models/Gong_et_al/ref_pbmc_clean_celltypist_model_AIFI_L3_2024-04-19.pkl'
print("🔍 Annotating target dataset with custom model...")
predictions = celltypist.annotate(harm_pbmc, model=model_path, majority_voting=True)
harm_pbmc.obs['AIFI_L3'] = predictions.predicted_labels['majority_voting']

########################### Balasubramanian et al. Nature Immunology 2025 (PMID: 38134877) ##########################

model_path = './models/cd4_sorted_pascual_celltypist_model.pkl'
print("🔍 Annotating target dataset with custom model...")
predictions = celltypist.annotate(harm_pbmc, model=model_path, majority_voting=True)
harm_pbmc.obs['Pascual_lab'] = predictions.predicted_labels['majority_voting']


########################## save data  ##########################

harm_pbmc.write('./h5ad/T_cell_Harmonized_celltypist_12262025.h5ad') 

