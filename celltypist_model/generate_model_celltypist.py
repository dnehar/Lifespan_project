import scanpy as sc
import celltypist

# This is an example of how to genreate a model from your own data 

# -------------------------------
# Config
# -------------------------------
INPUT_H5AD = '/projects/ucar-lab/USERS/dnehar/jupyter/lifespan/gex_data/old_data/h5ad/LS95_pbmc_10272025.h5ad'
LABEL_COL = 'pbmc_simple_clustering'  # Adjust if needed (Lineage (n=8), pbmc_simple_clustering (n=18), subset_simple_clustering (n=29), Final_annotations (n=42)
MODEL_PATH = './models/lifespan_celltypist_model_Level2.pkl' # add 

# -------------------------------
# 1) Load dataset
# -------------------------------
adata = sc.read(INPUT_H5AD)
print(f"✅ Loaded {INPUT_H5AD} with {adata.n_obs} cells and {adata.n_vars} genes.")
assert LABEL_COL in adata.obs.columns, f"Missing '{LABEL_COL}' in .obs."

# -------------------------------
# 2) Prepare labels
# -------------------------------
labels = adata.obs[LABEL_COL].astype(str).fillna('Unknown')
adata.obs[LABEL_COL] = labels.astype('category')

# -------------------------------
# 3) Train CellTypist model (GPU-enabled)
# -------------------------------
print("🔍 Training CellTypist model...")
model = celltypist.train(
    adata,          # Use as-is (must already be normalized/log1p)
    labels=labels,
    n_jobs=4,
    use_GPU=True,   # Set False if GPU not available
    max_iter=500
)

# -------------------------------
# 4) Save model
# -------------------------------
model.write(MODEL_PATH)
print(f"✅ Model saved to {MODEL_PATH}")
