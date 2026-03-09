# A single-cell atlas of the healthy human immune system across the lifespan reveals unique infant immune signatures

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

---

## Table of Contents

- [Goal of the study](#goal-of-the-study)
- [Cohort information (scRNAseq)](#cohort-information-scrnaseq)
- [Publications associated with the study](#publications-associated-with-the-study)
- [Data availability](#data-availability)
- [Interactive app (R shiny)](#interactive-app-r-shiny)
- [Repository structure](#repository-structure)
- [Citation / How to cite](#citation--how-to-cite)
- [Contact](#contact)

---

![UMAP of immune cell types across the human lifespan](https://github.com/user-attachments/assets/a6a21210-2fe6-4456-a946-0d14ac562763)

## Goal of the study:
Our goal is to analyze the infant immune system within the broader context of the human lifespan, with a focus on identifying key features of early‑life immunity.

## Cohort information (scRNAseq):
We performed integrated single‑cell RNA‑seq on peripheral blood mononuclear cells from 95 healthy individuals ranging from 2 months to 105 years of age. We further integrated our dataset with previously published single‑cell studies to enhance coverage across the lifespan.

**Cohort breakdown:**

| Group | scRNAseq (n=95) | snATACseq (n=23) |
|---|---|---|
| Infants | 27 | 8 |
| Children | 23 | 4 |
| Young adults | 18 | 5 |
| Older adults | 27 | 6 |

We also used four publicly available datasets:

- [Deng et al. Nat. Aging 2025](https://pubmed.ncbi.nlm.nih.gov/41136751) ([GSE231906](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE231906)): n=21 samples
- [Wang et al. Nat. Immunology 2025](https://pubmed.ncbi.nlm.nih.gov/39881000) (syn61609846): n=45
- [Wang et al. Nat. Commun. 2021](https://pubmed.ncbi.nlm.nih.gov/34521850) ([GSE168732](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE168732)): n=3
- [Zhong et al. Front. Immunol.](https://pubmed.ncbi.nlm.nih.gov/36703979) ([GSE206295](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE206295)): n=3

## Publications associated with the study:

> Coming soon

## Data availability:
**dbGaP**: [phs003259.v1.p1](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs003259.v1.p1) — available upon acceptance

**GEO**: [GSE233321](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE233321) — available upon acceptance

## Interactive app (R shiny):
[Launch the interactive app](https://dnehar.shinyapps.io/LS_app/)

## Repository structure:

```
Lifespan_project/
├── scripts/          # Analysis scripts
├── data/             # Input data files (not tracked by git)
├── Figures/          # Figure outputs
├── Ext_Data_Fig/     # Extended data figures
├── GSEA_analysis/    # Gene Set Enrichment Analysis files
├── age_associated_changes/  # Age-associated changes analysis
└── celltypist_model/ # CellTypist model files
```

## Citation / How to cite:

> Citation information will be added upon publication. Please check back later.

## Contact:

For questions or feedback, please contact the corresponding author or open an issue in this repository.

> Contact information will be updated upon publication.
