# A single-cell atlas of the healthy human immune system across the lifespan reveals unique infant immune signatures

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

---

## Table of Contents

- [Goal of the study](#goal-of-the-study)
- [Cohort information](#cohort-information-scrnaseq)
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

## Cohort information:

We performed integrated single‑cell RNA‑seq on peripheral blood mononuclear cells from 167 healthy individuals ranging from 2 months to 105 years of age. We further integrated our dataset with previously published single‑cell studies to enhance coverage across the lifespan. We also analyzed chromatin accessibility data using single cell ATAC-seq from 23 donors. 

**Cohort breakdown:**

| Group | scRNAseq (n=167) | scATACseq (n=23) |
|---|---:|---:|
| Infants (2–24 months) | 36 | 9 (infants and children) |
| Children (2–12 years) | 26 |  |
| Adolescents (12–18 years) | 20 | 3 |
| Young adults (18–40 years) | 24 | 6 (young and middle-aged adults) |
| Middle-aged (45–65 years) | 16 |  |
| Older adults (65–85 years) | 33 | 5 |
| Oldest old (85–105 years) | 12 |  |

We also used four publicly available datasets:

- [Deng et al. Nat. Aging 2025](https://pubmed.ncbi.nlm.nih.gov/41136751) ([GSE231906](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE231906)): n=21.
- [Wang et al. Nat. Immunology 2025](https://pubmed.ncbi.nlm.nih.gov/39881000) (syn61609846): n=45.
- [Wang et al. Nat. Commun. 2021](https://pubmed.ncbi.nlm.nih.gov/34521850) ([GSE168732](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE168732)): n=3.
- [Zhong et al. Front. Immunol.](https://pubmed.ncbi.nlm.nih.gov/36703979) ([GSE206295](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE206295)): n=3.


## Data availability - scRNAseq and snATACseq:
**📊 GEO (processed data)**: [GSE233321](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE233321) 

**📁dbGaP (fastq files)**: [phs003259.v1.p1](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs003259.v1.p1) 

## Publications associated with the study:

> Nehar-Belaid et al - Nature Communications (2026): [PMID: 42297833](https://www.nature.com/articles/s41467-026-73729-2)

## Interactive app (R shiny):
[Launch the interactive app](https://dnehar.shinyapps.io/LS_app/)

## Repository structure:

```
Lifespan_project/
├── Figures/          # Scripts used to generate main figures
├── Ext_Data_Fig/          # Scripts used to generate Supplementary Figures
├── GSEA_analysis/          #  Scripts used to perform GSEA  
├── age_associated_changes/          #  Scripts used to perform age associated changes
├── celltypist_models # CellTypist models and related scripts

```
