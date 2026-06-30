# Nuclear Families are Widespread in the Global South, but Not as Social Scientists Predicted

Replication repository for the paper:

> Cherlin, A. J., Galeano, J., Pesando, L., & Esteve, A. *Nuclear Families are Widespread in the Global South, but Not as Social Scientists Predicted.*

**Authors**
- Andrew J. Cherlin¹ — Department of Sociology, Johns Hopkins University, Baltimore, MD, USA
- Juan Galeano² — Centre d'Estudis Demogràfics, Universitat Autònoma de Barcelona, Barcelona, Spain
- Luca Pesando³ — Division of Social Science, New York University Abu Dhabi
- Albert Esteve² — Centre d'Estudis Demogràfics, Universitat Autònoma de Barcelona, Barcelona, Spain

*Corresponding author: Andrew J. Cherlin (cherlin@jhu.edu)*

---

## Overview

This repository contains the data-processing pipeline, analysis scripts, and outputs for a cross-national study of the **nuclear family household** and its relationship to **educational attainment** across roughly 100 countries. Building on the premise that nuclear family living arrangements were historically expected to spread mainly as a by-product of socioeconomic development, the project tests whether this prediction holds empirically once household structure is measured systematically and consistently at scale.

The analysis draws on two open-access, harmonized household-structure databases built at CED:

- **CoDB — Coresidence Database** (Esteve et al., *Scientific Data*, 2024)
- **GLAD — Global Living Arrangements Database** (Galeano et al., *Scientific Data*, 2025)

both of which are derived from IPUMS International census and survey microdata.

## Research Questions

1. How prevalent is nuclear household structure across countries of the Global South relative to predictions from classic modernization and family-change theory?
2. Is there a consistent **educational gradient** in nuclear family formation — i.e., does the likelihood of living in a nuclear household rise or fall with individual/household educational attainment?
3. Does this gradient vary systematically by world region, and what does that variation imply about the assumed link between development, education, and family nuclearization?

## Methods

- Household structure classification (nuclear vs. extended/complex/other) harmonized from IPUMS microdata via CoDB/GLAD.
- **Educational gradient (G)** measures: differences in nuclearity rates across education groups, including conditional gradient variants (`G_cond`).
- **Logit-scale transformations** of nuclearity rates to model gradients on a scale appropriate for bounded proportions.
- **Kitagawa-style decomposition** to separate compositional (education distribution) effects from rate (behavioral) effects.
- **Population-weighted regional aggregation** across seven world regions.
- All analysis conducted in **R** (tidyverse-based workflow).

## Repository Structure

```
.
├── data/
│   ├── raw/                # Pointers/scripts to source IPUMS, CoDB, GLAD extracts (not redistributed)
│   └── processed/          # Harmonized analysis-ready datasets (e.g., gradients_t1_weighted.xlsx)
├── scripts/
│   ├── 01_data_prep.R      # Build country/education/household-structure panel
│   ├── 02_gradients.R      # Compute G and G_cond, logit-scale transforms
│   ├── 03_decomposition.R  # Kitagawa decomposition
│   ├── 04_regional_agg.R   # Population-weighted regional aggregation
│   └── 05_figures_tables.R # Paper figures and tables
├── output/
│   ├── tables/
│   └── figures/
├── README.md
└── LICENSE
```

*(Adjust folder/file names above to match your actual repository contents.)*

## Data Sources

- **IPUMS International** — [https://international.ipums.org](https://international.ipums.org)
- **CoDB (Coresidence Database)** — Esteve et al. 2024, *Scientific Data*
- **GLAD (Global Living Arrangements Database)** — Galeano et al. 2025, *Scientific Data*

Raw IPUMS microdata are not redistributed in this repository in accordance with IPUMS data licensing terms. Scripts to reconstruct the analysis files from a user's own IPUMS extract are provided in `data/raw/`.

## Requirements

- R (≥ 4.x)
- R packages: `tidyverse`, `readxl`, `writexl`, and others as listed in `scripts/00_setup.R`

## Reproducing the Analysis

```r
# 1. Install dependencies
source("scripts/00_setup.R")

# 2. Run the pipeline in order
source("scripts/01_data_prep.R")
source("scripts/02_gradients.R")
source("scripts/03_decomposition.R")
source("scripts/04_regional_agg.R")
source("scripts/05_figures_tables.R")
```

## Citation

If you use this code or the underlying databases, please cite:

> Cherlin, A. J., Galeano, J., Pesando, L., & Esteve, A. (forthcoming). Nuclear Families are Widespread in the Global South, but Not as Social Scientists Predicted.

and, where relevant, the underlying data sources:

> Esteve, A., et al. (2024). The Coresidence Database (CoDB). *Scientific Data.*
> Galeano, J., et al. (2025). The Global Living Arrangements Database (GLAD). *Scientific Data.*

## Contact

For questions about this repository, contact:
- Andrew J. Cherlin — cherlin@jhu.edu
- Juan Galeano — jgaleano@ced.uab.es

## License

Add a license (e.g., MIT for code, CC-BY for derived data) appropriate for your repository's contents.

