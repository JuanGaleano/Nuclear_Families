# Nuclear Families are Widespread in the Global South, but Not as Social Scientists Predicted

Replication repository for the paper:

> Cherlin, A. J., Galeano, J., Pesando, L., & Esteve, A. *Nuclear Families are Widespread in the Global South, but Not as Social Scientists Predicted.*

**Authors**
- [Andrew J. Cherlin](https://soc.jhu.edu/directory/andrew-j-cherlin/) — Department of Sociology, Johns Hopkins University, Baltimore, MD, USA
- Juan Galeano — Centre d'Estudis Demogràfics, Universitat Autònoma de Barcelona, Barcelona, Spain
- Luca Pesando — Division of Social Science, New York University Abu Dhabi
- Albert Esteve — Centre d'Estudis Demogràfics, Universitat Autònoma de Barcelona, Barcelona, Spain

## Contact

For questions about this repository, contact:
- Juan Galeano — jgaleano@ced.uab.es

## Overview

This repository contains the data-processing pipeline, analysis scripts, and outputs for a cross-national study of the **nuclear family household** and its relationship to **educational attainment** across roughly 100 countries between 1970 and 2020. Building on the premise that nuclear family living arrangements were historically expected to spread mainly as a by-product of socioeconomic development, the project tests whether this prediction holds empirically.

The analysis draws on the open-access Global Living Arrangements Database  built at CED:

- **GLAD — Global Living Arrangements Database** [Galeano et al., *Scientific Data*, 2025](https://www.nature.com/articles/s41597-025-05787-y)

which is derived from IPUMS International census and survey microdata and from the EU Labour Fource Survey.

## Methods

- Reconstruction of living arragements from an indivudal-based perspective.
- **Educational gradient (G)** measures: differences in nuclearity rates across education groups, including conditional gradient variants (`G_cond`).
- **Logit-scale transformations** of nuclearity rates to model gradients on a scale appropriate for bounded proportions.
- **Population-weighted regional aggregation** across seven world regions.
- All analysis conducted in **R** (tidyverse-based workflow).

## Data Sources

- **GLAD (Global Living Arrangements Database)** — [Download GLAD - CORESIDENCE_GLAD_2025.Rda](https://zenodo.org/records/15655910)
- **IPUMS International** — [https://international.ipums.org](https://international.ipums.org)
- **EU-LFS** — [European Labour Force Survey](https://ec.europa.eu/eurostat/web/microdata/collections-research/european-union-labour-force-survey)

Raw IPUMS microdata are not redistributed in this repository in accordance with IPUMS data licensing terms. Scripts to reconstruct the analysis files from a user's own IPUMS extract are provided.

## Requirements

- R (≥ 4.x)
- R packages: `tidyverse`, `readxl`, `writexl`, `haven`,`scales`, `countrycode`, `ggrepel`, `giscoR`.

## Citation

If you use this code or the underlying databases, please cite:

> Cherlin, A. J., Galeano, J., Pesando, L., & Esteve, A. (forthcoming). Nuclear Families are Widespread in the Global South, but Not as Social Scientists Predicted.

and, where relevant, the underlying data sources:

> [Galeano, J., et al. (2025). The Global Living Arrangements Database (GLAD). *Scientific Data.*](https://www.nature.com/articles/s41597-025-05787-y)

## License

Add a license (e.g., MIT for code, CC-BY for derived data) appropriate for your repository's contents.

