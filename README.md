# CHL5222 Final Project

## Analysis of Childhood Growth Trajectories in an Ethiopian Cohort

---

## Overview

This project analyzes longitudinal growth trajectories among children in an Ethiopian cohort using linear mixed-effects models (LMM). The study examines how sex, access to safe drinking water, and baseline age influence Height-for-age Z-score trajectories over time.

---

## Data

The dataset contains repeated measurements of child growth from two cohorts (younger and older), with up to five follow-up rounds per child.

**Key Variables:**
- `childid`: Unique child identifier
- `yc`: Cohort indicator (1 = younger, 0 = older)
- `round`: Follow-up round (1–5)
- `chsex`: Sex (1 = Female, 2 = Male)
- `agemon`: Age in months
- `zhfa`: Height-for-age Z-score
- `drwaterq_new`: Access to safe drinking water (1 = Yes, 0 = No)

---

## Methods

Linear mixed-effects models were used to account for the hierarchical structure of longitudinal data. The analysis includes:

- Descriptive statistics stratified by cohort and round
- Random intercept vs. random slope model comparison
- Model selection via AIC/BIC for interaction terms
- Final model fitting with interaction plots
- Model diagnostics (residuals, Q-Q plots)

---

## File Structure

```
CHL5222_Final_Project/
├── README.md
├── data/
│   ├── original/ethiopia.csv
│   └── processed/
│       ├── ethiopia_processed.csv
│       └── lmmdata.RData
├── R/
│   ├── 1_data_processing.R
│   ├── 2_model_datasets.R
│   ├── 3_table_description.R
│   ├── 4_random_effect.R
│   ├── 5_model_selection_interaction.R
│   ├── 6_model_fitting.R
│   └── 7_diagnostics.R
└── output/
```

---

## Dependencies

Required R packages: `here`, `dplyr`, `lubridate`, `lme4`, `lmerTest`, `gtsummary`, `gt`, `flextable`, `knitr`, `kableExtra`, `texreg`, `ggplot2`, `patchwork`, `ggeffects`, `lattice`, `broom.mixed`, `performance`

---

## Usage

Run the R scripts in sequential order (1 through 7) to reproduce the analysis.

---

## License

This project was completed for CHL5222: Advanced Statistical Methods in Epidemiology at the Dalla Lana School of Public Health, University of Toronto.
