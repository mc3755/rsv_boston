# RSV Emergency Department Surveillance — Boston, MA

## Project Description

This repository analyzes Respiratory Syncytial Virus (RSV)-associated emergency department (ED) visits in Massachusetts using CDC's National Syndromic Surveillance Program (NSSP) BioSense Platform data. RSV disproportionately burdens very young children (<5 years) and older adults (≥65 years), and this analysis emphasizes age-stratified surveillance with a particular focus on the pediatric population.

The pipeline computes seasonal RSV trends, compares age-specific incidence patterns, identifies peak timing across seasons, and produces pediatric-focused visualizations for clinical and public health communication.

---

## Data Sources

| Source | Description | URL |
|--------|-------------|-----|
| CDC NSSP ED Visits | Weekly RSV-related ED visit percentages by state | https://data.cdc.gov/resource/7xva-uux8.json |

---

## Methods Summary

1. **Data acquisition**: CDC NSSP data queried for RSV pathogen in Massachusetts.
2. **Data cleaning**: Columns standardized, dates parsed, MMWR weeks derived, pediatric age groups classified (<5, 5–17, 18–64, 65+).
3. **Analysis**: Season-over-season comparisons, age-specific ED visit rates, peak timing analysis, pediatric burden estimation.
4. **Visualization**: Age-stratified time series, seasonal overlay charts, pediatric-focused heatmap.

---

## Age Group Classification

| Label | Age Range | Clinical Significance |
|-------|-----------|----------------------|
| `<5 years` | Under 5 | Highest RSV hospitalization risk; bronchiolitis burden |
| `5–17 years` | School-age | Lower severity; community transmission vector |
| `18–64 years` | Working-age adults | Mild illness; household transmission |
| `65+ years` | Older adults | Severe RSV risk; RSV vaccine target population |

---

## Repository Structure

```
rsv_boston/
├── README.md
├── .gitignore
├── scripts/
│   ├── 01_data_pull.R          # Query CDC NSSP for RSV data
│   ├── 02_clean.R              # Clean data, classify pediatric age groups
│   ├── 03_analysis.R           # Season comparison, age rates, peak timing
│   └── 04_visualization.R      # Age-stratified series, overlays, heatmap
├── data/                       # Raw and cleaned data (gitignored)
└── output/                     # Tables and figures (gitignored)
```

---

## How to Run

```r
source("scripts/01_data_pull.R")
source("scripts/02_clean.R")
source("scripts/03_analysis.R")
source("scripts/04_visualization.R")
```

---

## Required R Packages

```r
install.packages(c(
  "tidyverse",   # dplyr, ggplot2, tidyr, readr, stringr
  "janitor",     # clean_names()
  "httr",        # HTTP requests
  "jsonlite",    # JSON parsing
  "lubridate",   # Date manipulation
  "MMWRweek",    # MMWR week calculation
  "viridis",     # Color palettes
  "scales",      # Axis formatting
  "patchwork",   # Multi-panel plot composition
  "here"         # Relative paths
))
```

---

## Author

**Tahir Arif, MPH**  
Epidemiologist  
Date: March 2026

---

## Notes

- RSV seasonality in Massachusetts typically peaks from November through February, later than influenza.
- The <5 years age group is the primary surveillance target: ED visits for RSV in this group are a leading indicator of hospital strain.
- Since 2023, RSV vaccines are recommended for adults ≥60 years and pregnant persons; this may alter the 65+ pattern in future seasons.
- Peak timing is defined as the MMWR week with the highest % of ED visits in a given season.
